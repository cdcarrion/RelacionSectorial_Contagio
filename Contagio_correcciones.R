# =====================================================================
# CORRECCIONES — cronología real (ANIO FISCAL + MES FISCAL, no PERIODO)
# y relaciones Top-N por peso (no umbral absoluto). Reemplaza la
# construcción de ventas_cluster / z_shock de Paso 2 y la trazabilidad
# de Paso 4. Requiere Tab2_full y W_df ya calculados.
# =====================================================================

library(dplyr)
library(lubridate)
library(zoo)
library(purrr)

# ---- A. Diagnósticos rápidos antes de seguir -----------------------------

# ¿Algún codigo_actividad de la bitácora mapea a más de un CLUSTER?
# (922 filas / 882 codigo_actividad distintos -> hay duplicados; hay que
# saber si son duplicados inofensivos o contradicciones reales)
dta_bitacora2026 %>%
  count(codigo_actividad) %>%
  filter(n > 1) %>%
  nrow() %>%
  { cat("codigo_actividad con más de 1 fila:", ., "\n") }

dta_bitacora2026 %>%
  group_by(codigo_actividad) %>%
  summarise(n_clusters = n_distinct(CLUSTER), .groups = "drop") %>%
  filter(n_clusters > 1) %>%
  { cat("codigo_actividad que mapean a MÁS DE UN clúster distinto:", nrow(.), "\n"); print(.) }

# ---- B. Fecha real, construida desde ANIO FISCAL + MES FISCAL -----------
# PERIODO no rellena el mes con cero (ene 2022 = "20221", oct 2022 =
# "202210"), así que ordenar o comparar por PERIODO directamente rompe la
# cronología en cada cruce de año. Usamos los campos limpios en su lugar.

periodo_fecha <- dta_empresas_ventas %>%
  distinct(PERIODO, `ANIO FISCAL`, `MES FISCAL`) %>%
  mutate(fecha = as.Date(sprintf("%d-%02d-01", `ANIO FISCAL`, `MES FISCAL`)))

# ---- C. Reconstruir ventas_cluster con cronología correcta ----------------

ventas_cluster <- Tab2_full %>%
  distinct(PERIODO, CLUSTER_ORIGEN, total) %>%
  left_join(periodo_fecha %>% select(PERIODO, fecha), by = "PERIODO") %>%
  arrange(CLUSTER_ORIGEN, fecha)

# Variación interanual con match EXPLÍCITO de fecha (no lag posicional),
# para no asumir que la serie no tiene meses faltantes.
referencia_12m <- ventas_cluster %>%
  select(CLUSTER_ORIGEN, fecha, total) %>%
  mutate(fecha_objetivo = fecha %m+% months(12)) %>%
  select(CLUSTER_ORIGEN, fecha_objetivo, total_hace_12 = total)

ventas_cluster <- ventas_cluster %>%
  left_join(referencia_12m,
            by = c("CLUSTER_ORIGEN", "fecha" = "fecha_objetivo")) %>%
  mutate(var_interanual = (total - total_hace_12) / total_hace_12)

# ---- D. Choque con ventana móvil (sin mirar el futuro) --------------------

ventana <- 12

ventas_cluster <- ventas_cluster %>%
  group_by(CLUSTER_ORIGEN) %>%
  arrange(fecha) %>%
  mutate(
    media_movil = rollapply(var_interanual, ventana, mean, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    sd_movil    = rollapply(var_interanual, ventana, sd, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    z_shock_rolling = (var_interanual - lag(media_movil, 1)) / lag(sd_movil, 1),
    z_next = lead(z_shock_rolling, 1)
  ) %>%
  ungroup()

# ---- E. CEI recalculado con la cronología corregida -----------------------
# (reutiliza W_full de Paso 2; si no lo tienes en el entorno, vuelve a
# correr esa sección de Contagio_paso2.R)

calcular_cei <- function(fecha_i, datos, W){
  shocks <- datos %>% filter(fecha == fecha_i) %>%
    select(CLUSTER_ORIGEN, z = z_shock_rolling) %>%
    tibble::deframe()
  shocks <- shocks[colnames(W)]
  shocks[is.na(shocks)] <- 0
  cei <- W %*% shocks
  tibble::tibble(CLUSTER = rownames(W), fecha = fecha_i, CEI = as.numeric(cei))
}

fechas <- sort(unique(ventas_cluster$fecha))
cei_mensual <- map_df(fechas, calcular_cei, datos = ventas_cluster, W = W_full)

# ---- F. Relaciones Top-N por clúster de origen (no umbral absoluto) ------
# Los pesos son naturalmente chicos cuando hay ~40+ clústeres destino
# posibles -- un umbral fijo tipo 0.05 puede dejar fuera casi todo o
# quedarse con relaciones-ruido según el clúster. Mejor usar el Top(a)
# que ya propone el documento (sección 5.5): las k relaciones más fuertes
# de CADA clúster de origen, sin importar su valor absoluto.

top_n_relaciones <- 5   # AJUSTA

relaciones <- W_df %>%
  filter(CLUSTER_ORIGEN != CLUSTER_DESTINO) %>%
  group_by(CLUSTER_ORIGEN) %>%
  slice_max(peso, n = top_n_relaciones, with_ties = FALSE) %>%
  ungroup()

# Revisa la distribución de esos pesos top antes de seguir -- si el top 1
# de cada clúster ronda 1-2%, probablemente hay que revisar la
# normalización de dta_bce (ver punto 4 de la respuesta).
summary(relaciones$peso)

# ---- G. Trazabilidad y tasa de contagio, con las correcciones -----------

umbral_caida <- -1.5

serie_z <- ventas_cluster %>%
  select(CLUSTER = CLUSTER_ORIGEN, fecha, z = z_shock_rolling, z_next)

eventos_origen <- serie_z %>%
  filter(z <= umbral_caida) %>%
  rename(CLUSTER_ORIGEN = CLUSTER, z_origen = z) %>%
  select(-z_next)

trazabilidad <- eventos_origen %>%
  inner_join(relaciones, by = "CLUSTER_ORIGEN") %>%
  left_join(
    serie_z %>% rename(CLUSTER_DESTINO = CLUSTER,
                        z_destino_mismo_mes = z,
                        z_destino_mes_siguiente = z_next),
    by = c("CLUSTER_DESTINO", "fecha")
  ) %>%
  mutate(
    destino_cae_mismo_mes     = z_destino_mismo_mes     <= umbral_caida,
    destino_cae_mes_siguiente = z_destino_mes_siguiente <= umbral_caida
  ) %>%
  arrange(CLUSTER_ORIGEN, fecha, desc(peso))

tasa_base_por_cluster <- serie_z %>%
  group_by(CLUSTER) %>%
  summarise(tasa_base = mean(z <= umbral_caida, na.rm = TRUE),
            n_meses = sum(!is.na(z)), .groups = "drop") %>%
  rename(CLUSTER_DESTINO = CLUSTER)

tasa_contagio <- trazabilidad %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso) %>%
  summarise(
    eventos_origen     = n(),
    tasa_mismo_mes     = mean(destino_cae_mismo_mes, na.rm = TRUE),
    tasa_mes_siguiente = mean(destino_cae_mes_siguiente, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tasa_base_por_cluster, by = "CLUSTER_DESTINO") %>%
  mutate(
    lift_mismo_mes     = tasa_mismo_mes / tasa_base,
    lift_mes_siguiente = tasa_mes_siguiente / tasa_base
  ) %>%
  filter(eventos_origen >= 5) %>%   # subí el mínimo -- con 3 eventos el lift no es confiable
  arrange(desc(lift_mes_siguiente))

print(tasa_contagio)
