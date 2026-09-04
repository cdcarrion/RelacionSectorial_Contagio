# =====================================================================
# PIPELINE COMPLETO — Relación Sectorial y Contagio (multi-año BCE)
# =====================================================================
# Cambio clave de esta versión: antes se usaba la tabla insumo-producto
# de 2024 como estructura única para TODO el histórico de ventas
# (2022-2026). Eso está mal para 2021-2023, que ya tienen su propia
# tabla publicada por el BCE. Ahora cada año fiscal de ventas se cruza
# con la tabla BCE de SU PROPIO año; solo para años sin tabla publicada
# todavía (2025, 2026 por ahora) se usa la más reciente disponible
# (2024) como aproximación -- exactamente la lógica que describiste.
#
# Para extender esto cuando el BCE publique 2025: agrega el archivo a
# `archivos_bce` más abajo y ya. El resto del script no cambia.
# =====================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)

# =====================================================================
# 1. PARÁMETROS
# =====================================================================

ruta_base <- "C:/Users/crcarrio/OneDrive - Banco Pichincha C.A/Actividades CDCC/4_CorrelacionSectorial"

umbral_prop      <- 0     # corte mínimo de peso w_ij (magnitud, no conteo)
top_n_relaciones <- 5     # relaciones más fuertes por clúster de origen, para el análisis de contagio
umbral_caida     <- -1.5  # z-score para considerar una caída "significativa"
ventana_choque   <- 12    # meses de historia para el z-score móvil
minimo_eventos   <- 5     # mínimo de eventos de origen para confiar en una tasa de contagio

# =====================================================================
# 2. CARGA DE DATOS BASE
# =====================================================================

dta_bce_codigo <- read_excel("C:/Users/crcarrio/Downloads/correlacionador_productos.xlsx")

dta_bitacora2026 <- read_excel(file.path(ruta_base, "AJUSTA_bitacora.xlsx")) %>%
  separate(`ACTIVIDAD ECONÓMICA`,
           into = c("codigo_actividad", "descripcion_actividad"),
           sep = "\\|", remove = FALSE)

dta_empresas_ventas  <- read_excel(file.path(ruta_base, "AJUSTA_ventas.xlsx"))
dta_empresas_cartera <- read_excel(file.path(ruta_base, "AJUSTA_cartera.xlsx"))  # pendiente: paso de mora
dta_empresas_PI      <- read_excel(file.path(ruta_base, "AJUSTA_PI.xlsx"))

# ---- Diagnóstico rápido de la bitácora (correr una vez, revisar salida) ---
n_codigos_ambiguos <- dta_bitacora2026 %>%
  group_by(codigo_actividad) %>%
  summarise(n_clusters = n_distinct(CLUSTER), .groups = "drop") %>%
  filter(n_clusters > 1) %>%
  nrow()
cat("codigo_actividad que mapean a más de un CLUSTER distinto:", n_codigos_ambiguos, "\n")

# =====================================================================
# 3. CARGA MULTI-AÑO DE LA MATRIZ INSUMO-PRODUCTO (hoja "Ud" del BCE)
# =====================================================================
# La hoja "Ud" trae títulos de sección en las primeras 6 filas, el
# encabezado real en la fila 7 (Codigo, Industria, 01..76, demanda
# final...) y una fila de descripciones (fila 8) antes de los datos.
# Columnas 01-76 = consumo intermedio entre industrias (lo que se
# normaliza como w_ij); todo lo que viene después (Total consumo
# intermedio, consumo hogares, gobierno, exportaciones...) es demanda
# final y NO entra en la normalización. Verificado: la suma de 01-76
# calza exacto con la columna "Total consumo intermedio" del BCE.
# Fila 77 = "Compras Directas" (categoría residual del BCE, sin columna
# destino propia): queda con w_ij = 0 automáticamente, no requiere
# tratamiento especial.

cargar_ud <- function(path, anio) {

  ud_raw <- read_excel(path, sheet = "Ud", skip = 6, col_names = FALSE)

  encabezados <- as.character(unlist(ud_raw[1, ]))
  encabezados[1] <- "ID"; encabezados[2] <- "Codigo"; encabezados[3] <- "Industria"

  cols_bce <- sprintf("%02d", 1:76)

  dta <- ud_raw[-c(1, 2), ]
  names(dta) <- encabezados
  dta <- dta %>%
    filter(!is.na(Codigo)) %>%
    select(Codigo, Industria, all_of(cols_bce)) %>%
    mutate(across(all_of(cols_bce), as.numeric))

  dta <- dta %>%
    mutate(.row_sum = rowSums(across(all_of(cols_bce)), na.rm = TRUE)) %>%
    mutate(across(all_of(cols_bce), ~ ifelse(.row_sum == 0, 0, . / .row_sum))) %>%
    select(-.row_sum)

  dta %>%
    pivot_longer(cols = all_of(cols_bce), names_to = "col", values_to = "prop") %>%
    mutate(anio_bce = anio)
}

# AJUSTA rutas / agrega aquí el año que el BCE publique a futuro (ej. 2025)
archivos_bce <- list(
  `2021` = file.path(ruta_base, "mip_2021.xlsm"),
  `2022` = file.path(ruta_base, "mip_2022.xlsm"),
  `2023` = file.path(ruta_base, "mip_2023.xlsm"),
  `2024` = file.path(ruta_base, "mip_2024P.xlsm")
)

dta_long_multi <- map2_dfr(archivos_bce, as.integer(names(archivos_bce)), cargar_ud)

anios_bce_disponibles <- sort(as.integer(names(archivos_bce)))

# Relaciones ordenadas por magnitud, por año de tabla BCE.
frames_por_codigo_multi <- dta_long_multi %>%
  filter(prop > umbral_prop) %>%
  group_by(anio_bce, Codigo, Industria) %>%
  arrange(desc(prop), .by_group = TRUE) %>%
  select(anio_bce, Codigo, Industria, col, prop) %>%
  ungroup()

# ---- Mapeo año fiscal -> año BCE a usar ------------------------------------
# Años con tabla propia usan su propia tabla. Años posteriores al último
# publicado (hoy: 2025, 2026) usan la más reciente disponible (2024) como
# aproximación -- la limitante que describiste, documentada explícitamente
# en vez de quedar implícita.

mapear_anio_bce <- function(anio_fiscal, disponibles = anios_bce_disponibles) {
  vapply(anio_fiscal, function(a) {
    candidatos <- disponibles[disponibles <= a]
    if (length(candidatos) == 0) return(min(disponibles))  # ventas más antiguas que el BCE más viejo cargado
    max(candidatos)
  }, integer(1))
}

# Tabla de referencia rápida para ver qué año BCE se usó por año fiscal:
tibble(anio_fiscal = 2021:2027) %>%
  mutate(anio_bce_usado = mapear_anio_bce(anio_fiscal)) %>%
  print()

# =====================================================================
# 4. RECONSTRUIR VENTAS RELACIONADAS (Rkj = Ventas_k * w_ij), AÑO A AÑO
# =====================================================================

construir_tabla_relacion <- function(anios = NULL, meses = NULL) {

  ventas_filtradas <- dta_empresas_ventas
  if (!is.null(anios)) ventas_filtradas <- ventas_filtradas %>% filter(`ANIO FISCAL` %in% anios)
  if (!is.null(meses)) ventas_filtradas <- ventas_filtradas %>% filter(`MES FISCAL` %in% meses)

  ventas_filtradas <- ventas_filtradas %>%
    mutate(anio_bce = mapear_anio_bce(`ANIO FISCAL`))

  ventas_filtradas %>%
    left_join(
      dta_bce_codigo %>% distinct(CIIU4_6D, `CÓDIGO DE INDUSTRIA ECUATORIANO_CIE`),
      by = c("ACTIVIDAD ECONOMICA" = "CIIU4_6D")
    ) %>%
    left_join(
      frames_por_codigo_multi,
      by = c("CÓDIGO DE INDUSTRIA ECUATORIANO_CIE" = "Codigo", "anio_bce" = "anio_bce")
    ) %>%
    group_by(PERIODO, `ACTIVIDAD ECONOMICA`) %>%
    mutate(TOTAL_VENTAS2 = prop * `TOTAL VENTAS`) %>%
    ungroup()
}

Tab1_full <- construir_tabla_relacion(anios = NULL, meses = NULL)   # todo el histórico disponible

# =====================================================================
# 5. AGREGACIÓN A CLÚSTER (Tab2) Y A CIIU6 (Tab3)
# =====================================================================
# `colapsar_a`: si se indica una columna (p. ej. "CLUSTER"), se dedupea a
#   esa granularidad, repartiendo la venta por partes iguales entre los
#   valores distintos encontrados. `colapsar_a = NULL` no dedupea nada
#   (versión CIIU6) -- ver la nota del bug en el histórico de este script:
#   antes se dedupeaba por `ACTIVIDAD ECONOMICA`, que ya era la variable
#   de agrupación, y eso colapsaba todo el detalle CIIU6 a una sola fila.

repartir_ventas <- function(tab1, colapsar_a = NULL) {

  base <- tab1 %>%
    left_join(
      dta_bce_codigo %>%
        distinct(`CÓDIGO DE INDUSTRIA ECUATORIANO_CIE`, `DESCRIPCIÓN DE CIE`, CIIU4_6D),
      by = c("col" = "CÓDIGO DE INDUSTRIA ECUATORIANO_CIE")
    ) %>%
    left_join(
      dta_bitacora2026 %>%
        distinct(codigo_actividad, CLUSTER, .keep_all = TRUE) %>%
        select(codigo_actividad, descripcion_actividad, CLUSTER),
      by = c("CIIU4_6D" = "codigo_actividad")
    ) %>%
    group_by(PERIODO, `ACTIVIDAD ECONOMICA`, col)

  if (!is.null(colapsar_a)) {
    base <- base %>% distinct(across(all_of(colapsar_a)), .keep_all = TRUE)
  }

  base %>%
    mutate(TOTAL_VENTAS3 = TOTAL_VENTAS2 / n()) %>%
    ungroup()
}

# Entregable 2: por CLUSTER. SECTOR BP == CLUSTER (confirmado), así que
# esto ya es una matriz clúster -> clúster real, sin remapeo adicional.
Tab2_full <- repartir_ventas(Tab1_full, colapsar_a = "CLUSTER") %>%
  group_by(PERIODO, `SECTOR BP`, CLUSTER) %>%
  summarise(TOTAL_VENTAS4 = sum(TOTAL_VENTAS3, na.rm = TRUE), .groups = "drop") %>%
  group_by(PERIODO, `SECTOR BP`) %>%
  mutate(total = sum(TOTAL_VENTAS4, na.rm = TRUE),
         perc  = TOTAL_VENTAS4 / total) %>%
  ungroup() %>%
  rename(CLUSTER_ORIGEN = `SECTOR BP`, CLUSTER_DESTINO = CLUSTER)

# Entregable por CIIU6 (sin dedup -> conserva el detalle)
Tab3_full <- repartir_ventas(Tab1_full, colapsar_a = NULL) %>%
  group_by(PERIODO, `ACTIVIDAD ECONOMICA`, `ACTIVIDAD CIIU`, CIIU4_6D, descripcion_actividad) %>%
  summarise(TOTAL_VENTAS4 = sum(TOTAL_VENTAS3, na.rm = TRUE), .groups = "drop") %>%
  group_by(PERIODO, `ACTIVIDAD ECONOMICA`) %>%
  mutate(total = sum(TOTAL_VENTAS4, na.rm = TRUE),
         perc  = TOTAL_VENTAS4 / total) %>%
  ungroup() %>%
  left_join(
    dta_bitacora2026 %>% distinct(codigo_actividad, CLUSTER, .keep_all = TRUE) %>%
      select(codigo_actividad, CLUSTER),
    by = c("ACTIVIDAD ECONOMICA" = "codigo_actividad")
  )

write_xlsx(Tab2_full, file.path(ruta_base, "5_Documentacion/Req_cluster_multi_anio.xlsx"))
write_xlsx(Tab3_full, file.path(ruta_base, "5_Documentacion/Req_ciiu6_multi_anio.xlsx"))

# =====================================================================
# 6. FECHA REAL (ANIO FISCAL + MES FISCAL, no PERIODO) --------------------
# =====================================================================
# PERIODO no rellena el mes con cero (ene 2022 = "20221", oct 2022 =
# "202210"), lo que rompe la cronología en cada cruce de año si se ordena
# por PERIODO directamente. Se reconstruye desde los campos limpios.

periodo_fecha <- dta_empresas_ventas %>%
  distinct(PERIODO, `ANIO FISCAL`, `MES FISCAL`) %>%
  mutate(fecha    = as.Date(sprintf("%d-%02d-01", `ANIO FISCAL`, `MES FISCAL`)),
         anio_bce = mapear_anio_bce(`ANIO FISCAL`))

# =====================================================================
# 7. ESTRUCTURA DE REFERENCIA (W) PARA EL ANÁLISIS DE CONTAGIO -----------
# =====================================================================
# Para decidir "quién está conectado con quién" en el análisis de
# contagio (Top-N y matriz W) se usa el año BCE MÁS RECIENTE disponible
# (hoy: 2024) -- es la estructura vigente que se proyecta hacia adelante
# para 2025/2026, tal como la planteaste. Los años 2021-2023 SÍ se usaron
# ya correctamente para redistribuir sus propias ventas en Tab2/Tab3
# (paso 4-5); aquí solo se define la red usada para vigilar contagio
# hacia adelante.

anio_bce_referencia <- max(anios_bce_disponibles)

W_df <- Tab2_full %>%
  left_join(periodo_fecha %>% select(PERIODO, anio_bce), by = "PERIODO") %>%
  filter(anio_bce == anio_bce_referencia) %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO) %>%
  summarise(peso = mean(perc, na.rm = TRUE), .groups = "drop")

W_wide <- W_df %>% pivot_wider(names_from = CLUSTER_DESTINO, values_from = peso, values_fill = 0)
W <- W_wide %>% select(-CLUSTER_ORIGEN) %>% as.matrix()
rownames(W) <- W_wide$CLUSTER_ORIGEN

todos <- union(rownames(W), colnames(W))
W_full <- matrix(0, nrow = length(todos), ncol = length(todos), dimnames = list(todos, todos))
W_full[rownames(W), colnames(W)] <- W

relaciones <- W_df %>%
  filter(CLUSTER_ORIGEN != CLUSTER_DESTINO) %>%
  group_by(CLUSTER_ORIGEN) %>%
  slice_max(peso, n = top_n_relaciones, with_ties = FALSE) %>%
  ungroup()

cat("Distribución de pesos en las relaciones Top-", top_n_relaciones, " (año BCE ", anio_bce_referencia, "):\n", sep = "")
print(summary(relaciones$peso))

# =====================================================================
# 8. PANEL DE VENTAS POR CLÚSTER Y CHOQUE MENSUAL --------------------------
# =====================================================================

ventas_cluster <- Tab2_full %>%
  distinct(PERIODO, CLUSTER_ORIGEN, total) %>%
  left_join(periodo_fecha %>% select(PERIODO, fecha), by = "PERIODO") %>%
  arrange(CLUSTER_ORIGEN, fecha)

referencia_12m <- ventas_cluster %>%
  select(CLUSTER_ORIGEN, fecha, total) %>%
  mutate(fecha_objetivo = fecha %m+% months(12)) %>%
  select(CLUSTER_ORIGEN, fecha_objetivo, total_hace_12 = total)

ventas_cluster <- ventas_cluster %>%
  left_join(referencia_12m, by = c("CLUSTER_ORIGEN", "fecha" = "fecha_objetivo")) %>%
  mutate(var_interanual = (total - total_hace_12) / total_hace_12)

ventas_cluster <- ventas_cluster %>%
  group_by(CLUSTER_ORIGEN) %>%
  arrange(fecha) %>%
  mutate(
    media_movil = rollapply(var_interanual, ventana_choque, mean, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    sd_movil    = rollapply(var_interanual, ventana_choque, sd, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    z_shock_rolling = (var_interanual - lag(media_movil, 1)) / lag(sd_movil, 1),
    z_next = lead(z_shock_rolling, 1)
  ) %>%
  ungroup()

# =====================================================================
# 9. ÍNDICE DE EXPOSICIÓN A CONTAGIO (CEI) ---------------------------------
# =====================================================================

calcular_cei <- function(fecha_i, datos, W) {
  shocks <- datos %>% filter(fecha == fecha_i) %>%
    select(CLUSTER_ORIGEN, z = z_shock_rolling) %>% tibble::deframe()
  shocks <- shocks[colnames(W)]
  shocks[is.na(shocks)] <- 0
  cei <- W %*% shocks
  tibble(CLUSTER = rownames(W), fecha = fecha_i, CEI = as.numeric(cei))
}

fechas <- sort(unique(ventas_cluster$fecha))
cei_mensual <- map_df(fechas, calcular_cei, datos = ventas_cluster, W = W_full)

# =====================================================================
# 10. TRAZABILIDAD DE EVENTOS Y TASA DE CONTAGIO ---------------------------
# =====================================================================

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
  filter(eventos_origen >= minimo_eventos) %>%
  arrange(desc(lift_mes_siguiente))

print(tasa_contagio)

# =====================================================================
# 11. EXPLICAR CASOS PUNTUALES (para audiencia no técnica) ----------------
# =====================================================================

explicar_caso <- function(origen, destino) {

  resumen <- tasa_contagio %>% filter(CLUSTER_ORIGEN == origen, CLUSTER_DESTINO == destino)
  if (nrow(resumen) == 0) {
    cat("Ese par no aparece en tasa_contagio (nombre exacto, o quedó fuera por pocos eventos).\n")
    return(invisible(NULL))
  }

  serie_origen  <- ventas_cluster %>% filter(CLUSTER_ORIGEN == origen)  %>%
    select(fecha, var_interanual, z = z_shock_rolling) %>% arrange(fecha)
  serie_destino <- ventas_cluster %>% filter(CLUSTER_ORIGEN == destino) %>%
    select(fecha, var_interanual, z = z_shock_rolling) %>% arrange(fecha)

  fechas_evento <- serie_origen %>% filter(z <= umbral_caida) %>% pull(fecha)

  cat(strrep("=", 60), "\n")
  cat(sprintf("CASO:  %s  ->  %s\n", origen, destino))
  cat(strrep("=", 60), "\n\n")
  cat(sprintf("Peso estructural (BCE %d): %.1f%%\n", anio_bce_referencia, resumen$peso * 100))
  cat(sprintf("En %d meses, %s tuvo %d caídas fuertes.\n", resumen$n_meses, origen, resumen$eventos_origen))
  cat(sprintf("%s también cayó: mismo mes %.0f%% de las veces, mes siguiente %.0f%% (base: %.0f%%)\n",
              destino, resumen$tasa_mismo_mes * 100, resumen$tasa_mes_siguiente * 100, resumen$tasa_base * 100))
  cat(sprintf("Lift: %.1fx (mismo mes) / %.1fx (mes siguiente)\n\n",
              resumen$lift_mismo_mes, resumen$lift_mes_siguiente))

  for (f in fechas_evento) {
    o  <- serie_origen  %>% filter(fecha == f)
    d0 <- serie_destino %>% filter(fecha == f)
    d1 <- serie_destino %>% filter(fecha == f %m+% months(1))
    cat(sprintf("* %s: %s %.1f%%", format(f, "%b %Y"), origen, o$var_interanual * 100))
    if (nrow(d0) > 0) cat(sprintf(" | %s mismo mes: %.1f%% (%s)", destino, d0$var_interanual * 100,
                                   ifelse(d0$z <= umbral_caida, "cayó", "sin caída notable")))
    if (nrow(d1) > 0) cat(sprintf(" | %s mes siguiente: %.1f%% (%s)", destino, d1$var_interanual * 100,
                                   ifelse(d1$z <= umbral_caida, "cayó", "sin caída notable")))
    cat("\n")
  }
}

graficar_caso <- function(origen, destino) {
  datos <- ventas_cluster %>% filter(CLUSTER_ORIGEN %in% c(origen, destino)) %>%
    mutate(Clúster = CLUSTER_ORIGEN)
  fechas_evento <- datos %>% filter(Clúster == origen, z_shock_rolling <= umbral_caida) %>% pull(fecha)

  ggplot(datos, aes(x = fecha, y = var_interanual, color = Clúster)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = fechas_evento, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = paste0(origen, "  →  ", destino),
         subtitle = "Líneas rojas: meses con caída significativa en el origen",
         x = NULL, y = "Variación interanual de ventas") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
}

mejores_casos <- tasa_contagio %>%
  filter(lift_mismo_mes > 2, lift_mes_siguiente > 2) %>%
  arrange(desc(pmin(lift_mismo_mes, lift_mes_siguiente)))

cat("\nCasos con señal más consistente:\n")
print(mejores_casos)

# Ejemplos de uso:
# explicar_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO")
# graficar_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO")

# =====================================================================
# PENDIENTE (siguiente iteración): sustituir el choque de ventas por
# vencido30/vencido90 de dta_empresas_cartera (agregando por Periodo +
# clúster) como variable a explicar, una vez validada esta capa con
# ventas solamente.
# =====================================================================
