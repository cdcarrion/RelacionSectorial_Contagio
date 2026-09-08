# =====================================================================
# ANÁLISIS DE ROBUSTEZ — 4 chequeos sobre lo que salió de tasa_contagio
# Requiere Pipeline_completo_contagio.R ya corrido completo (usa
# ventas_cluster, W_df, W_full, relaciones, Tab2_full, periodo_fecha,
# umbral_caida, top_n_relaciones, tasa_contagio, anios_bce_disponibles).
# =====================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# =====================================================================
# 1. SENSIBILIDAD DE PARÁMETROS
# =====================================================================
# ¿La lista de "mejores_casos" se mantiene si cambian umbral_caida o
# top_n_relaciones, o es un artefacto de los valores que elegimos?

correr_tasa_contagio <- function(top_n, umbral, min_eventos = 3) {

  relaciones_i <- W_df %>%
    filter(CLUSTER_ORIGEN != CLUSTER_DESTINO) %>%
    group_by(CLUSTER_ORIGEN) %>%
    slice_max(peso, n = top_n, with_ties = FALSE) %>%
    ungroup()

  serie_z <- ventas_cluster %>%
    select(CLUSTER = CLUSTER_ORIGEN, fecha, z = z_shock_rolling, z_next)

  eventos_i <- serie_z %>%
    filter(z <= umbral) %>%
    rename(CLUSTER_ORIGEN = CLUSTER, z_origen = z) %>%
    select(-z_next)

  trazabilidad_i <- eventos_i %>%
    inner_join(relaciones_i, by = "CLUSTER_ORIGEN") %>%
    left_join(
      serie_z %>% rename(CLUSTER_DESTINO = CLUSTER,
                          z_destino_mismo_mes = z, z_destino_mes_siguiente = z_next),
      by = c("CLUSTER_DESTINO", "fecha")
    ) %>%
    mutate(
      destino_cae_mismo_mes     = z_destino_mismo_mes     <= umbral,
      destino_cae_mes_siguiente = z_destino_mes_siguiente <= umbral
    )

  tasa_base_i <- serie_z %>%
    group_by(CLUSTER) %>%
    summarise(tasa_base = mean(z <= umbral, na.rm = TRUE), .groups = "drop") %>%
    rename(CLUSTER_DESTINO = CLUSTER)

  trazabilidad_i %>%
    group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso) %>%
    summarise(
      eventos_origen     = n(),
      tasa_mismo_mes     = mean(destino_cae_mismo_mes, na.rm = TRUE),
      tasa_mes_siguiente = mean(destino_cae_mes_siguiente, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(tasa_base_i, by = "CLUSTER_DESTINO") %>%
    mutate(
      lift_mismo_mes     = tasa_mismo_mes / tasa_base,
      lift_mes_siguiente = tasa_mes_siguiente / tasa_base,
      top_n = top_n, umbral_caida = umbral
    ) %>%
    filter(eventos_origen >= min_eventos)
}

grid_parametros <- expand_grid(top_n = c(3, 5, 10), umbral = c(-1.0, -1.5, -2.0))

resultados_sensibilidad <- pmap_dfr(
  grid_parametros, ~ correr_tasa_contagio(top_n = ..1, umbral = ..2)
)

estabilidad_pares <- resultados_sensibilidad %>%
  mutate(es_candidato = lift_mismo_mes > 2 & lift_mes_siguiente > 2) %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO) %>%
  summarise(
    n_veces_candidato = sum(es_candidato, na.rm = TRUE),
    pct_estabilidad   = n_veces_candidato / nrow(grid_parametros),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_estabilidad))

cat("=== 1. SENSIBILIDAD DE PARÁMETROS ===\n")
cat("Pares que fueron 'candidato' (lift>2 en ambas ventanas) en más de la mitad\n")
cat("de las", nrow(grid_parametros), "combinaciones de top_n x umbral probadas:\n")
print(estabilidad_pares %>% filter(pct_estabilidad > 0.5))

# =====================================================================
# 2. CONCENTRACIÓN DENTRO DEL CLÚSTER
# =====================================================================
# ¿Las caídas del clúster de origen las explica una sola actividad
# económica (CIIU), o están repartidas entre varias? Una caída muy
# concentrada podría ser un evento de una sola empresa/actividad, no un
# choque sectorial de verdad.

concentracion_actividad <- dta_empresas_ventas %>%
  left_join(periodo_fecha %>% select(PERIODO, fecha), by = "PERIODO") %>%
  filter(!is.na(fecha)) %>%
  group_by(`SECTOR BP`, fecha) %>%
  mutate(participacion = `TOTAL VENTAS` / sum(`TOTAL VENTAS`, na.rm = TRUE)) %>%
  summarise(
    hhi            = sum(participacion^2, na.rm = TRUE),
    top1_actividad_pct = max(participacion, na.rm = TRUE),
    n_actividades  = n(),
    .groups = "drop"
  ) %>%
  rename(CLUSTER_ORIGEN = `SECTOR BP`)

hhi_base_por_cluster <- concentracion_actividad %>%
  group_by(CLUSTER_ORIGEN) %>%
  summarise(hhi_promedio = mean(hhi, na.rm = TRUE), .groups = "drop")

# eventos_origen viene del pipeline principal (caídas significativas ya
# detectadas); si no está en el entorno, se puede reconstruir con:
# eventos_origen <- ventas_cluster %>% filter(z_shock_rolling <= umbral_caida) %>%
#   rename(CLUSTER_ORIGEN = CLUSTER_ORIGEN)  # ya viene con ese nombre

eventos_con_concentracion <- ventas_cluster %>%
  filter(z_shock_rolling <= umbral_caida) %>%
  select(CLUSTER_ORIGEN, fecha) %>%
  left_join(concentracion_actividad, by = c("CLUSTER_ORIGEN", "fecha")) %>%
  left_join(hhi_base_por_cluster, by = "CLUSTER_ORIGEN") %>%
  mutate(concentracion_inusual = hhi > hhi_promedio * 1.3) %>%  # AJUSTA el multiplicador
  arrange(desc(hhi))

cat("\n=== 2. CONCENTRACIÓN POR EVENTO ===\n")
cat("Caídas donde la concentración de ventas fue inusualmente alta frente al\n")
cat("propio historial del clúster -- candidatas a ser un evento puntual de una\n")
cat("actividad/empresa, no un choque sectorial amplio:\n")
print(eventos_con_concentracion %>% filter(concentracion_inusual))

# =====================================================================
# 3. BACKTESTING FUERA DE MUESTRA
# =====================================================================
# Se eligen los "mejores_casos" usando SOLO el periodo de entrenamiento,
# y se verifica si esos mismos pares se sostienen en el periodo de
# prueba, que no se usó para elegirlos.

fecha_corte <- as.Date("2024-07-01")   # AJUSTA dónde partir entrenamiento/prueba

calcular_tasa_en_periodo <- function(datos_ventas, relaciones_ref, umbral, min_eventos = 0) {

  serie_z <- datos_ventas %>%
    select(CLUSTER = CLUSTER_ORIGEN, fecha, z = z_shock_rolling, z_next)

  eventos <- serie_z %>%
    filter(z <= umbral) %>%
    rename(CLUSTER_ORIGEN = CLUSTER, z_origen = z) %>%
    select(-z_next)

  trazabilidad_i <- eventos %>%
    inner_join(relaciones_ref, by = "CLUSTER_ORIGEN") %>%
    left_join(
      serie_z %>% rename(CLUSTER_DESTINO = CLUSTER,
                          z_destino_mismo_mes = z, z_destino_mes_siguiente = z_next),
      by = c("CLUSTER_DESTINO", "fecha")
    ) %>%
    mutate(
      destino_cae_mismo_mes     = z_destino_mismo_mes     <= umbral,
      destino_cae_mes_siguiente = z_destino_mes_siguiente <= umbral
    )

  tasa_base_i <- serie_z %>%
    group_by(CLUSTER) %>%
    summarise(tasa_base = mean(z <= umbral, na.rm = TRUE), .groups = "drop") %>%
    rename(CLUSTER_DESTINO = CLUSTER)

  trazabilidad_i %>%
    group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso) %>%
    summarise(
      eventos_origen     = n(),
      tasa_mismo_mes     = mean(destino_cae_mismo_mes, na.rm = TRUE),
      tasa_mes_siguiente = mean(destino_cae_mes_siguiente, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(tasa_base_i, by = "CLUSTER_DESTINO") %>%
    mutate(
      lift_mismo_mes     = tasa_mismo_mes / tasa_base,
      lift_mes_siguiente = tasa_mes_siguiente / tasa_base
    ) %>%
    filter(eventos_origen >= min_eventos)
}

ventas_train <- ventas_cluster %>% filter(fecha <  fecha_corte)
ventas_test  <- ventas_cluster %>% filter(fecha >= fecha_corte)

tasa_train <- calcular_tasa_en_periodo(ventas_train, relaciones, umbral_caida, min_eventos = 3)
candidatos_train <- tasa_train %>% filter(lift_mismo_mes > 2, lift_mes_siguiente > 2)

tasa_test_todos <- calcular_tasa_en_periodo(ventas_test, relaciones, umbral_caida, min_eventos = 0)

verificacion_oos <- candidatos_train %>%
  select(CLUSTER_ORIGEN, CLUSTER_DESTINO, lift_mismo_mes, lift_mes_siguiente) %>%
  rename(lift_mismo_mes_train = lift_mismo_mes, lift_mes_siguiente_train = lift_mes_siguiente) %>%
  left_join(
    tasa_test_todos %>%
      select(CLUSTER_ORIGEN, CLUSTER_DESTINO, eventos_origen,
             lift_mismo_mes, lift_mes_siguiente) %>%
      rename(eventos_origen_test = eventos_origen,
             lift_mismo_mes_test = lift_mismo_mes,
             lift_mes_siguiente_test = lift_mes_siguiente),
    by = c("CLUSTER_ORIGEN", "CLUSTER_DESTINO")
  ) %>%
  mutate(veredicto_oos = case_when(
    is.na(eventos_origen_test) ~ "Sin eventos en el periodo de prueba -- inconcluso",
    lift_mismo_mes_test > 1.5 | lift_mes_siguiente_test > 1.5 ~ "Se sostiene fuera de muestra",
    TRUE ~ "Se debilita fuera de muestra -- tratar con más cautela"
  ))

cat("\n=== 3. BACKTESTING FUERA DE MUESTRA ===\n")
cat("Corte entrenamiento/prueba:", format(fecha_corte, "%b %Y"), "\n")
cat("Candidatos elegidos SOLO con datos de entrenamiento, y qué pasó en la prueba:\n")
print(verificacion_oos)

# =====================================================================
# 4. ESTABILIDAD DE LA ESTRUCTURA AÑO A AÑO (2021-2024)
# =====================================================================
# ¿Qué tan parecidos son los pesos w_ij de un año BCE a otro? Si cambian
# poco, el supuesto de "estructura fija por año" es razonable. Si
# cambian mucho para algún clúster, hay que revisar ese caso con más
# cuidado (y quizás refrescar la referencia más seguido que una vez al
# año para él).

construir_W_para_anio <- function(anio) {
  Tab2_full %>%
    left_join(periodo_fecha %>% select(PERIODO, anio_bce), by = "PERIODO") %>%
    filter(anio_bce == anio) %>%
    group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO) %>%
    summarise(peso = mean(perc, na.rm = TRUE), .groups = "drop") %>%
    mutate(anio_bce = anio)
}

W_por_anio <- map_dfr(anios_bce_disponibles, construir_W_para_anio)

W_ancho_por_anio <- W_por_anio %>%
  pivot_wider(names_from = anio_bce, values_from = peso, values_fill = 0, names_prefix = "peso_")

anio_min <- min(anios_bce_disponibles)
anio_max <- max(anios_bce_disponibles)
col_min  <- paste0("peso_", anio_min)
col_max  <- paste0("peso_", anio_max)

estabilidad_estructura <- W_ancho_por_anio %>%
  group_by(CLUSTER_ORIGEN) %>%
  summarise(
    cambio_total = sum(abs(.data[[col_min]] - .data[[col_max]]), na.rm = TRUE) / 2,
    .groups = "drop"
  ) %>%
  arrange(desc(cambio_total))

cat("\n=== 4. ESTABILIDAD DE LA ESTRUCTURA", anio_min, "->", anio_max, "===\n")
cat("Clústeres de origen con MÁS cambio estructural (revisar con más cuidado\n")
cat("el supuesto de estructura fija para ellos):\n")
print(head(estabilidad_estructura, 10))
cat("\nClústeres con estructura MÁS estable:\n")
print(tail(estabilidad_estructura, 10))

# Para ver el detalle de un clúster puntual a través de los años:
# W_por_anio %>% filter(CLUSTER_ORIGEN == "AUTOMOTRIZ") %>%
#   group_by(anio_bce) %>% slice_max(peso, n = 3) %>% print(n = 20)
