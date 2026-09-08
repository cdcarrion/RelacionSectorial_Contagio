# =====================================================================
# ANÁLISIS TRIMESTRAL — mismo enfoque que el mensual, agregado a
# trimestre. Requiere haber corrido Pipeline_completo_contagio.R
# completo (usa ventas_cluster, W_full, relaciones, calcular_cei,
# umbral_caida, tasa_contagio ya en el entorno).
# =====================================================================

library(dplyr)
library(lubridate)
library(zoo)
library(purrr)

# ---- Parámetros específicos del trimestre ---------------------------------

ventana_choque_trim <- 4   # 4 trimestres = 1 año de historia para el z-score móvil
minimo_eventos_trim <- 3   # AJUSTA: con ~12 trimestres de historia, pedir 5
                            # eventos (como en mensual) deja la tabla casi
                            # vacía -- ver la nota de poder estadístico más
                            # abajo antes de confiar en esto por sí solo.

# ---- 1. Agregar el panel mensual a trimestre -------------------------------

ventas_cluster_trim <- ventas_cluster %>%
  mutate(fecha_trim = floor_date(fecha, "quarter")) %>%
  group_by(CLUSTER_ORIGEN, fecha_trim) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  rename(fecha = fecha_trim) %>%
  arrange(CLUSTER_ORIGEN, fecha)

# ---- 2. Variación interanual (mismo trimestre, un año antes) --------------
# fecha_trim + 12 meses siempre cae en el mismo trimestre del año
# siguiente (ene->ene, abr->abr, etc.), así que el mismo truco de match
# explícito por fecha sirve igual que en el mensual.

referencia_1a_trim <- ventas_cluster_trim %>%
  select(CLUSTER_ORIGEN, fecha, total) %>%
  mutate(fecha_objetivo = fecha %m+% months(12)) %>%
  select(CLUSTER_ORIGEN, fecha_objetivo, total_hace_1a = total)

ventas_cluster_trim <- ventas_cluster_trim %>%
  left_join(referencia_1a_trim, by = c("CLUSTER_ORIGEN", "fecha" = "fecha_objetivo")) %>%
  mutate(var_interanual = (total - total_hace_1a) / total_hace_1a)

# ---- 3. Choque con ventana móvil, en trimestres ----------------------------

ventas_cluster_trim <- ventas_cluster_trim %>%
  group_by(CLUSTER_ORIGEN) %>%
  arrange(fecha) %>%
  mutate(
    media_movil = rollapply(var_interanual, ventana_choque_trim, mean, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    sd_movil    = rollapply(var_interanual, ventana_choque_trim, sd, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    z_shock_rolling = (var_interanual - lag(media_movil, 1)) / lag(sd_movil, 1),
    z_next = lead(z_shock_rolling, 1)
  ) %>%
  ungroup()

# ---- 4. CEI trimestral --------------------------------------------------
# Reutiliza calcular_cei() y W_full tal cual -- la función no sabe ni le
# importa la granularidad de tiempo, solo necesita fecha + z por clúster.

fechas_trim <- sort(unique(ventas_cluster_trim$fecha))
cei_trim <- map_df(fechas_trim, calcular_cei, datos = ventas_cluster_trim, W = W_full)

# ---- 5. Trazabilidad y tasa de contagio trimestral -------------------------

serie_z_trim <- ventas_cluster_trim %>%
  select(CLUSTER = CLUSTER_ORIGEN, fecha, z = z_shock_rolling, z_next)

eventos_origen_trim <- serie_z_trim %>%
  filter(z <= umbral_caida) %>%
  rename(CLUSTER_ORIGEN = CLUSTER, z_origen = z) %>%
  select(-z_next)

trazabilidad_trim <- eventos_origen_trim %>%
  inner_join(relaciones, by = "CLUSTER_ORIGEN") %>%
  left_join(
    serie_z_trim %>% rename(CLUSTER_DESTINO = CLUSTER,
                             z_destino_mismo_trim = z,
                             z_destino_trim_siguiente = z_next),
    by = c("CLUSTER_DESTINO", "fecha")
  ) %>%
  mutate(
    destino_cae_mismo_trim     = z_destino_mismo_trim     <= umbral_caida,
    destino_cae_trim_siguiente = z_destino_trim_siguiente <= umbral_caida
  ) %>%
  arrange(CLUSTER_ORIGEN, fecha, desc(peso))

tasa_base_trim <- serie_z_trim %>%
  group_by(CLUSTER) %>%
  summarise(tasa_base = mean(z <= umbral_caida, na.rm = TRUE),
            n_trimestres = sum(!is.na(z)), .groups = "drop") %>%
  rename(CLUSTER_DESTINO = CLUSTER)

tasa_contagio_trim <- trazabilidad_trim %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso) %>%
  summarise(
    eventos_origen      = n(),
    tasa_mismo_trim     = mean(destino_cae_mismo_trim, na.rm = TRUE),
    tasa_trim_siguiente = mean(destino_cae_trim_siguiente, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tasa_base_trim, by = "CLUSTER_DESTINO") %>%
  mutate(
    lift_mismo_trim     = tasa_mismo_trim / tasa_base,
    lift_trim_siguiente = tasa_trim_siguiente / tasa_base
  ) %>%
  filter(eventos_origen >= minimo_eventos_trim) %>%
  arrange(desc(lift_trim_siguiente))

print(tasa_contagio_trim)

# ---- 6. Cruce contra el resultado mensual ----------------------------------
# El punto real de este análisis: los pares que aparecen en AMBAS
# granularidades son la señal más confiable -- se sostienen sin importar
# cómo se corte el tiempo, no es un artefacto de cómo se armaron los meses.

comparacion_mensual_trim <- tasa_contagio %>%
  select(CLUSTER_ORIGEN, CLUSTER_DESTINO, lift_mismo_mes, lift_mes_siguiente) %>%
  inner_join(
    tasa_contagio_trim %>%
      select(CLUSTER_ORIGEN, CLUSTER_DESTINO, lift_mismo_trim, lift_trim_siguiente),
    by = c("CLUSTER_ORIGEN", "CLUSTER_DESTINO")
  )

cat("\nPares que se sostienen en mensual Y en trimestral:\n")
print(comparacion_mensual_trim)

# =====================================================================
# NOTA DE PODER ESTADÍSTICO
# =====================================================================
# Con umbral_caida = -1.5 (~7% de los periodos) y solo ~12 trimestres de
# historia, el valor esperado es menos de 1 evento de origen por clúster
# en todo el periodo. Si `tasa_contagio_trim` sale casi vacía incluso
# bajando minimo_eventos_trim a 2-3, no es un error del código: es que
# hoy no hay suficiente historia trimestral para sostener conclusiones
# propias a ese nivel. Trátalo como cruce de validación del mensual, y
# revisítalo en serio cuando acumules más años de historia.
# =====================================================================
