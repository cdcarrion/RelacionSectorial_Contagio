# =====================================================================
# CONTAGIO SECTORIAL — Paso 4: contagio a nivel de PAR de clústeres,
# caso por caso (no generalizado). Responde: "si el clúster X cayó en
# el mes t, ¿el clúster relacionado Y también cayó (mismo mes o el
# siguiente)?"
# =====================================================================

library(dplyr)
library(lubridate)
library(zoo)

# ---- 16. Choque SIN mirar el futuro (ventana móvil, no scale() global) ---
# El z_shock original (Contagio_paso2.R) usaba scale() sobre TODA la serie,
# lo que mete información futura en la normalización de cada mes -- no
# sirve para producción mensual real. Aquí cada mes se compara solo contra
# su propio historial hasta ese punto (ventana móvil, con 1 mes de rezago
# para no usar el propio dato en su normalización).

PERIODO_a_fecha <- function(p) ym(as.character(p))   # 202201 -> 2022-01-01

ventana <- 12   # AJUSTA: cuántos meses de historia usar como referencia

ventas_cluster <- ventas_cluster %>%
  mutate(fecha = PERIODO_a_fecha(PERIODO)) %>%
  group_by(CLUSTER_ORIGEN) %>%
  arrange(fecha) %>%
  mutate(
    media_movil = rollapply(var_interanual, ventana, mean, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    sd_movil    = rollapply(var_interanual, ventana, sd, na.rm = TRUE,
                             align = "right", fill = NA, partial = TRUE),
    z_shock_rolling = (var_interanual - lag(media_movil, 1)) / lag(sd_movil, 1),
    z_next = lead(z_shock_rolling, 1)   # el propio choque del clúster el mes siguiente
  ) %>%
  ungroup()

# ---- 17. Umbral de caída significativa ------------------------------------

umbral_caida <- -1.5   # AJUSTA: -1.0 (moderada) / -1.5 (significativa) / -2.0 (fuerte)

serie_z <- ventas_cluster %>%
  select(CLUSTER = CLUSTER_ORIGEN, fecha, z = z_shock_rolling, z_next)

# ---- 18. Trazabilidad: por cada caída de origen, ¿qué pasó en sus
#          relaciones reales (peso de la matriz insumo-producto)? ---------

eventos_origen <- serie_z %>%
  filter(z <= umbral_caida) %>%
  rename(CLUSTER_ORIGEN = CLUSTER, z_origen = z) %>%
  select(-z_next)

relaciones <- W_df %>% filter(CLUSTER_ORIGEN != CLUSTER_DESTINO)

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

# Ejemplo de lectura directa:
# trazabilidad %>% filter(CLUSTER_ORIGEN == "ATUN Y CONSERVAS DEL MAR") %>% View()

# ---- 19. Tasa de contagio por relación: ¿es más que casualidad? -----------
# Compara qué tan seguido cae el destino CUANDO el origen cae (tasa
# condicional) contra qué tan seguido cae el destino en general (tasa
# base). lift > 1 = la relación sí parece transmitir algo, no es azar.
# lift ~ 1 = el destino cae igual de seguido con o sin el choque del
# origen -- probablemente coincidencia.

tasa_base_por_cluster <- serie_z %>%
  group_by(CLUSTER) %>%
  summarise(tasa_base = mean(z <= umbral_caida, na.rm = TRUE), n_meses = sum(!is.na(z)), .groups = "drop") %>%
  rename(CLUSTER_DESTINO = CLUSTER)

tasa_contagio <- trazabilidad %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso) %>%
  summarise(
    eventos_origen      = n(),
    tasa_mismo_mes       = mean(destino_cae_mismo_mes, na.rm = TRUE),
    tasa_mes_siguiente   = mean(destino_cae_mes_siguiente, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tasa_base_por_cluster, by = "CLUSTER_DESTINO") %>%
  mutate(
    lift_mismo_mes     = tasa_mismo_mes / tasa_base,
    lift_mes_siguiente = tasa_mes_siguiente / tasa_base
  ) %>%
  filter(eventos_origen >= 3) %>%   # AJUSTA: mínimo de casos para confiar en la tasa
  arrange(desc(lift_mes_siguiente))

print(tasa_contagio)

# =====================================================================
# CÓMO LEER LOS RESULTADOS
# =====================================================================
# - `trazabilidad`: la tabla evento por evento que pediste -- filtra por
#   CLUSTER_ORIGEN == "ATUN Y CONSERVAS DEL MAR" y ves, mes a mes, si cada
#   clúster relacionado (con su peso real) también cayó.
# - `tasa_contagio`: resume esa trazabilidad por PAR de clústeres.
#   - eventos_origen: cuántas veces cayó el origen (ojo si es un número
#     muy chico -- con 3-4 eventos cualquier tasa es poco confiable).
#   - lift_mes_siguiente > 1: el destino cae más seguido DESPUÉS de un
#     choque del origen que en general -- esa es tu señal de contagio
#     concreta, a nivel de par, no de promedio de red.
# =====================================================================
