# =====================================================================
# INTEGRACIÓN DE MORA — Paso pendiente desde el inicio del proyecto
# Requiere Pipeline_completo_contagio.R corrido completo (usa
# ventas_cluster, eventos_origen, relaciones, W_full, umbral_caida,
# minimo_eventos, dta_empresas_cartera).
# Usa `clusternew` y `FamliaHomologada new`, como indicaste -- NO la
# columna `industria` (es una taxonomía distinta, más gruesa) ni la
# `FamliaHomologada` sin homologar.
# =====================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(purrr)

# =====================================================================
# 1. PANEL DE CARTERA POR CLÚSTER Y PERIODO
# =====================================================================

# Periodo aquí sí viene siempre a 6 dígitos (a diferencia del de
# ventas), pero se reconstruye explícito con substr por seguridad.
cartera_fecha <- dta_empresas_cartera %>%
  distinct(Periodo) %>%
  mutate(
    anio  = as.integer(substr(as.character(Periodo), 1, 4)),
    mes   = as.integer(substr(as.character(Periodo), 5, 6)),
    fecha = as.Date(sprintf("%d-%02d-01", anio, mes))
  ) %>%
  select(Periodo, fecha)

# Agregación principal: todo el portafolio del clúster, sumando sobre
# CuentaContable_4, NombreTipoConcesion y `FamliaHomologada new`.
# SUPUESTO A VERIFICAR: que sumar montoCapitalTotal/vencido30/90 a
# través de todos los CuentaContable_4 (1401,1402,1404,6401,6402,6403)
# no duplica saldos -- es decir, que son componentes distintos del
# portafolio, no subtotales unos de otros. Si alguno es subtotal de
# otro, hay que excluirlo antes de sumar.
cartera_cluster <- dta_empresas_cartera %>%
  left_join(cartera_fecha, by = "Periodo") %>%
  group_by(clusternew, fecha) %>%
  summarise(
    montoCapitalTotal = sum(montoCapitalTotal, na.rm = TRUE),
    vencido30 = sum(vencido30, na.rm = TRUE),
    vencido90 = sum(vencido90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(CLUSTER_ORIGEN = clusternew) %>%
  mutate(
    tasa_mora30 = ifelse(montoCapitalTotal == 0, NA, vencido30 / montoCapitalTotal),
    tasa_mora90 = ifelse(montoCapitalTotal == 0, NA, vencido90 / montoCapitalTotal)
  ) %>%
  arrange(CLUSTER_ORIGEN, fecha)

# Desglose opcional por segmento, para revisar si un hallazgo viene de
# un segmento específico (ej. solo Microfinanzas) y no de todo el clúster.
cartera_segmento <- dta_empresas_cartera %>%
  left_join(cartera_fecha, by = "Periodo") %>%
  group_by(clusternew, `FamliaHomologada new`, fecha) %>%
  summarise(
    montoCapitalTotal = sum(montoCapitalTotal, na.rm = TRUE),
    vencido30 = sum(vencido30, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(CLUSTER_ORIGEN = clusternew, segmento = `FamliaHomologada new`) %>%
  mutate(tasa_mora30 = ifelse(montoCapitalTotal == 0, NA, vencido30 / montoCapitalTotal))

# ---- Chequeo de outliers antes de confiar en el z-score --------------------
# Un solo cliente grande en mora puede disparar la tasa de un clúster
# chico sin que sea un fenómeno sectorial. Revisar esta lista antes de
# interpretar cualquier evento de deterioro más abajo.
cat("Clúster-mes con tasa_mora30 > 50% (revisar si es un solo cliente grande):\n")
cartera_cluster %>%
  filter(tasa_mora30 > 0.5) %>%
  select(CLUSTER_ORIGEN, fecha, montoCapitalTotal, vencido30, tasa_mora30) %>%
  arrange(desc(tasa_mora30)) %>%
  print(n = 20)

# =====================================================================
# 2. CHOQUE DE MORA (deterioro significativo)
# =====================================================================
# A diferencia de ventas (interesan las CAÍDAS), en mora interesan las
# SUBIDAS. Se usa diferencia en PUNTOS PORCENTUALES (no % de
# crecimiento), porque tasa_mora suele partir de valores cercanos a
# cero, donde un % de crecimiento se dispara sin que el cambio real sea
# grande (ej. de 0.5% a 1.5% es "+200%" en crecimiento pero solo 1 punto
# porcentual real).

ventana_mora <- 12
umbral_mora  <- 1.5   # z-score POSITIVO = deterioro significativo

referencia_12m_mora <- cartera_cluster %>%
  select(CLUSTER_ORIGEN, fecha, tasa_mora30) %>%
  mutate(fecha_objetivo = fecha %m+% months(12)) %>%
  select(CLUSTER_ORIGEN, fecha_objetivo, tasa_mora30_hace_12 = tasa_mora30)

cartera_cluster <- cartera_cluster %>%
  left_join(referencia_12m_mora, by = c("CLUSTER_ORIGEN", "fecha" = "fecha_objetivo")) %>%
  mutate(delta_mora = tasa_mora30 - tasa_mora30_hace_12)

cartera_cluster <- cartera_cluster %>%
  group_by(CLUSTER_ORIGEN) %>%
  arrange(fecha) %>%
  mutate(
    media_movil_mora = rollapply(delta_mora, ventana_mora, mean, na.rm = TRUE,
                                  align = "right", fill = NA, partial = TRUE),
    sd_movil_mora    = rollapply(delta_mora, ventana_mora, sd, na.rm = TRUE,
                                  align = "right", fill = NA, partial = TRUE),
    z_mora = (delta_mora - lag(media_movil_mora, 1)) / lag(sd_movil_mora, 1)
  ) %>%
  ungroup()

serie_z_mora <- cartera_cluster %>% select(CLUSTER = CLUSTER_ORIGEN, fecha, z_mora)

# =====================================================================
# 3. VALIDACIÓN A — ¿el PROPIO clúster que cae en ventas, sube en mora
#    después? (chequeo básico antes de mirar contagio entre clústeres)
# =====================================================================

chequear_mora_propia <- function(rezago_meses) {
  eventos_origen %>%
    mutate(fecha_objetivo = fecha %m+% months(rezago_meses)) %>%
    select(CLUSTER_ORIGEN, fecha, fecha_objetivo) %>%
    left_join(
      serie_z_mora %>% rename(CLUSTER_ORIGEN = CLUSTER, fecha_objetivo = fecha),
      by = c("CLUSTER_ORIGEN", "fecha_objetivo")
    ) %>%
    mutate(rezago_meses = rezago_meses)
}

propio_ventas_a_mora <- map_dfr(0:3, chequear_mora_propia)

resumen_propio <- propio_ventas_a_mora %>%
  group_by(rezago_meses) %>%
  summarise(
    n_eventos_evaluables   = sum(!is.na(z_mora)),
    pct_con_deterioro_mora = mean(z_mora >= umbral_mora, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== A. ¿Un clúster que cae en ventas, sube en SU PROPIA mora después? ===\n")
print(resumen_propio)

# =====================================================================
# 4. VALIDACIÓN B — la pregunta original del proyecto: ¿una caída de
#    ventas en un clúster se transmite a la MORA de sus relacionados?
# =====================================================================

trazabilidad_mora <- eventos_origen %>%
  inner_join(relaciones, by = "CLUSTER_ORIGEN") %>%
  select(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso, fecha, z_origen) %>%
  { datos_evento <- .
    map_dfr(0:3, function(rezago) {
      datos_evento %>%
        mutate(fecha_objetivo = fecha %m+% months(rezago)) %>%
        left_join(
          serie_z_mora %>% rename(CLUSTER_DESTINO = CLUSTER, fecha_objetivo = fecha, z_mora_destino = z_mora),
          by = c("CLUSTER_DESTINO", "fecha_objetivo")
        ) %>%
        mutate(rezago_meses = rezago,
               destino_deteriora = z_mora_destino >= umbral_mora)
    })
  }

tasa_base_mora <- serie_z_mora %>%
  group_by(CLUSTER) %>%
  summarise(tasa_base_mora = mean(z_mora >= umbral_mora, na.rm = TRUE), .groups = "drop") %>%
  rename(CLUSTER_DESTINO = CLUSTER)

tasa_contagio_mora <- trazabilidad_mora %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO, peso, rezago_meses) %>%
  summarise(
    eventos_origen  = n(),
    tasa_deterioro  = mean(destino_deteriora, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tasa_base_mora, by = "CLUSTER_DESTINO") %>%
  mutate(lift_mora = tasa_deterioro / tasa_base_mora) %>%
  filter(eventos_origen >= minimo_eventos) %>%
  arrange(desc(lift_mora))

cat("\n=== B. Contagio ventas (origen) -> mora (destino relacionado), por rezago ===\n")
print(tasa_contagio_mora)

# =====================================================================
# 5. CRUCE: pares que YA mostraban contagio ventas->ventas, ¿también
#    muestran ventas->mora? Doble validación del mismo par.
# =====================================================================

cruce_ventas_mora <- tasa_contagio %>%
  select(CLUSTER_ORIGEN, CLUSTER_DESTINO, lift_mismo_mes, lift_mes_siguiente) %>%
  inner_join(
    tasa_contagio_mora %>%
      filter(rezago_meses %in% c(0, 1)) %>%
      select(CLUSTER_ORIGEN, CLUSTER_DESTINO, rezago_meses, lift_mora) %>%
      pivot_wider(names_from = rezago_meses, values_from = lift_mora,
                  names_prefix = "lift_mora_rezago"),
    by = c("CLUSTER_ORIGEN", "CLUSTER_DESTINO")
  ) %>%
  arrange(desc(lift_mora_rezago1))

cat("\n=== Pares con señal en AMBAS capas (ventas->ventas Y ventas->mora) ===\n")
cat("Estos son los más defendibles para llevar a comité -- se sostienen sin\n")
cat("importar qué variable de choque se mire.\n")
print(cruce_ventas_mora)

# =====================================================================
# NOTA: por qué no se testeó mora -> mora directamente en esta versión
# =====================================================================
# Con umbral_mora = 1.5 y ~36 meses de historia, el patrón va a ser el
# mismo problema de muestra chica que ya vimos en ventas -- se dejó
# fuera de esta primera pasada para no multiplicar pruebas sobre una
# base de datos que ya es limitada. Si A y B (arriba) dan señal, ahí sí
# vale la pena construir la red mora->mora con el mismo patrón.
# =====================================================================
