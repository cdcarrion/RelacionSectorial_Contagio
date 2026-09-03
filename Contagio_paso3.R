# =====================================================================
# CONTAGIO SECTORIAL — Paso 3: validar el contagio SOLO con ventas
# Usa los objetos de Contagio_paso2.R: ventas_cluster (con z_shock),
# cei_mensual (con CEI) y W_df (pesos estructurales por par).
# =====================================================================

library(dplyr)
library(purrr)

# ---- 12. Panel: choque propio + exposición de red -------------------------

panel_ventas <- ventas_cluster %>%
  rename(CLUSTER = CLUSTER_ORIGEN) %>%
  select(CLUSTER, PERIODO, z_shock) %>%
  left_join(cei_mensual, by = c("CLUSTER", "PERIODO")) %>%
  group_by(CLUSTER) %>%
  arrange(PERIODO) %>%
  mutate(
    z_shock_lag1  = lag(z_shock, 1),   # inercia propia (mes anterior)
    z_shock_lead1 = lead(z_shock, 1)   # choque propio del mes siguiente
  ) %>%
  ungroup()

# ---- 13. ¿El CEI explica el choque CONTEMPORÁNEO, más allá de la inercia? -

modelo_contemporaneo <- lm(z_shock ~ CEI + z_shock_lag1, data = panel_ventas)
summary(modelo_contemporaneo)

# ---- 14. ¿El CEI de HOY anticipa el choque del PRÓXIMO mes? --------------
# Esta es la prueba más parecida a "contagio" real: separa temporalmente
# la causa (exposición de red hoy) del efecto (choque propio futuro).

modelo_rezagado <- lm(z_shock_lead1 ~ CEI + z_shock, data = panel_ventas)
summary(modelo_rezagado)

# Nota: esto es OLS pooled entre todos los clústeres. Si quieres controlar
# por diferencias estructurales entre clústeres (efectos fijos), usa
# plm::plm(z_shock_lead1 ~ CEI + z_shock, data = panel_ventas,
#          index = c("CLUSTER","PERIODO"), model = "within")

# ---- 15. Diagnóstico por PAR de clústeres conectados ----------------------
# El modelo agregado (13-14) dice si la red "en promedio" transmite choques.
# Esto identifica CUÁLES relaciones específicas contagian más.

umbral_peso <- 0.05   # AJUSTA: solo mirar pares con peso estructural relevante

pares_relevantes <- W_df %>%
  filter(peso > umbral_peso, CLUSTER_ORIGEN != CLUSTER_DESTINO)

correlacion_par <- function(origen, destino, datos, rezago = 0){
  s1 <- datos %>% filter(CLUSTER_ORIGEN == origen)  %>% select(PERIODO, z_shock)
  s2 <- datos %>% filter(CLUSTER_ORIGEN == destino) %>%
    arrange(PERIODO) %>%
    mutate(z_shock = lead(z_shock, rezago)) %>%     # destino en t+rezago
    select(PERIODO, z_shock)

  m <- inner_join(s1, s2, by = "PERIODO", suffix = c("_origen", "_destino"))
  if (nrow(m) < 6) return(NA_real_)   # muy pocos períodos para confiar en la correlación
  cor(m$z_shock_origen, m$z_shock_destino, use = "complete.obs")
}

diagnostico_pares <- pares_relevantes %>%
  mutate(
    corr_contemporanea = map2_dbl(CLUSTER_ORIGEN, CLUSTER_DESTINO, correlacion_par,
                                    datos = ventas_cluster, rezago = 0),
    corr_rezago_1m      = map2_dbl(CLUSTER_ORIGEN, CLUSTER_DESTINO, correlacion_par,
                                    datos = ventas_cluster, rezago = 1)
  ) %>%
  arrange(desc(abs(corr_rezago_1m)))

print(diagnostico_pares)

# =====================================================================
# CÓMO LEER LOS RESULTADOS
# =====================================================================
# - modelo_contemporaneo / modelo_rezagado: si el coeficiente de CEI es
#   significativo (p < 0.05) y de signo coherente (positivo: choques se
#   propagan en la misma dirección; negativo: relación compensatoria),
#   hay evidencia de contagio a nivel de red agregada.
# - diagnostico_pares: ordena qué relaciones específicas (ORIGEN->DESTINO)
#   muestran mayor correlación de choques, con y sin rezago de 1 mes.
#   Esto te da candidatos concretos para explicarle al negocio ("el
#   deterioro en X se mueve junto/después del deterioro en Y").
# - Con pocos meses de historia estas pruebas van a tener poca potencia
#   estadística -- trátalas como señal exploratoria, no como confirmación
#   definitiva, hasta acumular más períodos.
# =====================================================================
