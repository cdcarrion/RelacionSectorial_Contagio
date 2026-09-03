# =====================================================================
# CONTAGIO SECTORIAL — Paso 2: panel completo + estructura W + choques
# Requiere haber corrido antes CorrelacionSectorial_limpio.R (usa Tab1,
# repartir_ventas(), dta_empresas_ventas, etc.)
# =====================================================================

library(dplyr)
library(tidyr)
library(zoo)

# ---- 7. Panel completo (todos los períodos) ------------------------------
# Como SECTOR BP == CLUSTER, no hace falta remapear el origen: Tab2 ya es
# una matriz clúster -> clúster. Solo generalizamos construir_tabla_relacion
# para que corra sobre TODO el histórico, no un año/mes a la vez.

construir_tabla_relacion <- function(anios = NULL, meses = NULL) {

  ventas_filtradas <- dta_empresas_ventas
  if (!is.null(anios)) ventas_filtradas <- ventas_filtradas %>% filter(`ANIO FISCAL` %in% anios)
  if (!is.null(meses)) ventas_filtradas <- ventas_filtradas %>% filter(`MES FISCAL` %in% meses)

  ventas_filtradas %>%
    left_join(
      dta_bce_codigo %>% distinct(CIIU4_6D, `CÓDIGO DE INDUSTRIA ECUATORIANO_CIE`),
      by = c("ACTIVIDAD ECONOMICA" = "CIIU4_6D")
    ) %>%
    left_join(frames_por_codigo, by = c("CÓDIGO DE INDUSTRIA ECUATORIANO_CIE" = "Código")) %>%
    group_by(PERIODO, `ACTIVIDAD ECONOMICA`) %>%
    mutate(TOTAL_VENTAS2 = prop * `TOTAL VENTAS`) %>%
    ungroup()
}

Tab1_full <- construir_tabla_relacion(anios = NULL, meses = NULL)   # todo el histórico disponible

Tab2_full <- repartir_ventas(Tab1_full, colapsar_a = "CLUSTER") %>%
  group_by(PERIODO, `SECTOR BP`, CLUSTER) %>%
  summarise(TOTAL_VENTAS4 = sum(TOTAL_VENTAS3, na.rm = TRUE), .groups = "drop") %>%
  group_by(PERIODO, `SECTOR BP`) %>%
  mutate(total = sum(TOTAL_VENTAS4, na.rm = TRUE),
         perc  = TOTAL_VENTAS4 / total) %>%
  ungroup() %>%
  rename(CLUSTER_ORIGEN = `SECTOR BP`, CLUSTER_DESTINO = CLUSTER)

# ---- 8. Estructura fija W ------------------------------------------------
# Promedio de "perc" a través de todos los períodos disponibles: esta es
# la matriz de pesos que se mantiene fija (solo se re-calcula cuando el
# BCE publique una tabla de utilización nueva), separada de la dinámica
# mensual que va en el paso 9.

W_df <- Tab2_full %>%
  group_by(CLUSTER_ORIGEN, CLUSTER_DESTINO) %>%
  summarise(peso = mean(perc, na.rm = TRUE), .groups = "drop")

W_wide <- W_df %>%
  pivot_wider(names_from = CLUSTER_DESTINO, values_from = peso, values_fill = 0)

W <- W_wide %>% select(-CLUSTER_ORIGEN) %>% as.matrix()
rownames(W) <- W_wide$CLUSTER_ORIGEN

# Aseguramos matriz cuadrada (mismos clústeres en filas y columnas)
todos <- union(rownames(W), colnames(W))
W_full <- matrix(0, nrow = length(todos), ncol = length(todos), dimnames = list(todos, todos))
W_full[rownames(W), colnames(W)] <- W

# ---- 9. Choque mensual por clúster (magnitud real, no conteo) -----------
# "total" en Tab2_full ya es la venta redistribuida agregada por
# (PERIODO, CLUSTER_ORIGEN) -- una fila por combinación, así que la
# serie de tiempo sale directo de ahí.

ventas_cluster <- Tab2_full %>%
  distinct(PERIODO, CLUSTER_ORIGEN, total) %>%
  arrange(CLUSTER_ORIGEN, PERIODO)

ventas_cluster <- ventas_cluster %>%
  group_by(CLUSTER_ORIGEN) %>%
  mutate(
    var_interanual = (total - lag(total, 12)) / lag(total, 12),  # AJUSTA el lag si PERIODO no es mensual consecutivo
    z_shock = as.numeric(scale(var_interanual))
  ) %>%
  ungroup()

# ---- 10. Índice de Exposición a Contagio (CEI) ---------------------------

calcular_cei <- function(periodo_i, datos, W){
  shocks <- datos %>% filter(PERIODO == periodo_i) %>%
    select(CLUSTER_ORIGEN, z_shock) %>%
    tibble::deframe()
  shocks <- shocks[colnames(W)]
  shocks[is.na(shocks)] <- 0
  cei <- W %*% shocks
  tibble::tibble(CLUSTER = rownames(W), PERIODO = periodo_i, CEI = as.numeric(cei))
}

periodos <- sort(unique(ventas_cluster$PERIODO))
cei_mensual <- purrr::map_df(periodos, calcular_cei, datos = ventas_cluster, W = W_full)

# ---- 11. (pendiente) Validación contra mora ------------------------------
# Falta traer dta_empresas_cartera agregada por (PERIODO, CLUSTER) y unirla
# aquí, igual que en el borrador anterior (mora_t1, mora_t2, modelo lm()).
# Se completa en el próximo paso una vez confirmes cómo viene esa tabla.
