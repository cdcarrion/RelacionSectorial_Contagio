# =====================================================================
# CORRELACIÓN SECTORIAL — pipeline limpio
# Reconstruido a partir de tu script original (CorrelacionSectorial.R /
# Code_CorrelacionSectorial - 2024.R). Revisa las rutas y nombres de
# columna marcados con "AJUSTA" antes de correr — algunas líneas del
# original quedaron cortadas en las capturas.
# =====================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ---- 1. Parámetros -----------------------------------------------------

ruta_base <- "C:/Users/crcarrio/OneDrive - Banco Pichincha C.A/Actividades CDCC/4_CorrelacionSectorial"

anio_analisis <- 2025
meses_analisis <- NULL   # NULL = todos los meses disponibles (panel mensual)
                          # o un vector, p. ej. c(1,2,3) para un trimestre

umbral_prop <- 0         # corte mínimo de peso (magnitud w_ij), no de conteo

# ---- 2. Carga de datos --------------------------------------------------

# La hoja "Ud" del archivo del BCE (mip_2024P.xlsm) trae títulos de sección
# en las primeras 6 filas, el encabezado real en la fila 7 (Código, Industria,
# 01..76, Total consumo intermedio, demanda final...) y una fila extra de
# descripciones (fila 8) antes de que empiecen los datos -- por eso se lee
# sin encabezado automático y se reconstruye a mano.
ud_raw <- read_excel(file.path(ruta_base, "mip_2024P.xlsm"), sheet = "Ud",
                     skip = 6, col_names = FALSE)

encabezados <- as.character(unlist(ud_raw[1, ]))
encabezados[1] <- "ID"; encabezados[2] <- "Codigo"; encabezados[3] <- "Industria"

dta_bce <- ud_raw[-c(1, 2), ]              # quita encabezado y fila de descripciones
names(dta_bce) <- encabezados
dta_bce <- dta_bce %>%
  filter(!is.na(Codigo)) %>%
  select(Código = Codigo, Industria, all_of(sprintf("%02d", 1:76))) %>%
  mutate(across(all_of(sprintf("%02d", 1:76)), as.numeric))
# Fila 77 = "Compras Directas" (categoría residual del BCE, sin columna
# destino propia) -- se queda con w_ij = 0 en todas sus relaciones gracias
# al ifelse(.row_sum==0,...) de más abajo, no hace falta excluirla a mano.
dta_bce_codigo <- read_excel("C:/Users/crcarrio/Downloads/correlacionador_productos.xlsx")

dta_bitacora2026 <- read_excel(file.path(ruta_base, "AJUSTA_bitacora.xlsx")) %>%
  separate(`ACTIVIDAD ECONÓMICA`,
           into = c("codigo_actividad", "descripcion_actividad"),
           sep = "\\|", remove = FALSE)

dta_empresas_ventas  <- read_excel(file.path(ruta_base, "AJUSTA_ventas.xlsx"))
dta_empresas_cartera <- read_excel(file.path(ruta_base, "AJUSTA_cartera.xlsx"))  # <- aquí probablemente vive tu mora
dta_empresas_PI      <- read_excel(file.path(ruta_base, "AJUSTA_PI.xlsx"))

# ---- 3. Normalización BCE — w_ij (sección 5.2 de la metodología) --------

# AJUSTA: confirma si son 70 u 76 columnas de industria en tu tabla real.
# El script original tenía el comentario "01-70" pero el rango usado era
# 1:76 -> si 71:76 son totales/subtotales, sácalos de aquí o corrompen
# tanto el rowSums() como la normalización.
cols_bce <- sprintf("%02d", 1:76)

dta_bce <- dta_bce %>%
  mutate(across(all_of(cols_bce), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(.row_sum = rowSums(across(all_of(cols_bce)), na.rm = TRUE)) %>%
  mutate(across(all_of(cols_bce), ~ ifelse(.row_sum == 0, 0, . / .row_sum))) %>%
  select(-.row_sum)

dta_long <- dta_bce %>%
  pivot_longer(cols = all_of(cols_bce), names_to = "col", values_to = "prop")

# Relaciones ordenadas por MAGNITUD real (no por conteo). Esto reemplaza
# el paso de "conteo en Excel" — ya tienes prop = w_ij, úsalo directo.
frames_por_codigo <- dta_long %>%
  filter(prop > umbral_prop) %>%
  group_by(Código, Industria) %>%
  arrange(desc(prop), .by_group = TRUE) %>%
  select(Código, Industria, col, prop) %>%
  ungroup()

# ---- 4. Ventas repartidas por relación sectorial (Rkj = Ventas_k * w_ij) --

construir_tabla_relacion <- function(anio, meses = NULL) {

  ventas_filtradas <- dta_empresas_ventas %>%
    filter(`ANIO FISCAL` == anio)

  if (!is.null(meses)) {
    ventas_filtradas <- ventas_filtradas %>% filter(`MES FISCAL` %in% meses)
  }

  ventas_filtradas %>%
    left_join(
      dta_bce_codigo %>% distinct(CIIU4_6D, `CÓDIGO DE INDUSTRIA ECUATORIANO_CIE`),
      by = c("ACTIVIDAD ECONOMICA" = "CIIU4_6D")
    ) %>%
    left_join(
      frames_por_codigo,
      by = c("CÓDIGO DE INDUSTRIA ECUATORIANO_CIE" = "Código")
    ) %>%
    group_by(PERIODO, `ACTIVIDAD ECONOMICA`) %>%
    mutate(TOTAL_VENTAS2 = prop * `TOTAL VENTAS`) %>%
    ungroup()
}

Tab1 <- construir_tabla_relacion(anio_analisis, meses_analisis)

# ---- 5. Agregación reutilizable (Entregable 2 y Entregable CIIU6) --------
#
# `colapsar_a`: si se indica una columna (p. ej. "CLUSTER"), se dedupea a
#   esa granularidad y la venta se reparte por partes iguales entre los
#   valores distintos encontrados en cada (PERIODO, ACTIVIDAD ECONOMICA, col)
#   -- este era el comportamiento original de la versión CLUSTER.
# `colapsar_a = NULL`: no se dedupea nada (versión CIIU6) — se reparte la
#   venta entre TODAS las filas del abanico, sin perder detalle. Este es
#   el fix del bug descrito arriba.
#
# Nota de diseño a revisar contigo: repartir "por partes iguales entre
# valores distintos" no es lo mismo que repartir "proporcional a cuántos
# códigos CIIU caen en cada valor". Si 9 de 10 códigos abanicados van a
# un mismo clúster A y solo 1 al clúster B, el reparto actual da 50/50,
# no 90/10. Si prefieres el reparto proporcional, dime y lo ajustamos
# (basta con no dedupear también en la versión CLUSTER).

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

# --- Entregable 2: por CLUSTER ---
Tab2 <- repartir_ventas(Tab1, colapsar_a = "CLUSTER") %>%
  group_by(PERIODO, `SECTOR BP`, CLUSTER) %>%
  summarise(TOTAL_VENTAS4 = sum(TOTAL_VENTAS3, na.rm = TRUE), .groups = "drop") %>%
  group_by(PERIODO, `SECTOR BP`) %>%
  mutate(total = sum(TOTAL_VENTAS4, na.rm = TRUE),
         perc  = TOTAL_VENTAS4 / total) %>%
  ungroup()

# --- Entregable por CIIU6 (sin dedup -> conserva el detalle) ---
Tab3 <- repartir_ventas(Tab1, colapsar_a = NULL) %>%
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

# ---- 6. Exportar ---------------------------------------------------------

write_xlsx(Tab2, file.path(ruta_base, "5_Documentacion/Req29052025_cluster_2024.xlsx"))
write_xlsx(Tab3, file.path(ruta_base, "5_Documentacion/Req29052025_ciiu6_2024.xlsx"))

# =====================================================================
# NOTA IMPORTANTE PARA VALIDAR EL REFACTOR
# =====================================================================
# Antes de reemplazar tu script original, corre ambas versiones sobre el
# mismo mes (ej. ANIO FISCAL=2025, MES FISCAL=12) y compara Tab2/Tab3
# fila por fila. Si coinciden en todo salvo en el detalle CIIU6 (que
# ahora sí debería tener más filas que antes por el fix del bug), el
# refactor es correcto.
# =====================================================================
