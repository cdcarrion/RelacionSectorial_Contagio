# =====================================================================
# EXPLICAR CASOS DE CONTAGIO — para mostrarle a alguien que no conoce
# la metodología a fondo. Usa lo que ya calculó Contagio_correcciones.R:
# ventas_cluster, trazabilidad, tasa_contagio, umbral_caida.
# =====================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# ---- 1. Narrativa de un caso puntual --------------------------------------
# Traduce las cifras de tasa_contagio a una explicación mes por mes, en
# porcentajes (más intuitivo que z-scores) para alguien no técnico.

explicar_caso <- function(origen, destino) {

  resumen <- tasa_contagio %>% filter(CLUSTER_ORIGEN == origen, CLUSTER_DESTINO == destino)
  if (nrow(resumen) == 0) {
    cat("Ese par no aparece en tasa_contagio (revisa el nombre exacto del clúster,\n",
        "o puede que se haya filtrado por tener pocos eventos).\n")
    return(invisible(NULL))
  }

  serie_origen <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN == origen) %>%
    select(fecha, var_interanual, z = z_shock_rolling) %>% arrange(fecha)

  serie_destino <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN == destino) %>%
    select(fecha, var_interanual, z = z_shock_rolling) %>% arrange(fecha)

  fechas_evento <- serie_origen %>% filter(z <= umbral_caida) %>% pull(fecha)

  cat(strrep("=", 60), "\n")
  cat(sprintf("CASO:  %s  ->  %s\n", origen, destino))
  cat(strrep("=", 60), "\n\n")

  cat(sprintf(
    "Según la matriz insumo-producto del BCE, %s depende de %s en un %.1f%% de sus relaciones.\n\n",
    origen, destino, resumen$peso * 100
  ))

  cat(sprintf(
    "En los últimos %d meses, %s tuvo %d caídas fuertes de ventas (frente a su propio historial).\n",
    resumen$n_meses, origen, resumen$eventos_origen
  ))

  cat(sprintf(
    "De esas caídas, %s también cayó fuerte:\n  - el MISMO mes en %.0f%% de los casos\n  - el mes SIGUIENTE en %.0f%% de los casos\n",
    destino, resumen$tasa_mismo_mes * 100, resumen$tasa_mes_siguiente * 100
  ))

  cat(sprintf(
    "\nEn general (sin condicionar en nada), %s cae fuerte solo el %.0f%% de los meses.\n",
    destino, resumen$tasa_base * 100
  ))

  cat(sprintf(
    "Por eso: cuando %s cae, la probabilidad de que %s también caiga es %.1fx (mismo mes) y %.1fx (mes siguiente) lo normal.\n\n",
    origen, destino, resumen$lift_mismo_mes, resumen$lift_mes_siguiente
  ))

  veredicto <- if (resumen$lift_mismo_mes > 2 & resumen$lift_mes_siguiente > 2) {
    "Señal consistente en ambas ventanas -> candidato real de contagio."
  } else if (resumen$lift_mismo_mes > 2 | resumen$lift_mes_siguiente > 2) {
    "Señal solo en una de las dos ventanas -> parcial, revisar con criterio experto."
  } else {
    "Lift bajo en ambas ventanas -> no hay evidencia clara de contagio en este par."
  }
  cat("Veredicto:", veredicto, "\n\n")

  cat("Detalle mes a mes:\n")
  for (f in fechas_evento) {
    o  <- serie_origen  %>% filter(fecha == f)
    d0 <- serie_destino %>% filter(fecha == f)
    d1 <- serie_destino %>% filter(fecha == f %m+% months(1))

    cat(sprintf("\n* %s: %s cayó %.1f%% frente a su historial.\n",
                format(f, "%b %Y"), origen, o$var_interanual * 100))

    if (nrow(d0) > 0) {
      marca <- if (d0$z <= umbral_caida) "SÍ cayó fuerte también" else "no tuvo caída notable"
      cat(sprintf("    %s ese mismo mes: %.1f%% (%s)\n", destino, d0$var_interanual * 100, marca))
    }
    if (nrow(d1) > 0) {
      marca <- if (d1$z <= umbral_caida) "SÍ cayó fuerte también" else "no tuvo caída notable"
      cat(sprintf("    %s al mes siguiente: %.1f%% (%s)\n", destino, d1$var_interanual * 100, marca))
    }
  }
  cat("\n")
}

# ---- 2. Gráfico del caso ---------------------------------------------------
# Visual para acompañar la narrativa: ambas series de ventas, con líneas
# verticales en los meses donde el origen tuvo una caída significativa.

graficar_caso <- function(origen, destino) {

  datos <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN %in% c(origen, destino)) %>%
    mutate(Clúster = CLUSTER_ORIGEN)

  fechas_evento <- datos %>%
    filter(Clúster == origen, z_shock_rolling <= umbral_caida) %>%
    pull(fecha)

  ggplot(datos, aes(x = fecha, y = var_interanual, color = Clúster)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = fechas_evento, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.6) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste0(origen, "  →  ", destino),
      subtitle = "Líneas rojas punteadas: meses con caída significativa en el clúster de origen",
      x = NULL, y = "Variación interanual de ventas"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# ---- 3. Ficha completa (narrativa + gráfico) para un caso -----------------

ficha_caso <- function(origen, destino, guardar = FALSE) {
  explicar_caso(origen, destino)
  g <- graficar_caso(origen, destino)
  print(g)
  if (guardar) {
    nombre <- paste0("caso_", gsub("[^A-Za-z0-9]", "_", origen), "_a_",
                      gsub("[^A-Za-z0-9]", "_", destino), ".png")
    ggsave(nombre, g, width = 8, height = 4.5, dpi = 150)
    cat("Gráfico guardado como:", nombre, "\n")
  }
}

# ---- 4. Selección automática de los mejores casos para mostrar -----------
# En vez de escoger a mano, esto filtra los pares más presentables: buena
# muestra y señal fuerte en ambas ventanas de tiempo.

mejores_casos <- tasa_contagio %>%
  filter(eventos_origen >= 5, lift_mismo_mes > 2, lift_mes_siguiente > 2) %>%
  arrange(desc(pmin(lift_mismo_mes, lift_mes_siguiente)))

cat("Casos con señal más consistente para mostrar como ejemplo:\n")
print(mejores_casos)

# Ejemplo de uso:
# ficha_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO")
# ficha_caso("ARROZ", "MANUFACTURA DE ALIMENTOS Y BEBIDAS")

# Para generar una ficha de cada uno de los mejores casos automáticamente:
# for (i in seq_len(nrow(mejores_casos))) {
#   ficha_caso(mejores_casos$CLUSTER_ORIGEN[i], mejores_casos$CLUSTER_DESTINO[i], guardar = TRUE)
# }
