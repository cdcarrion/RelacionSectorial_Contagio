# =====================================================================
# CONTAGIO SECTORIAL — Paso 5: casos ilustrativos
# Convierte los resultados de tasa_contagio en algo que se pueda explicar
# a alguien sin conocimiento técnico: fechas reales, % de variación real,
# y un gráfico de las dos series.
# Requiere: ventas_cluster, tasa_contagio, umbral_caida (de Contagio_correcciones.R)
# =====================================================================

library(dplyr)
library(ggplot2)
library(scales)

# ---- 20. Tabla narrativa de un caso específico ---------------------------

explicar_caso <- function(origen, destino, umbral = umbral_caida){

  serie_origen <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN == origen) %>%
    select(fecha, var_interanual, z = z_shock_rolling) %>%
    rename(var_origen = var_interanual, z_origen = z)

  serie_destino <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN == destino) %>%
    arrange(fecha) %>%
    mutate(z_destino_siguiente = lead(z_shock_rolling, 1)) %>%
    select(fecha, var_interanual, z_shock_rolling, z_destino_siguiente) %>%
    rename(var_destino = var_interanual, z_destino = z_shock_rolling)

  tabla <- serie_origen %>%
    inner_join(serie_destino, by = "fecha") %>%
    arrange(fecha) %>%
    filter(z_origen <= umbral) %>%     # solo los meses donde el ORIGEN cayó fuerte
    transmute(
      Fecha = format(fecha, "%b %Y"),
      `Var. interanual ORIGEN` = percent(var_origen, accuracy = 0.1),
      `Var. interanual DESTINO (mismo mes)` = percent(var_destino, accuracy = 0.1),
      `¿Destino cayó, mismo mes?` = ifelse(z_destino <= umbral, "SÍ", "no"),
      `¿Destino cayó, mes siguiente?` = ifelse(z_destino_siguiente <= umbral, "SÍ", "no")
    )

  cat("\n============================================================\n")
  cat("CASO:", origen, "->", destino, "\n")
  cat("Cada fila es un mes en que", origen, "tuvo una caída significativa (z <=", umbral, ")\n")
  cat("============================================================\n")
  print(tabla, n = Inf)
  invisible(tabla)
}

# Ejemplo: el caso más fuerte de tu corrida
explicar_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO")

# ---- 21. Gráfico de las dos series, marcando los eventos ------------------

graficar_caso <- function(origen, destino, umbral = umbral_caida){

  datos <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN %in% c(origen, destino)) %>%
    mutate(rol = ifelse(CLUSTER_ORIGEN == origen,
                         paste0(origen, " (origen)"),
                         paste0(destino, " (destino)")))

  eventos_fecha <- ventas_cluster %>%
    filter(CLUSTER_ORIGEN == origen, z_shock_rolling <= umbral) %>%
    pull(fecha)

  ggplot(datos, aes(x = fecha, y = var_interanual, color = rol)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = eventos_fecha, linetype = "dotted",
               color = "firebrick", alpha = 0.5) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = percent) +
    labs(
      title = paste0(origen, "  vs.  ", destino),
      subtitle = "Líneas rojas punteadas = meses con caída significativa en el clúster de origen",
      x = NULL, y = "Variación interanual de ventas", color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

graficar_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO")

# ---- 22. Generar el reporte completo de los mejores casos -----------------
# Toma los pares más confiables de tasa_contagio (lift alto en AMBAS
# ventanas, no solo una) y arma tabla + gráfico de cada uno automáticamente
# -- listo para pegar en un documento o presentación.

casos_top <- tasa_contagio %>%
  filter(eventos_origen >= 5, lift_mismo_mes > 2, lift_mes_siguiente > 2) %>%
  distinct(CLUSTER_ORIGEN, CLUSTER_DESTINO) %>%
  arrange(CLUSTER_ORIGEN)

cat("\nCasos que cumplen el criterio (lift > 2 en ambas ventanas, >=5 eventos):",
    nrow(casos_top), "\n")

for (i in seq_len(nrow(casos_top))) {
  explicar_caso(casos_top$CLUSTER_ORIGEN[i], casos_top$CLUSTER_DESTINO[i])
  print(graficar_caso(casos_top$CLUSTER_ORIGEN[i], casos_top$CLUSTER_DESTINO[i]))
}

# Para guardar los gráficos a disco en vez de solo verlos en el visor:
# ggsave("caso_automotriz_transporte.png", graficar_caso("AUTOMOTRIZ", "TRANSPORTE Y ALMACENAMIENTO"),
#        width = 8, height = 5, dpi = 150)
