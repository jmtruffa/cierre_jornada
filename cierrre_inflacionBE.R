
# hasta qué fecha tengo en la DB
dbExecuteQuery(query = "select max(fecha) from datos_infla_be", server = server, port = port) %>% pull()

# Define the start and end dates
start_date = start_date_inflabe
end_date = end_date_inflabe 

# Generate all working days between start_date and end_date
working_days = bizseq(start_date, end_date, cal = cal)

# Initialize an empty dataframe to store results
all_results = tibble()

# Loop through each working day and call the function
for (fecha in working_days) {
  #result = calculate_be_inflation(fecha, cal)
  result = finance::inflacionbe(fecha = fecha, server = server, port = port)
  all_results = bind_rows(all_results, result)
}

## Actualiza la tabla de inflabe en la base de datos
# consultamos la última fecha de la tabla datos_infla_be
dbWriteDF(table = "datos_infla_be", df = all_results, append = T, server = server, port = port)
# acá pongo desde cuando me traigo para graficar
db_infla_be = dbExecuteQuery(query = paste0("SELECT fecha, fechas_tasa_nominal, \"BE_inflation_mensual\", \"BE_inflation_anual\" FROM datos_infla_be WHERE fecha >= '", start_date_inflabe_graph, "'"), server = server, port = port)

# seteamos comas y puntos -> español
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

labelsBE = db_infla_be %>% 
  filter(fecha == max(fecha)) %>% 
  select(fecha, fechas_tasa_nominal, BE_inflation_mensual) %>%
  mutate(
    mes = paste0(
      tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
      "/", year(fechas_tasa_nominal)
    )
  ) %>% 
  select(-fechas_tasa_nominal) 

labelsBE_tea = db_infla_be %>% 
  filter(fecha == max(fecha)) %>% 
  select(fecha, fechas_tasa_nominal, BE_inflation_anual) %>%
  mutate(
    mes = paste0(
      tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
      "/", year(fechas_tasa_nominal)
    )
  ) %>% 
  select(-fechas_tasa_nominal) 

#be_inflation = all_results %>% left_join(labelsBE) %>% select(fecha, BE_inflation_mensual, mes, mes)



######
# curva proyectada de la última posición
maxBEInflation = db_infla_be %>% 
  filter(fecha == max(fecha)) %>% pull(BE_inflation_mensual) %>% max()
minBEInflation = db_infla_be %>% 
  filter(fecha == max(fecha)) %>% pull(BE_inflation_mensual) %>% min()

# g_inflabe_ultima = db_infla_be %>% 
#   filter(fecha == max(fecha)) %>% 
#   mutate(
#     mes = paste0(
#       tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
#       "/", year(fechas_tasa_nominal)
#     )
#   ) %>% 
#   select(fechas_tasa_nominal, BE_inflation_mensual, mes) %>% 
#   ggplot(aes(x = fechas_tasa_nominal, y = BE_inflation_mensual, group = 1, label = mes)) +
#   theme_usado() +
#   geom_point(color = .paleta[1]) +
#   geom_line(linewidth = 1, color = .paleta[1]) +
#   ggrepel::geom_label_repel(show.legend = FALSE, nudge_x = 0.5) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 0.01),
#                      breaks = breaks_extended(8),
#                      limits = c(minBEInflation * .9,  maxBEInflation * 1.1),
#                      name = "Break Even Inflation TEM") +
#   scale_x_date(date_breaks = "1 month", 
#                labels = label_date("%b\n%Y", locale = "es"),
#                expand = c(0.1,0.0))  +
#   
#   labs(title = "CURVA BREAKEVEN INFLATION",
#        subtitle = paste0('Por interpolado de curvas Lecap, Boncer. Último dato: ', max(db_infla_be$fecha)),
#        y = 'Inflación Mensual Breakeven (%)',
#        x = '',
#        caption = paste0(.pie, " en base a precios de mercado."))

tabla_promedios <- db_infla_be %>% 
  filter(fecha == end_date) %>% 
  mutate(anio = year(fechas_tasa_nominal)) %>%
  group_by(anio) %>%
  summarise(promedio = mean(BE_inflation_anual, na.rm = TRUE)) %>%
  mutate(
    anio = as.character(anio),  # Convertimos a texto para evitar separadores
    promedio = paste0(round(promedio * 100, 0), "%")
  ) %>%
  rename("AÑO" = anio, "PROMEDIO" = promedio)

t_inflacion_be_anual = flextable(tabla_promedios) %>%
  set_caption("PROMEDIOS INFLACIÓN BREAK EVEN") %>%
  align(align = "center", part = "all") %>%
  width(width = 2.5) %>%  # Ajustamos ancho general de columnas
  add_footer_lines(values = paste0("Outlier a estimaciones por regresión de fecha: ", end_date)) %>%
  autofit()

grabaTabla2(variable = t_inflacion_be_anual, path = path)

g_inflabe_anual_ultima = db_infla_be %>% 
  filter(fecha == start_date | fecha == end_date) %>% 
  #filter(fecha %in% tail(unique(fecha), 2)) %>%   # Filter the last two unique 'fecha' values
  mutate(
    mes = paste0(
      tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
      "/", year(fechas_tasa_nominal)
    ),
    group = as.factor(fecha)  # Create a 'group' variable for each unique 'fecha'
  ) %>% 
  select(fechas_tasa_nominal, BE_inflation_mensual, mes, group) %>% 
  ggplot(aes(x = fechas_tasa_nominal, y = BE_inflation_mensual, group = group, label = mes)) +
  theme_usado() +
  geom_point(aes(color = group), show.legend = F) +
  geom_line(linewidth = 1, aes(color = group)) +
  ggrepel::geom_label_repel(show.legend = FALSE, nudge_x = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01),
                     breaks = breaks_extended(8),
                     limits = c(minBEInflation * .9,  maxBEInflation * 1.2),
                     name = "Break Even Inflation TEM") +
  scale_x_date(date_breaks = "1 month", 
               labels = label_date("%b\n%Y", locale = "es"),
               expand = c(0.1,0.0))  +
  
  labs(title = "CURVA BREAKEVEN INFLATION",
       subtitle = paste0('Por interpolado de curvas Lecap, Boncer. Último dato: ', max(db_infla_be$fecha)),
       y = 'Inflación Mensual Breakeven (%)',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado.")) +
  scale_color_manual(values = .paleta, name = NULL) 

# imprimimos tabla con BE inflation para una fecha puntual

datosAL = max(db_infla_be$fecha)
texto = paste0("Inflación BE con datos al: ", datosAL)
tabla_be = db_infla_be %>% as_tibble() %>% 
  mutate(
    mes = paste0(
      tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))),
      " ",
      year(fechas_tasa_nominal)
    )
  ) %>% 
  filter(fecha == datosAL) %>% 
  select(mes, BE_inflation_mensual) %>% 
  mutate(BE_inflation_mensual = BE_inflation_mensual * 100) %>%
  mutate(BE_inflation_mensual = paste0(formatC(BE_inflation_mensual, format = "f", digits = 2, decimal.mark = ","), "%")) %>%
  flextable() %>% 
  flextable::set_caption(caption = 
                           as_paragraph(
                             as_chunk(
                               texto,
                               props = fp_text_default(font.size = 14, font.family = "MS Sans Serif"), bold = TRUE)
                           )
  ) %>% 
  flextable::add_footer_lines(values = as_paragraph("En base a curvas estimadas por regresión.")) %>% 
  flextable::set_header_labels(values = c("Mes", "Inflación BE")) %>% 
  width(j = 1, width = 2) %>% 
  width(j = 2, width = 2) %>% 
  align(align="center", part = "body") %>% 
  align(align="center", part = "header")  %>% 
  fontsize(size = 12, part = "header") 

grabaTabla2(variable = tabla_be, path = path)

# revertimos
Sys.setlocale("LC_TIME", "en_US.UTF-8")

  g_inflabe_dinamica = db_infla_be %>% 
    filter(fecha >= "2024-07-01") %>% 
    mutate(
      mes = paste0(
        tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
        "/", year(fechas_tasa_nominal)
      )
    ) %>% 
    {
      # Calculate unique mes values
      unique_mes_main <- length(unique(.$mes))
      unique_mes_labelsBE <- length(unique(labelsBE$mes))
      combined_unique_mes <- length(unique(c(.$mes, labelsBE$mes)))
      
      # # Debug output
      # message("Unique mes values in main dataset: ", unique_mes_main)
      # message("Unique mes values in labelsBE: ", unique_mes_labelsBE)
      # message("Combined unique mes values: ", combined_unique_mes)
      
      # Ensure consistent factor levels
      all_mes_levels <- unique(c(.$mes, labelsBE$mes))
      .$mes <- factor(.$mes, levels = all_mes_levels)
      labelsBE$mes <- factor(labelsBE$mes, levels = all_mes_levels)
      
      ggplot(data = ., aes(x = fecha, y = BE_inflation_mensual, color = mes, group = mes, label = mes)) +
        theme_usado() +
        geom_line(linewidth = 1) +
        ggrepel::geom_label_repel(data = labelsBE, show.legend = FALSE, nudge_x = 15) +
        scale_x_date(
          date_breaks = "2 weeks", 
          labels = label_date("%d-%b\n%Y", locale = "es"),
          expand = c(0.05, 0.0)
        ) +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 0.01),
          breaks = breaks_extended(12),
          name = "Break Even Inflation"
        ) +
        scale_color_manual(
          values = colorRampPalette(.paleta)(combined_unique_mes),  # Use combined unique mes values
          name = "Meses"
        ) +
        labs(
          title = "CURVA BREAKEVEN INFLATION",
          subtitle = paste0('Por interpolado de curvas Lecap, Boncer. Último dato: ', max(.$fecha)),
          y = 'Inflación Mensual Breakeven (%)',
          x = '',
          caption = paste0(.pie, " en base a precios de mercado.")
        )
    }
  grabaGrafo(variable = g_inflabe_dinamica, path = path)

g_inflabe_dinamica_tea = db_infla_be %>% 
  filter(fecha >= "2024-07-01") %>% 
  #mutate(mes = tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE)))) %>%  
  mutate(
    mes = paste0(
      tools::toTitleCase(as.character(month(fechas_tasa_nominal, label = TRUE, abbr = FALSE))), 
      "/", year(fechas_tasa_nominal)
    )
  ) %>% 
  ggplot(aes(x = fecha, y=BE_inflation_anual, color = as.factor(mes), group = mes, label = mes)) +
  theme_usado() +
  geom_line(linewidth=1) +
  
  #ggrepel::geom_label_repel(data = labelsBE_tea, show.legend = FALSE, nudge_x = 15) +
  
  scale_x_date(date_breaks = "2 weeks", labels = label_date("%d-%b\n%Y", locale = "es"),
               expand = c(0.15,0.0)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01),
                     breaks = breaks_extended(12),
                     name = "Break Even Inflation") +
  scale_color_manual(values = colorRampPalette(.paleta)(length(unique(db_infla_be$fechas_tasa_nominal))),
                     name = "Meses") +
  # scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "black", "brown", "pink", "gray", "yellow", "cornflowerblue", "navy", "cyan", "magenta", "gold", "darkolivegreen", "turquoise", "darkred", "darkblue", "lightgray"),
  #                    name = "Meses") +
  
  labs(title = "CURVA BREAKEVEN INFLATION",
       subtitle = paste0('Por interpolado de curvas Lecap, Boncer. Último dato: ', max(db_infla_be$fecha)),
       y = 'Inflación Mensual Breakeven (%)',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado."))

g_inflabe_dinamica_tea

grabaGrafo(variable = g_inflabe_dinamica_tea, path = path)

