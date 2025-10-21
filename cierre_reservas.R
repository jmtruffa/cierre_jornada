reservas = bcra::getDatosVariable(idVariable = 1, desde = "2020-01-01", hasta = Sys.Date())
# traigo los valores scrapeados de reservas que vienen del twitter del bcra
res = functions::dbGetTable(table = "reservas_scrape", server = server, port = port)

reservas = bind_rows(
  reservas,                              
  anti_join(res, reservas, by = "date")  # trae las de res que NO están en reservas
) %>%
  arrange(date) %>% 
  distinct(date, .keep_all = TRUE)

reservas = reservas %>% 
    mutate(
      reservasTXT = format(round(valor, 0), nsmall = 0, big.mark = ".", decimal.mark = ",")
    )

g_reservas_brutas = reservas %>% 
  
  ggplot(aes(x=date, y=valor, label = reservasTXT )) +
  theme_usado() +
  geom_line(linewidth=1, color = .paleta[1]) +
  
  ggrepel::geom_text_repel(data = reservas %>% tail(n=1), nudge_y = 1050,nudge_x = 40) +
  
  scale_x_date(date_breaks="3 months", labels = scales::label_date(format = "%b\n %Y", locale="es"),
               expand = c(0.01,0)) +
  scale_y_continuous(breaks = scales::breaks_extended(6),
                     labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  
  labs(title = "RESERVAS INTERNACIONALES BRUTAS",
       subtitle = paste0('Valores al: ', reservas %>% tail(n=1) %>% pull(date)),
       y = 'Millones de USD',
       x = '',
       caption = paste0(.pie, " en base a datos BCRA") )

grabaGrafo(variable = g_reservas_brutas, path = path)


# reservas_last_day <- reservas %>%
#   # Agrupar por año y mes
#   group_by(Year = year(date), Month = month(date)) %>%
#   # Seleccionar la fila con la fecha máxima dentro de cada grupo
#   slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
#   # Desagrupar los datos
#   ungroup() %>%
#   # Opcional: Eliminar las columnas auxiliares de año y mes
#   select(-Year, -Month) %>% 
#   #agrega variaciones entre los valores
#   mutate(
#     variacion = ((valor - lag(valor)))
#   ) %>% select(-reservasTXT, date, reservas = valor, variacion)
# tail(reservas_last_day)
