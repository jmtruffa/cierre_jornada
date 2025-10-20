desde = "2024-01-01"

adelantos = bcra::getDatosVariable(idVariable = 145, desde = desde, hasta = Sys.Date()) %>% as_tibble()
adelantos$adelanto_empresas = adelantos$valor / 100
adelantos$valor = NULL

prestamos = bcra::getDatosVariable(idVariable = 144, desde = desde, hasta = Sys.Date()) %>% as_tibble()
prestamos$personales = prestamos$valor / 100
prestamos$valor = NULL

tasa_tamar = bcra::getDatosVariable(idVariable = 136, desde = desde, hasta = Sys.Date()) %>% as_tibble()
tasa_tamar$valor = tasa_tamar$valor / 100


tasas_privados = full_join(
  adelantos %>% rename(adelantos = adelanto_empresas),
  prestamos  %>% rename(personales = personales),
  by = "date"
) %>%
  full_join(tasa_tamar %>% rename(tasa_tamar = valor), by = "date") %>%
  arrange(date)

g_tasas_privados = tasas_privados %>% 
  pivot_longer(!date) %>% 
  ggplot(aes(x=date, y=value, label = name, group = name, color = name)) +
  theme_usado() +
  geom_line(size=1) +
  #ggrepel::geom_text_repel(show.legend = F) +
  scale_color_manual(name = NULL, values = .paleta, labels = c("Adelantos","Personales", "Tamar Privados")) +  
  scale_y_continuous(breaks = breaks_extended(10), 
                     labels = scales::percent,
                     #limits = c(.03, .0425)
  ) +
  scale_x_cont_dates(name = "", 
                     business.dates = tasas_privados$date, labels=label_date(format = "%b-%y", locale = "es"), 
                     max.major.breaks=20) +
  labs(title = "TASAS PRESTAMOS Y ADELANTOS",
       subtitle = paste0('Último dato: ', max(tasas_privados$date)),
       y = 'TNA',
       x = '',
       caption = paste0(.pie, " en base a BCRA."))
grabaGrafo(variable = g_tasas_privados, name = "g_tasas_privados", path = path)
