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

g_tasas_privados = suppressWarnings(
  tasas_privados %>% 
    pivot_longer(!date) %>% 
    ggplot(aes(x=date, y=value, label = name, group = name, color = name)) +
    theme_usado() +
    geom_line(linewidth = 1) +
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
)
grabaGrafo(variable = g_tasas_privados, name = "g_tasas_privados", path = path)

## https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/diar_pas.xls 
url_bcra <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/diar_pas.xls"
tmp <- tempfile(fileext = ".xls")
suppressWarnings(
  download.file(url_bcra, destfile = tmp, mode = "wb", quiet = TRUE)
)

serie_remu <- suppressMessages(
  readxl::read_xls(
    path  = tmp,
    sheet = "Estra_dia_bcos.priv",
    range = "A2507"
  )
) %>%
  as_tibble() %>%
  select(1, 9)
unlink(tmp)

colnames(serie_remu) = c("date", "tasa_remu")
serie_remu$tasa_remu = serie_remu$tasa_remu / 100
serie_remu$date = as.Date(serie_remu$date, format = "%d/%m/%Y")
serie_remu = serie_remu %>% filter(!is.na(tasa_remu))

tasas_privados = tasas_privados %>%
  left_join(serie_remu, by = "date")

g_tasas_privados_remu = suppressMessages(
  suppressWarnings(
    tasas_privados %>%
      pivot_longer(!date) %>%
      ggplot(aes(x=date, y=value, label = name, group = name, color = name)) +
      theme_usado() +
      geom_line(linewidth = 1) +
      #ggrepel::geom_text_repel(show.legend = F) +
      scale_color_manual(name = NULL, values = .paleta, labels = c("Adelantos","Personales", "Remu", "Tamar Privados" )) +
      scale_y_continuous(breaks = breaks_extended(18),
                         labels = scales::percent,
                         limits = c(.05, 2)
      ) +
      scale_x_cont_dates(name = "",
                         business.dates = tasas_privados$date, labels=label_date(format = "%b\n%y", locale = "es"),
                         max.major.breaks=24) +
      labs(title = "TASAS PRESTAMOS, ADELANTOS, TAMAR y REMU",
           subtitle = paste0('Último dato: ', max(tasas_privados$date)),
           y = 'TNA',
           x = '',
           caption = paste0(.pie, " en base a BCRA."))
  )
)
grabaGrafo(variable = g_tasas_privados_remu, name = "g_tasas_privados_remu", path = path)
