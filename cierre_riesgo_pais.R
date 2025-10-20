file = paste0(format(Sys.Date(), "./%Y%m%d"), " embi")
download.file(url = "https://cdn.bancentral.gov.do/documents/entorno-internacional/documents/Serie_Historica_Spread_del_EMBI.xlsx?v=1713185477139",
              destfile = file)

rp = readxl::read_xlsx(file, skip = 1)

# borro el archivo file
file.remove(file)

rp$date = as.Date(rp$Fecha)
rp = rp %>% select(-Fecha)
rp = rp %>% relocate(date)


rp = rp %>%
  select(date, Argentina) %>%
  mutate(Argentina = Argentina * 100)

#rp %>% tail()

# rp = rp %>%
#   add_row(date = Sys.Date(), Argentina =  riesgo_pais)
  #add_row(date = as.Date("2025-03-07"), Argentina =  692)  
  
  
# fileDirectory = paste0('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/')
# fileName = paste0(fileDirectory, 'riesgoPais.csv')
# validUntil = 1500
#   
# if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
#   download.file(
#     paste0('http://clasico.rava.com/empresas/precioshistoricos.php?e=RIESGO%20PAIS&csv=1'),
#     fileName, 
#     mode = 'wb')
# }
#   
# rp = as_tibble(read.csv(fileName)) %>% select(date = fecha, Argentina = cierre)
# rp$date = as.Date(rp$date)
# tail(rp)
# rp = rp %>%
#  add_row(date = as.Date("2025-02-04"), Argentina = 640)
# controlo ultima fecha en db y actualizo si es necesario
rp_max_date_db = dbExecuteQuery(query = "select max(date) from riesgo_pais", server = server, port = port)$max
df_to_update = rp %>% filter(date > rp_max_date_db)
if (nrow(df_to_update) != 0) {
  df_to_update$Argentina <- round(df_to_update$Argentina)
  dbWriteDF(table = "riesgo_pais", df = df_to_update, port = port, server = server, append = TRUE)
}

valores = rp %>% filter(date >= "2019-12-10") %>% filter(  date == "2005-06-08" |
                          date == "2019-07-15" |
                          date == "2019-12-10" | 
                          date == "2020-03-23" |
                          date == "2020-09-10" |
                          date == "2023-10-05" |
                          date == "2024-01-09" | 
                          date == "2024-08-05" |  
                          date == "2025-04-08" |
                          date == max(date)) 
  

g_riesgopais = rp %>% 
  filter(date >= "2014-12-10") %>% 
  ggplot(aes(x=date, y=Argentina, label = sprintf("%.0f",Argentina))) +
  geom_line(linewidth = 1, color = .paleta[1]) +
  
  theme_usado() +
  
  geom_text_repel(data = valores, nudge_y = 3,nudge_x = 5) +
  
  #geom_hline(yintercept = tail(rp, n=1) %>% pull(Argentina), linetype = "dashed") +
  #geom_vline(xintercept = as.Date("2024-07-15"), linetype = "dashed")
  
  scale_y_continuous(breaks = breaks_extended(8)) +
  scale_x_date(date_breaks="1 year", date_labels="%b\n %Y",
               expand = c(0.05,0)) +

  labs(title = "RIESGO PAIS ARGENTINA",
       subtitle = paste0('EMBI. Valores al: ', rp %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado") ) 

grabaGrafo(name = "g_riesgo_pais", variable = g_riesgopais, path = path)

valores_ytd = rp %>% filter(date >= "2019-12-10") %>% filter(
                                                             date == "2025-04-08" |
                                                             date == max(date)) %>% filter(date >= "2025-01-01")


g_riesgopais_ytd = rp %>% 
  filter(date >= "2025-01-01") %>% 
  ggplot(aes(x=date, y=Argentina, label = sprintf("%.0f",Argentina))) +
  geom_line(linewidth = 1, color = .paleta[1]) +
  
  theme_usado() +
  
  geom_text_repel(data = valores_ytd, nudge_y = 3,nudge_x = 5) +
  
  #geom_hline(yintercept = tail(rp, n=1) %>% pull(Argentina), linetype = "dashed") +
  #geom_vline(xintercept = as.Date("2024-07-15"), linetype = "dashed")
  
  scale_y_continuous(breaks = breaks_extended(8)) +
  scale_x_date(date_breaks="2 weeks", date_labels="%d - %b\n %Y",
               expand = c(0.05,0)) +
  
  labs(title = "RIESGO PAIS ARGENTINA",
       subtitle = paste0('EMBI. Valores al: ', rp %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado") ) 

grabaGrafo(name = "g_riesgo_pais_ytd", variable = g_riesgopais_ytd, path = path)

