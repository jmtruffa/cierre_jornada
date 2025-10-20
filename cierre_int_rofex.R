# Traigo histórico
rofexH = functions::dbGetTable(table = "rofexHis", server = server, port = port)
# busco última fecha  
lastDate = rofexH %>% tail(n=1) %>% pull(date)
# Asigno en newDate el día hábil siguiente a lastDate
newDate = bizdays::offset(lastDate, n = 1, cal = "cal")
#newDate = ymd(lastDate) + days(1)
# Traigo desde nueva fecha hasta ahora desde Rofex

rofexOI = rbind(rofexH, rofex::getRofexPosition(from = newDate, to = adjust.previous(Sys.Date(), cal = cal))[[1]])

# check de cantidad de cotizaciones por día
# rofexOI %>% group_by(date) %>% summarise(cotizaciones = n()) %>% tail(n=10)

# si cantidad cotizaciones de hoy < 10 o NULL entonces stop
if (nrow(rofexOI) == 0 | nrow(rofexOI %>% filter(date == Sys.Date())) < 10) {
  stop("No hay cotizaciones para hoy")
}
# Grabamos lo actualizado en la tabla
# hace overwrite porque antes bajó toda la tabla.
functions::dbWriteDF(table = "rofexHis", server = server, port = port, 
                     append = TRUE, 
                     df = rofexOI %>% filter(date > lastDate))


data = rofexOI %>%
  group_by(pos, date, symbol) %>% 
  summarise(
    OI = sum(openInterest, na.rm = T),
    Vol = sum(volume, na.rm = T)
  )

vol = data %>% 
  group_by(date) %>% 
  summarise(OI = sum(OI),
            vol = sum(Vol))

vol %>% 
  mutate(
    across(c(OI, vol), ~ . - lag(.,1), .names = "diff_{.col}")
  ) %>% 
  tail()

####################################################################################
###################
## Interés Abierto

g_rofex_intabierto = data %>% 
  filter(date >= "2023-05-01") %>%
  ggplot(aes(x=date, y=OI, fill = as.factor(pos))) +
  geom_area(stat = "identity") +
  gghighlight(pos <= 2) +
  
  theme_usado() +
  
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[2])) +

  labs(fill = "Posiciones") +
  
  geom_vline(xintercept = as.Date("2023-08-11")) +
  geom_vline(xintercept = as.Date("2023-10-20")) +
  geom_vline(xintercept = as.Date("2023-11-17")) +
  
  annotate("text", x = as.Date("2023-08-24"), y = 5.5e6, label = "PASO", size = 3.5) +
  annotate("text", x = as.Date("2023-11-09"), y = 5.5e6, label = "GRALES", size = 3.5) +
  annotate("text", label = "BALLOTAGE", x = as.Date("2023-12-14"), y = 4.9e6, size = 3.5) +

  scale_y_continuous(labels = scales::comma, breaks = breaks_extended(10)) +#,
                     #breaks = seq(0, 23000000, 500000)) +
  scale_x_date(date_labels = "%d-%m\n%Y", date_breaks = "2 month", expand = c(0.03,0)) +
  
  labs(title = "INTERES ABIERTO FUTUROS DLR",
       subtitle = 'Destacadas posiciones 1 y 2. En gris el resto de las posiciones agrupadas',
       y = '',
       x = '',
       caption = paste0(.pie, " en base a datos de A3") )
g_rofex_intabierto

grabaGrafo(variable = g_rofex_intabierto, path = path)

#################################
# Variación interés abierto por posición
# calcular el día de hoy menos un día hábil

dia = as.Date(ifelse(viernes, prev_friday_date, adjust.previous(Sys.Date() - 1, cal = cal))) #adjust.previous(Sys.Date() - 1, cal = cal)
dia2 = Sys.Date() 

varOI = data[,-c(5)] %>% 
  filter(date >= dia & date <= dia2) %>% 
  #group_by(symbol, pos) %>% 
  group_by(symbol) %>% 
  mutate(
    across(c(OI), ~ . - lag(.,1), .names = "diff_{.col}")
  ) %>% 
  summarise(
    sumDiffOI = sum(diff_OI, na.rm = T),
    .groups = 'drop'
  ) %>% 
  summarise(varOI = sum(sumDiffOI, na.rm = T)) %>% 
  pull(varOI)
subtitulo = paste0(
  'Desde el día ', dia, 
  ' al ', dia2, 
  '. Variación de I.A. en el período: ', 
  format(varOI, big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 0), 
  ' contratos.'
)

g_rofex_var_int_abierto = data[,-c(5)] %>% 
  filter(date >= dia & date <= dia2) %>%
  group_by(symbol, pos) %>% 
  mutate(
    across(c(OI), ~ . - lag(.,1), .names = "diff_{.col}")
  ) %>% 
  summarise(
    sumDiffOI = sum(diff_OI, na.rm = T),
    .groups = 'drop'
  ) %>% 
  arrange(pos) %>% 
  ggplot(aes(x=reorder(symbol, +pos), y=sumDiffOI)) +
  theme_usado() +
  geom_bar(stat = "identity", fill = .paleta[1]) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = breaks_extended(6)) +
  labs(title = "VARIACION INTERES ABIERTO FUTUROS DLR POR POSICION",
       subtitle = subtitulo,
       y = 'Cantidad de Contratos',
       x = '',
       caption = paste0(.pie, " en base a datos de A3"))
  
grabaGrafo(variable = g_rofex_var_int_abierto, path = path)


## distribucion de interes abierto por posición
fecha_int_abierto = max(data$date)

total = data %>% filter(date == fecha_int_abierto) %>% ungroup() %>% summarise(totalOI = sum(OI)) %>% pull(totalOI)

g_rofex_distro_int_abierto = rofexOI %>% 
  filter(date == fecha_int_abierto ) %>% 
  ggplot(aes(x=reorder(symbol, +pos), y=openInterest)) +
  theme_usado() +
  geom_bar(stat = "identity", fill = .paleta[1]) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = breaks_extended(10)) +
  labs(title = "DISTRIBUCION INTERES ABIERTO FUTUROS DLR POR POSICION",
       subtitle = paste0("Total de contratos al: ", fecha_int_abierto, " :", format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 0), " contratos"),
       y = 'Cantidad de Contratos',
       x = '',
       caption = paste0(.pie, " en base a datos de A3"))

grabaGrafo(variable = g_rofex_distro_int_abierto, path = path)

##################
# Volumen comparado entre ruedas de cada mes.
ultimo_dia_fila = vol %>%  
  filter(date >= "2024-08-01") %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% tail(n=1) %>% pull(fila)

g_rofex_vol_comparado = vol %>% 
  filter(date>="2024-08-01") %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% mutate(volAc = cumsum(vol)) %>% 
  mutate(mes = factor(mes, levels = c(8:12, 1:7))) %>% 
  ggplot(aes(x=fila, y=vol, group = mes)) +
  theme_usado() +
  geom_col(aes(fill = as.factor(mes)), width = 0.5, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(1,30, 1)) +
  scale_fill_manual(name = "Mes", values = colorRampPalette(.paleta)(12)) +
  #scale_fill_manual(name = "Mes", values = .paleta) +
  scale_y_continuous(breaks = breaks_extended(10), labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(title = "VOLUMEN OPERADO COMPARADO POR RUEDA FUTUROS DLR",
       subtitle = 'Diario. Todas las posiciones',
       y = 'Cantidad de Contratos',
       x = 'Número de rueda del mes',
       caption = paste0(.pie, " en base a datos de A3")) +
  theme(
    axis.text.x = element_text(size = ifelse(seq(1, 30) == ultimo_dia_fila, 14, 10), color = ifelse(seq(1, 30) == ultimo_dia_fila, "red", "black"))
  )

grabaGrafo(variable = g_rofex_vol_comparado, path = path)


g_rofex_vol_comparado_acum = vol %>% 
  filter(date>="2024-08-01") %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% mutate(volAc = cumsum(vol)) %>% 
  mutate(mes = factor(mes, levels = c(8:12, 1:7))) %>% 
  ggplot(aes(x=fila, y=volAc, group = mes)) +
  theme_usado() +
  geom_col(aes(fill = as.factor(mes)), width = 0.5, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(1,30, 1)) +
  scale_fill_manual(name = "Mes", values = colorRampPalette(.paleta)(12)) +
  #scale_fill_manual(name = "Mes", values = .paleta) +
  scale_y_continuous(breaks = breaks_extended(10), labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(title = "VOLUMEN OPERADO ACUMULADO COMPARADO POR RUEDA FUTUROS DLR",
       subtitle = 'Diario. Todas las posiciones',
       y = 'Cantidad de Contratos',
       x = 'Número de rueda del mes',
       caption = paste0(.pie, " en base a datos de A3")) +
  theme(
    axis.text.x = element_text(size = ifelse(seq(1, 30) == ultimo_dia_fila, 14, 10), color = ifelse(seq(1, 30) == ultimo_dia_fila, "red", "black"))
  )

grabaGrafo(variable = g_rofex_vol_comparado_acum, path = path)

#################################
#Volumen posicion rango de fechas
dia = Sys.Date()  #as.Date(ifelse(viernes, prev_friday_date, adjust.previous(Sys.Date() - 1, cal = cal)))
dia2 = Sys.Date() 

# pct Volumen por posicion en un rango de fechas
g_rofex_vol_rango = data[,-c(4)] %>% 
  filter(date >= dia & date <= dia2) %>%
  group_by(pos) %>% 
  summarise(volPosi = sum(Vol)) %>% 
  mutate(pct = volPosi / sum(volPosi) ) %>% 
  ggplot(aes(x=reorder(pos, +pos), y=volPosi)) + 
  theme_usado() +
  geom_bar(stat = "identity", fill = .paleta[1]) +
  
  geom_label(aes(y=pct, 
                 label = scales::percent(pct, accuracy=0.01)),
             nudge_y = 10.0,
             show.legend = T,
  ) +
  
  labs(title = "VOLUMEN OPERADO POR POSICIÓN FUTUROS DLR",
       subtitle = paste0('Información Acumulada del: ', dia, ' al ', dia2),
       y = "Contratos",
       x = '',
       caption = paste0(.pie, " en base a datos de A3") ) +
  scale_y_continuous(breaks = breaks_extended(10), labels = label_comma(big.mark = ".", decimal.mark = ",")) 

grabaGrafo(variable = g_rofex_vol_rango, path = path)


## Precios
g_rofex_precios = rofexOI %>% 
  select(date, settlement, pos) %>%
  filter(date >= "2023-07-01") %>% #"2023-07-01"
  ggplot(aes(date, settlement, col = as.factor(pos))) + 
  theme_usado() +
  geom_line() +
  gghighlight(pos %in% c(1,2,3)) +
  
  labs(title = "VALORES EN PESOS DE FUTUROS DLR",
       subtitle = 'Destacadas posiciones 1, 2 y 3.',
       y = 'Pesos por Dólar',
       x = '',
       caption = paste0(.pie, " en base a datos de A3")  ) +

  theme(legend.position = c(0.85, 0.15), legend.background = element_rect(
    fill = "white",       # color de fondo
    colour = "black",     # color del borde
    linewidth = 0.5       # grosor del borde
  )) +
  
  labs(color = "Posiciones") +
  
  scale_y_continuous(breaks = breaks_extended(10), labels = label_currency(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month",  label = scales::label_date("%b\n%Y", locale = "es")) +
  
  geom_vline(xintercept = as.Date("2023-08-11")) +
  geom_vline(xintercept = as.Date("2023-10-20")) +
  geom_vline(xintercept = as.Date("2023-11-17")) +

  annotate("text", label = "PASO", x = as.Date("2023-08-25"), y = 2000 ) +
  annotate("text", label = "GRALES", x = as.Date("2023-11-08"), y = 2000 ) +
  annotate("text", label = "BALLOTAGE", x = as.Date("2023-12-14"), y = 1900)

grabaGrafo(variable = g_rofex_precios, path = path)


g_rofex_precios_ytd = rofexOI %>% 
  select(date, settlement, pos) %>%
  filter(date >= "2025-01-01") %>% #"2023-07-01"
  ggplot(aes(date, settlement, col = as.factor(pos))) + 
  theme_usado() +
  geom_line() +
  gghighlight(pos %in% c(1,2,3)) +
  
  labs(title = "VALORES EN PESOS DE FUTUROS DLR",
       subtitle = 'Destacadas posiciones 1, 2 y 3.',
       y = 'Pesos por Dólar',
       x = '',
       caption = paste0(.pie, " en base a datos de A3")  ) +
  
  theme(legend.position = c(0.85, 0.15), legend.background = element_rect(
    fill = "white",       # color de fondo
    colour = "black",     # color del borde
    linewidth = 0.5       # grosor del borde
  )) +
  
  labs(color = "Posiciones") +
  
  scale_y_continuous(breaks = breaks_extended(10), labels = label_currency(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month",  label = scales::label_date("%b\n%Y", locale = "es")) 

grabaGrafo(variable = g_rofex_precios_ytd, path = path)

##################
## volúmen operado
g_rofex_vol_lineas = vol %>%
  filter(date >= "2023-06-01") %>%
  ggplot(aes(x=date, y=vol)) +
  theme_usado() +
  #geom_line()+
  geom_bar(stat = "identity", fill = .paleta[1], width = 1.2) +
  #geom_line(linewidth = 1, col = "cornflowerblue") +
  
  labs(title = "VOLUMEN OPERADO FUTUROS DLR",
       subtitle = 'Diario. Todas las posiciones',
       y = 'Cantidad de Contratos',
       x = '',
       caption = paste0(.pie, " en base a datos de A3")) +

  bdscale::scale_x_bd(business.dates = vol$date, labels=date_format("%d-%b\n%y", locale = "es"), max.major.breaks=23) +
  scale_y_continuous(breaks = breaks_extended(10), labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  #scale_x_date(date_breaks = "1 month") +
  
  # geom_vline(xintercept = as.Date("2023-08-11")) +
  # geom_vline(xintercept = as.Date("2023-10-20")) +
  geom_vline(xintercept = as.Date("2023-11-17")) +
  # 
  # annotate("text", label = "PASO", x = as.Date("2023-08-19"), y = 20000000, size = 3.5) +
  # annotate("text", label = "GRALES", x = as.Date("2023-10-31"), y = 20000000, size = 3.5) +
  annotate("text", label = "BALLOTAGE", x = as.Date("2023-12-15"), y = 3000000, size = 3.5) 

grabaGrafo(variable = g_rofex_vol_lineas, path = path)



### TEAs implicitas histórico
g_rofex_teas_historico = rofexOI %>% 
  select(date, impliedRateTEA, pos) %>%
  filter(date >= "2023-12-15") %>% 
  drop_na() %>% 
  ggplot(aes(date, impliedRateTEA, col = as.factor(pos))) + 
  outlier::theme_outlier() +
  #mpt::theme_mpt() +
  geom_line() + 
  gghighlight(pos %in% c(1,2,3)) +
  
  labs(title = "TASAS IMPLICITAS (TEA) FUTUROS DLR",
       subtitle = 'Destacadas posiciones 1, 2 y 3.',
       y = 'TEA',
       x = '',
       caption = paste0(.pie, " en base a datos de A3")  ) +
   
  labs(color = "Posiciones") +
  
  scale_y_continuous(breaks = breaks_extended(10), labels = label_percent(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "4 weeks",  label = scales::label_date("%d-%b\n%Y", locale = "es")) 

grabaGrafo(variable = g_rofex_teas_historico, path = path)
