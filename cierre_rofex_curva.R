
date1 = as.Date(ifelse(viernes, prev_friday_date, adjust.previous(Sys.Date() - 1, cal = cal)))
date2 = Sys.Date() 

query = paste0("SELECT * FROM \"rofexHis\" WHERE date = '", date1, "' OR date ='", date2, "' ")
rofex = dbExecuteQuery(query = query, server = server, port = port)

# si cantidad cotizaciones de hoy < 10 o NULL entonces stop
if (nrow(rofex) == 0 | nrow(rofex %>% filter(date == date2)) < 10) {
  stop("No hay cotizaciones para hoy")
}

if (max(tc$date, na.rm = TRUE) != Sys.Date()) tc <- dbGetTable(table = "A3500", server = server, port = port)

datosGrafico = rofex %>% 
  left_join(tc) %>% 
  mutate(
         impliedRateTEA_last = ((settlement / last_mlc) ^ (365 / daysToMaturity)) - 1,
         impliedRateTNA_last = ((settlement / last_mlc) - 1) * 365 / daysToMaturity,
         ) %>% 
  select(date, symbol, settlement, impliedRateTNA, impliedRateTEA, impliedRateTEA_last, impliedRateTNA_last, pos, A3500, last_mlc) %>% 
  group_by(date) %>% 
  mutate(
    pase = ifelse(pos == 1, settlement / A3500 - 1, settlement / lag(settlement) - 1),
    pase_last = ifelse(pos == 1, settlement / last_mlc - 1, settlement / lag(settlement) - 1)
  ) %>% 
  group_by(pos) %>% 
  arrange(symbol) %>% 
  mutate(
    comparacion = paste0(symbol, " vs ", lag(symbol)),
    nombre = paste0(symbol, " (", settlement,  ")")
  ) %>% 
  ungroup()

############################
################################
################################    TEM VS A3500
datosGraficoTEM = datosGrafico %>% 
  mutate(
    TEM = (1+impliedRateTEA) ^ (1 / 12) - 1
  )


coeff = mean(datosGraficoTEM$TEM, na.rm = T) / mean(datosGraficoTEM$pase, na.rm = T)*.5

g_rofex_curva_tem = datosGraficoTEM %>%
  filter(date==date1 | date == date2) %>% 
  ggplot(aes(x=reorder(pos, +pos), group = date, color = as.factor(date))) +
  
  geom_col(aes(y=pase, fill = as.factor(date), color = as.factor(date)),
           linewidth= 1, 
           position = position_dodge2(preserve = "single"), 
           show.legend = FALSE,
           width = 0.75,
  ) +
  theme_usado() +
  geom_point(aes(y=TEM)) +
  geom_smooth(aes(y=TEM), se = F, show.legend = F) +
  #geom_line(aes(y=TEM, linetype = "TEM"), linewidth = 1) +
  
  geom_label_repel(aes(y=TEM, 
                       label = scales::percent(TEM, accuracy=0.01),
                       color = as.factor(date)),
                   nudge_y = -0.0,
                   show.legend = FALSE,
                   #color = "black",
                   max.overlaps = 9,
  ) +
  scale_y_continuous(name = "TEM",
                     labels = label_percent(accuracy = 0.01), breaks = breaks_extended(10),
                     ,
                     
  ) +
  scale_x_discrete(name = "Contrato desde cada fecha. (Barras es pase entre posiciones)") +
  scale_color_manual(name = "Fecha", values = c(.paleta[1], .paleta[2], .paleta[3])) +
  scale_linetype_manual(name = "Tasa", values = c(1 , 3)) +
  guides(fill = "none")  +
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[2])) +
  
  theme(legend.position.inside = c(0.95, 0.2), legend.box.background = element_rect()) +
  
  labs(title = "IMPLICITAS FUTUROS DLR (A3500)",
       subtitle = paste0('Curva Implícitas y barras de pase entre vencimientos. Primer pase contra A3500'),
       caption = paste0(.pie, " en base a precios de mercado (A3)"))

grabaGrafo(variable = g_rofex_curva_tem, path = path)

############################
################################
################################    TEM VS LAST
datosGraficoTEM = datosGrafico %>% 
  mutate(
    TEM = ifelse(impliedRateTEA_last == -1, NA, (1 + impliedRateTEA_last) ^ (1 / 12) - 1)
    #TEM = (1+impliedRateTEA_last) ^ (1 / 12) - 1
  )

coeff = mean(datosGraficoTEM$TEM, na.rm = T) / mean(datosGraficoTEM$pase_last, na.rm = T)*.5

g_rofex_curva_tem_last = datosGraficoTEM %>%
  filter(date==date1 | date == date2) %>% 
  ggplot(aes(x=reorder(pos, +pos), group = date, color = as.factor(date))) +
  
  geom_col(aes(y=pase_last, fill = as.factor(date), color = as.factor(date)),
           linewidth= 1, 
           position = position_dodge2(preserve = "single"), 
           show.legend = FALSE,
           width = 0.75,
  ) +
  theme_usado() +
  geom_point(aes(y=TEM)) +
  geom_smooth(aes(y=TEM), se = F, show.legend = F) +
  #geom_line(aes(y=TEM, linetype = "TEM"), linewidth = 1) +
  
  geom_label_repel(aes(y=TEM, 
                       label = scales::percent(TEM, accuracy=0.01),
                       color = as.factor(date)),
                   nudge_y = -0.0,
                   show.legend = FALSE,
                   #color = "black",
                   max.overlaps = 9,
  ) +
  scale_y_continuous(name = "TEM",
                     labels = label_percent(accuracy = 0.01), breaks = breaks_extended(10),
                     ,
                     
  ) +
  scale_x_discrete(name = "Contrato desde cada fecha. (Barras es pase entre posiciones)") +
  scale_color_manual(name = "Fecha", values = c(.paleta[1], .paleta[2], .paleta[3])) +
  scale_linetype_manual(name = "Tasa", values = c(1 , 3)) +
  guides(fill = "none")  +
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[2])) +
  
  theme(legend.position.inside = c(0.95, 0.2), legend.box.background = element_rect()) +
  
  labs(title = "IMPLICITAS FUTUROS DLR (LAST)",
       subtitle = paste0('Curva Implícitas contra last MLC y barras de pase entre vencimientos. Primer pase contra LAST MLC'),
       caption = paste0(.pie, " en base a precios de mercado (A3)"))

grabaGrafo(variable = g_rofex_curva_tem_last, path = path)


################################
#######SOLO TEM SIN PASES
datosGraficoTEM = datosGrafico %>% 
  mutate(
    TEM = (1+impliedRateTEA) ^ (1 / 12) - 1
  )


coeff = mean(datosGraficoTEM$TEM, na.rm = T) / mean(datosGraficoTEM$pase, na.rm = T)*.5

g_rofex_curva_solo_tem = datosGraficoTEM %>% arrange(date)  %>% 
  filter(date==date1 | date == date2) %>% 
  ggplot(aes(x=reorder(pos, +pos), group = date, color = as.factor(date))) +
  
  theme_usado() +
  geom_point(aes(y=TEM)) +
  geom_smooth(aes(y=TEM), se = F, show.legend = F) +
  #geom_line(aes(y=TEM, linetype = "TEM"), linewidth = 1) +
  
  geom_label_repel(aes(y=TEM, label = scales::percent(TEM, accuracy=0.01)),
                   nudge_y = -0.0,
                   show.legend = FALSE,
                   color = "black",
                   max.overlaps = 16,
  ) +
  
  
  scale_y_continuous(name = "TEM",
                     labels = label_percent(accuracy = 0.01), breaks = breaks_extended(10),
                     #limits = c(0.012, 0.026),
                     # oob = scales::squish permite que el dato esté aunque el limite diga que no
                     
  ) +
  scale_x_discrete(name = "Contrato desde cada fecha.") +
  scale_color_manual(name = "Fecha", values = c(.paleta[1], .paleta[2])) +
  scale_linetype_manual(name = "Tasa", values = c(1 , 3)) +
  guides(fill = "none")  +
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[2])) +
  
  theme(legend.position.inside = c(0.95, 0.2), legend.box.background = element_rect()) +
  
  labs(title = "IMPLICITAS FUTUROS DLR",
       subtitle = paste0('Curva Implícitas de cada posición contra A3500'),
       caption = paste0(.pie, " en base a precios de mercado (A3)"))

grabaGrafo(variable = g_rofex_curva_solo_tem, path = path)

################################
#######SOLO TEM SIN PASES (LAST)
datosGraficoTEM = datosGrafico %>% 
  mutate(
    TEM = ifelse(impliedRateTEA_last == -1, NA, (1 + impliedRateTEA_last) ^ (1 / 12) - 1)
  )


coeff = mean(datosGraficoTEM$TEM, na.rm = T) / mean(datosGraficoTEM$pase_last, na.rm = T)*.5

g_rofex_curva_solo_tem_last = datosGraficoTEM %>% arrange(date)  %>% 
  filter(date==date1 | date == date2) %>% 
  ggplot(aes(x=reorder(pos, +pos), group = date, color = as.factor(date))) +
  
  theme_usado() +
  geom_point(aes(y=TEM)) +
  geom_smooth(aes(y=TEM), se = F, show.legend = F) +
  #geom_line(aes(y=TEM, linetype = "TEM"), linewidth = 1) +
  
  geom_label_repel(aes(y=TEM, label = scales::percent(TEM, accuracy=0.01)),
                   nudge_y = -0.0,
                   show.legend = FALSE,
                   color = "black",
                   max.overlaps = 16,
  ) +
  
  
  scale_y_continuous(name = "TEM",
                     labels = label_percent(accuracy = 0.01), breaks = breaks_extended(10),
                     #limits = c(0.012, 0.026),
                     # oob = scales::squish permite que el dato esté aunque el limite diga que no
                     
  ) +
  scale_x_discrete(name = "Contrato desde cada fecha.") +
  scale_color_manual(name = "Fecha", values = c(.paleta[1], .paleta[2])) +
  scale_linetype_manual(name = "Tasa", values = c(1 , 3)) +
  guides(fill = "none")  +
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[2])) +
  
  theme(legend.position.inside = c(0.95, 0.2), legend.box.background = element_rect()) +
  
  labs(title = "IMPLICITAS FUTUROS DLR (LAST)",
       subtitle = paste0('Curva Implícitas de cada posición contra last MLC'),
       caption = paste0(.pie, " en base a precios de mercado (A3)"))

grabaGrafo(variable = g_rofex_curva_solo_tem_last, path = path)

### CURVA TEM LECAP ULTIMA vs ROFEX
# CORRER PRIMERO LAS LECAP PARA TENER CALCULADO EL DF curva_lecaps
#venc = dbGetTable("vencTitulos", server = server, port = port)


# curvaLecap = f %>% 
#   filter(date == max(date)) %>% drop_na() %>% 
#   mutate(
#     TEM = (1+tir) ^ (1 / 12) - 1,
#     group = "Lecap"
#     ) %>% 
#   left_join(venc) %>% select(TEM, vto, group)

curvaLecap = curva_lecaps %>% 
  filter(date == max(date)) %>% 
  mutate(group = "Lecap") %>% 
  select(ticker, TEM = tem, vto = date_vto, group) %>% filter(!grepl("_tmr",ticker))
#######################################################################################################################
curvaRofex = rofex %>% 
  left_join(tc) %>% 
  mutate(
    impliedRateTEA_last = ((settlement / last_mlc) ^ (365 / daysToMaturity)) - 1,
    impliedRateTNA_last = ((settlement / last_mlc) - 1) * 365 / daysToMaturity,
  ) %>% 
  filter(date == max(date)) %>%
  mutate(
    TEM = ((1+impliedRateTEA_last) ^ (1 / 12) - 1) 
  ) %>%   
  select(Futuro = symbol, TEM) %>% 
  mutate(
    month = as.numeric(substr(Futuro, 4, 5)),
    year = as.numeric(substr(Futuro, 6, 9)),
    vto = as.Date(paste(year, month, "01", sep = "-")) %m+% months(1) - days(1),
    group = "Futuro"
  ) %>%
  select(-month, -year, ticker = Futuro)

g_lecap_rofex = rbind(curvaLecap, curvaRofex) %>% 
  filter(vto>="2024-09-01") %>% 
  ggplot(aes(x=vto, y=TEM, color = group, label = ticker)) +
  theme_usado() +
  geom_point() +
  geom_smooth(se = F, formula = y~poly(x,2), method = "lm", show.legend = F) + 
  ggrepel::geom_text_repel(show.legend = F) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = scales::percent, breaks = breaks_extended(10)) +
  scale_color_manual(name = NULL, values = c(.paleta[1], .paleta[2])) +  
  labs(title = "CURVA LECAP VS FUTUROS DLR",
       subtitle = paste0('Valores al: ', rofex %>% tail(n=1) %>% pull(date)),
       y = 'TEM',
       x = 'Vto',
       caption = paste0(.pie, " en base a precios BYMA y A3"))  

grabaGrafo(variable = g_lecap_rofex, path = path)
########## 
## CALCULO DE SINTETICO LECAP VS ROFEX

grupoLecap = curva_lecaps %>%
  filter(date == max(date)) %>%
  mutate(group = "Lecap") %>%
  select(ticker, TEA = tea, vto = date_vto, group) %>% 
  filter(!grepl("_tmr",ticker))
grupoImplicitas = rofex %>%
  left_join(tc) %>% 
  mutate(
    impliedRateTEA_last = ((settlement / last_mlc) ^ (365 / daysToMaturity)) - 1,
    impliedRateTNA_last = ((settlement / last_mlc) - 1) * 365 / daysToMaturity,
  ) %>% 
  #filter(date == date2) %>% 
  filter(date == max(date)) %>%
  select(Futuro = symbol, impliedRateTEA_last) %>%
  mutate(
    month = as.numeric(substr(Futuro, 4, 5)),
    year = as.numeric(substr(Futuro, 6, 9)),
    vto = as.Date(paste(year, month, "01", sep = "-")) %m+% months(1) - days(1),
    group = "Futuro"
  ) %>%
  select(-month, -year, ticker = Futuro)



df=rbind(grupoLecap, grupoImplicitas %>% rename(TEA = impliedRateTEA_last))


# Dividir el dataframe en dos grupos
df_futuro <- df %>% filter(group == "Futuro")
df_lecap <- df %>% filter(group == "Lecap")

# Crear un dataframe vacío para almacenar los emparejamientos
matched_df <- data.frame()

# Realizar el emparejamiento
for (i in 1:nrow(df_futuro)) {
  # Obtener la fecha del elemento en 'Futuro'
  date_futuro <- df_futuro$vto[i]
  
  # Buscar elementos en 'Lecap' dentro del rango de ±10 días
  matches <- df_lecap %>% filter(vto >= (date_futuro - 10) & vto <= (date_futuro + 10))
  
  # Si hay coincidencias, agregarlas al dataframe de emparejamiento
  if (nrow(matches) > 0) {
    for (j in 1:nrow(matches)) {
      matched_row <- data.frame(
        ticker_futuro = df_futuro$ticker[i],
        tea_futuro = df_futuro$TEA[i],
        date_futuro = date_futuro,
        ticker_lecap = matches$ticker[j],
        tea_lecap = matches$TEA[j],
        date_lecap = matches$vto[j]
      )
      matched_df <- rbind(matched_df, matched_row)
    }
  }
}

g_sinteticosrofex = matched_df %>% 
  mutate(sinteticoTEA = tea_lecap - tea_futuro,
         ticker = paste0(ticker_futuro, "/", ticker_lecap),
         dateVto = pmax(date_futuro, date_lecap)) %>%
  ggplot(aes(x = dateVto, y = sinteticoTEA, color = ticker, label = ticker)) +
  theme_usado() +
  geom_point(show.legend = FALSE) +
  geom_smooth(se = F, formula = y~poly(x,2), method = "lm", show.legend = F) + 
  ggrepel::geom_text_repel(show.legend = F) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = scales::percent, breaks = breaks_extended(10)) +
  #scale_color_manual(name = NULL, values = c(.paleta[1], .paleta[2])) +  
  labs(title = "SINTETICO LECAP VS FUTUROS DLR",
       subtitle = paste0('Valores al: ', rofex %>% tail(n=1) %>% pull(date)),
       y = 'TEA',
       x = 'Vto',
       caption = paste0(.pie, " en base a precios BYMA y A3"))  

grabaGrafo(name="g_sintetico_rofex", variable = g_sinteticosrofex, path = path)


#################
## Curva Sintéticos y DLK
sinteticos = matched_df %>% #filter(ticker_futuro != "DLR072025") %>% 
  mutate(sinteticoTEA = tea_lecap - tea_futuro,
         ticker = paste0(ticker_futuro, "/", ticker_lecap),
         dateVto = pmax(date_futuro, date_lecap),
         duration=as.numeric((dateVto - Sys.Date())/365),
         mduration=duration / (1 + sinteticoTEA),
         date = Sys.Date(),
         group = "Sintético"   ) %>% 
  select(date, ticker, yield = sinteticoTEA, mduration,group)

sint_bonos = dl %>% 
  filter(date == max(date)) %>% 
  mutate(group = "DLK") %>% 
  bind_rows(sinteticos) %>% 
  select(date, ticker, yield, mduration, group) 
g_sint_bonosdl = sint_bonos %>% 
  ggplot(aes(x = mduration, y = yield, color = group, group = group, label = ticker)) +
  theme_usado() +
  geom_point(data = sint_bonos[sint_bonos$group == "Sintético",], size = 1) +
  geom_point(data = sint_bonos[sint_bonos$group == "DLK",], size = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, show.legend = FALSE) +
  ggrepel::geom_text_repel(show.legend = FALSE, max.overlaps = 14) +
  scale_color_manual(name = NULL, 
                     values = c("Sintético" = .paleta[1], "DLK" = .paleta[2])) +  
  scale_y_continuous(breaks = breaks_extended(14), labels = scales::percent) +
  scale_x_continuous(breaks = breaks_extended(10)) +
  labs(title = "CURVA SINTETICO Y DOLLAR LINKED",
       subtitle = paste0('Último dato: ', max(dl$date)),
       y = 'TIR',
       x = 'Duration Modificada',
       caption = paste0(.pie, " en base a precios de mercado."))
  
grabaGrafo(variable = g_sint_bonosdl, path = path)

################################
################################
### TNA Y TEA

coeff = mean(datosGrafico$impliedRateTEA, na.rm = T) / mean(datosGrafico$pase, na.rm = T)*.5

g_rofex_curva_tna = datosGrafico %>% 
  #filter(date=="2024-08-23") %>% 
  ggplot(aes(x=reorder(pos, +pos), group = date, color = as.factor(date))) +
  
  geom_col(aes(y=pase * coeff, fill = as.factor(date), color = as.factor(date)),
           linewidth= 1, 
           position = position_dodge2(preserve = "single"), 
           show.legend = FALSE,
           width = 0.75,
           ) +
  theme_usado() +
  geom_line(aes(y=impliedRateTNA, linetype = "TNA"), linewidth = 1) +
  geom_line(aes(y=impliedRateTEA, linetype = "TEA"), linewidth = 1) +

  geom_text_repel(aes(y=impliedRateTEA, label = scales::percent(impliedRateTEA, accuracy=0.01)),
  nudge_y = .15,
  show.legend = FALSE,
  color = "black",
  max.overlaps = 16) +
  geom_text_repel(aes(y=impliedRateTNA, label = scales::percent(impliedRateTNA, accuracy=0.01)),
                  nudge_y = -.15,
                  nudge_x = 0.25,
                  show.legend = FALSE,
                  color = "black") +
  scale_y_continuous(name = "TNA y TEA",
                     labels = label_percent(accuracy = 0.01), breaks = breaks_extended(10),
                     sec.axis = sec_axis(
                       ~ . / coeff,
                       name = "PASE",
                       breaks = breaks_extended(10),
                       labels = scales::percent)
                     ) +
  scale_x_discrete(name = "Contrato desde cada fecha. (Barras es pase entre posiciones)") +
  scale_color_manual(name = "Fecha", values = c(.paleta[1], .paleta[3])) +
  scale_linetype_manual(name = "Tasa", values = c(1 , 3)) +
  guides(fill = "none")  +
  scale_fill_manual(name = "", values = c(.paleta[1], .paleta[3])) +
  
  theme(legend.position.inside = c(0.95, 0.2), legend.box.background = element_rect()) +
  
  labs(title = "IMPLICITAS ROFEX",
       subtitle = paste0('Curva Implícitas y barras de pase entre vencimientos. Primer pase contra A3500'),
       caption = paste0(.pie, " en base a precios de mercado (A3)"))

grabaGrafo(variable = g_rofex_curva_tna, path = path)


############# Cuadro de TEM
# rofex %>% 
#   filter(date == max(date)) %>% 
#   mutate(
#         TEM = format(round(((1+impliedRateTEA) ^ (1 / 12) - 1) * 100, 2),2), 
#          TNA = format(round(impliedRateTNA * 100,2),2),
#          TEM = paste0(TEM, "%"),
#          TNA = paste0(TNA, "%")) %>% 
#   select(Futuro = symbol, Ajuste = settlement, TNA, TEM) %>% 
#   kableExtra::kbl(caption = paste0('Cierre Futuros DLR al: ', rofex %>% tail(n=1) %>% pull(date))) %>% 
#   # agregar bordes a la tabla y ajustarla al centro con titulos de columnas centrados
#   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"),
#                             full_width = F, position = "center")
  

