curvaAR = function(from = "2020-09-15", to = Sys.Date(), comi = 0.0) {
   
  if (from < "2020-09-15") {
    stop("No puede seleccionar una fecha anterior al canje. 2020-09-15")
  }
  business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
  tickers = c(
    'AL29D',
    'AL30D',
    'AL35D',
    'AE38D',
    'AL41D',
    'GD29D',
    'GD30D',
    'GD35D',
    'GD38D',
    'GD41D',
    'GD46D'
  )
  type = rep("BONOS", length(tickers))
  methodsPPI::getPPILogin()
  curva = getPPIPriceHistoryMultiple3(token$token, ticker = tickers, 
                             type = type, 
                             from = from, 
                             to = to, 
                             settlement = 'A-48HS')
  curva = curva[[1]]
  curva$ticker = sapply(curva$ticker, str_sub, "1" ,"-2", USE.NAMES = FALSE)
  curva = cbind(curva, getYields(curva$ticker,
                                   settlementDate = as.character(bizdays::offset(curva$date, ifelse(settlement == "INMEDIATA", 0, 2), cal = business_calendar)),
                                   precios = curva$price, 
                                   initialFee = comi,
                                   endpoint = 'yield'))
}

globales = curvaAR(from = "2023-12-02")

curva_sob = globales %>% group_by(ticker) %>% arrange(date) %>% 
  do(tail(.,n=1)) %>% 
  mutate(Ley = ifelse(str_detect(ticker, "GD"), "Global", "Bonar")) %>% 
  ggplot(aes(x=mduration, y=yield, color = Ley, label = ticker)) +
  theme_usado() +
  geom_point() +
  geom_text_repel(show.legend = F) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, show.legend = F) +
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent, ) +
  scale_color_manual(name = NULL, labels = c("Bonar", "Global"), values = .paleta) +
  labs(title = paste0("CURVA BONOS SOBERANOS"),
       subtitle = paste0("Precios BYMA. Último dato: ", max(globales$date)),
       x = 'Duration',
       y = 'TIR',
       caption = paste0(.pie, ' en base a datos BYMA')
  )

grabaGrafo(variable = curva_sob, name = "g_curva_sob", path = path)

##################################
## spread 35 - 30 por legislación
spread30_35 = globales %>% 
  filter(str_detect(ticker, "30") | str_detect(ticker, "35")) %>% 
  select(ticker, date, yield) %>% 
  pivot_wider(names_from = ticker, values_from = yield) %>% 
  mutate(
    `Spread NY` = GD35 - GD30,
    `Spread Bonar` = AL35 - AL30
  ) %>% 
  select(date, `Spread NY`, `Spread Bonar`) %>%
  pivot_longer(!date) %>% 
  ggplot(aes(x=date, y=value, color = name)) +
  theme_usado() +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks="1 months", labels = label_date(format = "%b\n %Y", locale="es")) +
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent, limits = c(-0.35, 0.05)) +
  scale_color_manual(name = NULL, values = .paleta) +
  labs(title = paste0("SPREAD YTM 2035 - 2030"),
       subtitle = paste0("Último dato: ", max(globales$date)),
       x = 'Fecha',
       y = 'Diferencia TIR',
       caption = paste0(.pie, ' en base a datos de BYMA')) +
  geom_hline(yintercept = 0)
grabaGrafo(variable = spread30_35, name = "g_spread30_35", path = path)

################################
## regresiones por legislación
reg_legis = globales %>% 
  select(date, ticker, yield, mduration) %>% 
  mutate(Ley = ifelse(str_detect(ticker, "GD"), "Global", "Bonar")) %>%
  ggplot(aes(x=date, y=yield, color = Ley)) +
  theme_usado() +
  geom_smooth(linewidth = 1, se = F) +
  scale_x_date(date_breaks="3 months", labels = label_date(format = "%b\n %Y", locale="es")) +
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent) +
  scale_color_manual(name = NULL, values = .paleta) +
  labs(title = paste0("TIR CURVA SOBERANA"),
       subtitle = paste0("Regresión de YTM segmentada por Legislacion. Último dato: ", max(globales$date)),
       x = 'Fecha',
       y = 'TIR',
       caption = paste0(.pie, ' en base a datos de BYMA'))
grabaGrafo(variable = reg_legis, name = "g_reg_legis", path = path)


hist_YTM_sob = globales %>% 
  ggplot(aes(x=date, y=yield, color = ticker, label = ticker)) +
  theme_usado() +
  geom_point() +
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b\n%Y") +
  labs(title = paste0("TIR SOBERANOS"),
       subtitle = paste0("Último dato: ", max(globales$date)),
       x = '',
       y = 'TIR',
       color = NULL,
       caption = paste0(.pie, ' en base a datos de mercado (BYMA)')) +
  
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    #panel.grid.major.y = element_blank(),
    title = element_text(size=12, face='bold')
  ) +
  geom_vline(xintercept = as.Date("2024-08-01"))
grabaGrafo(variable = hist_YTM_sob, name = "g_hist_YTM_sob", path = path)  

###############################
## bopreales
tickers = c("BPA7D", "BPA7C", "BPB7D", "BPB7C",  "BPC7D", "BPC7C", "BPD7D", "BPD7C",  "BPO7D", "BPO7C", "BPY6D", "BPY6C", "BPJ5D", "BPJ5C")

from = as.Date(ifelse(viernes, prev_friday_date, adjust.previous(Sys.Date() - 1, cal = cal))) 
to  = Sys.Date()
methodsPPI::getPPILogin() 

bono = getPPIPriceHistoryMultiple3(
  token$token,
  ticker = tickers,
  type = rep("BONOS", length(tickers)),
  from = from,
  to = to,
  settlement = "A-24HS"
)

max_fecha_bopreales = dbExecuteQuery(query="select max(date) from historico_bopreales", server = server, port = port) %>% pull()
dbWriteDF(table = "historico_bopreales", df = bono[[1]] %>% filter(date > max_fecha_bopreales), server = server, port = port, append = T) 


### traigo datos para graficar
from_bopreales = "2024-02-01"
bono = dbExecuteQuery(query = paste0("select * from historico_bopreales where date >= '", from_bopreales, "' order by date"), server = server, port = port) %>% as_tibble() 

bono = bono %>% filter(!(ticker %in% c("BPJ5D", "BPJ5C") ))
bopres = cbind(bono, getYields(bono$ticker,
                             settlementDate = as.character(bizdays::offset(bono$date, 1, cal = cal)),
                             precios = bono$price,
                             endpoint = 'yield')) %>% select(-precios)

g_bopres = bopres %>% 
  #filter(str_detect(ticker, "D$")) %>% 
  filter(date == max(date)) %>% 
  #filter(date == Sys.Date()  | date == Sys.Date() - 1) %>% 
  ggplot(aes(x=mduration, y=yield, label = ticker, group = date, color = as.factor(date))) +
  theme_usado() +
  
  geom_point() +
  
  geom_smooth(method = "lm", se=F, show.legend = FALSE, linewidth = 1) +
  
  geom_text_repel(show.legend = F) +
  
  scale_color_manual(name = NULL, values = .paleta) + 
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent) +
  scale_x_continuous(breaks = breaks_extended(10)) +
  
  labs(title = "CURVA TIR BOPREALES",
       subtitle = paste0("En base a precios de mercado.", "Último dato: ", max(bopres$date)),
       y = "TIR",
       x = "Duration Modificada",
       caption = paste0(.pie, ' en base a datos de mercado (BYMA)'))

grabaGrafo(variable = g_bopres, name = "g_bopreales", path = path)

### grafico dinamico de TIRs
smooth_data <- bopres %>% 
  #filter(str_detect(ticker, "D$")) %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(smooth = predict(loess(yield ~ as.numeric(date), span = 0.75))) %>%
  group_by(ticker) %>%
  slice_tail(n = 1)  # Tomar el último valor para ubicar el label al final de la curva

## ajuste en cantidad de colores
q_colores = bopres %>% 
  #filter(str_detect(ticker, "D$")) %>%
  distinct(ticker) %>% 
  summarise(n = n()) %>% 
  pull(n)

# Gráfico
g_bopres_dinamico = bopres %>% 
  #filter(str_detect(ticker, "D$")) %>%
  ggplot(aes(x = date, y = yield, color = ticker, group = ticker)) +
  theme_usado() + 
  geom_point() +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  geom_text_repel(data = smooth_data,
                  aes(x = date, y = smooth, label = ticker, color = ticker),
                  show.legend = FALSE,
                  nudge_x = 5, hjust = 0, direction = "y") +
  scale_y_continuous(breaks = breaks_extended(10), labels = percent) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_color_manual(values = colorRampPalette(.paleta)(q_colores)) +
  labs(
    title = "EVOLUCIÓN CURVA TIR BOPREALES",
    subtitle = paste0("En base a precios de mercado.", "Último dato: ", max(bopres$date)),
    y = "TIR",
    x = "",
    caption = paste0(.pie, ' en base a datos de mercado (BYMA)'),
    color = NULL
  )
grabaGrafo(variable = g_bopres_dinamico, path = path)  


glob = globales %>% filter(date == max(globales$date)) %>% 
  mutate(tipo = ifelse(str_detect(ticker, "GD"), "Global", "Bonar")) %>% select(ticker, date, yield, mduration, tipo)
#glob$tipo = "SOBERANOS"

bopres$tipo = "BOPREAL"


df = rbind(glob, bopres %>% select(ticker, date, yield, mduration,tipo)) 
g_glob_bopre = df %>% 
  filter(date == max(date)) %>% 
  filter(ticker != "BPJ5C" & ticker != "BPJ5D") %>% 
  ggplot(aes(x=mduration, y=yield, label=ticker, color = tipo)) +
  theme_usado() +
  geom_point() +
  geom_smooth(se=F, method = "lm", formula = "y ~ log(x)", show.legend = F) + 
  geom_text_repel(show.legend = F) +
  scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent) +
  scale_color_manual(values = .paleta) +
  scale_x_continuous(breaks = seq(0, max(df$mduration) + 1, by = 0.5)) +
  labs(title = paste0("TIR SOBERANOS Y BOPREAL"),
       subtitle = paste0("Último dato: ", max(df$date)),
       x = '',
       y = 'TIR',
       color = NULL,
       caption = paste0(.pie, ' en base a datos de mercado (BYMA)'))

grabaGrafo(variable = g_glob_bopre, name = "g_glob_bopre", path = path)
