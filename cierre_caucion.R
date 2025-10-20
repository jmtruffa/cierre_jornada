cauciones = dbGetTable(table = "caucionesBYMA", server = server, port = port)


cauciones_filtrado = cauciones %>% 
  select(date, vwap, denominationCcy, daysToMaturity, tradingHighPrice, closingPrice) %>%
  group_by(date) %>% 
  filter(daysToMaturity == min(daysToMaturity), denominationCcy == "ARS") %>% 
  select(-denominationCcy, -daysToMaturity) %>% 
  pivot_longer(!date) %>% 
  filter(date >= "2024-04-01")

g_caucion = cauciones_filtrado %>% 
  ggplot(aes(x=date, y=value, type = name, color = name, label = paste0(round(value*100,2),"%"))) +
  theme_usado() +
  geom_line(data = cauciones_filtrado[cauciones_filtrado$name == "vwap",], linetype = 1, linewidth = 1) +
  geom_line(data = cauciones_filtrado[cauciones_filtrado$name == "tradingHighPrice",], linetype = "dashed") +
  geom_line(data = cauciones_filtrado[cauciones_filtrado$name == "closingPrice",], linewidth = 1) +
  
  geom_text_repel(data = cauciones_filtrado[cauciones_filtrado$name == "vwap",] %>% tail(n=1), nudge_x = 2, show.legend = F) +
  geom_text_repel(data = cauciones_filtrado[cauciones_filtrado$name == "tradingHighPrice",] %>% tail(n=1), nudge_x = 2, show.legend = F) +
  geom_text_repel(data = cauciones_filtrado[cauciones_filtrado$name == "closingPrice",] %>% tail(n=1), nudge_x = 2, show.legend = F) +
  
  scale_color_manual(name = NULL, values = c(.paleta[2], .paleta[1], .paleta[3]), labels = c("Cierre", "Máximo", "VWAP")) +
  scale_y_continuous(labels = label_percent(), breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "1 month", labels = label_date(format = "%b-%y", locale = "es")) +
  
  labs(title = "CAUCION OVERNIGHT",
       subtitle = paste0("Data al: ", tail(cauciones_filtrado, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a BYMA"),
       x = "",
       y = "TNA") 
  

grabaGrafo(variable = g_caucion, path = path)
##################
### Caucion en USD
cauciones_filtrado_usd = cauciones %>% 
  select(date, vwap, denominationCcy, daysToMaturity, tradingHighPrice, closingPrice) %>%
  group_by(date) %>% 
  filter(daysToMaturity == min(daysToMaturity), denominationCcy == "USD") %>% 
  select(-denominationCcy, -daysToMaturity) %>% 
  pivot_longer(!date) %>% 
  filter(date >= "2024-04-01")

g_caucion_dolar = cauciones_filtrado_usd %>% 
  ggplot(aes(x=date, y=value, type = name, color = name, label = paste0(round(value*100,2),"%"))) +
  theme_usado() +
  geom_line(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "vwap",], linetype = 1, linewidth = 1) +
  geom_line(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "tradingHighPrice",], linetype = "dashed") +
  geom_line(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "closingPrice",], linewidth = 1) +
  
  geom_text_repel(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "vwap",] %>% tail(n=1), nudge_x = 4, nudge_y = +.005, show.legend = F) +
  geom_text_repel(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "tradingHighPrice",] %>% tail(n=1), nudge_x = 3, show.legend = F) +
  geom_text_repel(data = cauciones_filtrado_usd[cauciones_filtrado_usd$name == "closingPrice",] %>% tail(n=1), nudge_x = 2, show.legend = F) +
  
  scale_color_manual(name = NULL, values = c(.paleta[2], .paleta[1], .paleta[3]), labels = c("Cierre", "Máximo", "VWAP")) +
  scale_y_continuous(labels = label_percent(), breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "1 month", labels = label_date(format = "%b-%y", locale = "es")) +
  
  labs(title = "CAUCION OVERNIGHT (USD)",
       subtitle = paste0("Data al: ", tail(cauciones_filtrado_usd, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a BYMA"),
       x = "",
       y = "TNA") 

grabaGrafo(variable = g_caucion_dolar, path = path)


settle = "t+0"
from = "2016-01-01"
to = Sys.Date()
dlr = methodsPPI::getPPIDLR(from = from, to = to, settle = settle)
ccl = dbGetTable(table = "ccl", server = server, port = port) 
tc = dbGetTable("A3500", server = server, port = port)
fx=left_join(dlr, ccl, join_by(date == date)) %>%
  mutate(
    across(-c(date, Canje), ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}"),
    across(c(mepAL, mepGD, cclGD, ccl3, ccl), ~ (. / lag(., 5) - 1) * 100, .names = "varS_{.col}"),
    canjeCCL3 = (ccl3 / mepAL - 1) * 100
  ) %>%
  select(date, mepAL, ccl3) %>% 
  left_join(tc) %>% 
  mutate(
    canje = ccl3 / mepAL - 1
  )

serie = cauciones_filtrado_usd %>% 
  pivot_wider() %>% 
  left_join(fx  %>% select(date, Canje), by = c("date" = "date")) %>% 
  select(date, vwap,  Canje) %>% 
  filter(date >= "2024-04-01") %>% 
  pivot_longer(!date) %>% 
  drop_na()

g_caucion_usd_canje = serie %>% 
  ggplot(aes(x=date, y=value, type = name, color = name, label = paste0(round(value*100,2),"%"))) +
  theme_usado() +
  geom_line(data = serie[serie$name == "vwap",], linetype = 1, linewidth = 1) +
  geom_line(data = serie[serie$name == "Canje",], linetype = "dashed") +
  
  geom_text_repel(data = serie[serie$name == "vwap",] %>% tail(n=1), nudge_x = 4, nudge_y = +.005, show.legend = F) +
  geom_text_repel(data = serie[serie$name == "Canje",] %>% tail(n=1), nudge_x = 3, show.legend = F) +
  
  
  scale_color_manual(name = NULL, values = c(.paleta[2], .paleta[1], .paleta[3]), labels = c("Canje", "VWAP", "VWAP")) +
  scale_y_continuous(labels = label_percent(), breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "1 month", labels = label_date(format = "%b-%y", locale = "es")) +
  
  labs(title = "CAUCION OVERNIGHT (USD) Y CANJE",
       subtitle = paste0("Data al: ", tail(cauciones_filtrado_usd, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a BYMA"),
       x = "",
       y = "TNA") 
grabaGrafo(variable = g_caucion_usd_canje)
