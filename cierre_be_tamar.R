# comparativa duales contra bonos tasa fija
#Traer curva_lecaps con los tamar incluidos
temp_lecap = dbExecuteQuery(query = paste0("select date, ticker, price from historico_lecaps where date >= '", from_dinamica, "'"), server = server, port = port)
curva_lecaps = finance::tasasLecap(temp_lecap, server = server, port = port)
tamar = bcra::getDatosVariable(idVariable = 136, desde = from_dinamica, hasta = Sys.Date()) %>% mutate(valor = valor / 100)
#curva_lecaps = rbind(curva_lecaps, lecaps_nueva %>% select(-group))
duales = curva_lecaps %>% 
  filter(str_detect(ticker, "TT")) %>% 
  #agrego un sufijo para que no se confunda con las letras
  mutate(
    ticker = paste0(ticker, "_tmr")
  ) %>% 
  mutate(
    date_start = add.bizdays(date_liq, - 10, cal),
    date_end = add.bizdays(date, - 9, cal)
  ) %>% 
  rowwise() %>%
  mutate(
    tamar_prom_tna = mean(
      tamar %>%
        filter(date >= date_start & date <= date_end) %>%
        pull(valor),
      na.rm = TRUE
    ),
    tamar_tem = ( ( 1 + tamar_prom_tna * 32 / 365 ) ^ (365/32) ) ^(1/12)-1,
    vpv = 100 * (1 + tamar_tem) ^ ( (days360(date_liq, date_vto) / 360) * 12 ),
    tem =  (vpv / price) ^ (1 / ((as.numeric(date_vto - date) / 360) * 12)) - 1
  ) %>%
  ungroup()



curva_lecaps = curva_lecaps %>% 
  rbind(duales %>% select(-date_start, -date_end, -tamar_prom_tna, -tamar_tem, -vpv))

## seelcciono
g_tamar_comparado1 = curva_lecaps %>% 
  filter(ticker %in% c("T13F6", "TTM26", "TTM26_tmr")) %>% 
  
  
  ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) + 
  
  theme_usado() +
  geom_point() +
  geom_line(linewidth = 1) +
  #geom_smooth(se = F) +
  
  scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
               expand = c(0.07,0.0)) +
  
  scale_y_continuous(breaks = breaks_extended(10), 
                     labels = scales::percent) +
  
  labs(title = "CURVA T13F6 VS TTM26 - DINAMICA",
       subtitle = paste0('Último dato: ', tail(curva_lecaps, n = 1) %>% pull(date)),
       y = 'TEM',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado."))+
  theme(legend.title =  element_blank()) 
grabaGrafo(variable = g_tamar_comparado1, path = path)

g_tamar_comparado2 = curva_lecaps %>% 
  filter(ticker %in% c("T30J6", "TTJ26", "TTJ26_tmr")) %>% 
  ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) + 
  theme_usado() +
  geom_point() +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
               expand = c(0.07,0.0)) +
  scale_y_continuous(breaks = breaks_extended(10), 
                     labels = scales::percent) +
  labs(title = "CURVA T30J6 VS TTJ26 - DINAMICA",
       subtitle = paste0('Último dato: ', tail(curva_lecaps, n = 1) %>% pull(date)),
       y = 'TEM',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado."))+
  theme(legend.title =  element_blank())
grabaGrafo(variable = g_tamar_comparado2, path = path)

g_tamar_comparado3 = curva_lecaps %>% 
  filter(ticker %in% c("TTS26", "TTS26_tmr")) %>% 
  ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) + 
  theme_usado() +
  geom_point() +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
               expand = c(0.07,0.0)) +
  scale_y_continuous(breaks = breaks_extended(10), 
                     labels = scales::percent) + 
  labs(title = "CURVA TTS26 - DINAMICA",
       subtitle = paste0('Último dato: ', tail(curva_lecaps, n = 1) %>% pull(date)),
       y = 'TEM',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado."))+
  theme(legend.title =  element_blank())
grabaGrafo(variable = g_tamar_comparado3, path = path)

g_tamar_comparado4 = curva_lecaps %>% 
  filter(ticker %in% c("TTD26", "TTD26_tmr")) %>% 
  ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) + 
  theme_usado() +
  geom_point() +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
               expand = c(0.07,0.0)) +
  scale_y_continuous(breaks = breaks_extended(10), 
                     labels = scales::percent) +
  labs(title = "CURVA TTD26 - DINAMICA",
       subtitle = paste0('Último dato: ', tail(curva_lecaps, n = 1) %>% pull(date)),
       y = 'TEM',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado."))+
  theme(legend.title =  element_blank())
grabaGrafo(variable = g_tamar_comparado4, path = path)

##### TAMAR PROMEDIO indiferencia RESTO DEL PLAZO
# traer todas las lecaps
# temp_lecap = dbExecuteQuery(query = paste0("select * from historico_lecaps where date >= '", date1, "'"), server = server, port = port)
# curva_lecaps = finance::tasasLecap(temp_lecap, server = server, port = port)
# tamar = bcra::getDatosVariable(idVariable = 136, desde = "2024-01-01", hasta = Sys.Date()) %>% mutate(valor = valor / 100)
duales = curva_lecaps %>% 
  filter(str_detect(ticker, "TT")) %>% 
  #agrego un sufijo para que no se confunda con las letras
  mutate(
    ticker = paste0(ticker, "_tmr")
  ) %>% 
  mutate(
    date_start = add.bizdays(date_liq, - 10, cal),
    date_end = add.bizdays(date, -9, cal)
  ) %>% 
  rowwise() %>%
  mutate(
    tamar_prom_tna = mean(
      tamar %>%
        filter(date >= date_start & date <= date_end) %>%
        pull(valor),
      na.rm = TRUE
    ),
    tamar_prom_tna_completa = mean(
      tamar %>%
        filter(date >= date_start & date <= add.bizdays(date_end, 9,cal)) %>%
        pull(valor),
      na.rm = TRUE
    ),
    tamar_tem = ( ( 1 + tamar_prom_tna * 32 / 365 ) ^ (365/32) ) ^(1/12)-1,
    tamar_tem_completa = ( ( 1 + tamar_prom_tna_completa * 32 / 365 ) ^ (365/32) ) ^(1/12)-1,
    vpv = 100 * (1 + tamar_tem) ^ ( (days360(date_liq, date_vto) / 360) * 12 ),
    tem =  (vpv / price) ^ (30 / (as.numeric(date_vto - settle - 1))) - 1
  ) %>%
  ungroup() %>% 
  mutate(
    plazo_inicial = as.numeric(date - add.bizdays(date_liq, - 10, cal)),
    plazo_restante = as.numeric(add.bizdays(date_vto, - 10, cal) - date) + 1
  ) %>% 
  mutate(ticker_join = str_remove(ticker, "_tmr$")) %>% 
  left_join(curva_lecaps %>% select(date, ticker) %>% filter(str_detect(ticker, "TT")), 
            join_by(date, ticker_join == ticker)) %>% 
  mutate(
    tasa_emision_TEM365 = ( vf / 100) ^ ( 30 / (as.numeric(date_vto - date_liq)) ) - 1,
    tem_be_vto = ((plazo_inicial + plazo_restante) * tasa_emision_TEM365 - plazo_inicial * tamar_tem_completa) / plazo_restante,
    tna_be_vto = (((1 + tem_be_vto) ^ 12) ^ (32/365) - 1) * (365 / 32),
    vpv_25 = price * (1 + .025) ^ (as.numeric(date_vto - settle) / 30),
    vpv_3 = price * (1 + .03) ^ (as.numeric(date_vto - settle) / 30),
    vpv_35 = price * (1 + .035) ^ (as.numeric(date_vto - settle) / 30),
    vpv_4 = price * (1 + .04) ^ (as.numeric(date_vto - settle) / 30),
    vpv_45 = price * (1 + .045) ^ (as.numeric(date_vto - settle) / 30),
    y_bar_25 = ((plazo_inicial + plazo_restante) / plazo_restante) * (365/32) * ((vpv_25/100)^(32/(plazo_inicial + plazo_restante)) - 1) - (plazo_inicial/plazo_restante) * tamar_prom_tna_completa,
    y_bar_3 = ((plazo_inicial + plazo_restante) / plazo_restante) * (365/32) * ((vpv_3/100)^(32/(plazo_inicial + plazo_restante)) - 1) - (plazo_inicial/plazo_restante) * tamar_prom_tna_completa,
    y_bar_35 = ((plazo_inicial + plazo_restante) / plazo_restante) * (365/32) * ((vpv_35/100)^(32/(plazo_inicial + plazo_restante)) - 1) - (plazo_inicial/plazo_restante) * tamar_prom_tna_completa,
    y_bar_4 = ((plazo_inicial + plazo_restante) / plazo_restante) * (365/32) * ((vpv_4/100)^(32/(plazo_inicial + plazo_restante)) - 1) - (plazo_inicial/plazo_restante) * tamar_prom_tna_completa,
    y_bar_45 = ((plazo_inicial + plazo_restante) / plazo_restante) * (365/32) * ((vpv_45/100)^(32/(plazo_inicial + plazo_restante)) - 1) - (plazo_inicial/plazo_restante) * tamar_prom_tna_completa
  )

t_duales_breakeven = duales %>% 
  select(date, ticker, tem, tasa, tem_be_vto, tna_be_vto) %>% 
  filter(date == max(date)) %>% 
  mutate(
    across(
      .cols = -c(date, ticker),
      .fns = ~ paste0(round(.x * 100, 2), " %")
    )
  ) %>% 
  rename(
    DATE = date,
    TICKER = ticker,
    TASA_EMISION = tasa,
    TEM_MKT = tem,
    TEM_BE = tem_be_vto,
    TNA_BE = tna_be_vto
  ) %>% 
  flextable() %>% 
  width(j = 1, width = 1.) %>%
  fix_border_issues() %>%
  set_caption("TAMAR BREAKEVEN DUALES") %>% 
  align(align = "center", part = "all")
grabaTabla2(variable = t_duales_breakeven, path = path)
  
df_duales = duales %>% 
  filter(date==max(date)) %>% 
  select(ticker, price, tem, tamar_prom_tna_completa, starts_with("y_bar") ) %>% 
  as.data.frame() %>% 
  mutate(
    ticker = str_remove(ticker, "_tmr$") 
  ) %>% 
  mutate(
    across(
      .cols = -c(ticker, price),
      .fns = ~ paste0(round(.x * 100, 2), " %")
    )
  ) %>% 
  rename(
    TICKER = ticker,
    PRICE = price,
    `TEM A MKT` = tem,
    `TAMAR PROM HOY` = tamar_prom_tna_completa,
    `TAMAR BE 2.5%` = y_bar_25,
    `TAMAR BE 3%` = y_bar_3,
    `TAMAR BE 3.5%` = y_bar_35,
    `TAMAR BE 4%` = y_bar_4,
    `TAMAR BE 4.5%` = y_bar_45
  )
num_cols <- names(df_duales)[sapply(df_duales, is.numeric)]

t_duales_be_distintas = df_duales %>% 
  flextable() %>% 
  width(width = 1.) %>%
  colformat_num(j = num_cols, digits = 2) %>% 
  fix_border_issues() %>%
  set_caption("TAMAR BREAKEVEN DUALES A DISTINTAS TEM") %>% 
  align(align = "center", part = "all") %>% 
  add_footer_lines(as_paragraph("Fuente: Elaboración propia en base a BYMA y BCRA.")) %>% 
  bg(bg = "white", part = "all")
grabaTabla2(variable = t_duales_be_distintas, path = path)
