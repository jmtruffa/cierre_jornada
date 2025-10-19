vol_mulc_todos_plazos = "
SELECT 
  \"forex\".date, 
  SUM(CASE WHEN \"currencyOut\" = 'UST ' THEN \"Monto Negociado.*\" ELSE 0 END)::NUMERIC AS volumen,
  SUM(CASE WHEN \"currencyOut\" = 'USMEP ' THEN \"Monto Negociado.*\" ELSE 0 END)::NUMERIC AS volumen_usmep
FROM 
  \"forex\"
WHERE 
  \"currencyOut\" = 'UST ' OR \"currencyOut\" = 'USMEP '
GROUP BY 
  \"forex\".date
ORDER BY 
  \"forex\".date;
"
vol_mulc = as_tibble(functions::dbExecuteQuery(vol_mulc_todos_plazos, server = server, port = port)) %>% arrange(date)
compras_bcra = dbGetTable(table = "comprasMULCBCRA", server = server, port = port) %>% arrange(date) %>% mutate(comprasBCRA = comprasBCRA* 1e6)
mulc = vol_mulc %>% left_join(compras_bcra) %>% mutate(vol_total = volumen + volumen_usmep, pctBCRA = (comprasBCRA / (volumen+volumen_usmep ))) 


######## 
# Graficos

mlc_volumenMLC_long <- mulc %>%
  select(date, volumen, volumen_usmep) %>%
  pivot_longer(cols = c(volumen, volumen_usmep), 
               names_to = "type", 
               values_to = "value")
fechas = mlc_volumenMLC_long %>% distinct(date) %>% pull(date)
g_mlc_volumenMLC = ggplot(mlc_volumenMLC_long, aes(x = date, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_point(
    data = mulc %>% select(date, pctBCRA), 
    aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value), color = "Compras BCRA"),
    inherit.aes = FALSE, 
  ) +
  geom_point(
    data = mulc %>% select(date, pctBCRA) %>% tail(n=1),
    aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value), color = "Compras BCRA"),
    size = 4.5,    
    inherit.aes = FALSE, 
  ) +
  geom_smooth(
    data = mulc %>% select(date, pctBCRA), 
    aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value)),  # Specify y aesthetic here
    color = .paleta[2],  # Use a specific color instead of fill
    inherit.aes = FALSE,  # Prevent inheriting fill from the main ggplot call
    se = F
  ) +
  theme_usado() +
  scale_y_continuous(
    name = "Millones USD",
    labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
    breaks_extended(10),
    sec.axis = sec_axis(
      ~ . / max(mlc_volumenMLC_long$value),
      name = "pctBCRA",
      labels = scales::percent,
      breaks = breaks_extended(10)
    )
  ) +
  scale_x_cont_dates(
    name = "",
    business.dates = fechas,
    labels = label_date(format = "%b-%y", locale = "es"),
    max.major.breaks = 20
  ) +
  scale_fill_manual(
    name = "",
    values = .paleta[1:2],  # Use two colors from your palette
    labels = c("Volumen MLC", "Volumen USMEP")  # Customize labels
  ) +
  scale_color_manual(
    name = "",
    values = .paleta[2],
    labels = "Compras BCRA"
  ) +
  labs(
    title = "VOLUMEN MULC (DIVISA Y MEP TODOS LOS PLAZOS) Y COMPRAS BCRA",
    subtitle = paste0("Compras destacada última. Datos al ", mulc %>% tail(n = 1) %>% pull(date)),
    y = "",
    x = "",
    caption = paste0(.pie, " en base a BCRA")
  ) 


grabaGrafo(variable = g_mlc_volumenMLC, path = path)

##########################################################
### comprasBCRA en el MLC
graf = mulc %>% 
  mutate(resul = ifelse(comprasBCRA < 0, "Ventas", "Compras")) 
graf$Fecha = graf$date
g_mlc_comprasBCRA = graf %>% 
  drop_na() %>% 
  ggplot(aes(x=Fecha, y=comprasBCRA)) +
  #mpt::theme_mpt() +
  theme_outlier() +
  geom_bar(stat = "identity", aes(fill = as.factor(resul))) +
  
  geom_smooth( se = F, show.legend = F) + 
  #geom_vline(xintercept = as.Date("2023-12-10")) +
  
  
  scale_fill_manual(name = "", label = c("Compras", "Ventas"), values = .paleta) +
  
  scale_x_cont_dates(name = "", business.dates = graf$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
  #scale_x_date(date_breaks = "1 week", date_labels = "%d-%m") +
  scale_y_continuous(labels = unit_format(unit = "M", style_negative = "parens", scale = 1e-6),
                     name = "Millones de dólares",
                     breaks = breaks_extended(8)) +
  
  labs(title = "Operación del BCRA en el MLC",
       subtitle = paste0('Datos al ', mulc %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a BCRA")) +
  theme(legend.position = "bottom")

grabaGrafo(variable = g_mlc_comprasBCRA, path = path)



######################################################################
#### compras y volumen mensual
# la serieCal trae los datos de los días hábiles, basado en un calendario, para 
# no omitir ningun día y asegurar que si alguna de las tablas no tiene valroes para un día
# que en realidad fue hábil, se vería un NA en la tabla.
g_mlc_tablaMLC = serieCal(from = min(volTodos$date), to = max(volTodos$date), server = server, port = port) %>% 
  left_join(volTodos) %>%  
  left_join(compras) %>% 
  filter(date>="2023-12-01") %>% 
  group_by(`Año` = year(date), Mes = month(date)) %>% 
  summarise(`Compras BCRA` = sum(comprasBCRA) / 1e6,
            `Volumen Divisa` = sum(volumen) / 1e6,
            `Volumen MEP` = sum(volumen_usmep) / 1e6,
            `Prom Dia Compras` = mean(comprasBCRA) / 1e6 ,
            `Prom Dia Volumen` = mean(volumen) / 1e6,
            `Prom Día MEP` = mean(volumen_usmep) / 1e6
  ) %>% 
  flextable::flextable() %>% 
  flextable::footnote(i = 1, 
                      j = 1, 
                      value = as_paragraph(paste0('Millones USD al: ', volTodos %>% tail(n=1) %>% pull(date)))) %>% 
  #theme_box() %>% 
  colformat_double(digits = 0, big.mark = ".", decimal.mark = ",") %>% 
  colformat_char(j=3) %>% 
  align(align="center", part = "header") %>% 
  align(align="center", part = "body") %>% 
  fontsize(size = 10, part = "header") %>% 
  font(fontname = "MS Sans Serif", part = "header") %>% bg(bg = "white", part = "all")
 
grabaTabla2(variable = g_mlc_tablaMLC, path = path)


###### ultima semana
g_mlc_compras_semana_mulc = serieCal(from = min(volTodos$date), to = max(volTodos$date), server = server, port = port) %>% 
  left_join(volTodos) %>%  
  left_join(compras) %>% 
  filter(date >= lubridate::floor_date(Sys.Date(), unit="week") +1) %>% 
  summarise(`Compras BCRA` = sum(comprasBCRA) / 1e6,
            `Volumen Divisa` = sum(volumen) / 1e6,
            `Volumen MEP` = sum(volumen_usmep) / 1e6,
            `Prom Dia Compras` = mean(comprasBCRA) / 1e6,
            `Prom Dia Volumen` = mean(volumen) / 1e6,
            `Prom Día MEP` = mean(volumen_usmep) / 1e6
  ) %>% 
  flextable::flextable() %>% 
  flextable::footnote(i = 1, 
                      j = 1, 
                      value = as_paragraph(paste0('Millones USD al: ', volTodos %>% tail(n=1) %>% pull(date)))) %>% 
  colformat_double(digits = 0, big.mark = ".", decimal.mark = ",") %>% 
  colformat_char(j=3) %>% 
  align(align="center", part = "header") %>% 
  align(align="center", part = "body") %>% 
  fontsize(size = 10, part = "header") %>% 
  font(fontname = "MS Sans Serif", part = "header") %>% 
  bg(bg = "white", part = "all")
  
grabaTabla2(variable = g_mlc_compras_semana_mulc, path = path)


######################################################################
# DEMANDA PUNTUAL

ultimo_dia_fila = mulc %>% 
  mutate(demanda = volumen - comprasBCRA) %>% 
  filter(date >= "2024-08-01") %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% tail(n=1) %>% pull(fila)

g_mlc_demanda_mlc_puntual = mulc %>% 
  mutate(demanda = volumen - comprasBCRA) %>% 
  filter(date >= "2024-08-01") %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% 
  mutate(mes = factor(mes, levels = c(8:12, 1:3))) %>% 
  
  ggplot(aes(x = fila, y = demanda, group = mes)) +
  theme_usado() +
  geom_col(aes(fill = as.factor(mes)), width = 0.5, position = position_dodge(width = 0.6)) +
  scale_y_continuous(name = "Millones USD",
                     labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
                     breaks_extended(10)) +
  scale_x_continuous(breaks = seq(1, 30, 1)) +
  scale_fill_manual(name = "Mes", values = colorRampPalette(.paleta)(12)) +
  labs(title = "DEMANDA PRIVADA DIARIA MULC (DIVISA TODOS LOS PLAZOS)",
       subtitle = paste0('VOLUMEN - COMPRAS BCRA. Datos al ', mulc %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a BCRA")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = ifelse(seq(1, 30) == ultimo_dia_fila, 14, 10), color = ifelse(seq(1, 30) == ultimo_dia_fila, "red", "black"))) 

grabaGrafo(variable = g_mlc_demanda_mlc_puntual, path = path)

######################################################################
# DEMANDA ACUMULADA COMPARADA POR RUEDAS
# NUEVO FORMATO
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
meses_es <- c("Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr")

mulc_prepared <- mulc %>% 
  mutate(demanda = volumen - comprasBCRA) %>% 
  filter(date >= "2024-07-01") %>% 
  mutate(mes_num = month(date)) %>% 
  group_by(mes_num) %>% 
  arrange(date) %>% 
  mutate(fila = row_number(),
         demandaAc = cumsum(demanda)) %>% 
  ungroup() %>% 
  mutate(mes = factor(mes_num, levels = c(7:12, 1:4), labels = meses_es))  # Asigna etiquetas de mes en español

labels_df <- mulc_prepared %>% 
  group_by(mes_num) %>% 
  filter(fila == max(fila)) %>% 
  ungroup()

g_mlc_demanda_mlc_acum = mulc_prepared %>% 
  ggplot(aes(x = fila, y = demandaAc, group = mes, color = mes)) +
  
  theme_usado() +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 1) +
  
  geom_text_repel(
    data = labels_df,
    aes(label = mes),
    nudge_x = 0.5,      # Ajusta según sea necesario
    nudge_y = 0.5,      # Ajusta según sea necesario
    size = 4,
    color = "black",
    show.legend = FALSE
  ) +
  
  scale_color_manual(name = "Mes", values = colorRampPalette(.paleta)(12)) +  
  
  scale_y_continuous(
    name = "Millones USD",
    labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
    breaks = breaks_extended(10)
  ) +
  
  scale_x_continuous(
    breaks = seq(1, 30, 1)
  ) +
  
  labs(
    title = "DEMANDA ACUMULADA MULC (DIVISA TODOS LOS PLAZOS)",
    subtitle = paste0('VOLUMEN - COMPRAS BCRA. Datos al ', max(mulc$date)),
    y = '',
    x = 'Ruedas de cada mes',
    caption = paste0(.pie, " en base a BCRA")
  ) +
  
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      size = 10, 
      color = "black"
    )
  ) +
  
  guides(
    color = guide_legend(nrow = 1, title = "Mes")
  )

Sys.setlocale("LC_TIME", "en_US.UTF-8")
grabaGrafo(variable = g_mlc_demanda_mlc_acum, path = path)

######################################################################
# DIVISA Y MEP EN MULC
valores = mulc %>% 
  select(date, volumen, volumen_usmep) %>% tail(n=1) %>% pivot_longer(!date)
g_mlc_volumen_divisa_mep = mulc %>% 
  select(date, volumen, volumen_usmep) %>% 
  pivot_longer(!date) %>%
  ggplot(aes(x=date, y=value, color = name)) + 
  theme_usado() +
  geom_point(size = 0.75, show.legend = F) +
  geom_point(data = valores, size = 4.5, show.legend = F) +
  geom_smooth(se = F)  +
  scale_y_continuous(name = "Volumen en Millones de USD", labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(name = "Plaza", values = .paleta, labels = c("Divisa", "MEP")) +
  labs(title = "DIVISA Y MEP EN MULC",
       subtitle = paste0('Últimos valores destacados. Datos al ', mulc %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a BCRA")) +
  theme(legend.position = "bottom")
grabaGrafo(variable = g_mlc_volumen_divisa_mep, path = path)

####
# pctMEP
g_mlc_pct_mep_mlc = mulc %>% 
  mutate(pctMEP = volumen_usmep / (volumen + volumen_usmep)) %>% 
  drop_na() %>% 
  ggplot(aes(date, pctMEP)) +
  theme_usado() +
  geom_point(size=0.75) +
  geom_smooth(se = F) +
  scale_x_cont_dates(name = "", business.dates = mulc$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
  scale_y_continuous(name = "Porcentaje MEP",
                     labels = label_percent()
  ) +
  #scale_x_continuous(breaks = seq(1,30, 1)) +
  scale_fill_manual(name = "Mes", values = .paleta) +
  labs(title = "MEP COMO PORCENTAJE DEL VOLUMEN TOTAL",
       subtitle = paste0('Datos al ', df %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a BCRA")) +
  theme(legend.position = "bottom")


grabaGrafo(variable = g_mlc_pct_mep_mlc, path = path)

##################################
# DISTRIBUCION RUEDA MLC
g_mlc_pct_areas_mlc = mulc %>% 
  mutate(pctMEP =  (volumen_usmep / (volumen + volumen_usmep)),
         pctDivisa =  (volumen / (volumen + volumen_usmep))) %>% 
  
  select(date, pctMEP, pctDivisa) %>%
  pivot_longer(!date) %>% 
  drop_na() %>% 
  
  ggplot(aes(x=date, y=value, fill = name)) +
  theme_usado() +
  geom_area(stat="identity") +
  
  scale_x_cont_dates(name = "", business.dates = mulc$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
  
  scale_y_continuous(name = "Total MLC",
                     labels = label_percent(),
                     breaks_extended(10)) +
  scale_fill_manual(name = NULL, values = .paleta, labels = c("% Divisa", "% MEP")) +
  labs(title = "DISTRIBUCION DE LA RUEDA MLC",
       subtitle = paste0('Datos al ', mulc %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a BCRA")) +
  theme(legend.position = "bottom")


grabaGrafo(variable = g_mlc_pct_areas_mlc, path = path)