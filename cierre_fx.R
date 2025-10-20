# lógica para chequear si son más de las 18:05 para que actualice el valor del ccl
# desde methodsPPI::getPPIDLR y actualice la tabla de ccl
ba_now <- as.POSIXct(Sys.time(), tz = "America/Argentina/Buenos_Aires")
thresh <- as.POSIXct(strftime(ba_now, "%Y-%m-%d 18:10:00"),
                     tz = "America/Argentina/Buenos_Aires")

if (ba_now > thresh) {
  # vamos a tomar el valor del ccl y actualizar la ta bla
  query = "SELECT MAX(DATE) FROM ccl"
  from_tabla = dbExecuteQuery(query = query, server = server, port = port) %>% pull()
  df_to_save = methodsPPI::getPPIDLR(from = add.bizdays(from_tabla, 1, cal), to = Sys.Date(), settle = "t+0") %>% 
    select(date, ccl = cclAL, ccl3 = cclAL) %>% 
    # truncamos en lugar de redondear
    mutate(
      across(-date, ~ trunc(. * 100) / 100)
    )
  
  # grabamos
  functions::dbWriteDF(table = "ccl",
                       df = df_to_save, 
                       port = port,
                       server = server,
                       dbname = dbname,
                       append = T)
}

# una vez actualizado, usamos los valores y sacamos todo


dlr = methodsPPI::getPPIDLR(from = from_fx, to = to, settle = settle) %>% 
  # metemos fix a mano.
  dplyr::mutate(mepAL = ifelse(date == as.Date("2025-09-04"), 1377, mepAL))

ccl = functions::dbGetTable(table = "ccl", server = server, port = port) %>% distinct(date, .keep_all = T) %>% arrange(date)
# el query toma, si no hay cotización T+0 por que afuera es feriado, toma la T+1,2,3, hasta 5.
query = "
SELECT DISTINCT ON (\"date\")
  \"date\",
  \"cotizacion\" AS last_mlc
FROM forex
WHERE \"instrumento\" LIKE 'UST / ART%'
  AND \"rueda\" = 'CAM1'
  AND settle IN ('0','1','2','3','4','5')
ORDER BY
  \"date\",
  CASE settle
    WHEN '0' THEN 0
    WHEN '1' THEN 1
    WHEN '2' THEN 2
    WHEN '3' THEN 3
    WHEN '4' THEN 4
    WHEN '5' THEN 5
    ELSE 99
  END;
"
last = functions::dbExecuteQuery(query = query, server = server, port = port) #%>% add_row(date = Sys.Date(), last_mlc = 1240)
tc = functions::dbGetTable("A3500", server = server, port = port) %>% left_join(last) #%>% 

fx=left_join(dlr, ccl, join_by(date == date)) %>%
  mutate(
    across(-c(date, Canje), ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}"),
    across(c(mepAL, mepGD, cclGD, ccl), ~ (. / lag(., 5) - 1) * 100, .names = "varS_{.col}"),
    canjeCCL = (ccl / mepAL - 1) * 100
  ) %>%
  relocate(date, mepAL, mepGD, cclGD, Canje, ccl) %>% 
  select(date, mepAL, varD_mepAL, varS_mepAL, mepGD, varD_mepGD, varS_mepGD, cclGD, varD_cclGD, varS_cclGD, Canje, ccl, varD_ccl, varS_ccl) %>% 
  left_join(tc) %>% 
  mutate(
    brechaCCL = (ccl / A3500) - 1,
    brechaTXT = paste0(format(round(brechaCCL * 100, 0), nsmall = 0), "%"),
    
    A3500PAIS = case_when(
      date >= "2024-12-23" ~ A3500,
      date >= "2024-09-01" ~ A3500 * 1.075,  # If date is on or after 2024-09-01
      date >= "2023-12-13" ~ A3500 * 1.175,  # If date is on or after 2023-12-13 and before 2024-09-01
      TRUE ~ A3500                        # Otherwise, just A3500
    ),
    #canje = ccl / mepAL - 1
    
  )
set = left_join(dlr, ccl) %>% left_join(tc) %>% select(-ccl3, -cclAL)
diaria = set %>% mutate(across(-c(date, Canje), ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}"),)

tabla_fx = set %>% 
  functions::prev_friday(date, c(mepAL, mepGD, cclGD, ccl, A3500, last_mlc), calendar_name = "cal") %>% 
  mutate(
    across(
      ends_with("_lagged"),
      ~ ((get(sub("_lagged", "", cur_column())) / . - 1) * 100),
      .names = "varS_{sub('_lagged', '', .col)}"
    ),
    
  ) %>% 
  select(-(ends_with("_lagged"))) %>% 
  left_join(diaria) %>% 
  relocate(date, mepAL, varD_mepAL, varS_mepAL, mepGD, varD_mepGD, varS_mepGD, cclGD, varD_cclGD, varS_cclGD, ccl, varD_ccl, varS_ccl, Canje, A3500, varD_A3500, varS_A3500, last_mlc, varD_last_mlc, varS_last_mlc) %>% 
  mutate(Canje = Canje * 100) %>% 
  tail(n=15) %>% 
  mutate(
    across(
      c(mepAL, mepGD, cclGD, ccl, A3500),  
      ~ round(., digits = 0)
    )
  ) %>% 
  select(-c(mepGD, varD_mepGD, varS_mepGD, cclGD, varD_cclGD, varS_cclGD)) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(
    j = c("varD_mepAL", "varS_mepAL","varD_ccl", "varS_ccl", "Canje", "varD_A3500", "varS_A3500", "varD_last_mlc", "varS_last_mlc" ),  # Columns to format
    digits = 2,     
    suffix = "%", 
    na_str = "NA"                                                            # String for NA values
  ) %>% 
  colformat_double(
    j = c("mepAL", "ccl"),  # Columns to format
    digits = 0,                                                              # Number of decimal places
    na_str = "NA"                                                            # String for NA values
  ) %>%
  vline(j = c(1, 4, 8, 14), border = officer::fp_border()) %>%
  vline_left(border = officer::fp_border(), part = "all") %>% 
  align(part = "all", align = "center") %>%
  bold(part = "header") %>%
  width(j = "date", width = 1.5) %>% 
  set_header_labels(
    date = "DATE",
    mepAL = "MEP(AL)",
    varD_mepAL = "VARD_MEP(AL)",
    varS_mepAL = "VARS_MEP(AL)",
    ccl = "CCL",
    varD_ccl = "VARD_CCL",
    varS_ccl = "VARS_CCL",
    Canje = "CANJE",
    varD_A3500 = "VARD_A3500",
    varS_A3500 = "VARS_A3500",
    last_mlc = "LAST_MLC",
    varD_last_mlc = "VARD_LAST_MLC",
    varS_last_mlc = "VARS_LAST_MLC"
  ) 


### BRECHA
valores = fx %>% filter(
                        # date == "2023-10-20" |
                        # date == "2023-12-12" |
                        #   date == "2024-01-22" |
                        #   date == "2023-12-27" |
                          date == max(fx$date))
g_fx_brecha = fx %>% 
  drop_na() %>% 
  select(date, contains("brecha")) %>%
  filter(date >= "2025-01-01") %>%
  ggplot(aes(x=date, y=brechaCCL, label = brechaTXT)) +
  theme_usado() +
  geom_line(linewidth = 1, color = .paleta[1]) +
  geom_text_repel(data = valores, nudge_y = 0.03,nudge_x = 10, size = 4.5) +
  scale_y_continuous(labels = scales::percent,
                     breaks = breaks_extended(12)) +
  scale_x_date(date_breaks="1 month", date_labels="%b\n%Y",
                   expand = c(0.06,0)) +
  labs(title = "Brecha CCL vs - TC A3500",
       subtitle = paste0('Valores al: ', max(fx$date)),
       y = 'Brecha',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA") ) #+
  #geom_vline(xintercept = as.Date("2024-07-15"), linetype = "dashed") 

grabaGrafo(variable = g_fx_brecha, path = path)

### Financieros
valores = fx %>% filter(date >= "2025-01-01") %>% select(date, mepAL, ccl, A3500) %>% 
  tail(n=1) %>% pivot_longer(!date)
g_fx = fx %>% filter(date >= "2025-01-01") %>% 
  select(date, mepAL, ccl, A3500) %>% 
  #drop_na() %>% tail()
  select(date, mepAL, ccl, A3500) %>% 
  pivot_longer(!date) %>% 
  ggplot(aes(x=date, y=value, color=name)) +
  theme_usado() +
  geom_line(linewidth = 1) +
  geom_text_repel(data = valores, aes(label = format(round(value,0), big.mark = ".", decimal.mark = ",")), 
                  nudge_y = 10,
                  nudge_x = 10, 
                  show.legend = F) +
  scale_x_date(date_breaks = "1 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  scale_y_continuous(labels = label_currency(), breaks = breaks_extended(10)) +
  labs(title = "TIPOS DE CAMBIO",
       subtitle = paste0('Valores al: ', max(fx$date)),
       y = 'Pesos',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA") ) +
  scale_color_manual(name = "", labels = c("A3500", "CCL", "MEP AL30"), values = .paleta)

grabaGrafo(variable = g_fx, path = path)

#################
### volatilidad 
fx_vol <- fx %>% 
  # propago valor anterior cuando hay NA
  #mutate(ccl3 = zoo::na.locf(ccl3, na.rm = FALSE)) %>%
  filter(date >= "2023-01-01") %>% 
  select(date, mepAL, ccl, last_mlc) %>%                    # quitás 'canje'
  arrange(date) %>% 
  mutate(
    across(-date, ~ log(.x / lag(.x)),  .names = "{.col}_ret"),
    across(ends_with("_ret"),
           ~ slide_dbl(
             .x,
             .f        = stats::sd,   # ← usa la de stats sí o sí
             .before   = 4,
             .complete = TRUE),
           .names = "{.col}_vol5")
  )

## volatilidad en desvíos
# Parámetros
n_win <- 20          # ventana móvil en días (cambiá a gusto: 5, 20, 60, etc.)
trading_days <- 252  # días hábiles para anualizar

# 1) Volatilidad móvil (sd) sobre columnas *_ret
fx_vol2 <- fx_vol %>%
  arrange(date) %>%
  mutate(
    across(
      .cols = ends_with("_ret"),
      .fns  = ~ slide_dbl(.x, stats::sd, na.rm = TRUE, .before = n_win - 1, .complete = TRUE),
      .names = "{.col}_sd{n_win}"
    ),
    # 1b) Volatilidad anualizada (opcional)
    across(
      .cols = ends_with(paste0("_sd", n_win)),
      .fns  = ~ .x * sqrt(trading_days),
      .names = "{.col}_ann"
    ),
    # 2) Z-score de retornos: retorno / sd_móvil
    across(
      .cols = ends_with("_ret"),
      .fns  = ~ .x / slide_dbl(.x, stats::sd, na.rm = TRUE, .before = n_win - 1, .complete = TRUE),
      .names = "{.col}_z{n_win}"
    )
  )

# Armamos formato largo para ggplot
vol_cols <- names(fx_vol2) %>% grep(paste0("_ret_sd", n_win, "_ann$"), ., value = TRUE)

vol_long <- fx_vol2 %>%
  select(date, all_of(vol_cols)) %>%
  pivot_longer(-date, names_to = "serie", values_to = "vol_ann")

# Limpieza de etiquetas: mepAL_ret_sd20_ann -> mepAL
vol_long <- vol_long %>%
  mutate(serie_clean = sub("_ret_sd\\d+_ann$", "", serie))

z_cols <- names(fx_vol2) %>% grep(paste0("_ret_z", n_win, "$"), ., value = TRUE)

z_long <- fx_vol2 %>%
  select(date, all_of(z_cols)) %>%
  pivot_longer(-date, names_to = "serie", values_to = "zscore")

z_long <- z_long %>%
  mutate(#serie_clean = sub(paste0("_ret_z", n_win, "$"), "", serie)) %>% 
  serie_clean = case_when(
    str_starts(tolower(serie), "mepal")    ~ "MEP AL30",
    str_starts(tolower(serie), "ccl")    ~ "CCL",
    str_starts(tolower(serie), "last")  ~ "LAST MLC",
    TRUE                                 ~ serie
  )
  )

g_fx_volatilidad_zscore = ggplot(z_long, aes(x = date, y = zscore, color = serie_clean)) +
  theme_usado() +
  geom_line() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", linewidth = 0.3) +
  scale_x_date(date_breaks = "3 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  labs(
    title = paste0("RETORNOS ESTANDARIZADOS (Z-SCORE) CON VENTANA ", n_win, " DÍAS"),
    subtitle = paste0('Valores al: ', max(z_long$date)),
    x = NULL, y = "Z-score (retorno / sd móvil)",
    caption = paste0(.pie, " en base a BYMA y BCRA") ,
    color = "Serie"
  ) + facet_wrap(~ serie_clean, ncol = 1, scales = "free_y")
grabaGrafo(variable = g_fx_volatilidad_zscore, path = path)


# 1) Pasamos a formato long
fx_long <- fx_vol %>% 
  select(date, ends_with("_ret_vol5")) %>% 
  pivot_longer(-date,
               names_to  = "serie",
               values_to = "vol") %>% 
  mutate(
    serie = str_remove(serie, "_ret_vol5$"),
    serie = case_when(
      str_starts(tolower(serie), "mep")    ~ "MEP AL30",
      str_starts(tolower(serie), "ccl")    ~ "CCL",
      str_starts(tolower(serie), "last")  ~ "LAST MLC",
      TRUE                                 ~ serie
    )
  ) 

   


g_fx_vol_separada = ggplot(fx_long, aes(date, vol)) +
  theme_usado() +
  geom_line() +
  scale_x_date(date_breaks = "2 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  facet_wrap(~ serie, ncol = 1, scales = "free_y") +
  labs(title = "Volatilidad (σ) móvil de 5 ruedas – Comparativa",
       subtitle = paste0('Valores al: ', max(fx_long$date)),
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA") ) 

g_fx_vol_junta = ggplot(fx_long, aes(date, vol, colour = serie)) +
  theme_usado() +
  geom_line(size = 0.9) +
  scale_x_date(date_breaks = "2 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  labs(title = "Volatilidad (σ) móvil de 5 ruedas – Comparativa",
       subtitle = paste0('Valores al: ', max(fx_long$date)),
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA"), color = NULL )  

grabaGrafo(variable = g_fx_vol_separada, path = path)
grabaGrafo(variable = g_fx_vol_junta, path = path)

g_fx_vol_separada_ytd = ggplot(fx_long %>% filter(date >= "2025-01-01"), aes(date, vol)) +
  theme_usado() +
  geom_line() +
  scale_x_date(date_breaks = "2 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  facet_wrap(~ serie, ncol = 1, scales = "free_y") +
  labs(title = "Volatilidad (σ) móvil de 5 ruedas – Comparativa",
       subtitle = paste0('Valores al: ', max(fx_long$date)),
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA") ) 

g_fx_vol_junta_ytd = ggplot(fx_long %>% filter(date >= "2025-01-01"), aes(date, vol, colour = serie)) +
  theme_usado() +
  geom_line(size = 0.9) +
  scale_x_date(date_breaks = "2 months",  label = scales::label_date("%b\n%Y", locale = "es")) +
  labs(title = "Volatilidad (σ) móvil de 5 ruedas – Comparativa",
       subtitle = paste0('Valores al: ', max(fx_long$date)),
       x = '',
       caption = paste0(.pie, " en base a precios de mercado y BCRA"), color = NULL )  
grabaGrafo(variable = g_fx_vol_separada_ytd, path = path)
grabaGrafo(variable = g_fx_vol_junta_ytd, path = path)

### CANJE
g_fx_canje = fx %>% 
  drop_na() %>% 
  ggplot(aes(x=date, y=Canje, label = paste0(format(round(Canje * 100, 2), nsmall = 2),"%"))) +
  theme_usado() +
  geom_line(linewidth = 1, color = .paleta[1]) +
  geom_text_repel(aes(size = 2), data = . %>% tail(n=1), nudge_x = 50, show.legend = F) +
  scale_y_continuous(labels = label_percent(), breaks = breaks_extended(10)) +
  scale_x_date(date_breaks="2 months", labels = label_date(format = "%b\n %Y", locale = "es"), # date_labels="%b\n %Y",
               expand = c(0.0,20)) +
  labs(title = "CANJE CCL - MEP (VIA AL30)",
       subtitle = paste0("Último operado: ", fx %>% tail(n=1) %>% pull(date)),
       x = "", 
       y = "PESOS",
       caption = paste0(.pie, " en base a BCRA")) 

grabaGrafo(variable = g_fx_canje, path = path)


