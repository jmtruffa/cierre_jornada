fechaInicio = "2010-01-04"
ccl = dbGetTable(table = "ccl", server = server, port = port) %>% distinct(date, .keep_all = T) %>% arrange(date)
ccl = left_join(ccl, functions::getUSCPI(format = "daily", server = server, port = port) %>% 
                  select(-series_id, USCPI = value)) %>% 
  fill(USCPI)

merval = tq_get("^MERV", from = "2010-01-01", to = Sys.Date() + 1) %>% 
  select(date, merval = adjusted)
# merval = merval %>% mutate(merval = ifelse(date == "2022-07-14", 
#                                              100518.41, merval))
###############################
## dato a agregar a mano si aún no esta
# merval = merval %>%
#   add_row(date = Sys.Date() ,
#           merval = as.numeric(gsub(",","","2,045,707.37")))
# ########################

merval = merval %>% 
  left_join(ccl) %>% 
  drop_na() %>%  
  mutate(
    varD = (merval /lag(merval) -1) * 100,
    varS = (merval / lag(merval, n=5) - 1) * 100,
    mervalCCL = merval / ccl3,
    varD_ccl = (mervalCCL / lag(mervalCCL) - 1) * 100,
    varS_ccl = (mervalCCL / lag(mervalCCL, n=5) - 1) * 100,
    mervalCCLAjustado = (mervalCCL) * (LAST(USCPI) / (USCPI))
         ) %>% 
  select(-ccl3, -ccl, -USCPI)

calculate_ytd <- function(df) {
  # First, create a dataframe with last values of each year
  year_end_values <- df %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    slice_tail(n = 1) %>%
    select(year, mervalCCL) %>%
    # Shift the year up by 1 to match with next year's data
    mutate(year = year + 1) %>%
    rename(prev_year_last = mervalCCL)
  
# Then join this with the original data and calculate YTD
df %>%
  mutate(year = year(date)) %>%
  left_join(year_end_values, by = "year") %>%
  mutate(
    ytd = ((mervalCCL / prev_year_last) - 1) * 100
  ) %>%
  # Remove the temporary year column
  select(-year)
}
merval =  calculate_ytd(merval)


valores = merval %>% filter(date == "2018-01-31" |
                               
                               date == "2024-05-20" |
                               date == "2024-08-05" |
                               date == "2025-01-09" ) %>% 
  bind_rows(merval %>% tail(n=1))

g_mervalccl = merval %>% 
  ggplot(aes(x=date, y=mervalCCL, label = format(round(mervalCCL,0), big.mark = ".", decimal.mark = ",")) ) + 
  theme_usado() +
  
  geom_line(linewidth = 1, color = .paleta[1]) +
  
  geom_text_repel(data = valores , nudge_y = 10,nudge_x = 100) +
  
  scale_x_date(date_breaks="8 months", date_labels="%b\n %Y",
               expand = c(0.01,0)) +
  scale_y_continuous(breaks = breaks_extended(12)) +
  
  labs(title = "Merval a CCL",
       subtitle = paste0('Valores al: ', merval %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado") )

grabaGrafo(variable = g_mervalccl, path = path)

###### 
## Gráfico Merval CCL YTD

valores = merval %>% mutate(ytd=ytd/100) %>% filter(date == "2023-12-22" |
                                 date == "2018-08-30" |
                                 date == "2020-03-18" |
                                 date == "2019-09-03" |
                                 date == "2013-11-27" |
                                 date == "2024-12-30" |
                                 date == max(date)) 


g_merval_ytd = merval %>%
  drop_na(ytd) %>%
  ggplot(aes(x=date, y=ytd/100, label = paste0(format(round(ytd * 100,1), big.mark = ".", decimal.mark = ","), "%"))) +
  theme_usado() +
  geom_line(linewidth = 1, color = .paleta[1]) +
  geom_text_repel(data = valores[valores$ytd > 0,] , nudge_y = 0.01) +
  geom_text_repel(data = valores[valores$ytd < 0,] , nudge_y = -0.01) +
  # Add vertical lines at the end of each year
  geom_vline(
    xintercept = as.Date(paste0(2010:2024, "-12-31")),
    color = "gray70",
    linetype = "dashed",
    alpha = 0.5
  ) +
  # Optional: Add small ticks below the x-axis
  scale_x_date(
    breaks = as.Date(paste0(2010:2024, "-12-31")),
    date_minor_breaks = "1 year",
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_continuous(breaks = breaks_extended(12), labels = label_percent(accuracy = 1.0)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  labs(title = "RENDIMIENO YTD MERVAL (CCL) PARA CADA AÑO",
       subtitle = paste0('Valores al: ', merval %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado") )

grabaGrafo(variable = g_merval_ytd, path = path)

  
###########################
### Ajustado por infla US
#df$prePaso2019 = df[df$date == "2019-08-09","mervalCCLAjustado"] %>% pull(mervalCCLAjustado)

# df = df %>% 
#   drop_na() %>% 
#   select(date, mervalCCLAjustado) %>% 
#   pivot_longer(-date)

valores = merval %>% 
  filter(date == "2018-01-31" |
        date == "2019-08-09" |
        date == "2025-01-09" |
          date == max(date))

g_mervalccl_constante = merval %>% 
  ggplot(aes(x=date, y=mervalCCLAjustado, color = .paleta[1], label = format(round(mervalCCLAjustado,0), big.mark = ".", decimal.mark = ","))) + 
  theme_usado() +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks="9 months", date_labels="%b\n %Y",
               expand = c(0.05,0)) +
  scale_y_continuous(limits = c(200, 2500), breaks = breaks_extended(10)) + 
  geom_text_repel(data = valores, nudge_y = 50,nudge_x = 50, color = "black") +
  scale_color_manual(name = "", labels = c("S&P Merval USD Ajustado por CPI US"),
                     values = .paleta[1]) +
  labs(title = "MERVAL A CCL CONSTANTE",
       subtitle = paste0('Ajustado por inflación USA. Valores al: ', merval %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a precios de mercado") ) +
  theme( 
    legend.position.inside = c(0.8, .80),
  )

grabaGrafo(variable = g_mervalccl_constante, path = path)

