library(tidyverse)
library(methodsPPI)
library(xts)
library(tidyquant)
library(functions)
library(outlier)
library(scales)


library(dplyr)
library(functions)

## 1) Traer los tramos desde la tabla Postgres -----------------------------

globales_db <- functions::dbExecuteQuery(
  "
  SELECT
      ticker,
      outstanding AS outStand,
      desde AS from,
      hasta AS to
  FROM outstanding_soberanos
  WHERE emisor = 'argentina'
    AND ticker LIKE 'GD%';      -- solo bonos globales
  ",
  server = server,
  port   = port
) 


## 2) Adaptar tickers al formato PPI (GD30 -> GD30D) ----------------------

globales_db <- globales_db %>%
  mutate(ticker = paste0(ticker, "D"))

## 3) Traemos los precios
getPPILogin()
from = "2020-09-01"
to = Sys.Date()
settlement = "A-24HS"
preciosDeuda = getPPIPriceHistoryMultiple3(
  token$token, 
  ticker = as.vector(globales_db %>% distinct(ticker) %>% pull(ticker)), 
  type = rep("BONOS",6), 
  from = from, 
  to = to, 
  settlement = settlement)
#preciosDeuda
preciosDeuda = preciosDeuda[[1]] %>% select(ticker, date, price)


## 4) Vamos ahora a traer las paridades de cada uno
comi = 0
cal <- create.calendar('cal', functions::getFeriados(server = server, port = port), weekdays = c('saturday','sunday'))
preciosDeuda <- preciosDeuda %>% mutate(ticker = sub("D$", "", ticker))
paridades = getYields(preciosDeuda$ticker,
                        settlementDate = as.character(bizdays::offset(preciosDeuda$date, ifelse(settlement == "INMEDIATA", 0, 1), cal = cal)),
                        precios = preciosDeuda$price,
                        initialFee = comi,
                        endpoint = 'yield')
preciosDeuda = cbind(preciosDeuda, paridades)



## 5) Calcular ponderadores EXACTAMENTE como en tu tibble original --------
##    Es decir: ponderación = outStand / sum(outStand) *dentro de cada tramo*
##    Para eso agrupamos por (from, to)

outstGlobales <- globales_db %>%
  group_by(from, to) %>%
  mutate(pond = outstand / sum(outstand)) %>%
  ungroup() %>%
  arrange(from, ticker)


preciosPond = preciosDeuda %>%
  mutate(ticker = paste0(ticker, "D")) %>% 
  inner_join(outstGlobales, 
             by = "ticker", 
             relationship = "many-to-many") %>%
  filter(date >= from & date <= to) %>%
  group_by(date) %>%
  summarize(precio = sum(price * pond),
            paridad_ponderada = sum(parity * pond)
            ) %>%
  arrange(date)

calculate_ytd <- function(df) {
  # First, create a dataframe with last values of each year
  year_end_values <- df %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    slice_tail(n = 1) %>%
    select(year, paridad_ponderada) %>%
    # Shift the year up by 1 to match with next year's data
    mutate(year = year + 1) %>%
    rename(prev_year_last = paridad_ponderada)
  
  # Then join this with the original data and calculate YTD
  df %>%
    mutate(year = year(date)) %>%
    left_join(year_end_values, by = "year") %>%
    mutate(
      ytd = (paridad_ponderada / prev_year_last) - 1
    ) %>%
    # Remove the temporary year column
    select(-year)
}

preciosPond =  calculate_ytd(preciosPond)
# variaciones serie ponderada
preciosPond %>% 
  mutate(varD = (paridad_ponderada / lag(paridad_ponderada) - 1) * 100, 
         varS = (paridad_ponderada / lag(paridad_ponderada, n=5) - 1) * 100
         ) %>% 
  relocate(date, paridad_ponderada, varD, varS, ytd) %>% 
  tail(n=10)




# variaciones de los bonos en BYMA
# preciosDeuda[, c(1:3)] %>% 
#   pivot_wider(names_from = ticker, values_from = price) %>% 
#   mutate(across(-date, ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}")) %>% 
#   rowwise() %>% 
#   mutate(
#     mean_varD = mean(c_across(starts_with("varD")), na.rm = TRUE),
#     max_varD = if_any(starts_with("varD"), ~ !is.na(.)) %>% 
#       if_else(., max(c_across(starts_with("varD")), na.rm = TRUE), NA_real_),
#     min_varD = if_any(starts_with("varD"), ~ !is.na(.)) %>% 
#       if_else(., min(c_across(starts_with("varD")), na.rm = TRUE), NA_real_)
#   ) %>% 
#   ungroup() %>% 
#   tail()


# medias y desvíos
sd=preciosPond %>% filter(date >= "2024-03-20") %>% summarise(sd=sd(paridad_ponderada)) %>% pull(.)
var= preciosPond %>% filter(date >= "2024-03-20") %>% summarise(sd=var(paridad_ponderada)) %>% pull(.)
mean=preciosPond %>% filter(date >= "2024-03-20") %>% summarise(mean=mean(paridad_ponderada)) %>% pull(.)

# grafico
g_deuda_pond_byma = preciosPond  %>% 
  filter(date >= "2020-09-01") %>% 
  ggplot(aes(x=date, y=paridad_ponderada)) +
  theme_outlier() +
  geom_line(linewidth = 1, color = .paleta[1]) +
  # geom_vline(xintercept = as.Date("2024-07-09"), linetype = 2) +
  # geom_vline(xintercept = as.Date("2024-01-09"), linetype = 2) +
  scale_y_continuous(breaks = breaks_extended(14)) +
  
  scale_x_date(expand = c(0,10), #guide = guide_axis(angle = 90),
               date_breaks="6 month", labels=date_format("%b-%Y", locale = "es")) +
  
  
  labs(title = paste0("PARIDAD DEUDA SOBERANA HD GLOBALES (PRECIOS BYMA)"),
       subtitle = paste0("Ponderada por Outstanding. En base al precio del: ", preciosPond %>% tail(n=1) %>% pull(date)),
       y = "Paridad Promedio Ponderada por Outstanding",
       x = '',
       caption = paste0(.pie, " en base a BYMA"))+
  
  scale_color_manual(name = "", labels = c("Paridad Globales Ponderada"), values = .paleta[1]) 
  

grabaGrafo(variable = g_deuda_pond_byma, name = "g_deuda_pond_byma", path = path)


##########################################
# versión con Bollinger Bands


sd_multiplier = 1
ma_window_size = 20  # Window size for the simple moving average

# Calculate rolling standard deviation, simple moving average, and bands
preciosPondBB <- preciosPond %>%
  arrange(date) %>%  # Ensure data is sorted by date
  mutate(
    sma = rollapply(paridad_ponderada, width = ma_window_size, FUN = mean, fill = NA, align = "right"),
    rolling_sd = rollapply(sma, width = ma_window_size, FUN = sd, fill = NA, align = "right"),
    # Simple moving average (SMA)
    sma = rollapply(paridad_ponderada, width = ma_window_size, FUN = mean, fill = NA, align = "right"),
    # 1 Standard Deviation bands
    sd_1_up = sma + sd_multiplier * rolling_sd,
    sd_1_down = sma - sd_multiplier * rolling_sd,
    # 2* Standard Deviation bands
    sd_2_up = sma + sd_multiplier * 2 * rolling_sd,
    sd_2_down = sma - sd_multiplier * 2 * rolling_sd,
    # Flags for points where price surpasses 2* SD up or down
    surpass_up = ifelse(paridad_ponderada > sd_2_up, TRUE, NA),
    surpass_down = ifelse(paridad_ponderada < sd_2_down, TRUE, NA)
  )

# Plot the result with highlights for points surpassing 2 SD and SMA

size = 0.7
g_deuda_byma_bollingerb = ggplot(preciosPondBB %>% filter(date>="2023-12-29"), aes(x = date)) +
  theme_outlier() +
  
  # Main price series
  geom_line(aes(y = paridad_ponderada, color = "Paridad"), linewidth = 1) +  
  # Simple Moving Average (SMA)
  geom_line(aes(y = sma, color = "SMA"), linewidth = 1) +  
  # 1 SD bands
  geom_line(aes(y = sd_1_up, color = "1 d.e."), linewidth = size, linetype = "dashed") +  
  geom_line(aes(y = sd_1_down, color = "1 d.e."), linewidth = size, linetype = "dashed") +  
  # 2 SD bands
  geom_line(aes(y = sd_2_up, color = "2 d.e."), linewidth = size, linetype = "dashed") +  
  geom_line(aes(y = sd_2_down, color = "2 d.e."), linewidth = size, linetype = "dashed") +  
  

  # Custom scales for y and x axis
  scale_y_continuous(breaks = breaks_extended(14)) +
  scale_x_date(expand = c(0,10), 
               date_breaks = "3 month", labels = date_format("%b-%Y", locale = "es")) +
  
  # Custom color mapping
  scale_color_manual(values = c("Paridad" = "blue", 
                                "SMA" = "orange", 
                                "1 d.e." = "green", 
                                "1 d.e." = "green", 
                                "2 d.e." = "red", 
                                "2 d.e." = "red")) +
  
  # Additional information
  labs(title = paste0("PARIDAD DEUDA SOBERANA HD GLOBALES (PRECIOS BYMA)"),
       subtitle = paste0("Ponderada por Outstanding. SMA de ", ma_window_size, " ruedas. En base al precio del: ", preciosPond %>% tail(n=1) %>% pull(date)),
       y = "Paridad Promedio Ponderada por Outstanding",
       x = '',
       caption = paste0(.pie, " en base a BYMA"),
       color = NULL)


grabaGrafo(variable = g_deuda_byma_bollingerb, name = "g_deuda_byma_bollingerb", path = path)

