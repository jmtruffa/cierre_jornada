tickers = map_dfr(.x = "ADR", .f = methodsPPI::sets, server = server, port = port) %>% 
  select(ticker) %>% 
  pull(ticker)

nombres = tickers

tickers = c(tickers, "^GSPC", "^NDX", "EWZ", "ILF")
nombres = c(nombres, "SPX", "NDX", "EWZ", "ILF")

nombres_display = data.frame(
  ticker = c(tickers, "Merval CCL"),
  nombre = c(nombres, "Merval")
)

orden <- c("BANCOS", "GENERADORAS", "UTILITIES", "PETROLEO", "BASIC MATERIALS", "REAL ESTATE", "TELCO")

prices_raw <- tidyquant::tq_get(
  tickers,
  from = Sys.Date() - lubridate::years(2),
  to = Sys.Date() + 1
)

datos <- prices_raw %>%
  dplyr::select(date, ticker = symbol, price = adjusted) %>%
  tidyr::drop_na() %>%
  dplyr::arrange(ticker, date)

mrv = getMerval(
  fechaInicio = Sys.Date() - lubridate::years(2),
  server = server,
  port = port
) %>% 
  dplyr::select(date, price = mervalCCL) %>% 
  dplyr::mutate(ticker = "Merval CCL") %>% 
  dplyr::relocate(date, ticker, price)

mrv = mrv %>% drop_na(price)
  #dplyr::distinct(merval, .keep_all = TRUE)

tickers = c(tickers, "Merval CCL")

full = rbind(datos, mrv) %>% 
  dplyr::arrange(ticker, date)

calendarios <- setNames(
  c(
    rep("cal_usa", length(tickers) - 1),
    "cal"
  ),
  tickers
)

grupos = data.frame(
  ticker = c("Merval CCL", "^GSPC", "^NDX", "EWZ", "ILF"),
  grupo  = c("01", "02", "03", "04", "05")
)

t_panel_adrs = finance::panel_variaciones_generico(
  datos = full,
  calendarios = calendarios,
  grupos = grupos,
  nombres_display = nombres_display,
  fecha_referencia = Sys.Date(),
  max_tickers_por_panel = 20,
  titulo = "Panel de Variaciones - ADRs Seleccionados",
  nota_pie = "Outlier en base a precios Yahoo Finance y BYMA",
  sector_order = orden
)

grabaTabla2(variable = t_panel_adrs, path = path)