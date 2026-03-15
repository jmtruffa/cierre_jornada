ticker_etfs <- c("XLB", "XLC", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "XLRE")
ticker_etfs = c(ticker_etfs, "^GSPC","^NDX")
nombres = head(ticker_etfs, -2)
nombres = c(nombres, "SPX", "NDX")
nombres_display = data.frame("ticker" = ticker_etfs, "nombre" = nombres)
prices_raw <- tidyquant::tq_get(ticker_etfs, from = Sys.Date() - lubridate::years(1), to = Sys.Date() + 1)

datos <- prices_raw %>%
  dplyr::select(date, ticker = symbol, price = adjusted) %>%
  tidyr::drop_na() %>%
  dplyr::arrange(ticker, date)

calendarios <- setNames(rep("calendario_feriados_usa", length(ticker_etfs)), ticker_etfs)

t_panel_etfs <- finance::panel_variaciones_generico(
  datos = datos,
  calendarios = calendarios,
  nombres_display = nombres_display,
  fecha_referencia = Sys.Date() - 1,
  max_tickers_por_panel = 15,
  titulo = "Panel de Variaciones - ETFs",
  nota_pie = "Outlier en base a precios Yahoo Finance."
)

grabaTabla2(variable = t_panel_etfs, path = path)
