varADR = function(tickers, date2) {

  # Create or use an existing calendar with known holidays
  bizdays::create.calendar(name = "MyCalendar", 
                           start.date = as.Date(date2) %m-% months(13), 
                           end.date = as.Date(date2),
                           weekdays = c("saturday", "sunday"), 
                           holidays = c(as.Date("2024-09-02"),
                                        as.Date("2024-12-25"),
                                        as.Date("2025-01-01"),
                                        as.Date("2025-01-09"),
                                        as.Date("2025-01-20"),
                                        as.Date("2025-02-17"),
                                        as.Date("2025-04-18"),
                                        as.Date("2025-05-26"),
                                        as.Date("2025-06-19"),
                                        as.Date("2025-07-04"),
                                        as.Date("2025-09-01")
                                        ))
   
  # Convert date2 to Date object
  date2 = as.Date(date2)
  
  # Calculate the same day from the previous month
  date30d = date2 %m-% months(1)
  # Adjust date30d for weekends and holidays
  date30d = bizdays::adjust.next(date30d, "MyCalendar")
  
  # Calculate 1 working day and 5 working days prior to date2
  date1d = bizdays::offset(date2, -1, "MyCalendar")
  date5d = bizdays::offset(date2, -5, "MyCalendar")
  
  # Calculate the last business day of the previous year
  end_of_last_year = as.Date(paste0(year(date2) - 1, "-12-31"))
  dateYTD = bizdays::adjust.previous(end_of_last_year, "MyCalendar")
  
  # Fetch stock prices for the given tickers and date range
  prices = tq_get(tickers, get = "stock.prices", from = min(c(dateYTD, date1d, date5d, date30d)), to = date2 + 1)
  
  # Filter for the specified dates
  prices_filtered = prices %>%
    filter(date %in% c(dateYTD, date1d, date5d, date30d, date2))
  
  # Spread the data to have separate columns for each date's prices
  price_comparison = prices_filtered %>%
    select(symbol, date, adjusted) %>%
    pivot_wider(names_from = date, values_from = adjusted, names_prefix = "precio_") %>%
    mutate(
      `1d` = (get(paste0("precio_", date2)) - get(paste0("precio_", date1d))) / get(paste0("precio_", date1d)),
      `5d` = (get(paste0("precio_", date2)) - get(paste0("precio_", date5d))) / get(paste0("precio_", date5d)),
      `1m` = (get(paste0("precio_", date2)) - get(paste0("precio_", date30d))) / get(paste0("precio_", date30d)),
      `YTD` = (get(paste0("precio_", date2)) - get(paste0("precio_", dateYTD))) / get(paste0("precio_", dateYTD))
    ) %>% 
    select(symbol, precio = head(tail(everything(), 5),1),tail(everything(), 4))
  
  # Return the comparison data
  return(price_comparison)
}

date2 = Sys.Date()

tickers = map_dfr(.x = "ADR", .f = methodsPPI::sets, server = server, port = port)
#tickers = map_dfr("ADR", sets)
orden = c("BANCOS", "GENERADORAS", "UTILITIES", "PETROLEO", "BASIC MATERIALS", "REAL ESTATE", "TELCO")
tickers = tickers %>% add_row(ticker = "^GSPC") %>% add_row(ticker="^NDX") #%>% add_row(ticker="^STOXX50E") %>% add_row(ticker="^N225")
result = varADR(tickers$ticker, date2)
result = result %>% mutate(symbol = ifelse(symbol == '^GSPC', 'SPX', symbol)) %>% mutate(symbol = ifelse(symbol == '^NDX', 'NDX', symbol))
result_formatted <- result %>%
  mutate(across(c(`1d`, `5d`, `1m`, YTD), 
                ~sprintf("%0.2f%%", . * 100),  # Format as percentage with 2 decimal places
                .names = "{.col}_formatted")) %>%
  mutate(across(c(`1d`, `5d`, `1m`, YTD), 
                ~ifelse(. < 0, "red", "darkgreen"), 
                .names = "{.col}_color"))

# Join with the 'sectores' table and arrange
# Assuming dbGetTable is a custom function to fetch data; replace with your actual data source
datos_tabla_ADR <- result_formatted %>%
  left_join(dbGetTable(table = "sectores", server = server, port = port), by = c("symbol" = "ticker")) %>%
  mutate(sector = factor(sector, levels = orden)) %>%
  arrange(sector, symbol) %>%
  select(-sector) %>% 
  rename(ultimo = precio)


# Create the flextable with only the visible columns
g_tablaADRs <- flextable(datos_tabla_ADR, 
                         col_keys = c("symbol", "ultimo", "1d_formatted", "5d_formatted", "1m_formatted", "YTD_formatted")) %>%
  # Rename column headers
  set_header_labels(
    symbol = "Ticker",
    ultimo = "Ultimo",
    `1d_formatted` = "1D",
    `5d_formatted` = "5D",
    `1m_formatted` = "1M",
    `YTD_formatted` = "YTD"
  ) %>%
  bold(j = "symbol", part = "body") %>%
  bold(j = "symbol", part = "header") %>%
  bold(j = c("symbol", "ultimo", "1d_formatted", "5d_formatted", "1m_formatted", "YTD_formatted"), part = "header") %>%
  # Apply conditional coloring directly using the color columns from the data frame
  color(j = "1d_formatted", color = datos_tabla_ADR$`1d_color`) %>%
  color(j = "5d_formatted", color = datos_tabla_ADR$`5d_color`) %>%
  color(j = "1m_formatted", color = datos_tabla_ADR$`1m_color`) %>%
  color(j = "YTD_formatted", color = datos_tabla_ADR$`YTD_color`) %>%
  vline(j = c("symbol", "ultimo"), border = fp_border_default(color = "black")) %>%
  # Add borders (equivalent to column_spec with border_left)
  #border(j = c("ultimo"), border = fp_border_default(color = "black"), part = "all") %>%
  # Center-align all columns
  align(align = "center", part = "all") %>%
  # Set table properties (similar to kable_classic full_width = FALSE)
  set_table_properties(layout = "autofit", width = 0.8) %>%
  # Add footnote
  add_footer_lines("Outlier en base a precios Yahoo Finance.") %>% 
  # Add title
  add_header_lines("Tabla de Selección de ADRs Argentinos") %>% 
  # Align footer to right
  align(align="right", part = "footer") %>% 
  hline(i = 14, border = fp_border(color = "black", style = "solid", width = 1), part = "body")  
  #bg(part = "all", bg = "white")


grabaTabla2(variable = g_tablaADRs, path = path)
