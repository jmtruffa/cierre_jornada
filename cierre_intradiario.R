url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
URLIntraday = "MarketData/Intraday"

ticker = c("AL30", "AL30D", "AL30C")
type = rep("BONOS", length(ticker))
settlement = rep("A-24HS", length(ticker))
methodsPPI::getPPILogin()
result = tibble(
  date = as.POSIXct(rep.int(NA_real_, 0), tz = "UTC"),
  price = numeric(),
  volume = numeric(),
)

fail = tibble(
  ticker = character()
)

for (i in 1:length(ticker)) {
  error = FALSE
  tryCatch(
    {
      rPriceHistory = request(paste0(url, URLIntraday)) %>%
        req_headers(Authorization = token$token,
                    AuthorizedClient = 'API_CLI_REST',
                    ClientKey = 'pp19CliApp12',
                    `User-Agent` = "http://github.com/jmtruffa") %>%
        req_url_query(ticker = ticker[i],
                      type = type[i],
                      settlement = settlement[i]) %>%
        req_method("GET") %>%
        req_perform()
    },
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
  )
  
  if (!error) {
    # A veces la API de PPI devuelve 200 pero el body está vacío. Vamos a controlar por body vacío
    if (rawToChar(rPriceHistory$body) == "[]") {
      error = TRUE
      fail = fail %>% add_row(ticker = ticker[i])
    } else {
      history = fromJSON(rawToChar(rPriceHistory$body))
      history$date <- as_datetime(history$date, tz = "America/Buenos_Aires")
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
}

result$plazo = settlement[1]

dbWriteDF(table = "precios_intradiarios", df = result, server = server, port = port, append = T)

# ggplot(result, aes(x = date, y = price, color = ticker)) +
#   theme_usado() +
#   geom_line(color = .paleta[1]) +  # Use lines to connect points
#   geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
#   facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
#   labs(title = "PRECIO - INTRADIARIO",
#        subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
#        x = max(as.Date(result$date)),
#        y = "Precio",
#        caption = paste0(.pie, " en base a BYMA")) +
#   theme(legend.position = "none")  # Hide the legend if not needed


price_plot <- ggplot(result, aes(x = date, y = price, color = ticker)) +
  theme_usado() +
  geom_line(color = .paleta[1]) +  # Use lines to connect points
  geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "PRECIO - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Precio",
       caption = paste0(.pie, " en base a BYMA")) +
  theme(legend.position = "none")  # Hide the legend if not needed

# Volume plot
volume_plot <- ggplot(result, aes(x = date, y = volume, fill = ticker)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "VOLUMEN - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Volumen") +
  theme_usado() +
  theme(legend.position = "none")  # Hide the legend if not needed

g_intradiario_soberanos <- price_plot / volume_plot  # Use patchwork to stack plots
#print(g_intradiario_soberanos)

grabaGrafo(variable = g_intradiario_soberanos, name = paste0("g_intradiario_soberanosAL_", settlement[1]), path = path)


##############

settlement = rep("INMEDIATA", length(ticker))
methodsPPI::getPPILogin()
result = tibble(
  date = as.POSIXct(rep.int(NA_real_, 0), tz = "UTC"),
  price = numeric(),
  volume = numeric(),
)


fail = tibble(
  ticker = character()
)

for (i in 1:length(ticker)) {
  error = FALSE
  tryCatch(
    {
      rPriceHistory = request(paste0(url, URLIntraday)) %>%
        req_headers(Authorization = token$token,
                    AuthorizedClient = 'API_CLI_REST',
                    ClientKey = 'pp19CliApp12',
                    `User-Agent` = "http://github.com/jmtruffa") %>%
        req_url_query(ticker = ticker[i],
                      type = type[i],
                      settlement = settlement[i]) %>%
        req_method("GET") %>%
        req_perform()
    },
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
  )
  
  if (!error) {
    # A veces la API de PPI devuelve 200 pero el body está vacío. Vamos a controlar por body vacío
    if (rawToChar(rPriceHistory$body) == "[]") {
      error = TRUE
      fail = fail %>% add_row(ticker = ticker[i])
    } else {
      history = fromJSON(rawToChar(rPriceHistory$body))
      history$date <- as_datetime(history$date, tz = "America/Buenos_Aires")
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
}

result$plazo = settlement[1]


dbWriteDF(table = "precios_intradiarios", df = result, server = server, port = port, append = T)

# ggplot(result, aes(x = date, y = price, color = ticker)) +
#   theme_usado() +
#   geom_line(color = .paleta[1]) +  # Use lines to connect points
#   geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
#   facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
#   labs(title = "PRECIO - INTRADIARIO",
#        subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
#        x = max(as.Date(result$date)),
#        y = "Precio",
#        caption = paste0(.pie, " en base a BYMA")) +
#   theme(legend.position = "none")  # Hide the legend if not needed


price_plot <- ggplot(result, aes(x = date, y = price, color = ticker)) +
  theme_usado() +
  geom_line(color = .paleta[1]) +  # Use lines to connect points
  geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "PRECIO - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Precio",
       caption = paste0(.pie, " en base a BYMA")) +
  theme(legend.position = "none")  # Hide the legend if not needed

# Volume plot
volume_plot <- ggplot(result, aes(x = date, y = volume, fill = ticker)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "VOLUMEN - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Volumen") +
  theme_usado() +
  theme(legend.position = "none")  # Hide the legend if not needed

g_intradiario_soberanos <- price_plot / volume_plot  # Use patchwork to stack plots
#print(g_intradiario_soberanos)

grabaGrafo(variable = g_intradiario_soberanos, name = paste0("g_intradiario_soberanosAL_", settlement[1]), path = path)



########################################################
########################################################
########################################################

url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
URLIntraday = "MarketData/Intraday"

ticker = c("GD30", "GD30D", "GD30C")
type = rep("BONOS", length(ticker))
settlement = rep("A-24HS", length(ticker))
methodsPPI::getPPILogin()
result = tibble(
  date = as.POSIXct(rep.int(NA_real_, 0), tz = "UTC"),
  price = numeric(),
  volume = numeric(),
)


fail = tibble(
  ticker = character()
)

for (i in 1:length(ticker)) {
  error = FALSE
  tryCatch(
    {
      rPriceHistory = request(paste0(url, URLIntraday)) %>%
        req_headers(Authorization = token$token,
                    AuthorizedClient = 'API_CLI_REST',
                    ClientKey = 'pp19CliApp12',
                    `User-Agent` = "http://github.com/jmtruffa") %>%
        req_url_query(ticker = ticker[i],
                      type = type[i],
                      settlement = settlement[i]) %>%
        req_method("GET") %>%
        req_perform()
    },
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
  )
  
  if (!error) {
    # A veces la API de PPI devuelve 200 pero el body está vacío. Vamos a controlar por body vacío
    if (rawToChar(rPriceHistory$body) == "[]") {
      error = TRUE
      fail = fail %>% add_row(ticker = ticker[i])
    } else {
      history = fromJSON(rawToChar(rPriceHistory$body))
      history$date <- as_datetime(history$date, tz = "America/Buenos_Aires")
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
}

result$plazo = settlement[1]


dbWriteDF(table = "precios_intradiarios", df = result, server = server, port = port, append = T)

ggplot(result, aes(x = date, y = price, color = ticker)) +
  theme_usado() +
  geom_line(color = .paleta[1]) +  # Use lines to connect points
  geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "PRECIO - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = max(as.Date(result$date)),
       y = "Precio",
       caption = paste0(.pie, " en base a BYMA")) +
  theme(legend.position = "none")  # Hide the legend if not needed



price_plot <- ggplot(result, aes(x = date, y = price, color = ticker)) +
  theme_usado() +
  geom_line(color = .paleta[1]) +  # Use lines to connect points
  geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "PRECIO - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Precio",
       caption = paste0(.pie, " en base a BYMA")) +
  theme(legend.position = "none")  # Hide the legend if not needed

# Volume plot
volume_plot <- ggplot(result, aes(x = date, y = volume, fill = ticker)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "VOLUMEN - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Volumen") +
  theme_usado() +
  theme(legend.position = "none")  # Hide the legend if not needed

g_intradiario_soberanos <- price_plot / volume_plot  # Use patchwork to stack plots


grabaGrafo(variable = g_intradiario_soberanos, name = paste0("g_intradiario_soberanosGD_", settlement[1]), path = path)


##############

settlement = rep("INMEDIATA", length(ticker))
methodsPPI::getPPILogin()
result = tibble(
  date = as.POSIXct(rep.int(NA_real_, 0), tz = "UTC"),
  price = numeric(),
  volume = numeric(),
)


fail = tibble(
  ticker = character()
)

for (i in 1:length(ticker)) {
  error = FALSE
  tryCatch(
    {
      rPriceHistory = request(paste0(url, URLIntraday)) %>%
        req_headers(Authorization = token$token,
                    AuthorizedClient = 'API_CLI_REST',
                    ClientKey = 'pp19CliApp12',
                    `User-Agent` = "http://github.com/jmtruffa") %>%
        req_url_query(ticker = ticker[i],
                      type = type[i],
                      settlement = settlement[i]) %>%
        req_method("GET") %>%
        req_perform()
    },
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
  )
  
  if (!error) {
    # A veces la API de PPI devuelve 200 pero el body está vacío. Vamos a controlar por body vacío
    if (rawToChar(rPriceHistory$body) == "[]") {
      error = TRUE
      fail = fail %>% add_row(ticker = ticker[i])
    } else {
      history = fromJSON(rawToChar(rPriceHistory$body))
      history$date <- as_datetime(history$date, tz = "America/Buenos_Aires")
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
}

result$plazo = settlement[1]


dbWriteDF(table = "precios_intradiarios", df = result, server = server, port = port, append = T)

# ggplot(result, aes(x = date, y = price, color = ticker)) +
#   theme_usado() +
#   geom_line(color = .paleta[1]) +  # Use lines to connect points
#   geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
#   facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
#   labs(title = "PRECIO - INTRADIARIO",
#        subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
#        x = max(as.Date(result$date)),
#        y = "Precio",
#        caption = paste0(.pie, " en base a BYMA")) +
#   theme(legend.position = "none")  # Hide the legend if not needed



price_plot <- ggplot(result, aes(x = date, y = price, color = ticker)) +
  theme_usado() +
  geom_line(color = .paleta[1]) +  # Use lines to connect points
  geom_point(size = 1, color = .paleta[1]) +  # Add points to the lines
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "PRECIO - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Precio",
       caption = paste0(.pie, " en base a BYMA")) +
  theme(legend.position = "none")  # Hide the legend if not needed

# Volume plot
volume_plot <- ggplot(result, aes(x = date, y = volume, fill = ticker)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_x_datetime(date_labels = "%H:%M", breaks = date_breaks("1 hour")) +  # More hours in x-axis
  facet_wrap(~ ticker, scales = "free_y") +  # Create separate panels for each ticker
  labs(title = "VOLUMEN - INTRADIARIO",
       subtitle = paste0("Settlement: ", ifelse(settlement[1] == "A-24HS", "T+1", "T+0")),
       x = "Fecha",
       y = "Volumen") +
  theme_usado() +
  theme(legend.position = "none")  # Hide the legend if not needed

g_intradiario_soberanos <- price_plot / volume_plot  # Use patchwork to stack plots


grabaGrafo(variable = g_intradiario_soberanos, name = paste0("g_intradiario_soberanosGD_", settlement[1]), path = path)



