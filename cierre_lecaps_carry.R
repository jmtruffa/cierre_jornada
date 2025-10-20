lecaps_carry <- functions::dbExecuteQuery(
    query  = paste0("select date, ticker, price from historico_lecaps"),
    server = server, port = port
  ) %>% 
  filter(!(date == as.Date("2024-07-10") & ticker == "S12L4") &
           !(date == as.Date("2025-02-17") & ticker == "S28F5") 
  )

hist_lecap = finance::tasasLecap(lecaps_carry, server = server, port = port)
hl = hist_lecap %>% 
  select(ticker, date, tea, date_vto) %>% 
  mutate(
    diasVto = as.numeric(date_vto - date),
    diasVto2 = diasVto^2
  ) %>% 
  group_by(date) %>% 
  nest()

fit_model <- function(data) {
  lm(tea ~ diasVto + diasVto2, data = data)
}


hl_models <- hl %>%
  mutate(model = map(data, fit_model))


vector = c(30)
hl_predictions <- hl_models %>%
  mutate(
    predictions = map(model, ~predict(
      .x, 
      newdata = tibble(
        diasVto = vector,
        diasVto2 = vector^2
      )
    ))
  ) %>%
  # Unnest predictions and create columns
  unnest(predictions) %>%
  group_by(date) %>%
  mutate(
    days = c("30")  # Label each prediction
  ) %>%
  ungroup() %>%
  select(date, days, predictions) %>%
  pivot_wider(
    names_from = days,
    values_from = predictions,
    names_prefix = ""
  ) %>% 
  mutate(
    across(
      .cols = c("30"),  # Select columns to transform
      .fns = ~ ((1 + .)^(30/365)) - 1       # Apply the transformation
    )
  )



df <- hl_predictions %>%
  mutate(days_to_end = as.numeric(max(date) - date))


compound_forward <- function(start_idx, df) {
  current_date <- df$date[start_idx]
  current_rate <- df$`30`[start_idx]
  days_left <- df$days_to_end[start_idx]
  
  compounded <- 1
  
  while (days_left > 0) {
    if (days_left < 30) {
      compounded = compounded * ( 1 + current_rate) ^(days_left / 30)
      break
    } 
    # periodo completo de 30 días
    compounded = compounded * (1 + current_rate)
    # Moverse 30 días hacia adelante
    next_date = current_date + days(30)
    # Encontrar la próxima fila con una fecha >= next_date
    next_row = which(df$date >= next_date)[1]
    # si no hay más filas, tomar el último valor
    if (is.na(next_row)) {
      next_row = nrow(df)
    }
    # Actualizar current_date y current_rate para la próxima iteración
    current_date = df$date[next_row]
    current_rate = df$`30`[next_row]
    days_left = df$days_to_end[next_row]
  }
  
  return(compounded)
}


df <- df %>%
  mutate(lecap_capitalizada = sapply(seq_along(date), compound_forward, df = .) ^ (365/days_to_end) - 1)

df_ccl = dbGetTable("ccl", server = server, port = port) %>% select(date, ccl3) %>% filter(date >= "2024-01-01")

final = left_join(df, df_ccl) %>% 
  fill(ccl3) %>% 
  mutate(usd_final = last(ccl3),
         fecha_final = last(date),
         dias = as.numeric(fecha_final - date),
         usd_tasa_directa = usd_final / ccl3 - 1,
         usd_tem = (1 + usd_tasa_directa) ^ (30/dias) - 1,
         usd_capitalizada = (1 + usd_tasa_directa) ^ (365/dias) - 1,
         tea_usd = (1 + lecap_capitalizada) / (1 + usd_capitalizada) - 1
  ) 

# Primero determinamos el rango real de los datos
ccl3_min <- min(final$ccl3, na.rm = TRUE)
ccl3_max <- max(final$ccl3, na.rm = TRUE)
tea_usd_min <- min(final$tea_usd, na.rm = TRUE)
tea_usd_max <- min(max(final$tea_usd, na.rm = TRUE), 10)

# Aplicamos el gráfico con los rangos reales
g_tasa_usd_carry = final[1:(nrow(final)),] %>% 
  select(date, tea_usd, ccl3) %>% 
  ggplot() +
  # Graficar res en el eje principal
  geom_line(aes(x = date, y = tea_usd, color = "TEA USD - carry (eje izq)"), linewidth = 1) +
  # Graficar ccl3 con escala y estética propia, usando rangos reales
  geom_line(aes(x = date, y = (ccl3 - ccl3_min) / (ccl3_max - ccl3_min) * (tea_usd_max - tea_usd_min) + tea_usd_min, 
                color = "CCL (eje der)"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_usado() +
  # Configuración del eje Y primario (para res)
  scale_y_continuous(
    breaks = breaks_extended(15), 
    labels = scales::percent_format(),
    name = "",
    limits = c(tea_usd_min, tea_usd_max),
    
    # Eje secundario para ccl3
    sec.axis = sec_axis(
      # Transformación inversa: de la escala de res a la de ccl3
      transform = ~ (. - tea_usd_min) / (tea_usd_max - tea_usd_min) * (ccl3_max - ccl3_min) + ccl3_min,
      name = "CCL",
      breaks = pretty(c(ccl3_min, ccl3_max), n = 6)
    )
  ) +
  scale_x_cont_dates(
    name = "", 
    business.dates = df$date, 
    labels = label_date(format = "%b-%y", locale = "es"), 
    max.major.breaks = 20
  ) +
  scale_color_manual(
    name = NULL, 
    values = .paleta[1:2]
  ) +
  labs(
    title = "TASA EN USD REALIZADA CON LECAPS SEGUN FECHA DE COMIENZO",
    subtitle = paste0('Capitalizado cada 30 días. Último dato: ',final[1:(nrow(final)-1),] %>% tail(n=1) %>% pull(date)),
    y = 'DIFERENCIAL',
    x = '',
    caption = paste0(.pie, " en base a precios de mercado. Curva lecap 30 días en función a regresión diaria")
  ) +
  theme(plot.margin = margin(10, 20, 10, 10))

grabaGrafo(variable = g_tasa_usd_carry, path = path)
