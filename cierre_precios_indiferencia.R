######## Indiferencia
## tiene que ser corrido el final, cuando fx (brecha), curva_lecaps (varTasasLecap) y rofexOI (intAbiertoRofex) ya estén disponibles
###

df_tc <-
  curva_lecaps %>% 
  select(date, ticker, price, vf, date_vto) %>% 
    #filter(date == "2025-07-02") %>% 
  filter(date == max(date)) %>%                       # último día
  left_join(
    fx %>% distinct(date, .keep_all = TRUE) %>% 
      select(-canje),                                 # (renombrá acá si querés)
    by = "date"
  ) %>% 
  filter(!is.na(vf)) %>% 
  left_join(finance::get_bandas(), join_by(date_vto == date)) %>% 
  mutate(across(c(mepAL, ccl3, A3500),
                ~ (vf / price) * .x,
                .names = "tc_{.col}")) %>% 
  select(-c(mepAL, ccl3, A3500))

# traigo los settle para cada mes de date_vto
# rofexOI = functions::dbGetTable(table = "rofexHis", server = server, port = port)
# tmpA3 = readxl::read_xlsx("~/Downloads/todoSeBorra/archivosTemp/tmpA3.xlsx")
# tmpA3$date = as.Date(tmpA3$date)
# tmpA3$EOM = as.Date(tmpA3$EOM)
# rofexOI = rofexOI %>% add_row(tmpA3) 
rofexOI_ampliado = rofexOI %>% 
  mutate(
    mes = month(EOM),
    anio = year(EOM)
  ) %>% 
  select(date, settlement, mes, anio, symbol, EOM) %>% 
  filter(date == max(date))

df_tc = df_tc %>% 
  mutate(
    mes = month(date_vto),
    anio = year(date_vto)
  ) %>% 
  full_join(rofexOI_ampliado, join_by(mes == mes, anio == anio, date ==date)) 

df_grafo = df_tc

df_tc = df_tc %>% 
  arrange(date_vto) %>% 
  select(-mes, -anio, -date_vto, -EOM)
  
tipos_cambio = fx %>% distinct(date, .keep_all = T) %>% filter(date == max(date)) %>% select(-Canje)
valores = finance::get_bandas(end_date = Sys.Date()) %>% filter(date == Sys.Date())
footer = paste0("Último dato: ", max(df_tc$date), " | ", paste0("MEP: ", round(pull(tipos_cambio, 2),2), " | CCL: ", round(pull(tipos_cambio, 3),2), " | A3500: ", round(pull(tipos_cambio, 4),2), 
                                                                " | Banda Inferior: ", round(valores$banda_inferior, 2), " | Banda Superior: ", round(valores$banda_superior, 2)))

# 2. Reordenamos y ponemos nombres “clave” (sin espacios)…
df_out <- df_tc %>% 
  select(
    FECHA          = date,
    TICKER         = ticker,
    PRECIO         = price,
    VF             = vf,
    BANDA_INFERIOR = banda_inferior,
    BANDA_SUPERIOR = banda_superior,
    tc_MEP         = tc_mepAL,
    tc_CCL         = tc_ccl3,
    tc_SPOT_A3500  = tc_A3500,
    LAST           = settlement,
    CONTRATO       = symbol
  )

# ── 2. header de dos niveles ────────────────────────────────────────────────────
encabezado <- data.frame(
  col_keys = names(df_out),
  nivel1   = c(rep("", 6), rep("TC INDIFERENCIA", 3), rep("", 2)),
  nivel2   = c("FECHA", "TICKER", "PRECIO", "VF", "BANDA INFERIOR", "BANDA SUPERIOR",
               "MEP", "CCL", "SPOT (A3500)", "LAST", "CONTRATO")
)

# ── 3. flextable con alineación centrada y líneas verticales ───────────────────
#library(officer)
borda <- officer::fp_border(color = "black", width = .75)


num_cols <- names(df_out)[sapply(df_out, is.numeric)]

t_lecap_px_indiferencia <- df_out |>
  flextable() |>
  set_header_df(mapping = encabezado, key = "col_keys") |>
  merge_at(i = 1, j = 7:9, part = "header") |>
  theme_booktabs() |>
  #align(part = "header", align = "center") |>
  align(part = "all", align = "center") |>
  vline(j = c(1, 2, 4, 6,9), border = borda, part = "all") |>
  colformat_double(
    j = num_cols,
    digits = 2
  ) |> add_footer_lines(values = footer) |>       # crea la fila 1 del 'footer'
  merge_h(i = 1, part = "footer") |>         # ahora sí existe la fila
  align(part = "footer", align = "left") |>  # o "center"
  bg(part = "all", bg = "white") |>   # ← fondo blanco en todo el objeto
  autofit()

grabaTabla2(variable = t_lecap_px_indiferencia, path = path)

  
  ## ── 1. Reordenar df_grafo en formato largo ───────────────────────────
  # a) puntos de tipos de cambio (MEP / CCL / A3500)
  df_grafo_tc <- df_grafo %>% 
    select(date_vto, ticker, starts_with("tc_")) %>% 
    pivot_longer(
      cols      = starts_with("tc_"),
      names_to  = "serie",
      values_to = "valor"
    ) %>% 
    filter(!is.na(valor))
  
  # b) puntos de settlement
  df_settle <- df_grafo %>% 
    select(EOM, settlement, symbol) %>% 
    filter(!is.na(EOM) & !is.na(settlement))
  
  ## ── 2. Paleta de colores (opcional) ──────────────────────────────────
  col_tc <- c(tc_mepAL = .paleta[1],
              tc_ccl3  = .paleta[2],
              tc_A3500 = .paleta[3])
  
  ## ── 3. Armado del gráfico ────────────────────────────────────────────
  min_fecha = min(df_grafo$date_vto, na.rm = TRUE)
  max_fecha = max(max(df_grafo$date_vto, na.rm = TRUE) ,   max(df_grafo$EOM, na.rm = TRUE) )
  
  
  p <- finance::get_bandas("2027-02-01") %>% 
    filter(date >= min_fecha & date <= max_fecha) %>%
    ggplot(aes(x = date)) +
    theme_usado() +
    
    # bandas
    geom_line(aes(y = banda_superior), linetype = "dashed", colour = "grey50") +
    geom_line(aes(y = banda_inferior),  linetype = "dashed", colour = "grey50") +
    
    # puntos + etiquetas: tipos de cambio
    geom_point(data = df_grafo_tc,
               aes(x = date_vto, y = valor, colour = serie),
               size = 2.8) +
    geom_text_repel(data = df_grafo_tc %>% distinct(ticker, .keep_all = TRUE),
                    aes(x = date_vto, y = valor, label = ticker, colour = serie, ),
                    size = 3, family = "sans", show.legend = FALSE, max.overlaps = Inf) +
    
    # puntos + etiquetas: settlement
    geom_point(data = df_settle,
               aes(x = EOM, y = settlement),
               shape = 21, fill = "white", colour = "black", size = 3) +
    geom_text_repel(data = df_settle %>%  distinct(symbol, .keep_all = TRUE),
                    aes(x = EOM, y = settlement, label = symbol),
                    size = 4, family = "sans", colour = "black", show.legend = FALSE, max.overlaps = Inf, nudge_y = - 50) +
    
    scale_colour_manual(values = col_tc, name = NULL, labels = c("tc_A3500", "tc_CCL", "tc_MEP")) +
    
    # TC actuales
    geom_hline(data = tipos_cambio, aes(yintercept = mepAL), linetype = "dashed", colour = col_tc["tc_mepAL"]) +
    geom_hline(data = tipos_cambio, aes(yintercept = ccl3), linetype = "dashed", colour = col_tc["tc_ccl3"]) +
    geom_hline(data = tipos_cambio, aes(yintercept = A3500), linetype = "dashed", colour = col_tc["tc_A3500"]) +
    
    scale_x_date(date_breaks="1 month", label = scales::label_date("%m-%y")) +
    scale_y_continuous(breaks = breaks_extended(10), labels = label_currency(big.mark = ".", decimal.mark = ",")) +
    labs(x = NULL,
         y = "Pesos",
         title = "TIPOS DE CAMBIO INDIFERENCIA LECAPS/FUTUROS",
         subtitle = paste0("Valores de cierre de futuros. Valores al: " , max(df_grafo$date)))

  
grabaGrafo(variable = p, name = "g_lecap_px_indiferencia", path = path)
  