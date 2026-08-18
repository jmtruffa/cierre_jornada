# Los tickers viven en bonds_tickers (bond_id -> ticker); bonds_db no tiene columna ticker.
# Antes esto leia la tabla legacy `bonds`, que quedo desactualizada: no traia TZXO7 ni TZXD8
# y por eso esos bonos entraban con issue_date NA. cierre_boncer.R y cierre_dl.R ya migraron
# a bonds_db, este era el ultimo consumidor de `bonds` en el repo.
query = "
SELECT DISTINCT
  bt.ticker,
  db.id,
  db.issue_date,
  db.maturity,
  db.coupon,
  db.index_type_id AS index,
  db.\"offset\"
FROM bonds_db db
JOIN bonds_tickers bt
  ON db.id = bt.bond_id
WHERE bt.ticker LIKE 'TZX%'
  AND bt.cotizacion = 1
ORDER BY bt.ticker
"

instrumentos <- as_tibble(
  functions::dbExecuteQuery(query = query, server = server, port = port)
) %>% janitor::clean_names()
#instrumentos$issue_date = as.Date(instrumentos$issue_date)


cer_nuevo = extend_CER(c(.017,.015,.0179, 0.017,.048, rep(0.0155, 10)), server = server, port = port)

# El horizonte de la proyeccion de CER define hasta que vencimiento se puede calcular el
# valor final del bono: pasada esa fecha el join contra cer_nuevo devuelve NA y la fila no
# sirve. Se deriva de cer_nuevo en lugar de fijarse a mano, porque la fecha fija que habia
# antes ('2026-08-15') caduco cuando vencio el ultimo TZX corto y dejo el filtro en cero
# filas, lo que hacia explotar add.bizdays() con "Given date out of range".
cer_max_date <- max(cer_nuevo$date, na.rm = TRUE)

base_boncer_be <- boncer %>%
  dplyr::filter(str_detect(ticker, 'TZX')) %>%
  dplyr::select(date, ticker, price, date_vto) %>%
  dplyr::left_join(instrumentos %>% dplyr::select(ticker, issue_date), by = "ticker") %>%
  dplyr::filter(!is.na(date_vto), !is.na(issue_date))

# Red de seguridad: ticker que cotiza pero todavia no esta cargado en bonds_db. Sin issue_date
# no hay CER inicial, asi que se descarta. Se loguea para que la falta de dato quede visible.
sin_issue_date <- boncer %>%
  dplyr::filter(str_detect(ticker, 'TZX')) %>%
  dplyr::distinct(ticker) %>%
  dplyr::anti_join(instrumentos, by = "ticker") %>%
  dplyr::pull(ticker)

if (length(sin_issue_date) > 0) {
  functions::log_msg(
    sprintf("Boncer BE: %d ticker(s) sin issue_date en bonds_db, se excluyen: %s",
            length(sin_issue_date), paste(sin_issue_date, collapse = ", ")),
    "WARN",
    log_file = "./cierre.log"
  )
}

base_boncer_be <- base_boncer_be %>%
  dplyr::mutate(cer_date = add.bizdays(date_vto, -10, cal)) %>%
  dplyr::filter(cer_date <= cer_max_date)

if (nrow(base_boncer_be) == 0) {
  functions::log_msg(
    sprintf("Abortamos proceso con Boncer BE: ningun TZX vigente vence dentro de la proyeccion de CER (hasta %s).",
            format(cer_max_date)),
    "ERROR",
    log_file = "./cierre.log"
  )
} else {

out = base_boncer_be %>%
  left_join(cer_nuevo, by = c('cer_date' = 'date')) %>% 
  mutate(issue_date_ajustada = add.bizdays(issue_date, -10, cal)) %>% 
  left_join(cer_nuevo, join_by(issue_date_ajustada == date)) %>% 
  rename(CER_final = CER.x, CER_inicial = CER.y) %>% 
  left_join(tc %>% select(date, last_mlc), join_by(date)) %>% 
  mutate(vf = 100 * (CER_final / CER_inicial),
         tc_equilibrio = (vf/price)*last_mlc,
         upside = (tc_equilibrio / last_mlc -1) * 100) %>% 
  filter(date==max(date)) %>% 
  select(FECHA = date, TICKER = ticker, PRICE = price, VTO = date_vto, FX = last_mlc, FX_EQUILIBRIO = tc_equilibrio, UPSIDE = upside) %>% 
  mutate(
    across(
      .cols = c(UPSIDE),
      .fns = ~ paste0(round(.x , 2), " %")
    ),
    across(
      .cols = c(PRICE, FX, FX_EQUILIBRIO),
      .fns = ~ round(.x, 2)
    )
  )

num_cols <- names(out)[sapply(out, is.numeric)]

t_boncer_be = out %>% 
  arrange(VTO) %>% 
  flextable() %>% 
  width(width = 1.) %>%
  colformat_num(j = num_cols, digits = 2) %>% 
  fix_border_issues() %>%
  set_caption("TIPO DE CAMBIO EQUILIBRIO BONOS CER") %>% 
  align(align = "center", part = "all") %>% 
  #agregamos nota el pie
  add_footer_lines(as_paragraph("Fuente: Elaboración propia en base a BYMA, BCRA y proyección de CER segun REM.")) %>% 
  bg(part = "all", bg = "white")
grabaTabla2(variable = t_boncer_be, path = path)  



}
