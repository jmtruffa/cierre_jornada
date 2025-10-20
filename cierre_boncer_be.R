# Leer un archivo JSON local
data <- fromJSON("~/go/src/github.com/jmtruffa/yields/bonds.json", simplifyVector = FALSE)

instrumentos <- lapply(data, function(x) {
  data.frame(
    ID        = x$ID,
    Ticker    = x$Ticker,
    IssueDate = x$IssueDate,
    Maturity  = x$Maturity,
    Coupon    = x$Coupon,
    Index     = x$Index,
    Offset    = x$Offset,
    stringsAsFactors = FALSE
  )
}) |> bind_rows() %>% janitor::clean_names()
instrumentos$issue_date = as.Date(instrumentos$issue_date)


cer_nuevo = extend_CER(c(.017,.015,.0179, 0.017,.048, rep(0.0155, 10)), server = server, port = port)
out = boncer %>% filter(str_detect(ticker, 'TZX'))  %>% 
  select(date, ticker, price, date_vto) %>%  
  filter(date_vto <= as.Date('2026-08-15')) %>%
  mutate(cer_date = add.bizdays(date_vto, -10,cal)) %>% 
  left_join(cer_nuevo, by = c('cer_date' = 'date')) %>% 
  left_join(instrumentos %>% select(ticker, issue_date)) %>% 
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


