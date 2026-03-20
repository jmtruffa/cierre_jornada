# Curvas reales CER vía Nelson-Siegel (datos desde tabla boncer_dinamica)

boncer_dinamica <- tryCatch(
  dbExecuteQuery(
    query = paste0(
      "SELECT * FROM boncer_dinamica WHERE date >= '", from_dinamica, "' ORDER BY date, ticker"
    ),
    server = server,
    port = port
  ),
  error = function(e) NULL
)

if (is.null(boncer_dinamica) || nrow(boncer_dinamica) == 0L) {
  functions::log_msg(
    "nelson_siegel.r: sin datos en tabla boncer_dinamica — se omite.",
    "WARN",
    log_file = file.path(path, "cierre.log")
  )
} else {
  has_vol <- "volume" %in% names(boncer_dinamica)

    # =========================
    # 1) Preparación de datos
    # =========================
    boncer_fit <- boncer_dinamica %>%
      mutate(
        date_vto = as.Date(date_vto),
        tau = as.numeric(date_vto - date) / 365.25
      ) %>%
      filter(!is.na(yield), !is.na(tau)) %>%
      filter(tau > 30 / 365.25) %>%
      filter(yield > -0.20, yield < 1.00) %>%
      mutate(
        w_vol = if (has_vol) pmax(volume, 0) else 1,
        w = sqrt(w_vol)
      ) %>%
      select(date, ticker, tau, yield, w)

    if (nrow(boncer_fit) < 5L) {
      functions::log_msg(
        "nelson_siegel.r: boncer_fit con menos de 5 filas — se omite.",
        "WARN",
        log_file = file.path(path, "cierre.log")
      )
    } else {
      # =========================
      # 2) Funciones Nelson-Siegel
      # =========================
      ns_loadings <- function(tau, lambda) {
        x <- tau / lambda
        L1 <- (1 - exp(-x)) / x
        L2 <- L1 - exp(-x)
        cbind(L1 = L1, L2 = L2)
      }

      ns_yield <- function(tau, b0, b1, b2, lambda) {
        L <- ns_loadings(tau, lambda)
        b0 + b1 * L[, "L1"] + b2 * L[, "L2"]
      }

      fit_ns_one_day <- function(df_day, lambda, min_n = 5) {
        df_day <- df_day %>% filter(is.finite(tau), is.finite(yield), is.finite(w), w > 0)
        if (nrow(df_day) < min_n) {
          return(NULL)
        }

        tau <- df_day$tau
        y <- df_day$yield
        w <- df_day$w

        L <- ns_loadings(tau, lambda)
        X <- cbind(1, L)
        W <- diag(w, nrow = length(w))

        XtWX <- t(X) %*% W %*% X
        if (rcond(XtWX) < 1e-12) {
          return(NULL)
        }

        beta <- solve(XtWX, t(X) %*% W %*% y)
        yhat <- as.numeric(X %*% beta)

        rmse <- sqrt(weighted.mean((y - yhat)^2, w))

        tibble(
          beta0 = beta[1],
          beta1 = beta[2],
          beta2 = beta[3],
          lambda = lambda,
          rmse = rmse,
          n = nrow(df_day)
        )
      }

      # ==========================================
      # 3) Grid search de lambda + lambda_best
      # ==========================================
      lambdas <- seq(0.2, 2, by = 0.1)

      lambda_grid <- boncer_fit %>%
        group_by(date) %>%
        nest() %>%
        mutate(
          stats = map(data, function(d) {
            tibble(lambda_try = lambdas) %>%
              mutate(fit = map(lambda_try, ~ fit_ns_one_day(d, lambda = .x, min_n = 5))) %>%
              unnest(fit, keep_empty = TRUE) %>%
              select(-lambda) %>%
              rename(lambda = lambda_try)
          })
        ) %>%
        select(date, stats) %>%
        unnest(stats) %>%
        filter(!is.na(rmse))

      if (nrow(lambda_grid) == 0L) {
        functions::log_msg(
          "nelson_siegel.r: lambda_grid vacío — se omite.",
          "WARN",
          log_file = file.path(path, "cierre.log")
        )
      } else {
        lambda_best <- lambda_grid %>%
          group_by(lambda) %>%
          summarise(rmse_avg = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
          arrange(rmse_avg) %>%
          slice(1) %>%
          pull(lambda)

        functions::log_msg(
          paste0("nelson_siegel.r: lambda_best = ", lambda_best),
          "INFO",
          log_file = file.path(path, "cierre.log")
        )

        # ==========================================
        # 4) Gráficos de diagnóstico de lambda
        # ==========================================
        rmse_by_lambda <- lambda_grid %>%
          group_by(lambda) %>%
          summarise(
            rmse_avg = mean(rmse, na.rm = TRUE),
            rmse_med = median(rmse, na.rm = TRUE),
            .groups = "drop"
          )

        g_ns_rmse_lambda <- ggplot(rmse_by_lambda, aes(x = lambda, y = rmse_avg)) +
          theme_usado() +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = breaks_extended(10)) +
          scale_y_continuous(breaks = breaks_extended(10)) +
          labs(
            title = "Nelson-Siegel BONCER: RMSE promedio por lambda",
            subtitle = paste0("Último dato: ", max(boncer_dinamica$date, na.rm = TRUE)),
            x = "lambda (años)",
            y = "RMSE (yield real)",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        rmse_time <- lambda_grid %>%
          filter(lambda == lambda_best) %>%
          select(date, rmse, n)

        g_ns_rmse_time <- ggplot(rmse_time, aes(x = date, y = rmse)) +
          theme_usado() +
          geom_line() +
          scale_x_date(
            date_breaks = "2 month",
            labels = date_format("%d-%b-%Y", locale = "es"),
            expand = c(0.02, 0.02)
          ) +
          scale_y_continuous(breaks = breaks_extended(10)) +
          labs(
            title = paste0("Nelson-Siegel BONCER: RMSE diario (lambda_best = ", lambda_best, ")"),
            subtitle = paste0("Último dato: ", max(boncer_dinamica$date, na.rm = TRUE)),
            x = NULL,
            y = "RMSE (yield real)",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        grabaGrafo2(variable = g_ns_rmse_lambda, name = "g_ns_boncer_rmse_lambda", path = path)
        grabaGrafo2(variable = g_ns_rmse_time, name = "g_ns_boncer_rmse_time", path = path)

        # ==========================================
        # 5) Estimación final con lambda_best
        # ==========================================
        ns_params_best <- boncer_fit %>%
          group_by(date) %>%
          nest() %>%
          mutate(fit = map(data, ~ fit_ns_one_day(.x, lambda = lambda_best, min_n = 5))) %>%
          select(date, fit) %>%
          unnest(fit, keep_empty = FALSE) %>%
          ungroup()

        stopifnot(length(unique(ns_params_best$lambda)) == 1)
        stopifnot(abs(unique(ns_params_best$lambda) - lambda_best) < 1e-12)

        # ==========================================
        # 6) Curva por tenores + gráficos principales
        # ==========================================
        tenors <- c(0.5, 1, 2, 3, 5, 7, 10)

        ns_curve_best <- ns_params_best %>%
          tidyr::crossing(tau = tenors) %>%
          mutate(y_ns = ns_yield(tau, beta0, beta1, beta2, lambda)) %>%
          select(date, tau, y_ns)

        last_day <- max(ns_curve_best$date)

        curve_last <- ns_curve_best %>%
          filter(date == last_day)

        g_ns_curve_last <- ggplot(curve_last, aes(x = tau, y = y_ns)) +
          theme_usado() +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = breaks_extended(10)) +
          scale_y_continuous(breaks = breaks_extended(14), labels = scales::percent) +
          labs(
            title = "Curva real CER (Nelson-Siegel) — último día",
            subtitle = paste0("Fecha: ", last_day, " | lambda_best = ", lambda_best),
            x = "Plazo (años)",
            y = "Tasa real",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        g_ns_curve_hist <- ggplot(
          ns_curve_best %>% filter(tau %in% c(1, 2, 5)),
          aes(x = date, y = y_ns, color = factor(tau))
        ) +
          theme_usado() +
          geom_line() +
          scale_x_date(
            date_breaks = "2 month",
            labels = date_format("%d-%b-%Y", locale = "es"),
            expand = c(0.02, 0.02)
          ) +
          scale_y_continuous(
            labels = scales::percent,
            breaks = seq(0.06, 0.25, by = 0.01)
          ) +
          coord_cartesian(ylim = c(0.06, 0.25)) +
          scale_color_manual(name = "Tenor (años)", values = .paleta) +
          labs(
            title = "Tasa real CER estimada por tenor (Nelson-Siegel)",
            subtitle = paste0("lambda_best = ", lambda_best, " | Último dato: ", last_day),
            x = NULL,
            y = "Tasa real",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        grabaGrafo2(variable = g_ns_curve_last, name = "g_ns_boncer_curve_last", path = path)
        grabaGrafo2(variable = g_ns_curve_hist, name = "g_ns_boncer_curve_hist", path = path)

        # ==========================================
        # 7) Factores (beta0, beta1, beta2)
        # ==========================================
        factors <- ns_params_best %>%
          select(date, beta0, beta1, beta2)

        g_ns_beta0 <- ggplot(factors, aes(x = date, y = beta0)) +
          theme_usado() +
          geom_line() +
          scale_x_date(
            date_breaks = "2 month",
            labels = date_format("%d-%b-%Y", locale = "es"),
            expand = c(0.02, 0.02)
          ) +
          scale_y_continuous(labels = scales::percent, breaks = breaks_extended(12)) +
          labs(
            title = "Nelson-Siegel BONCER: beta0 (nivel asintótico)",
            subtitle = paste0("Último dato: ", max(factors$date, na.rm = TRUE)),
            x = NULL,
            y = "beta0",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        g_ns_beta1 <- ggplot(factors, aes(x = date, y = beta1)) +
          theme_usado() +
          geom_line() +
          scale_x_date(
            date_breaks = "2 month",
            labels = date_format("%d-%b-%Y", locale = "es"),
            expand = c(0.02, 0.02)
          ) +
          scale_y_continuous(labels = scales::percent, breaks = breaks_extended(12)) +
          labs(
            title = "Nelson-Siegel BONCER: beta1 (pendiente)",
            subtitle = paste0("Último dato: ", max(factors$date, na.rm = TRUE)),
            x = NULL,
            y = "beta1",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        g_ns_beta2 <- ggplot(factors, aes(x = date, y = beta2)) +
          theme_usado() +
          geom_line() +
          scale_x_date(
            date_breaks = "2 month",
            labels = date_format("%d-%b-%Y", locale = "es"),
            expand = c(0.02, 0.02)
          ) +
          scale_y_continuous(labels = scales::percent, breaks = breaks_extended(12)) +
          labs(
            title = "Nelson-Siegel BONCER: beta2 (curvatura)",
            subtitle = paste0("Último dato: ", max(factors$date, na.rm = TRUE)),
            x = NULL,
            y = "beta2",
            caption = paste0(.pie, " en base a precios de mercado.")
          )

        grabaGrafo2(variable = g_ns_beta0, name = "g_ns_boncer_beta0", path = path)
        grabaGrafo2(variable = g_ns_beta1, name = "g_ns_boncer_beta1", path = path)
        grabaGrafo2(variable = g_ns_beta2, name = "g_ns_boncer_beta2", path = path)
      }
    }
}
