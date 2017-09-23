
require(dplyr)

# SERVER

shiny::shinyServer(function(input, output) {

  seed_r <- shiny::reactive({
    x <- input$ss
    return(x)
  })

  # ----------------------------------------------------------------------------
  API_mean_r <- shiny::reactive({
    x <- input$API_mean
    return(x)
  })
  API_sd_r <- shiny::reactive({
    x <- input$API_sd
    return(x)
  })
  API_distro_r <- shiny::reactive({
    x <- input$API_distro
    return(x)
  })
  API_lower_r <- shiny::reactive({
    x <- input$API_lower
    return(x)
  })
  API_upper_r <- shiny::reactive({
    x <- input$API_upper
    return(x)
  })

  GOR_mean_r <- shiny::reactive({
    x <- input$GOR_mean
    return(x)
  })
  GOR_sd_r <- shiny::reactive({
    x <- input$GOR_sd
    return(x)
  })
  GOR_distro_r <- shiny::reactive({
    x <- input$GOR_distro
    return(x)
  })
  GOR_lower_r <- shiny::reactive({
    x <- input$GOR_lower
    return(x)
  })
  GOR_upper_r <- shiny::reactive({
    x <- input$GOR_upper
    return(x)
  })

  DEPT_err_mean_r <- shiny::reactive({
    x <- input$DEPT_err_mean
    return(x)
  })
  DEPT_err_sd_r <- shiny::reactive({
    x <- input$DEPT_err_sd
    return(x)
  })
  DEPT_err_distro_r <- shiny::reactive({
    x <- input$DEPT_err_distro
    return(x)
  })
  DEPT_err_lower_r <- shiny::reactive({
    x <- input$DEPT_err_lower
    return(x)
  })
  DEPT_err_upper_r <- shiny::reactive({
    x <- input$DEPT_err_upper
    return(x)
  })

  grad_P_mean_r <- shiny::reactive({
    x <- input$grad_P_mean
    return(x)
  })
  grad_P_sd_r <- shiny::reactive({
    x <- input$grad_P_sd
    return(x)
  })
  grad_P_distro_r <- shiny::reactive({
    x <- input$grad_P_distro
    return(x)
  })
  grad_P_lower_r <- shiny::reactive({
    x <- input$grad_P_lower
    return(x)
  })
  grad_P_upper_r <- shiny::reactive({
    x <- input$grad_P_upper
    return(x)
  })

  grad_T_mean_r <- shiny::reactive({
    x <- input$grad_T_mean
    return(x)
  })
  grad_T_sd_r <- shiny::reactive({
    x <- input$grad_T_sd
    return(x)
  })
  grad_T_distro_r <- shiny::reactive({
    x <- input$grad_T_distro
    return(x)
  })
  grad_T_lower_r <- shiny::reactive({
    x <- input$grad_T_lower
    return(x)
  })
  grad_T_upper_r <- shiny::reactive({
    x <- input$grad_T_upper
    return(x)
  })

  depth_r <- shiny::reactive({
    x <- input$depth
    return(x)
  })
  waterdepth_r <- shiny::reactive({
    x <- input$wd
    return(x)
  })

  # ----------------------------------------------------------------------------
  # Latin Hypercube Sampling
  LHS_r <- shiny::reactive({
    n <- input$nn
    rr <- input$rr # API vs GOR correlation
    nvars <- 5

    sigma <- matrix(rep(0, nvars*nvars), nrow = nvars, ncol = nvars) # uncorrelated
    sigma[1,] <- c(1.00,   rr, 0.00, 0.00, 0.00)
    sigma[2,] <- c(  rr, 1.00, 0.00, 0.00, 0.00)
    sigma[3,] <- c(0.00, 0.00, 1.00, 0.00, 0.00)
    sigma[4,] <- c(0.00, 0.00, 0.00, 1.00, 0.00)
    sigma[5,] <- c(0.00, 0.00, 0.00, 0.00, 1.00)

    corrLHS <- pse::LHS(factors = nvars, N = n, method = "HL", opts = list(COR = sigma, eps = input$lhs_accuracy))
    XX <- pse::get.data(corrLHS)
    # err <- sum(abs(cor(XX) - sigma)) / (2 * nvars)

    return(XX)
  })

  sampling_r <- shiny::reactive({
    x <- ifelse(input$sampling_method == 0, "LHS", "RND")
    return(x)
  })

  API_r <- shiny::reactive({
    n <- input$nn
    seed <- seed_r()

    X_mean <- API_mean_r()
    X_sd <- API_sd_r()
    X_distro <- API_distro_r()
    X_lower <- API_lower_r()
    X_upper <- API_upper_r()

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 1],
                                  distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # API [deg]
  GOR_r <- shiny::reactive({
    n <- input$nn
    seed <- seed_r()

    X_mean <- GOR_mean_r()
    X_sd <- GOR_sd_r()
    X_distro <- GOR_distro_r()
    X_lower <- GOR_lower_r()
    X_upper <- GOR_upper_r()
    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 2],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # GOR [scf/stb]
  DEPT_err_r <- shiny::reactive({
    n <- input$nn
    seed <- seed_r()

    X_mean <- DEPT_err_mean_r()
    X_sd <- DEPT_err_sd_r()
    X_distro <- DEPT_err_distro_r()
    X_lower <- DEPT_err_lower_r()
    X_upper <- DEPT_err_upper_r()

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 3],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # DEPTH UNCERTAINTY [ft]
  grad_P_r <- shiny::reactive({
    n <- input$nn
    seed <- seed_r()

    X_mean <- grad_P_mean_r()
    X_sd <- grad_P_sd_r()
    X_distro <- grad_P_distro_r()
    X_lower <- grad_P_lower_r()
    X_upper <- grad_P_upper_r()

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 4],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # Pressure Gradient [psi/ft]
  grad_T_r <- shiny::reactive({
    n <- input$nn
    seed <- seed_r()

    X_mean <- grad_T_mean_r()
    X_sd <- grad_T_sd_r()
    X_distro <- grad_T_distro_r()
    X_lower <- grad_T_lower_r()
    X_upper <- grad_T_upper_r()

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 5],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)
    return(X)
  }) # Temperature Gradient [F/100 ft]

  DATA_r <- shiny::reactive({
    n <- input$nn
    CASE <- rep("CASE", n)
    for (i in 1:n) {
      CASE[i] <- ifelse(i < 10, paste0("CASE_00", i), ifelse(i < 100, paste0("CASE_0", i), paste0("CASE_", i)))
    }

    x <- dplyr::data_frame(CASE = CASE) %>%
      dplyr::mutate(API = API_r(),
                    GOR = GOR_r(),
                    DEPTH = depth_r() + DEPT_err_r(),
                    PRESS = DEPTH * grad_P_r(),
                    TEMP = ifelse(input$offshore == TRUE,
                                  4 * 1.8 + 32 + (DEPTH - waterdepth_r()) / 100 * grad_T_r(),
                                  80 + DEPTH / 100 * grad_T_r()),
                    GRAD_P = grad_P_r(),
                    GRAD_T = grad_T_r())

    # add MEAN case
   xm <- dplyr::data_frame(CASE = "CASE_MEAN", API = mean(x$API), GOR = mean(x$GOR),
                           DEPTH = mean(x$DEPTH), PRESS = mean(x$PRESS), TEMP = mean(x$TEMP),
                           GRAD_P = mean(x$GRAD_P), GRAD_T = mean(x$GRAD_T) )

    colnames(xm) <- colnames(x)
    x <- rbind(x, xm)

    return(x)
  })

  # read excel data
  # results_r <- shiny::reactive({
  #   excelfile <- system.file("extdata", "Flamingo.xls", package = "Flamingo")
  #   read_excel(excelfile, sheet = "X")
  # })

  # ----------------------------------------------------------------------------
  output$hist_apigor <- shiny::renderPlot({
    data <- DATA_r()

    plot_api <- ggplot2::ggplot(data, ggplot2::aes(x = API)) +
      ggplot2::geom_histogram(ggplot2::aes(x = API, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
      ggplot2::stat_ecdf(color = "darkred") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(API)), color = "green", linetype = 2) +
      ggplot2::theme_bw() +
      ggplot2::xlab("API Gravity") +
      ggplot2::ggtitle("API")

    plot_gor <- ggplot2::ggplot(data, ggplot2::aes(x = GOR)) +
      ggplot2::geom_histogram(ggplot2::aes(x = GOR, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
      ggplot2::stat_ecdf(color = "darkred") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(GOR)), color = "green", linetype = 2) +
      ggplot2::theme_bw() +
      ggplot2::xlab("GOR [scf/stb]") +
      ggplot2::ggtitle("GOR [scf/stb]")

    gridExtra::grid.arrange(plot_api, plot_gor, ncol = 2)
  })
  output$hist_presstemp <- shiny::renderPlot({
    data <- DATA_r()

    plot_api <- ggplot2::ggplot(data, ggplot2::aes(x = PRESS)) +
      ggplot2::geom_histogram(ggplot2::aes(x = PRESS, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
      ggplot2::stat_ecdf(color = "darkred") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(PRESS)), color = "green", linetype = 2) +
      ggplot2::theme_bw() +
      ggplot2::xlab("psia") +
      ggplot2::ggtitle("Reservoir Pressure")

    plot_gor <- ggplot2::ggplot(data, ggplot2::aes(x = TEMP)) +
      ggplot2::geom_histogram(ggplot2::aes(x = TEMP, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
      ggplot2::stat_ecdf(color = "darkred") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(TEMP)), color = "green", linetype = 2) +
      ggplot2::theme_bw() +
      ggplot2::xlab("F") +
      ggplot2::ggtitle("Reservoir Temperature")

    gridExtra::grid.arrange(plot_api, plot_gor, ncol = 2)
  })
  output$stats_input <- shiny::renderTable({
    temp <- DATA_r()
    x <- do.call(cbind, lapply(temp[2:ncol(temp)], petroreadr::summary_mod))

    xx <- as.data.frame(x) %>%
      dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev")) %>%
      dplyr::select(Statistic, API, GOR, DEPTH, PRESS, TEMP, GRAD_P, GRAD_T)

    return(xx)
  })
  output$pairs_input <- shiny::renderPlot({
    data <- DATA_r()
    GGally::ggpairs(data, columns = 2:ncol(data)) +
      ggplot2::theme_bw()
  })
  output$depth_plot <- shiny::renderPlot({
    data <- DATA_r()

    pres <- ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x = PRESS, y = DEPTH), alpha = 0.7) +
      ggplot2::geom_abline(ggplot2::aes(intercept = 14.7 / mean(GRAD_P), slope = -1 / mean(GRAD_P)), color = "darkblue") +
      ggplot2::geom_abline(ggplot2::aes(intercept = 14.7 / quantile(GRAD_P, 0.9), slope = -1 / quantile(GRAD_P, 0.9)),
                           color = "darkblue", linetype = 3) +
      ggplot2::geom_abline(ggplot2::aes(intercept = 14.7 / quantile(GRAD_P, 0.1), slope = -1 / quantile(GRAD_P, 0.1)),
                           color = "darkblue", linetype = 3) +
      ggplot2::scale_y_reverse(limits = c(NA , 0)) +
      ggplot2::xlim(0, NA) +
      ggplot2::ggtitle("Pressure") +
      ggplot2::xlab("psia") +
      ggplot2::ylab("ft.TVDss") +
      ggplot2::theme_bw()

    temp <- ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x = TEMP, y = DEPTH), color = "red", alpha = 0.7) +
      ggplot2::scale_y_reverse(limits = c(NA, 0)) +
      ggplot2::xlim(0, NA) +
      ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = -1 / mean(GRAD_T) * 100 * 0.3048), color = "darkred") +
      ggplot2::ggtitle("Temperature") +
      ggplot2::xlab("F") +
      ggplot2::ylab("ft.TVDss") +
      ggplot2::theme_bw()

    gridExtra::grid.arrange(pres, temp, ncol = 2)
  })

  output$data_input <- DT::renderDataTable({
    data <- DATA_r() %>%
      dplyr::mutate(API = round(API, digits = 1),
                    GOR = round(GOR, digits = 0),
                    DEPTH = round(DEPTH, digits = 0),
                    PRESS = round(PRESS, digits = 0),
                    TEMP = round(TEMP, digits = 0),
                    GRAD_P = round(GRAD_P, digits = 3),
                    GRAD_T = round(GRAD_T, digits = 2))

    DT::datatable(data, options = list(pageLength = 25)) %>%
      DT::formatStyle("API", fontWeight = 'bold',
                      background = DT::styleColorBar(data$API,'#888888'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center') %>%
      DT::formatStyle("GOR", fontWeight = 'bold',
                      background = DT::styleColorBar(data$GOR,'#cc5555'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
  })


})


