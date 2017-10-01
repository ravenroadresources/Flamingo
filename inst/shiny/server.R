
require(dplyr)
require(xlsx)
# require(rJava)

# SERVER

shiny::shinyServer(function(input, output) {

  package_path <- system.file(package = "Flamingo")
  dp_user_path <- file.path(package_path, "shiny", "www", "default", "distro_params_user.txt")
  dp_def_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default.txt")

  observe({
    if (input$close > 0) shiny::stopApp()  # stop shiny
  }) # stop shinyapp
  observe({
    if (input$validate > 0) {
      input_data <- as.data.frame(DATA_r())
      xls_path <- file.path(package_path, "extdata", "Flamingo.xls")

      wb <- xlsx::loadWorkbook(xls_path)
      sheets <- xlsx::getSheets(wb)

      cs1 <- xlsx::CellStyle(wb) + xlsx::Font(wb, isItalic=TRUE) # rowcolumns

      xlsx::addDataFrame(input_data, sheet = sheets$X,
                   startRow = 1 , startColumn = 1,
                   col.names = TRUE, row.names = FALSE,
                   rownamesStyle = cs1)
      xlsx::saveWorkbook(wb, xls_path)
    }
  }) # validate data (write them nto excel)
  observe({
    if (input$open_prosper > 0) {
      # package_path <- system.file(package = "Flamingo")
      prosper_path <- file.path(package_path, "extdata", "Flamingo.Out")
      shell.exec(prosper_path)
      path_to_vbs_file <- "../extdata/Flamingo.vbs"
      shell(shQuote(normalizePath(path_to_vbs_file)), "cscript", flag = "//nologo")
    }
  }) # open prosper
  observe({
    if (input$run_mc > 0) {
      # package_path <- system.file(package = "Flamingo")
      vbs_path <- file.path(package_path, "extdata", "Flamingo.vbs")
      shell(shQuote(normalizePath(vbs_path)), "cscript", flag = "//nologo")
    }
  }) # run simulation
  observe({
    if (input$pt_method == 0) aa <- TRUE
    else aa <- FALSE
    shinyjs::toggle(condition = aa, selector = "#navbar li a[data-value=PTDEPTH]")
  }) # hide tab
  observe({
    if (input$dp_set_def > 0) {
      file.create(dp_user_path)
      write.table(values$distroparam, file = dp_user_path)
    }
  }) # set new default distro params
  observe({
    if (input$dp_reset > 0) {
      file.remove(dp_user_path) # delete file
      distroparam <- read.table(dp_def_path, header = TRUE)
    }
  }) # reset default distro params

  seed_r <- shiny::reactive({
    x <- input$ss
    return(x)
  })
  n_r <- shiny::reactive({
    x <- input$nn
    return(x)
  })
  r_r <- shiny::reactive({
    x <- input$rr
    return(x)
  })

  # ----------------------------------------------------------------------------
  # API_mean_r <- shiny::reactive({
  #   x <- input$API_mean
  #   return(x)
  # })
  # API_sd_r <- shiny::reactive({
  #   x <- input$API_sd
  #   return(x)
  # })
  # API_distro_r <- shiny::reactive({
  #   x <- input$API_distro
  #   return(x)
  # })
  # API_lower_r <- shiny::reactive({
  #   x <- input$API_lower
  #   return(x)
  # })
  # API_upper_r <- shiny::reactive({
  #   x <- input$API_upper
  #   return(x)
  # })

  # GOR_mean_r <- shiny::reactive({
  #   x <- input$GOR_mean
  #   return(x)
  # })
  # GOR_sd_r <- shiny::reactive({
  #   x <- input$GOR_sd
  #   return(x)
  # })
  # GOR_distro_r <- shiny::reactive({
  #   x <- input$GOR_distro
  #   return(x)
  # })
  # GOR_lower_r <- shiny::reactive({
  #   x <- input$GOR_lower
  #   return(x)
  # })
  # GOR_upper_r <- shiny::reactive({
  #   x <- input$GOR_upper
  #   return(x)
  # })
  #
  # DEPT_err_mean_r <- shiny::reactive({
  #   x <- input$DEPT_err_mean
  #   return(x)
  # })
  # DEPT_err_sd_r <- shiny::reactive({
  #   x <- input$DEPT_err_sd
  #   return(x)
  # })
  # DEPT_err_distro_r <- shiny::reactive({
  #   x <- input$DEPT_err_distro
  #   return(x)
  # })
  # DEPT_err_lower_r <- shiny::reactive({
  #   x <- input$DEPT_err_lower
  #   return(x)
  # })
  # DEPT_err_upper_r <- shiny::reactive({
  #   x <- input$DEPT_err_upper
  #   return(x)
  # })
  #
  # grad_P_mean_r <- shiny::reactive({
  #   x <- input$grad_P_mean
  #   return(x)
  # })
  # grad_P_sd_r <- shiny::reactive({
  #   x <- input$grad_P_sd
  #   return(x)
  # })
  # grad_P_distro_r <- shiny::reactive({
  #   x <- input$grad_P_distro
  #   return(x)
  # })
  # grad_P_lower_r <- shiny::reactive({
  #   x <- input$grad_P_lower
  #   return(x)
  # })
  # grad_P_upper_r <- shiny::reactive({
  #   x <- input$grad_P_upper
  #   return(x)
  # })
  #
  # grad_T_mean_r <- shiny::reactive({
  #   x <- input$grad_T_mean
  #   return(x)
  # })
  # grad_T_sd_r <- shiny::reactive({
  #   x <- input$grad_T_sd
  #   return(x)
  # })
  # grad_T_distro_r <- shiny::reactive({
  #   x <- input$grad_T_distro
  #   return(x)
  # })
  # grad_T_lower_r <- shiny::reactive({
  #   x <- input$grad_T_lower
  #   return(x)
  # })
  # grad_T_upper_r <- shiny::reactive({
  #   x <- input$grad_T_upper
  #   return(x)
  # })
  #
  # water_mean_r <- shiny::reactive({
  #   x <- input$water_mean
  #   return(x)
  # })
  # water_sd_r <- shiny::reactive({
  #   x <- input$water_sd
  #   return(x)
  # })
  # water_distro_r <- shiny::reactive({
  #   x <- input$water_distro
  #   return(x)
  # })
  # water_lower_r <- shiny::reactive({
  #   x <- input$water_lower
  #   return(x)
  # })
  # water_upper_r <- shiny::reactive({
  #   x <- input$water_upper
  #   return(x)
  # })
  #
  # press_mean_r <- shiny::reactive({
  #   x <- input$press_mean
  #   return(x)
  # })
  # press_sd_r <- shiny::reactive({
  #   x <- input$press_sd
  #   return(x)
  # })
  # press_distro_r <- shiny::reactive({
  #   x <- input$press_distro
  #   return(x)
  # })
  # press_lower_r <- shiny::reactive({
  #   x <- input$press_lower
  #   return(x)
  # })
  # press_upper_r <- shiny::reactive({
  #   x <- input$press_upper
  #   return(x)
  # })
  #
  # temp_mean_r <- shiny::reactive({
  #   x <- input$temp_mean
  #   return(x)
  # })
  # temp_sd_r <- shiny::reactive({
  #   x <- input$temp_sd
  #   return(x)
  # })
  # temp_distro_r <- shiny::reactive({
  #   x <- input$temp_distro
  #   return(x)
  # })
  # temp_lower_r <- shiny::reactive({
  #   x <- input$temp_lower
  #   return(x)
  # })
  # temp_upper_r <- shiny::reactive({
  #   x <- input$temp_upper
  #   return(x)
  # })

  # ----------------------------------------------------------------------------
  depth_r <- shiny::reactive({
    x <- input$depth
    return(x)
  })

  # distroparam <- dplyr::data_frame(variable = c("API", "GOR", "Depth Uncertainty", "Pressure gradient",
  #                                         "Temperature gradient", "Pressure", "Temperature", "Water salinity"),
  #                            distro = factor(rep("normal", 8),
  #                                            levels = c("normal", "truncated normal", "lognormal",
  #                                                       "truncated lognormal", "uniform", "triangular", "constant value"),
  #                                            ordered = TRUE),
  #                            mean = c(25, 300, rep(1, 6)),
  #                            sd = c(5, 100, rep(1, 6)),
  #                            lower = c(20, 100, rep(1, 6)),
  #                            higher = c(40, 800, rep(1, 6)))


  # variables distributions parameters
  if (file.exists(dp_user_path)) {
    distroparam <- read.table(dp_user_path, header = TRUE)
  }
  else if (file.exists(dp_def_path)) {
    distroparam <- read.table(dp_def_path, header = TRUE)
  }
  else {
    distroparam <- matrix(seq(NA, 48), nrow = 8)
    colnames(distroparam) <- c("API", "GOR", "Depth Uncertainty", "Pressure gradient",
                               "Temperature gradient", "Pressure", "Temperature", "Water salinity")
  }

  values <- reactiveValues()

  ## Handsontable
  observe({
    # if(input$pt_mehtod == 0) {
    #   distroparam <- distroparam %>%
    #     dplyr::filter(variable != "Pressure",
    #                   variable != "Temperature")
    #   }
    # else {
    #   distroparam <- distroparam %>%
    #     dplyr::filter(variable != "Depth Uncertainty",
    #                   variable != "Pressure gradient",
    #                   variable != "Temperature gradient")
    # }

    if (!is.null(input$distro_params_table)) {
      values$previous <- isolate(values$distroparam)
      distroparam = hot_to_r(input$distro_params_table)
    } else {
      if (is.null(values$distroparam))
        distroparam <- distroparam
      else
        distroparam <- values$distroparam
    }
    values$distroparam <- distroparam
  })

  output$distro_params_table <- renderRHandsontable({
    # distroparam_temp <- distroparam_r()
    # distroparam <- values$distroparam_r()
    if (!is.null(distroparam))
      rhandsontable(distroparam, useTypes = TRUE, stretchH = "all") %>%
        hot_col("variable", readOnly = TRUE) %>%
        hot_col(col = "distro", type = "dropdown", source = c("normal", "truncated normal", "lognormal",
                                                              "truncated lognormal", "uniform", "triangular", "fixed value"))
  })

  # ----------------------------------------------------------------------------
  # Latin Hypercube Sampling
  LHS_r <- shiny::reactive({
    n <- n_r()
    rr <- r_r() # API vs GOR correlation
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
    n <- n_r()
    seed <- seed_r()

    # X_mean <- API_mean_r()
    # X_sd <- API_sd_r()
    # X_distro <- API_distro_r()
    # X_lower <- API_lower_r()
    # X_upper <- API_upper_r()

    X_distro <- c("normal", "truncated normal", "lognormal",
                 "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[1, 2]]
    X_mean <- values$distroparam[1, 3]
    X_sd <- values$distroparam[1, 4]
    X_lower <- values$distroparam[1, 5]
    X_upper <- values$distroparam[1, 6]


    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 1],
                                  distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # API [deg]
  GOR_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- GOR_mean_r()
    # X_sd <- GOR_sd_r()
    # X_distro <- GOR_distro_r()
    # X_lower <- GOR_lower_r()
    # X_upper <- GOR_upper_r()

    var_id <- 2
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 2],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # GOR [scf/stb]
  DEPT_err_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- DEPT_err_mean_r()
    # X_sd <- DEPT_err_sd_r()
    # X_distro <- DEPT_err_distro_r()
    # X_lower <- DEPT_err_lower_r()
    # X_upper <- DEPT_err_upper_r()

    var_id <- 3
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 3],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # DEPTH UNCERTAINTY [ft]
  grad_P_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- grad_P_mean_r()
    # X_sd <- grad_P_sd_r()
    # X_distro <- grad_P_distro_r()
    # X_lower <- grad_P_lower_r()
    # X_upper <- grad_P_upper_r()

    var_id <- 4
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 4],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # Pressure Gradient [psi/ft]
  grad_T_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- grad_T_mean_r()
    # X_sd <- grad_T_sd_r()
    # X_distro <- grad_T_distro_r()
    # X_lower <- grad_T_lower_r()
    # X_upper <- grad_T_upper_r()


    var_id <- 5
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 5],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)
    return(X)
  }) # Temperature Gradient [F/100 ft]
  water_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- water_mean_r()
    # X_sd <- water_sd_r()
    # X_distro <- water_distro_r()
    # X_lower <- water_lower_r()
    # X_upper <- water_upper_r()
    #
    var_id <- 8
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 1],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # Water Salinity [ppm]
  PRESS_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()

    # X_mean <- press_mean_r()
    # X_sd <- press_sd_r()
    # X_distro <- press_distro_r()
    # X_lower <- press_lower_r()
    # X_upper <- press_upper_r()
    var_id <- 6
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 1],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # Reservoir Pressure [psia]
  TEMP_r <- shiny::reactive({
    n <- n_r()
    seed <- seed_r()
#
#     X_mean <- temp_mean_r()
#     X_sd <- temp_sd_r()
#     X_distro <- temp_distro_r()
#     X_lower <- temp_lower_r()
#     X_upper <- temp_upper_r()

    var_id <- 7
    X_distro <- c("normal", "truncated normal", "lognormal",
                  "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
    X_mean <- values$distroparam[var_id, 3]
    X_sd <- values$distroparam[var_id, 4]
    X_lower <- values$distroparam[var_id, 5]
    X_upper <- values$distroparam[var_id, 6]

    X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 1],
                                distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

    return(X)
  }) # Reservoir Temperature [F]

  DATA_r <- shiny::reactive({
    n <- n_r()
    pt_method <- input$pt_method

    CASE <- rep("CASE", n)
    for (i in 1:n) {
      CASE[i] <- ifelse(i < 10, paste0("CASE_00", i), ifelse(i < 100, paste0("CASE_0", i), paste0("CASE_", i)))
    }
    CORR <- rep(as.numeric(input$fcorr_1), n)

    x <- dplyr::data_frame(CASE = CASE) %>%
      dplyr::mutate(CORR = CORR,
                    API = API_r(),
                    GOR = GOR_r(),
                    DEPTH = depth_r() + DEPT_err_r(),
                    PRESS = ifelse(pt_method == 0,
                                ifelse(input$offshore == TRUE,
                                    input$wd * 0.433 + DEPTH * grad_P_r(),
                                    (DEPTH + input$gle) * grad_P_r() ),
                                PRESS_r()),
                    TEMP = ifelse(pt_method == 0,
                                ifelse(input$offshore == TRUE,
                                    input$temp_bottomsea + (DEPTH - input$wd) / 100 * grad_T_r(),
                                    input$temp_surface + (DEPTH + input$gle) / 100 * grad_T_r()),
                                TEMP_r()),
                    GRAD_P = grad_P_r(),
                    GRAD_T = grad_T_r(),
                    WATER = water_r())

    # add MEAN case
   xm <- dplyr::data_frame(CASE = "CASE_MEAN", CORR = input$fcorr_1, API = mean(x$API), GOR = mean(x$GOR),
                           DEPTH = mean(x$DEPTH), PRESS = mean(x$PRESS), TEMP = mean(x$TEMP),
                           GRAD_P = mean(x$GRAD_P), GRAD_T = mean(x$GRAD_T), WATER = mean(x$WATER))

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
    x <- do.call(cbind, lapply(temp[3:ncol(temp)], petroreadr::summary_mod))

    if (input$pt_method == 0) {
      xx <- as.data.frame(x) %>%
        dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev")) %>%
        dplyr::select(Statistic, API, GOR, DEPTH, PRESS, TEMP, GRAD_P, GRAD_T, WATER)
    }
    else {
      xx <- as.data.frame(x) %>%
        dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev")) %>%
        dplyr::select(Statistic, API, GOR, PRESS, TEMP, WATER)
    }


    return(xx)
  })
  output$pairs_input <- shiny::renderPlot({
    data <- DATA_r()
    suppressWarnings(GGally::ggpairs(data, columns = input$corr_var)) +
      ggplot2::theme_bw()
  })
  output$depth_plot <- shiny::renderPlot({
    data <- DATA_r()

    p_intercept <- ifelse(input$offshore == TRUE,
                          (input$wd * 0.433 - 14.7) / mean(data$GRAD_P),
                          -input$gle - 14.7 / mean(data$GRAD_P))

    pres <- ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x = PRESS, y = DEPTH), alpha = 0.7) +
      ggplot2::geom_abline(ggplot2::aes(intercept = p_intercept, slope = -1 / mean(GRAD_P)), color = "darkblue") +
      ggplot2::geom_abline(ggplot2::aes(intercept = p_intercept, slope = -1 / quantile(GRAD_P, 0.9)),
                           color = "darkblue", linetype = 3) +
      ggplot2::geom_abline(ggplot2::aes(intercept = p_intercept, slope = -1 / quantile(GRAD_P, 0.1)),
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

  # ----------------------------------------------------------------------------
  output$selectize_corr_var <- renderUI({
    data <- DATA_r()
    selectizeInput('corr_var', 'Choose variables to plotted:',
                   choices = colnames(data[3:ncol(data)]),
                   multiple = TRUE,
                   selected = c("API", "GOR"))
  })

  output$lhs_acc_slider <- renderUI({
    if (input$sampling_method == 0) {
      shiny::sliderInput("lhs_accuracy", "LHS Accuracy level:", min = 0.01, max = 1, value = 0.05, step = 0.01)
    }
  })
  output$depth_msl <- renderUI({
    if (input$pt_method == 0) {
      if (input$offshore == TRUE) shiny::numericInput('wd', "Water Depth [ft]", 1000, step = 1, min = 0)
      else shiny::numericInput('gle', "Ground level Elevation [ft]", 500, step = 1, min = 0)
    }
  })
  output$depth_ref <- renderUI({
    if (input$pt_method == 0) {
      shiny::numericInput('depth', "Reference Depth [ft.TVDss]:", 8000, step = 10)
    }
  })
  output$depth_offshore <- renderUI({
    if (input$pt_method == 0) {
      shiny::checkboxInput("offshore", "Offshore:", FALSE)
    }
  })
  output$depth_temp <- renderUI({
    if (input$pt_method == 0) {
      if (input$offshore == TRUE) shiny::numericInput('temp_bottomsea', 'Bottomsea temperature [F]:', 32 + 4 * 1.8)
      else shiny::numericInput('temp_surface', 'Surface temperature [F]:', 60)
    }
  })
  output$button_check_1 <- renderText({
    x <- NULL
    if (input$open_prosper > 0) x <- "Data validated!"
    return(x)
  })


  output$debug01 <- renderText({
    # x <- values$distroparam[1,3]
    # x <- c("normal", "truncated normal", "lognormal",
    #               "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[1, 2]]
    x <- summary(API_r())
    return(x)
  })

})


