
observe({
  if (input$validate > 0) {
    input_data <- as.data.frame(DATA_r())
    if (length(input_data) > 0) {

      # version for .xlsX file
      # class(input_data$CORR) <- "Number"
      # # wb <- openxlsx::write.xlsx(input_data, file = xls_path, sheetName = "X")
      # wb <- openxlsx::loadWorkbook(xls_path)
      # openxlsx::writeDataTable(wb, sheet = "OIL", input_data)
      # openxlsx::saveWorkbook(wb, xls_path, overwrite = TRUE)
    #
    # version for .xls file
        wb <- xlsx::loadWorkbook(xls_path)
        sheets <- xlsx::getSheets(wb)

        cs1 <- xlsx::CellStyle(wb) + xlsx::Font(wb, isItalic = TRUE) # rowcolumns

        ifelse(input$fluid_type == 0,
          xlsx::addDataFrame(input_data, sheet = sheets$OIL,
                       startRow = 1 , startColumn = 1,
                       col.names = TRUE, row.names = FALSE,
                       rownamesStyle = cs1),
          xlsx::addDataFrame(input_data, sheet = sheets$GAS,
                             startRow = 1 , startColumn = 1,
                             col.names = TRUE, row.names = FALSE,
                             rownamesStyle = cs1))

        xlsx::saveWorkbook(wb, xls_path)
    }
  }
}) # validate data (write them nto excel)
observe({
  if (input$open_prosper > 0) shell.exec(prosper_path)
}) # open prosper
observe({
  if (input$run_mc > 0) shell(shQuote(normalizePath(vbs_path)), "cscript", flag = "//nologo")
}) # run simulation
observe({
  if (input$pt_method == 0) aa <- TRUE
  else aa <- FALSE
  shinyjs::toggle(condition = aa, selector = "#navbar li a[data-value=PTDEPTH]")
}) # hide tab Pressure and Temp vs Depth
observe({
  if (input$fluid_corr_method == 1) aa <- TRUE
  else aa <- FALSE
  shinyjs::toggle(condition = aa, selector = "#navbar li a[data-value=CORRSENS]")
}) # hide tab Pressure and Temp vs Depth
observe({
  if (input$points_method == 1) aa <- TRUE
  else aa <- FALSE
  shinyjs::toggle(condition = aa, selector = "#navbar li a[data-value=MULTIPOINT]")
}) # hide tab Pressure and Temp vs Depth
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

# import result data
results_r <- eventReactive(input$import_results, {

  ifelse(input$fluid_type == 0,
    x <- readxl::read_excel(xls_path, sheet = "OIL"),
    x <- readxl::read_excel(xls_path, sheet = "GAS"))

  xx <- x %>%
    dplyr::mutate(OILCOL = (PRES - PSAT) / DENO,
                  DD = PRES - PSAT)

  return(xx)
}) # import results

# ----------------------------------------------------------------------------
seed_r <- shiny::reactive({
  x <- input$ss
  return(x)
})
n_r <- shiny::reactive({
  x <- input$nn
  return(x)
})

depth_r <- shiny::reactive({
  x <- input$depth
  return(x)
})

# -------------------------------------
# variables distributions parameters

  if (file.exists(dp_user_path)) {
    distroparam <- read.table(dp_user_path, header = TRUE)
  } else if (file.exists(dp_def_path)) {
    distroparam <- read.table(dp_def_path, header = TRUE)
  } else {
    distroparam <- matrix(seq(NA, 48), nrow = 8)
    colnames(distroparam0) <- c("API", "GOR", "Depth Uncertainty", "Pressure gradient",
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

# table with distribution parameters
output$distro_params_table <- renderRHandsontable({
  if (!is.null(distroparam)) {
    rhandsontable(distroparam, useTypes = TRUE, stretchH = "all") %>%
    hot_col("variable", readOnly = TRUE) %>%
    hot_col(col = "distro", type = "dropdown", source = c("normal", "truncated normal", "lognormal",
                                                          "truncated lognormal", "uniform", "triangular", "fixed value"))
  }
})

# -------------------------------------
sigma_r <- shiny::reactive({

  rr <- input$rr # API vs GOR correlation
  nvars <- ifelse(input$pt_method == 0, 6, 5)

  sigma <- matrix(rep(0, nvars*nvars), nrow = nvars, ncol = nvars) # uncorrelated

  if(input$corr_method == 1) {
    return(sigma)
  } else {
    if(input$pt_method == 0) {
      sigma[1,] <- c(1.00,   rr, 0.00, 0.00, 0.00, 0.00)
      sigma[2,] <- c(  rr, 1.00, 0.00, 0.00, 0.00, 0.00)
      sigma[3,] <- c(0.00, 0.00, 1.00, 0.00, 0.00, 0.00)
      sigma[4,] <- c(0.00, 0.00, 0.00, 1.00, 0.00, 0.00)
      sigma[5,] <- c(0.00, 0.00, 0.00, 0.00, 1.00, 0.00)
      sigma[6,] <- c(0.00, 0.00, 0.00, 0.00, 0.00, 1.00)
    } else {
      sigma[1,] <- c(1.00,   rr, 0.00, 0.00, 0.00)
      sigma[2,] <- c(  rr, 1.00, 0.00, 0.00, 0.00)
      sigma[3,] <- c(0.00, 0.00, 1.00, 0.00, 0.00)
      sigma[4,] <- c(0.00, 0.00, 0.00, 1.00, 0.00)
      sigma[5,] <- c(0.00, 0.00, 0.00, 0.00, 1.00)
    }
    return(sigma)
  }

})


# table with sigma matrix
output$sigma_table <- shiny::renderTable({
  x <- sigma_r()
  xx <- as.data.frame(x)

  if(input$pt_method == 0) {
    colnames(xx) <- c("API", "GOR", "Depth", "Grad P", "Grad T", "Water Salinity")
    rownames(xx) <- c("API", "GOR", "Depth", "Grad P", "Grad T", "Water Salinity")
  } else {
    colnames(xx) <- c("API", "GOR", "Pressure", "Temperature", "Water Salinity")
    rownames(xx) <- c("API", "GOR", "Pressure", "Temperature", "Water Salinity")
  }
  return(xx)
}, colnames = TRUE)

# ----------------------------------------------------------------------------
# Latin Hypercube Sampling
LHS_r <- shiny::reactive({
  n <- n_r()
  nvars <- ifelse(input$pt_method == 0, 6, 5)

  sigma <- sigma_r()

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
  nvar <- ifelse(input$pt_method == 0, 6, 5)

  var_id <- 8
  X_distro <- c("normal", "truncated normal", "lognormal",
                "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
  X_mean <- values$distroparam[var_id, 3]
  X_sd <- values$distroparam[var_id, 4]
  X_lower <- values$distroparam[var_id, 5]
  X_upper <- values$distroparam[var_id, 6]

  X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , nvar],
                              distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

  return(X)
}) # Water Salinity [ppm]
PRESS_r <- shiny::reactive({
  n <- n_r()
  seed <- seed_r()

  var_id <- 6
  X_distro <- c("normal", "truncated normal", "lognormal",
                "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
  X_mean <- values$distroparam[var_id, 3]
  X_sd <- values$distroparam[var_id, 4]
  X_lower <- values$distroparam[var_id, 5]
  X_upper <- values$distroparam[var_id, 6]

  X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 3],
                              distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

  return(X)
}) # Reservoir Pressure [psia]
TEMP_r <- shiny::reactive({
  n <- n_r()
  seed <- seed_r()

  var_id <- 7
  X_distro <- c("normal", "truncated normal", "lognormal",
                "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[var_id, 2]]
  X_mean <- values$distroparam[var_id, 3]
  X_sd <- values$distroparam[var_id, 4]
  X_lower <- values$distroparam[var_id, 5]
  X_upper <- values$distroparam[var_id, 6]

  X <- Flamingo::run_variable(n, X_mean, X_sd, method = sampling_r() , lhs = LHS_r()[ , 4],
                              distro = X_distro, lower = X_lower, upper = X_upper, seed = seed)

  return(X)
}) # Reservoir Temperature [F]

DATA_r <- shiny::reactive({
  n <- n_r()
  grad_P <- grad_P_r()
  grad_T <- grad_T_r()


  CASE <- rep("CASE", n)
  for (i in 1:n) {
    CASE[i] <- ifelse(i < 10, paste0("CASE_00", i), ifelse(i < 100, paste0("CASE_0", i), paste0("CASE_", i)))
  }
  CORR <- rep(as.numeric(input$corr_oil_1), n)
  UCORR <- rep(as.numeric(input$corr_oil_2), n)

  x <- dplyr::data_frame(CASE = CASE) %>%
    dplyr::mutate(CORR = CORR,
                  UCORR = UCORR,
                  API = API_r(),
                  GOR = GOR_r(),
                  DEPTH = depth_r() + DEPT_err_r(),
                  PRESS = PRESS_r(),
                  TEMP = TEMP_r(),
                  GRAD_P = grad_P,
                  GRAD_T = grad_T,
                  PPM = water_r())

  if(input$pt_method == 0) {
    if(input$offshore == TRUE) {
      x$PRESS <- 14.7 + input$wd * 0.433 + (x$DEPTH - input$wd) * grad_P
      x$TEMP <- input$temp_bottomsea + (x$DEPTH - input$wd) / 100 * grad_T
    } else {
      x$PRESS <- 14.7 + (x$DEPTH + input$gle) * grad_P
      x$TEMP <- input$temp_surface + (x$DEPTH + input$gle) / 100 * grad_T
    }
  }


  # add MEAN case
  xm <- dplyr::data_frame(CASE = "CASE_MEAN", CORR = as.numeric(input$corr_oil_1), UCORR = as.numeric(input$corr_oil_1),
                          API = mean(x$API), GOR = mean(x$GOR),
                          DEPTH = mean(x$DEPTH), PRESS = mean(x$PRESS), TEMP = mean(x$TEMP),
                          GRAD_P = mean(x$GRAD_P), GRAD_T = mean(x$GRAD_T), PPM = mean(x$PPM))

  colnames(xm) <- colnames(x)
  x <- rbind(x, xm)

  return(x)
})

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
      dplyr::select(Statistic, API, GOR, DEPTH, PRESS, TEMP, GRAD_P, GRAD_T, PPM)
  }
  else {
    xx <- as.data.frame(x) %>%
      dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev")) %>%
      dplyr::select(Statistic, API, GOR, PRESS, TEMP, PPM)
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
                        input$wd - (input$wd * 0.433 + 14.7) / mean(data$GRAD_P),
                        input$gle + (14.7 / mean(data$GRAD_P)))

  t_intercept <- ifelse(input$offshore == TRUE,
                        -input$wd + 39.2 / mean(data$GRAD_T) *100,
                        input$gle + (input$temp_surface / mean(data$GRAD_T) * 100))

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
    ggplot2::geom_abline(ggplot2::aes(intercept = t_intercept, slope = -1 / mean(GRAD_T) * 100 ), color = "darkred") +
    ggplot2::geom_abline(ggplot2::aes(intercept = t_intercept, slope = -1 / quantile(GRAD_T, 0.9) * 100 ),
                         color = "darkred", linetype = 3) +
    ggplot2::geom_abline(ggplot2::aes(intercept = t_intercept, slope = -1 / quantile(GRAD_T, 0.1) * 100 ),
                         color = "darkred", linetype = 3) +
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
output$stats_output <- shiny::renderTable({
  temp <- results_r()

  # exclude columns 1 and 2 that are text: CASE and CORR
  x <- do.call(cbind, lapply(temp[3:ncol(temp)], petroreadr::summary_mod))

  xx <- as.data.frame(x) %>%
    dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev"))

  return(xx)
})

output$hist_bovisco <- shiny::renderPlot({
  data <- results_r()

  plot_bo <- ggplot2::ggplot(data, ggplot2::aes(x = BO)) +
    ggplot2::geom_histogram(ggplot2::aes(x = BO, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(BO)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[rb/stb]") +
    ggplot2::ggtitle("Bo")

  plot_visco <- ggplot2::ggplot(data, ggplot2::aes(x = VISCO)) +
    ggplot2::geom_histogram(ggplot2::aes(x = VISCO, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(VISCO)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[cP]") +
    ggplot2::ggtitle("Viscosity")

  gridExtra::grid.arrange(plot_bo, plot_visco, ncol = 2)
})
output$hist_psatcol <- shiny::renderPlot({
  data <- results_r()

  plot_psat <- ggplot2::ggplot(data, ggplot2::aes(x = PSAT)) +
    ggplot2::geom_histogram(ggplot2::aes(x = PSAT, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(PSAT)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[psia]") +
    ggplot2::ggtitle("Saturation Pressure")

  plot_oilcol <- ggplot2::ggplot(data, ggplot2::aes(x = OILCOL)) +
    ggplot2::geom_histogram(ggplot2::aes(x = OILCOL, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(OILCOL)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[ft]") +
    ggplot2::ggtitle("Oil Column to GOC")

  gridExtra::grid.arrange(plot_psat, plot_oilcol, ncol = 2)
})
output$hist_dens <- shiny::renderPlot({
  data <- results_r()

  plot_deno <- ggplot2::ggplot(data, ggplot2::aes(x = DENO)) +
    ggplot2::geom_histogram(ggplot2::aes(x = DENO, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(DENO)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[psi/ft]") +
    ggplot2::ggtitle("Oil fluid gradient")

  plot_denw <- ggplot2::ggplot(data, ggplot2::aes(x = DENW)) +
    ggplot2::geom_histogram(ggplot2::aes(x = DENW, ..ncount..), color = "grey79", alpha = 0.4, bins = 25) +
    ggplot2::stat_ecdf(color = "darkred") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(DENW)), color = "green", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::xlab("[psi/ft]") +
    ggplot2::ggtitle("Water fluid gradient")

  gridExtra::grid.arrange(plot_deno, plot_denw, ncol = 2)
})

output$plot_oilcol <- shiny::renderPlot({
  data <- results_r() %>%
    dplyr::mutate(DD = PRES - PSAT)

  oilcol_lim <- c(min(data$depth_err), max(data$OILCOL))
  press_lim <- c(0, max(data$PRES))
  dd_lim <- c(mean(data$DD) + mean(data$PSAT), mean(data$DD) + mean(data$PSAT) - press_lim[2])

  xy <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(x = PRES, y = depth_err), color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = PSAT, y = OILCOL), color = "orange") +
    ggplot2::geom_segment(ggplot2::aes(x = mean(PRES), xend = mean(PSAT), y = 0, yend = mean(OILCOL)), color = "darkgreen", linetype = 2) +
    ggplot2::geom_segment(ggplot2::aes(x = mean(PSAT), xend = mean(PSAT), y = 0, yend = mean(OILCOL)), color = "orange", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(c(5.5, 5.5, 5.5, 14), "points")) +
    ggplot2::xlim(press_lim) +
    ggplot2::ylim(oilcol_lim) +
    ggplot2::xlab("Pressure [psia]") +
    ggplot2::ylab("Depth [ft]")

  plot_dd <- ggplot2::ggplot(data, ggplot2::aes(x = DD)) +
    ggplot2::geom_histogram(ggplot2::aes(x = DD, ..ncount..), color = "grey27", fill = "red", alpha = 0.5) +
    ggplot2::stat_ecdf(color = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean (DD)), color = "red", linetype = 2) +
    ggplot2::scale_x_reverse(limits = dd_lim) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(c(5.5, 5.5, 5.5, 20), "points")) +
    ggplot2::xlab("Drawdown to Psat [psia]")

  plot_psat <- ggplot2::ggplot(data, ggplot2::aes(x = PSAT)) +
    ggplot2::geom_histogram(ggplot2::aes(x = PSAT, ..ncount..), color = "grey27", fill = "orange", alpha = 0.5) +
    ggplot2::stat_ecdf(color = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean (PSAT)), color = "red", linetype = 2) +
    ggplot2::xlim(press_lim) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(c(5.5, 5.5, 5.5, 20), "points")) +
    ggplot2::xlab("Psat [psia]")

  plot_oilcol <- ggplot2::ggplot(data, ggplot2::aes(x = OILCOL)) +
    ggplot2::geom_histogram(ggplot2::aes(x = OILCOL, ..ncount..), fill = "darkgreen", alpha = 0.4, color = "grey27") +
    ggplot2::stat_ecdf(color = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), color = "blue", linetype = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.1), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean (OILCOL)), color = "red", linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(c(5.5, 5.5, 5.5, 10), "points")) +
    ggplot2::xlim(oilcol_lim) +
    ggplot2::xlab("Oil Column [ft]") +
    ggplot2::coord_flip()

  plot_void <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::theme_void()

  lay <- rbind(c(1,1,1,1,1,2,2),
               c(3,3,3,3,3,4,4),
               c(3,3,3,3,3,4,4),
               c(3,3,3,3,3,4,4),
               c(5,5,5,5,5,6,6))

  gridExtra::grid.arrange(plot_psat, plot_void,
                          xy, plot_oilcol,
                          plot_dd, plot_void,
                          ncol = 2, layout_matrix = lay)
})

# ----------------------------------------------------------------------------
output$selectize_corr_var <- renderUI({
  data <- DATA_r()
  selectizeInput('corr_var', 'Choose variables to be plotted:',
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


output$debug01 <- renderTable({
  # x <- values$distroparam[1,3]
  # x <- c("normal", "truncated normal", "lognormal",
  #               "truncated lognormal", "uniform", "triangular", "fixed value")[values$distroparam[1, 2]]
  # x <- summary(API_r())
  # x <- c(input$depth,
  #         input$wd,
  #         input$gle,
  #         "temp",
  #         input$temp_bottomsea,
  #         input$temp_surface,
  #         "offshore?",
  #         input$offshore)

  x <-  LHS_r()

  return(x)
})
