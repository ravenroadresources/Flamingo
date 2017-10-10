
require(dplyr)
# require(xlsx)
# require(rJava)

# SERVER

shiny::shinyServer(function(input, output) {

  package_path <- system.file(package = "Flamingo")
  dp_user_path <- file.path(package_path, "shiny", "www", "default", "distro_params_user.txt")
  dp_def_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default.txt")
  vbs_path <- file.path(package_path, "extdata", "Flamingo.vbs")
  prosper_path <- file.path(package_path, "extdata", "Flamingo.Out")
  xls_path <- file.path(package_path, "extdata", "Flamingo.xls")
  rmd_path <- file.path(package_path, "shiny", "www", "rmd", "report.Rmd")
  report_path <- file.path(package_path, "shiny", "www", "rmd", "report.html")

  # output$report_path <- shiny::renderText({
  #   x <- report_path
  #   return(x)
  # })

  observe({
    if (input$close > 0) shiny::stopApp()  # stop shiny
  }) # stop shinyapp

  # ANALOGS
  source("www/mc.R", local = TRUE)
  # ANALOGS
  source("www/analog.R", local = TRUE)
  # REPORT
  source("www/report.R", local = TRUE)



})


