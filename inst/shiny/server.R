
require(dplyr)
# require(xlsx)
# require(rJava)
Sys.setenv("R_ZIPCMD" = "C:/R-3.4.2/Rtools/bin/zip.exe")

package_path <- system.file(package = "Flamingo")
dp_user_path <- file.path(package_path, "shiny", "www", "default", "distro_params_user.txt")
dp_def_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default.txt")
# dp_def1_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default_1.txt")
vbs_path <- file.path(package_path, "extdata", "Flamingo.vbs")
prosper_path <- file.path(package_path, "extdata", "Flamingo_0.0.4.Out")
xls_path <- file.path(package_path, "extdata", "Flamingo.xlsm")
rmd_path <- file.path(package_path, "shiny", "www", "rmd", "report.Rmd")
report_path <- file.path(package_path, "shiny", "www", "rmd", "report.html")
analog_default_path <- file.path(package_path, "extdata", "Analogs_example.xlsx")

rsource_mc_path <- file.path(package_path, "shiny", "www", "mc.R")
rsource_analog_path <- file.path(package_path, "shiny", "www", "analog.R")
rsource_report_path <- file.path(package_path, "shiny", "www", "report.R")


# SERVER
shiny::shinyServer(function(input, output) {

  observe({
    if (input$close > 0) shiny::stopApp()  # stop shiny
  }) # stop shinyapp

  # ANALOGS
  source(rsource_mc_path, local = TRUE)
  # ANALOGS
  source(rsource_analog_path, local = TRUE)
  # REPORT
  source(rsource_report_path, local = TRUE)

})


