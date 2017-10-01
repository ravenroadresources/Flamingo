library(shiny)
library(shinydashboard)
library(shinythemes)
library(rhandsontable)
library(shinyjs)

# UI
#######
# variables: api, gor, water ppm, depth, depth error, pressure grad, temperature grad, Hc column
# direct caclulations: final depth, pressure, temperature
# calculatins after openserver: oil and water density, oil column to Pb,
#    deltaP between oil and water (Requires HC column distribution), chance of having gas cap,
#    downhole volumes of oil and gas (to see if it make sense the GOR)
# plot: pressure and temp vs depth (inlcuding grad lines), api vs gor, api vs gor + oil column
#    lines (at mean pressure and temp), correlation, ggpairs
# distro:
# output: summary_mod
# !!! Machine learning algorithm ?? estimate API and GOR ??
####
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "red",
  ######################################################################################
  dashboardHeader(title = "Flamingo",
                  dropdownMenu(type = "tasks",
                    taskItem("Variables validated"),
                    menuItem("AA", tabName = "results", icon = icon("info-circle")))
                  # dropdownMenu(type = "notification",
                  #   messageItem(from = "x", message = "aaa"),
                  #   menuItem("AA", tabName = "results", icon = icon("dashboard")),
                  #   menuItem("BB", tabName = "Settings", icon = icon("dashboard")))
  ),
  ######################################################################################
  dashboardSidebar(
    shinyjs::useShinyjs(),
    h4("Fluid Properties Uncertainty Analysis"),
    hr(),
    sidebarMenu(selected = "_intro",
      menuItem(HTML("<b>Analogs:</b>"), icon = icon("bullseye"),
        menuSubItem("Analogs Data", tabName = "_analogs", icon = icon("puzzle-piece")),
        menuSubItem("Cluster Analysis", tabName = "_cluster", icon = icon("object-group")),
        menuSubItem("Heatmap", tabName = "_heatmap", icon = icon("map-o")),
        menuSubItem("Statistics", tabName = "_stats", icon = icon("bars"))),
      menuItem(HTML("<b>MonteCarlo:</b>"), icon = icon("bar-chart"),
        menuSubItem("Settings", tabName = "_settings", icon = icon("cogs")),
        menuSubItem("Definition", tabName = "_definition", icon = icon("delicious")),
        # menuSubItem("P&T vs depth", tabName = "_depth", icon = icon("thermometer-half")),
        menuSubItem("Simulation", tabName = "_sim", icon = icon("check-square-o")),
        menuSubItem("Dataset", tabName = "_data", icon = icon("table"))),
      menuItem(HTML("<b>Report:</b>"), tabName = "_report", icon = icon("file-text")),
      menuItem(HTML("<b>Help:</b>"), tabName = "_support", icon = icon("life-buoy")),
      menuItem(HTML("<b><font color=#ff0000>Close</font></b>"),
                shiny::tags$div(id = 'close', "Close",
                               class = "btn action-button",
                               onclick = "setTimeout(function(){window.close();},500);"),
                shiny::tags$div(id = 'new_session',
                                class = "btn action-button",
                                HTML("<a id='new_session' href='./' target='_blank'>New Session</a>")),
                               icon = icon("power-off"))
    )
    # shiny::tags$div(id = 'close',
    #   class = "btn action-button",
    #   onclick = "setTimeout(function(){window.close();},500);",  # close browser
    #   HTML("<b><font color=#ff0000>Close</font></b>"))

  ),
  ######################################################################################
  dashboardBody(
    tabItems(
      tabItem(tabName = "_intro",
              shiny::h3("Flamingo"),
              shiny::h4("Fluid Properties Uncertainty Analysis")),
      tabItem(tabName = "_analogs", shiny::p("load data, map")),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_cluster", shiny::p("cluster analysis")),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_heatmap", shiny::p("Heat Map"),
              fluidRow(
                column(width = 4,
                       "4"
                ),
                column(width = 3, offset = 2,
                       "3 offset 2"
                )
              )

              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_stats", p("Statistics"),
              sidebarPanel(p("sidebar")),
              mainPanel(p("mainpanel"))
              ),
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_settings",
              sidebarPanel(
                h4("Montecarlo Settings:"),
                shiny::br(),
                shiny::numericInput('nn', 'Number of runs:', 100, step = 1),
                shiny::numericInput('ss', 'Set seed:', 1234, step = 1),
                shiny::radioButtons("corr_method", "Correlated  Variables:", c("Correlated" = 0,
                                                                               "Uncorrelated)" = 1), selected = 0),
                shiny::radioButtons("sampling_method", "Sampling Method", c("Latin Hypercube" = 0,
                                                                            "Random" = 1), selected = 0),
                shiny::uiOutput("lhs_acc_slider")
              ),
             sidebarPanel(
                h4("Problem Definition:"),
                shiny::br(),
                shiny::radioButtons("fluid_type", "Fluid type:",
                                    c("Oil" = 0, "Gas" = 1), selected = 0),
                shiny::radioButtons("fluid_corr_method", "Select type of analysis:",
                                    c("Select one fluid correlation" = 0, "Fluid correlations sensitivity" = 1), selected = 0),
                shiny::radioButtons("pt_method", "Pressure and Temperature calculation:",
                                    c("Depth + Gradients" = 0, "Direct Pressure and Temperature" = 1), selected = 0),
                shiny::uiOutput("depth_ref"),
                shiny::uiOutput("depth_offshore"),
                shiny::uiOutput("depth_msl"),
                shiny::uiOutput("depth_temp")
              )
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_definition",
              tabsetPanel(id = "navbar",
                tabPanel("Variables",
                          h4("Variables:"),
                         rHandsontableOutput("distro_params_table"),
                         br(),
                         shiny::tags$button(
                           id = 'dp_set_def',
                           type = "button",
                           class = "btn action-button",
                           "Set as default"),
                         shiny::tags$button(
                           id = 'dp_reset',
                           type = "button",
                           class = "btn action-button",
                           "Reset default"),

                         br(),
                         verbatimTextOutput("debug01"),
                         br(),


                         # fluidRow(
                         #   column(width = 2, shiny::h5("API")),
                         #   column(width = 2, shiny::selectInput("API_distro","", choices = list(
                         #     "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #     selected = "normal")),
                         #   column(width = 1, shiny::numericInput('API_mean',"", 30, step = 0.5, min = 0.1)),
                         #   column(width = 1, shiny::numericInput('API_sd', "", 1, step = 0.5, min = 0)),
                         #   column(width = 1, shiny::numericInput('API_lower', "", 8, step = 0.5, min = 0)),
                         #   column(width = 1, shiny::numericInput('API_upper', "", 69, step = 0.5, min = 1))
                         # ),
                         # fluidRow(
                         #   column(width = 2,shiny::h5("GOR [scf/stb]")),
                         #   column(width = 3, shiny::selectInput("API_distro","", choices = list(
                         #     "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #     selected = "normal")),
                         #   column(width = 1, shiny::numericInput('GOR_mean',"", 250, step = 0.5, min = 0.1)),
                         #   column(width = 1, shiny::numericInput('GOR_sd', "", 75, step = 0.5, min = 0)),
                         #   column(width = 1, shiny::numericInput('GOR_lower', "", 10, step = 0.5, min = 0)),
                         #   column(width = 1, shiny::numericInput('GOR_upper', "", 1000, step = 0.5, min = 1))
                         # ),

                         # shiny::tags$table(
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("API")),
                         #     shiny::tags$td(shiny::selectInput("API_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('API_mean',"", 30, step = 0.5, min = 0.1, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('API_sd', "", 1, step = 0.5, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('API_lower', "", 8, step = 0.5, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('API_upper', "", 69, step = 0.5, min = 1, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("GOR [scf/stb]")),
                         #     shiny::tags$td(shiny::selectInput("GOR_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('GOR_mean',"", 250, step = 0.5, min = 0.1, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('GOR_sd', "", 75, step = 0.5, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('GOR_lower', "", 10, step = 0.5, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('GOR_upper', "", 1000, step = 0.5, min = 1, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Depth Uncertainty [ft]")),
                         #     shiny::tags$td(shiny::selectInput("DEPT_err_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('DEPT_err_mean',"", 100, step = 5, min = 0, max = 500, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('DEPT_err_sd', "", 25, step = 1, min = 0, max = 200, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('DEPT_err_lower', "", 0, step = 5, min = 0, max = 500, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('DEPT_err_upper', "", 500, step = 5, min = 1, max = 500, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Pressure Gradient [psi/ft]")),
                         #     shiny::tags$td(shiny::selectInput("grad_P_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('grad_P_mean',"", 0.45, step = 0.01, min = 0.3, max = 0.99, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_P_sd', "", 0.02, step = 0.005, min = 0, max = 0.1,width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_P_lower', "", 0.3, step = 0.01, min = 0.3, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_P_upper', "", 0.99, step = 0.01, min = 0.3, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Temperature Gradient [F/100ft]")),
                         #     shiny::tags$td(shiny::selectInput("grad_T_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('grad_T_mean',"", 1.6, step = 0.05, min = 1.2, max = 3, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_T_sd', "", 0.2, step = 0.02, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_T_lower', "", 1.2, step = 0.05, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('grad_T_upper', "", 3, step = 0.05, min = 1, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Water Salinity [ppm]")),
                         #     shiny::tags$td(shiny::selectInput("water_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('water_mean',"", 1.6, step = 0.05, min = 1.2, max = 3, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('water_sd', "", 0.2, step = 0.02, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('water_lower', "", 1.2, step = 0.05, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('water_upper', "", 3, step = 0.05, min = 1, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Reservoir Pressure [psia]")),
                         #     shiny::tags$td(shiny::selectInput("press_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('press_mean',"", 1.6, step = 0.05, min = 1.2, max = 3, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('press_sd', "", 0.2, step = 0.02, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('gpress_lower', "", 1.2, step = 0.05, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('press_upper', "", 3, step = 0.05, min = 1, width = 100))
                         #   ),
                         #   shiny::tags$tr(
                         #     shiny::tags$td(shiny::h5("Reservoir Temperature [F]")),
                         #     shiny::tags$td(shiny::selectInput("temp_distro","", choices = list(
                         #       "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                         #       selected = "normal", width = 200)),
                         #     shiny::tags$td(shiny::numericInput('temp_mean',"", 1.6, step = 0.05, min = 1.2, max = 3, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('temp_sd', "", 0.2, step = 0.02, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('temp_lower', "", 1.2, step = 0.05, min = 0, width = 100)),
                         #     shiny::tags$td(shiny::numericInput('temp_upper', "", 3, step = 0.05, min = 1, width = 100))
                         #   )),
                         shiny::br(),
                         shiny::selectInput("fcorr_1", "Fluid Correlation:",
                                            c("Glaso" = 0, "Standing" = 1, "Lasater" = 2, "VazquezBeggs" = 3,
                                              "Petroskyetal" = 4, "AlMarhoun" = 5, "DeGhettoetalHeavyoil" = 6)),
                         shiny::tableOutput("stats_input")
                ),
                tabPanel("Correlation",
                         shiny::br(),
                         shiny::numericInput('rr', 'API vs. GOR correlation coefficient:', 0.75, step = 0.05),
                         shiny::br(),
                         shiny::uiOutput("selectize_corr_var"),
                         shiny::br(),
                         shiny::plotOutput("pairs_input", height = 600)),
                tabPanel("Histograms",
                         shiny::br(),
                         shiny::plotOutput("hist_apigor"),
                         shiny::plotOutput("hist_presstemp")),
                tabPanel("P&T vs Depth", id = "PTDEPTH", value = "PTDEPTH",
                         shiny::br(),
                         shiny::plotOutput("depth_plot", height = 600))
              )
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_data",
              shiny::br(),
              shiny::p("Data"),
              shiny::br(),
              shiny::downloadButton("aaa", label = "Download Results"),
              DT::dataTableOutput("data_input")
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_sim",
              shiny::tags$button(
                id = 'validate',
                type = "button",
                class = "btn action-button",
                "Validate Data"),
              shiny::br(),
              shiny::br(),
              shiny::tags$button(
                id = 'run_mc',
                type = "button",
                class = "btn action-button",
                "Lunch MonteCarlo"),
              shiny::br(),
              shiny::p("show results after runnning prosper")),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_report",
              shiny::br(),
              checkboxGroupInput("report_items", "Select Analysis to include in the report:",
                c("Analogs", "MonteCarlo", "histograms", "P&T vs depth", "Correlations"),
                selected = c("MonteCarlo", "histograms")),
              shiny::tags$button(
                id = 'edit_report',
                type = "button",
                class = "btn action-button",
                "Edit Report"),
              shiny::tags$button(
                id = 'generate_report',
                type = "button",
                class = "btn action-button",
                "Generate Report")
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_support",
              shiny::h3("Analogs"),
              shiny::h5("import and visualize analogs data"),
              shiny::br(),
              shiny::br(),
              shiny::h3("MonteCarlo"),
              shiny::h4("Sampling Method"),
              shiny::h5("Latin Hypercube assure a more uniform distribution of the points in the space.
                        Use it for a reduced number of realizations.
                        For a large number of realizations, it is recommended to use Random sampling."),
              shiny::br(),
              shiny::h4("Type of analysis"),
              shiny::h5("The general purpose analysis is based on one fluid correlation. There is the option to run a
                        sensitivty on the different fluid correlations: the dataset is rebuilt sampling 25 realizations of the
                        orignal dataset, and this reduced dataset is run with each of the correlations."),
              shiny::br(),
              shiny::h4("Pressure and Temperature method"),
              shiny::h5("Reservoir pressure and tenperature can be introduced directly or calculated from reservoir depth with
                        pressure and temperature gradients. The offshore option can be selcted.
                        Temperature ground level is fixed at 60F, at sea bottom is 39F (4ºC)")
      )
   )
  )
  )
  )
