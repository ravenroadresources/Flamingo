library(shiny)
library(shinydashboard)
# library(shinythemes)
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
    sidebarMenu(#selected = "_intro",
      menuItem(HTML("<b>Analogs:</b>"), icon = icon("bullseye"),
        menuSubItem("Analogs Data", tabName = "_analogs", icon = icon("puzzle-piece")),
        menuSubItem("Saturation Families", tabName = "_satfam", icon = icon("bars")),
        menuSubItem("Cluster Analysis", tabName = "_cluster", icon = icon("th")), #object-group
        menuSubItem("Heatmap", tabName = "_heatmap", icon = icon("map-o"))
       ),
      menuItem(HTML("<b>MonteCarlo:</b>"), icon = icon("bar-chart"),
        menuSubItem("Settings", tabName = "_settings", icon = icon("cogs")),
        menuSubItem("Definition", tabName = "_definition", icon = icon("delicious")),
        menuSubItem("Simulation", tabName = "_sim", icon = icon("refresh")),
        menuSubItem("Dataset", tabName = "_data", icon = icon("table"))),
      menuItem(HTML("<b>Report:</b>"), tabName = "_report", icon = icon("file-text")),
      menuItem(HTML("<b>Help:</b>"), tabName = "_support", icon = icon("life-buoy"), selected = TRUE),
      menuItem(HTML("<b><font color=#DD4A39>I/O</font></b>"),
                shiny::tags$div(id = 'close', "Close",
                               class = "btn action-button",
                               stile = "text-align: left;",
                               onclick = "setTimeout(function(){window.close();},500);"),
                shiny::tags$div(id = 'new_session',
                                class = "btn action-button",
                                HTML("<a id='new_session' href='./'>Reload</a>")), #target='_blank'
                               icon = icon("power-off") )
    )
  ),
  ######################################################################################
  dashboardBody(
    tabItems(
      tabItem(tabName = "_analogs",
              tabsetPanel(
                tabPanel("Data",
                    br(),
                    fileInput('file1', 'Upload .xlsx data file:',
                              accept = c(".xlsx"),
                              width= '200%'),
                    br(),
                    DT::dataTableOutput("analogdatatable")),
                tabPanel("Correction",
                         br(),
                         p("Correct Fluid properties to a reference Pressure and Temperature")
                ),
                tabPanel("Statistics",
                    br(),
                    shiny::tableOutput("stats_analog") ))
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_satfam",
              shiny::br(),
              shiny::h4("Saturation Families:"),
              shiny::br(),
              shiny::plotOutput("analog_apigor", width = 800, height = 600),
              shiny::br(),
              shiny::p("Show parameters of the families lines (slope and intercept), allowing to adjust them."),
              shiny::p("Estimate error with respect to the families lines. build an optimizer??"),
              shiny::p("select with lazo saturation families and use classification as mappable variable")
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_cluster",
                tabsetPanel(
                  tabPanel("Variables",
                           h5("Covariance Matrix"), br(),
                           selectInput("cor_method","Select covariance method:",
                                       choices = list("pearson", "kendall", "spearman"),
                                       selected = "pearson"),
                           selectInput("cor_na_method","Select how to treat missing values:",
                                       choices = list("everything", "all.obs", "complete.obs",
                                                      "na.or.complete", "pairwise.complete.obs"),
                                       selected = "complete.obs"),
                           plotOutput("corrplot", height = 800, width = 800),
                           br(),
                           #filtering ??
                           uiOutput("selectize_name"),
                           uiOutput("selectize_variable"),
                           uiOutput("selectize_log"),
                           checkboxInput("normalize", "Normalize variables before calculating distances.", FALSE),
                           br(),
                           plotOutput("dataplot", height = 800, width = 800) ),
                  tabPanel("Cluster Analysis",
                           br(),
                           selectInput("d_method","Select distance method:",
                                       choices = list("euclidian", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                                       selected = "euclidian"),
                           selectInput("h_method","Select agglomeration method:",
                                       choices = list("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                                       selected = "complete"),
                           radioButtons("cutoff_method", "", c("Cutoff" = 0, "Number of clusters" = 1)),
                           uiOutput("cutoff_slider"),
                           radioButtons("plot_type", "", c("standard" = 0,
                                                           "ggdendro" = 1,
                                                           "phylo" = 2,
                                                           "unrooted" = 3,
                                                           "fan" = 4)),
                           br(),
                           plotOutput("clusterplot", height = 800, width = 800),
                           plotOutput("clusterplot2", width = 800) ),
                  tabPanel("Results",
                           br(),
                           selectInput("palette","Colors Palette:",
                                       choices = list("terrain", "heat", "topo", "cm", "rainbow"),
                                       selected = "rainbow"),
                           plotOutput("resultsplot", height = 800, width = 800),
                           br(), br(),
                           h4("Centroids:"), br(),
                           dataTableOutput("centroids"),
                           downloadButton('downloadCentroids', 'Download Centroids'),
                           br(), br(),
                           h4("Dataset and Clusters:"), br(),
                           dataTableOutput("resultstable"),
                           downloadButton('downloadResults', 'Download Results') )
                )
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_heatmap",
              tabsetPanel(
                tabPanel("Coordinates",
                         p("define variables to be used as coordinates and reference system.")
                         ),
                tabPanel("Map",
                         shiny::h4("Map"),
                         shiny::br(),
                         shiny::uiOutput("map_variable"),
                         leaflet::leafletOutput("map", height = 800),
                         shiny::radioButtons("mapstyle", "", c("Default" = "Stamen.Toner",
                                                               "OpenStreetMap" = "OpenStreetMap.Mapnik",
                                                               "Topographic" = "Esri.WorldTopoMap"),
                                             selected= "Stamen.Toner")
                )
              )
            ),

      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_settings",
              sidebarPanel(
                h4("Montecarlo Settings:"),
                shiny::br(),
                shiny::uiOutput("nn_runs"),
                # shiny::numericInput('nn', 'Number of runs:', 100, step = 1),
                shiny::numericInput('ss', 'Set seed:', 1234, step = 1),
                shiny::radioButtons("corr_method", "Correlated  Variables:", c("Correlated" = 0,
                                                                               "Uncorrelated" = 1), selected = 0),
                shiny::radioButtons("sampling_method", "Sampling Method", c("Latin Hypercube" = 0,
                                                                            "Random" = 1), selected = 0),
                shiny::uiOutput("lhs_acc_slider")
              ),
             sidebarPanel(
                h4("Problem Definition:"),
                shiny::br(),
                shiny::radioButtons("fluid_type", "Fluid type:",
                                    c("Oil" = 0, "Gas" = 1), selected = 0),
                # shiny::radioButtons("fluid_corr_method", "Select type of analysis:",
                #                     c("Select one fluid correlation" = 0, "Fluid correlations sensitivity" = 1), selected = 0),
                # shiny::radioButtons("points_method", "Select pressure points:",
                #                     c("Single point" = 0, "Multiple points" = 1), selected = 0),
                shiny::radioButtons("analysis_method", "Select type of analysis:",
                                    c("Single Fluid Correlation - Single Pressure Point" = 0,
                                      "Single Fluid Correlation - Multiple Pressure Points" = 1,
                                      "Fluid Correlations Sensitivity - Single Pressure points" = 2), selected = 0),
                shiny::uiOutput("pressure_min"),
                shiny::uiOutput("pressure_max"),
                shiny::uiOutput("pressure_steps"),
                shiny::radioButtons("pt_method", "Pressure and Temperature calculation:",
                                    c("Depth + Gradients" = 0, "Direct Pressure and Temperature" = 1), selected = 0),
                shiny::uiOutput("depth_ref"),
                shiny::uiOutput("depth_offshore"),
                shiny::uiOutput("depth_msl"),
                shiny::uiOutput("depth_temp"),
                shiny::numericInput('gas_sp', "Gas Specific Gravity (Air = 1)", 0.7, step = 0.01, min = 0.54, max = 3.0)
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
                         # verbatimTextOutput("debug01"),
                         # tableOutput("debug01"),
                         # br(),
                         shiny::br(),
                         # shiny::selectInput("corr_oil_1", "Density / Bo /Psat Correlation:",
                         #                    c("Glaso" = 0, "Standing" = 1, "Lasater" = 2, "VazquezBeggs" = 3,
                         #                      "Petroskyetal" = 4, "AlMarhoun" = 5, "DeGhettoetalHeavyoil" = 6)),
                         # shiny::selectInput("corr_oil_2", "Viscosity Correlation:",
                         #                    c("Glaso" = 0, "Standing" = 1, "Lasater" = 2, "VazquezBeggs" = 3,
                         #                      "Petroskyetal" = 4, "AlMarhoun" = 5, "DeGhettoetalHeavyoil" = 6)),
                         shiny::uiOutput("selectize_corr_oil_1"),
                         shiny::uiOutput("selectize_corr_oil_2"),
                         shiny::tableOutput("stats_input")
                ),
                tabPanel("Sigma",
                         shiny::br(),
                         shiny::numericInput('rr', 'API vs. GOR correlation coefficient:', 0.75, step = 0.05),
                         # shiny::sliderInput("corr_API_GOR", "API vs GOR correlation:", min = -1, max = 1, value = 0.75, step = 0.05),
                         # shiny::sliderInput("corr_API_DEPTH", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_API_gradP", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_API_gradT", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_API_ppm", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_GOR_DEPTH", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_GOR_gradP", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_GOR_gradT", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_GOR_ppm", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_DEPTH_gradP", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_DEPTH_gradT", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_DEPTH_ppm", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_gradP_gradT", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_gradP_ppm", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         # shiny::sliderInput("corr_gradT_ppm", "API vs GOR correlation:", min = -1, max = 1, value = 0.0, step = 0.05),
                         shiny::br(),
                         shiny::h4("Sigma Matrix (correlation between variables):"),
                         shiny::tableOutput("sigma_table"),
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
                         shiny::plotOutput("depth_plot", height = 800))
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
          tabsetPanel(id = "navbar",
            tabPanel("Run",
              shiny::br(),
              shiny::tags$button(
                id = 'validate',
                type = "button",
                class = "btn action-button",
                "Validate Data", style = 'width:150px'),
              shiny::uiOutput("button_check_1"),
              shiny::br(),
              shiny::br(),
              shiny::tags$button(
                id = 'open_prosper',
                type = "button",
                class = "btn action-button",
                "Open Propser", style='width:150px'),
              shiny::br(),
              shiny::br(),
              shiny::tags$button(
                id = 'run_mc',
                type = "button",
                class = "btn action-button",
                "Run Simulation", style='width:150px'),
              shiny::br(),
              shiny::br(),
              actionButton(
                inputId = 'import_results',
                # type = "button",
                # class = "btn action-button",
                label = "Import results", style='width:150px'),
              shiny::br(),
              shiny::br(),
              shiny::p("show results after runnning prosper"),
              shiny::br(),
              shiny::tableOutput("stats_output") ),
            tabPanel("Histograms",
                     shiny::br(),
                     shiny::plotOutput("hist_bovisco"),
                     shiny::plotOutput("hist_psatcol"),
                     shiny::plotOutput("hist_dens") ),
            tabPanel("Sensitivity",
                     shiny::br(),
                     shiny::uiOutput("sensitivity_ref"),
                     shiny::br(),
                     shiny::plotOutput("plot_sensitivity", height = 800, width = 600) ),
            tabPanel("Column",
                     shiny::br(),
                     shiny::plotOutput("plot_oilcol", height = 800) ),
            tabPanel("Properties vs Pressure", id = "MULTIPOINT", value = "MULTIPOINT",
                     shiny::br(),
                     shiny::uiOutput("pvsp_reference"),
                     shiny::br(),
                     shiny::plotOutput("plot_pptyvspres", height = 800)
                     ),
            tabPanel("Fluid Correlations", id = "CORRSENS", value = "CORRSENS",
                    shiny::br(),
                    shiny::uiOutput("fluid_corr_sens_ref"),
                    shiny::br(),
                    shiny::plotOutput("plot_corr_sens", height = 600)
                    )
            )
          ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_report",
              shiny::sidebarPanel(
                shiny::br(),
                checkboxGroupInput("report_items", "Select Analysis to include in the report:",
                                   c("Analogs", "MonteCarlo", "histograms", "P&T vs depth", "Correlations"),
                                   selected = c("MonteCarlo", "histograms")),
                shiny::tags$button(
                  id = 'edit_report',
                  type = "button",
                  class = "btn action-button",
                  "Edit Report"),
                # downloadButton("report", "Generate report")
                actionButton("report", "Generate report")
                #         shiny::tags$button(
                #           id = 'generate_report',
                #           type = "button",
                #           class = "btn action-button",
                #           "Generate Report")
              ),
              shiny::mainPanel(
                # shiny::a(href = output$report_path)
                # shiny::a(href = "shiny/www/rmd/report.html")
                shiny::includeMarkdown("www/rmd/report.Rmd")
              )
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_support",
              tabsetPanel(
                tabPanel("About",
                         br(),
                         br(),
                         shiny::img(src = "img/flamingo_logo.png", align = "center", width = '100%'),
                         br(),
                         h5("Developed by Francesco Giorgetti - October 2017"),
                         br(),
                         h6("Flamingo software has 2 main core areas: the first deals with regional analog fluid data to try to define patterns and
                            predict the API and GOR of new prospects; the second is an aimplementation of a montacarlo simulation which estimate the
                            uncertainty ranges of fluid porperties in a consistent way."),
                         h6("The two areas are independent and can work standalone, but ideally the workflow is try to capture and narrow the API and GOR
                            expected range for a defined prospect using data analytics techniques, such as cluster analysis, and then pass those ranges
                            to the simulator to obtain derived fluid properties."),
                         h6("The montecarlo simulator deliberately relies on the Prosper software: that way it is possible to refine and match the
                            fluid correlation to regional data using a consolidated industry plaftorm to obtain an even more accurate prediction
                            of fluid properties."),
                         h6("Flamingo automatically generate a report of all the analys performed.")
                         ),
                tabPanel("Help",
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
                        Temperature ground level is fixed at 60F, at sea bottom is 39F (4ºC)") ),
                tabPanel("Credits",
                         h5("Packages dependecy:"),
                         h6("Imports: shiny, shinydashboard, rhandsontable, shinyjs, dplyr, ggplot2, gridExtra, truncdist, mc2d,
                              petroreadr, DT, pse, GGally, readxl, openxlsx"),
                         h6("Suggests: knitr, rmarkdown, roxygen2, testthat")
                         )
              )
      )
   )
  )
  )
  )
