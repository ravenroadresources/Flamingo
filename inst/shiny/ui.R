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
        menuSubItem("Statistics", tabName = "_stats", icon = icon("bars")),
        menuSubItem("Heatmap", tabName = "_heatmap", icon = icon("map-o")),
        menuSubItem("Cluster Analysis", tabName = "_cluster", icon = icon("th")) #object-group
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
              fileInput('file1', 'Upload .xlsx data file:',
                        accept = c(".xlsx"),
                        width= '200%'),
              br(),
              DT::dataTableOutput("analogdatatable")),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_stats", p("Statistics"),
              shiny::plotOutput("analog_apigor", width = 800, height = 600),
              shiny::tableOutput("stats_analog")
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
                           plotOutput("clusterplot", height = 800, width=1000),
                           plotOutput("clusterplot2", width = 1000) ),
                  tabPanel("Results",
                           br(),
                           selectInput("palette","Colors Palette:",
                                       choices = list("terrain", "heat", "topo", "cm", "rainbow"),
                                       selected = "rainbow"),
                           plotOutput("resultsplot", height = 1600, width = 1600),
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
                shiny::radioButtons("points_method", "Select pressure points:",
                                    c("Single point" = 0, "Multiple points" = 1), selected = 0),
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
            tabPanel("Column",
                     shiny::br(),
                     shiny::plotOutput("plot_oilcol", height = 800) ),
            tabPanel("Properties vs Pressure"),
            tabPanel("Fluid Correlations"))
          ),
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
              tabsetPanel(
                tabPanel("About",
                         br(),
                         br(),
                         shiny::img(src = "img/flamingo_logo.png", align = "center", width = '100%'),
                         br(),
                         h5("Developed by Francesco Giorgetti - October 2017")
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
                        Temperature ground level is fixed at 60F, at sea bottom is 39F (4ºC)") )
              )
      )
   )
  )
  )
  )
