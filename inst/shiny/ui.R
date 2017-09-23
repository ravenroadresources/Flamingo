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
                # shiny::tags$head(
                #   shiny::tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
                #   shiny::tags$title("Flamingo - RavenRoad S.A.S."),
                #   shiny::tags$meta(name="description", content="Flamingo: FLuid Properties Uncertainty Analysis"),
                #   shiny::tags$meta(name="keywords", content=""),
                #   shiny::tags$meta(name="author", content="RavenRoad S.A.S."),
                #   shiny::tags$meta(name="robots", content="noodp,noydir"),
                #   shiny::tags$link(href="./css/style_shiny.css", rel="stylesheet")
                # ),

                shinydashboard::dashboardHeader(title = "Flamingo", titleWidth = 350),
                # SIDEBAR
                #-----------------------------------------------------------------------------------
                shinydashboard::dashboardSidebar(
                  shiny::tags$head(
                    shiny::tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
                    shiny::tags$title("Flamingo - RavenRoad S.A.S."),
                    shiny::tags$meta(name="description", content="Flamingo: FLuid Properties Uncertainty Analysis"),
                    shiny::tags$meta(name="keywords", content=""),
                    shiny::tags$meta(name="author", content="RavenRoad S.A.S."),
                    shiny::tags$meta(name="robots", content="noodp,noydir")
                    # shiny::tags$link(rel = "stylesheet", type = "text/css", href = "/css/style_shiny.css")
                  ),
                  shiny::fileInput('file1', 'Upload .xlsx data file:',
                            accept = c(".xlsx"),
                            width= '200%'),
                  shiny::checkboxInput("normalize", "Normalize variables before calculating distances.", FALSE),
                  shiny::selectInput("d_method","Select distance method:",
                            choices = list("euclidian", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                            selected = "euclidian"),
                  #shiny::submitButton("Run MonteCarlo"),
                  shiny::br()
                  #shiny::downloadButton("aaa", label = "Download Results")
                ),
                #-----------------------------------------------------------------------------------
                shinydashboard::dashboardBody(
                  shiny::tabsetPanel(id = "navbar",
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("Settings",
                      shiny::br(),
                      shiny::numericInput('nn', 'Number of runs:', 100, step = 1),
                      shiny::numericInput('ss', 'Set seed:', 1234, step = 1),
                      shiny::radioButtons("sampling_method", "", c("Latin Hypercube" = 0, "Random" = 1), selected = 0),
                      shiny::sliderInput("lhs_accuracy", "LHS Accuracy level:", min = 0.01, max = 1, value = 0.05, step = 0.01)
                    ),
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("Input",
                      shiny::br(),
                      shiny::p("API, GOR, [Pressure, Temp] or [depth, pressure and temp grsdients]"),
                      shiny::br(),
                      shiny::numericInput('rr', 'API vs. GOR correlation coefficient:', 0.75, step = 1, width = 100),
                      shiny::numericInput('depth', "Reference Depth [ft.TVDss]:", 8000, step = 10),
                      shiny::checkboxInput("offshore", "Offshore:", FALSE),
                      shiny::numericInput('wd', "Water Depth [ft]", 1000, step = 1, min = 0),
                      shiny::tags$table(
                        shiny::tags$tr(
                          shiny::tags$td(shiny::h4("API")),
                          shiny::tags$td(shiny::selectInput("API_distro","", choices = list(
                            "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                            selected = "normal", width = 200)),
                          shiny::tags$td(shiny::numericInput('API_mean',"", 30, step = 0.5, min = 0.1, width = 100)),
                          shiny::tags$td(shiny::numericInput('API_sd', "", 1, step = 0.5, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('API_lower', "", 8, step = 0.5, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('API_upper', "", 69, step = 0.5, min = 1, width = 100))
                        ),
                        shiny::tags$tr(
                          shiny::tags$td(shiny::h4("GOR [scf/stb]")),
                          shiny::tags$td(shiny::selectInput("GOR_distro","", choices = list(
                            "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                            selected = "normal", width = 200)),
                          shiny::tags$td(shiny::numericInput('GOR_mean',"", 250, step = 0.5, min = 0.1, width = 100)),
                          shiny::tags$td(shiny::numericInput('GOR_sd', "", 75, step = 0.5, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('GOR_lower', "", 10, step = 0.5, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('GOR_upper', "", 1000, step = 0.5, min = 1, width = 100))
                        ),
                        shiny::tags$tr(
                          shiny::tags$td(shiny::h4("Depth Uncertainty [ft]")),
                          shiny::tags$td(shiny::selectInput("DEPT_err_distro","", choices = list(
                            "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                            selected = "normal", width = 200)),
                          shiny::tags$td(shiny::numericInput('DEPT_err_mean',"", 100, step = 5, min = 0, max = 500, width = 100)),
                          shiny::tags$td(shiny::numericInput('DEPT_err_sd', "", 25, step = 1, min = 0, max = 200, width = 100)),
                          shiny::tags$td(shiny::numericInput('DEPT_err_lower', "", 0, step = 5, min = 0, max = 500, width = 100)),
                          shiny::tags$td(shiny::numericInput('DEPT_err_upper', "", 500, step = 5, min = 1, max = 500, width = 100))
                        ),
                        shiny::tags$tr(
                          shiny::tags$td(shiny::h4("Pressure Gradient [psi/ft]")),
                          shiny::tags$td(shiny::selectInput("grad_P_distro","", choices = list(
                            "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                            selected = "normal", width = 200)),
                          shiny::tags$td(shiny::numericInput('grad_P_mean',"", 0.45, step = 0.01, min = 0.3, max = 0.99, width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_P_sd', "", 0.02, step = 0.005, min = 0, max = 0.1,width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_P_lower', "", 0.3, step = 0.01, min = 0.3, width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_P_upper', "", 0.99, step = 0.01, min = 0.3, width = 100))
                        ),
                        shiny::tags$tr(
                          shiny::tags$td(shiny::h4("Temperature Gradient [F/100ft]")),
                          shiny::tags$td(shiny::selectInput("grad_T_distro","", choices = list(
                            "normal", "truncated normal", "lognormal", "truncated lognormal", "uniform", "triangular", "fixed value"),
                            selected = "normal", width = 200)),
                          shiny::tags$td(shiny::numericInput('grad_T_mean',"", 1.6, step = 0.05, min = 1.2, max = 3, width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_T_sd', "", 0.2, step = 0.02, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_T_lower', "", 1.2, step = 0.05, min = 0, width = 100)),
                          shiny::tags$td(shiny::numericInput('grad_T_upper', "", 3, step = 0.05, min = 1, width = 100))
                        )),
                      shiny::br(),
                      shiny::plotOutput("hist_apigor"),
                      shiny::plotOutput("hist_presstemp"),
                      shiny::br(),
                      shiny::tableOutput("stats_input")
                    ),
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("QC",
                      shiny::br(),
                      shiny::p("Quality Check of input data"),
                      shiny::br(),
                      shiny::plotOutput("pairs_input", height = 600),
                      shiny::br(),
                      shiny::plotOutput("depth_plot", height = 600)
                    ),
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("Data",
                                    shiny::br(),
                                    shiny::p("Data"),
                                    DT::dataTableOutput("data_input")
                    ),
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("Results",
                                    shiny::br(),
                                    shiny::p("show results after runnning prosper")
                    ),
                    #-------------------------------------------------------------------------------
                    shiny::tabPanel("Help",
                      shiny::br(),
                      shiny::p("Help!!!!!!!!!!!!"),
                      shiny::br(),
                      shiny::h3("Sampling Method"),
                      shiny::h5("Latin Hypercube assure a more uniform distribution of the points in the space.
                                Use it for a reduced number of realizations.
                                For a large number of realizations, it is recommended to use Random sampling.")
                    )
                  )
                )
  )
)
