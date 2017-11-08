# load analogs data, or use diamands data set by default
analogdata_r <- reactive({
  # data <- ggplot2::diamonds[sample(1:dim(ggplot2::diamonds)[1],100),] #sample of the diamonds dataset
  data <- readxl::read_excel(analog_default_path, sheet = 1)

  inFile <- input$file1
  if (is.null(input$file1)) return(data)
  # trick to read excel files: need to rename first the file loaded into the temp folder
  file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
  data <- readxl::read_excel(paste0(inFile$datapath, ".xlsx"), sheet=1)

  data <- as.data.frame(data)
})

records_name_r <- reactive({
  if (is.null(input$records_name)) return(NULL)
  input$records_name
})

data_names_r <- reactive({
  data <- analogdata_r()
  records_name <- records_name_r()

  # rownames
  if (is.null(records_name)) temp <- paste0("ID: ",1:dim(data)[1])
  else { #temp <- data[,records_name]
    temp <- sapply(data[records_name], paste)
  }

  return(temp)
})

data_selected_r <- reactive({
  data <- analogdata_r()

  num_col <- sapply(data, is.numeric)
  num_data <- data[, num_col]

  # selected
  if (!is.null(input$variables)) num_data <- num_data[input$variables]

  return(as.data.frame(num_data))
})

data_log_r <- reactive({
  data <- data_selected_r()

  # log
  if (!is.null(input$varlog)) {
    if (length(as.array(input$varlog)) == 1) {
      data[as.array(input$varlog)] <- log10(data[as.array(input$varlog)])
    }
    else {
      data[as.array(input$varlog)] <- sapply(data[as.array(input$varlog)], log10)
    }
    # take log() of the selected columns, and overvrite them
    # logdata[input$log] <- log10(data[input$log])
  }

  return(data)
})

data_cluster_r <- reactive({
  data <- data_log_r()

  # normalize
  if (input$normalize) {
    data <- data %>%
      # mutate_each_(funs(scale(.) %>% as.vector), vars=colnames(data))
      mutate_each_(funs( ( . - mean(.) ) / sd(.) ), vars = colnames(data))
  }

  return(as.data.frame(data))
})

### Custer Algorithm
h_r <- reactive({
  data <- data_cluster_r()

  d <- dist(data, method = input$d_method, upper = TRUE)
  h <- hclust(d, method = input$h_method)
})

analogresults_r <- reactive({
  data <- data_cluster_r()
  h <- h_r()

  if (input$cutoff_method==0) {
    cutoff <- input$cutoff
    clusters <- stats::cutree(h, h=cutoff)
    numclusters<-h$height>cutoff
  }
  else {
    numclusters <- input$numclusters
    clusters <- stats::cutree(h, k=numclusters)
  }

  data$cluster <- clusters
  data
})

centroids_r <- reactive({
  data_temp <- analogresults_r()

  centroids <- data_temp %>%
    reshape::melt(id.vars=c("cluster")) %>%
    dplyr::group_by(cluster, variable) %>%
    dplyr::summarise(centroids=mean(value)) %>%
    reshape::cast(cluster ~ variable)

  centroids
})

### OUTPUT:

# UI

output$cutoff_slider <- shiny::renderUI({
  h <- h_r()
  if (input$cutoff_method==0) {
    sliderInput("cutoff", "Select height cutoff",
                min = 0, max = round(max(h$height), digits=0)+1,
                value = max(h$height)/2, step=round(max(h$height)/25, digits=1))
  }
  else {
    sliderInput("numclusters", "Define how many cluster you want to group the data in:",
                min = 1, max = 20, value = 3, 1)
  }
})
output$selectize_name <- shiny::renderUI({
  data <- analogdata_r()
  selectizeInput('records_name', 'Choose variable to be used as id:',
                 choices = colnames(data),
                 multiple = FALSE)
})
output$selectize_variable <- shiny::renderUI({
  data <- analogdata_r()
  num_col <- sapply(data, is.numeric)
  num_data <- data[, num_col]
  selectizeInput('variables', 'Select Variables:',
                 choices = colnames(num_data),
                 multiple = TRUE)
})
output$selectize_log <- shiny::renderUI({
  data <- data_selected_r()
  if (is.null(input$variables)) selectizeInput('varlog', 'Use Log10 (all variables):', choices = colnames(data), multiple=TRUE)
  else selectizeInput('varlog', 'Use Log10 (selected variables):', choices = colnames(data[input$variables]), multiple=TRUE)
})
output$map_variable <- shiny::renderUI({
  data <- analogdata_r()
  # shiny::selectInput("map_var", "Select Variable to plot on the map:", c(colnames(data), "cluster"))
  selectizeInput('map_var', 'Select Variable to plot on the map:', choices = c(colnames(data), "cluster"), multiple = FALSE)
})


# DataTable
output$resultstable <- renderDataTable({
  data <- analogresults_r()
  data_names <- data_names_r()
  dataout <- data.frame(data_names, data)
}, options = list(pageLength = 10))
output$centroids <- renderDataTable({
  xx <- centroids_r()
}, options = list(pageLength = 10))

# Plot
output$analog_apigor <- renderPlot({
  data <- analogdata_r()

  if ("API" %in% colnames(data) & "GOR" %in% colnames(data)) {
    ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x = API, y = GOR), size = 1.5) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_log10()
  }
  else NULL

})
output$corrplot <- renderPlot({
  data <- data_selected_r()
  data <- as.data.frame(data)

  GGally::ggcorr(data, label = TRUE, label_size = 3, label_round = 2,
                 label_alpha = TRUE, method = c(input$cor_na_method, input$cor_method))
})
output$dataplot <- renderPlot({
  data <- data_cluster_r()
  plot(data)
})
output$clusterplot <- renderPlot({
  h <- h_r()
  data <- analogdata_r()
  results <- analogresults_r()
  data_names <- data_names_r()

  clusters <- results$cluster
  numclusters <- max(clusters)

  # h$labels <- sapply(data_names, paste)

  # set color template and number of clusters to divide the palette into
  # numclusters<-h$height>cutoff
  numclusters <- sum(numclusters)+2
  colortemplate <- switch(input$palette,
                          terrain = c(terrain.colors(numclusters, alpha=1)),
                          heat = c(heat.colors(numclusters, alpha=1)),
                          topo =c(topo.colors(numclusters, alpha=1)),
                          cm = c(cm.colors(numclusters, alpha=1)),
                          rainbow = c(rainbow(numclusters, alpha=1)))

  if (input$plot_type==0) plot(h, hang = -1, cex = 0.6, labels = data_names)
  else if (input$plot_type==1) ggdendrogram(h, rotate = FALSE, size = 2)
  else if (input$plot_type==2) plot(as.phylo(h), type = "cladogram", cex = 0.6, label.offset = 0.5, tip.color = colortemplate[clusters])
  else if (input$plot_type==3) plot(as.phylo(h), type = "unrooted", cex = 0.6, no.margin = TRUE, tip.color = colortemplate[clusters])
  else if (input$plot_type==4) plot(as.phylo(h), type = "fan", tip.color = colortemplate[clusters])


  # +
  #   geom_hline(aes(cutoff))
  # abline(h=cutoff, col="red", lty=2)
})
output$clusterplot2 <- renderPlot({
  # data <- data_r()
  h <- h_r()
  cutoff <- input$cutoff

  # clusters <- stats::cutree(h, h=cutoff)
  par(mfrow=c(1,2))

  ## plot1
  plot(density((h$height)), main="density of branching heights", xlab="", ylab="")
  abline(v = cutoff, col="red", lty=2)

  ## plot2
  seq <- seq(0,max(h$height),length.out=20)
  num <- sapply(seq, function(x){length(unique(stats::cutree(h,h=x)))})
  plot(seq, num, ylim=c(0,12), xlim=c(0,max(h$height)), xaxt="n", yaxt="n",
       main="num of clusters (y) when cutting at height (x)",
       xlab="", ylab="")
  axis(1,at=seq)
  axis(2,at=0:max(num))
  abline(v = cutoff, col="red", lty=2)
})
output$resultsplot <- renderPlot({
  data <- analogdata_r()
  h <- h_r()

  num_col <- sapply(data, is.numeric)
  num_data <- data[, num_col]

  # selected
  if (!is.null(input$variables)) num_data <- num_data[input$variables]

  if (input$cutoff_method==0) {
    cutoff <- input$cutoff
    clusters <- stats::cutree(h, h=cutoff)
    numclusters<-h$height>cutoff
  }
  else {
    numclusters <- input$numclusters
    clusters <- stats::cutree(h, k=numclusters)
  }

  # set color template and number of clusters to divide the palette into
  # numclusters<-h$height>cutoff
  numclusters<-sum(numclusters)+2
  colortemplate <- switch(input$palette,
                          terrain = c(terrain.colors(numclusters, alpha=1)),
                          heat = c(heat.colors(numclusters, alpha=1)),
                          topo =c(topo.colors(numclusters, alpha=1)),
                          cm = c(cm.colors(numclusters, alpha=1)),
                          rainbow = c(rainbow(numclusters, alpha=1)))

  plot(num_data, col=colortemplate[clusters], pch=16, cex=1.8)
})

# Map
output$map <- leaflet::renderLeaflet({
  rawdata <- analogdata_r()
  resdata <- analogresults_r()
  data <- rawdata %>%
    dplyr::mutate(cluster = resdata$cluster)

  p <- leaflet::leaflet(data) %>%
    leaflet::addProviderTiles(input$mapstyle) %>%
    leaflet::fitBounds(~min(LONG), ~min(LAT), ~max(LONG), ~max(LAT))

    p <- p %>%
      leaflet::addCircleMarkers(~LONG, ~LAT ,
                       radius = 10,
                       color = "darkred",
                       stroke = TRUE,
                       fillOpacity = 0.2,
                       popup = ~as.character(input$map_var))


  p
})


# Text
output$datasummary <- renderText({
  data <- analogdata_r()
  temp <- summary(data)
  t(temp)
})
output$selectedvars <- renderText({
  xx <- input$variables
})

# Download Buttons
output$downloadResults <- downloadHandler(
  filename = function() { paste("ClusterAnalysisResults", '.csv', sep='') },
  content = function(file) { write.csv(results_r(), file) }
)
output$downloadCentroids <- downloadHandler(
  filename = function() { paste("ClusterAnalysisCentroids", '.csv', sep='') },
  content = function(file) { write.csv(centroids_r(), file) }
)

output$analogdatatable <- DT::renderDataTable({
  data <- analogdata_r()

  DT::datatable(data, options = list(pageLength = 25))
})
output$stats_analog <- shiny::renderTable({
  temp <- analogdata_r()
  x <- do.call(cbind, lapply(temp[3:ncol(temp)], petroreadr::summary_mod))

  xx <- as.data.frame(x) %>%
    dplyr::mutate(Statistic = c("Min", "p90", "p75", "p50", "Mean", "p25", "p10", "Max", "St.Dev")) %>%
    dplyr::select(Statistic, colnames(x))

  return(xx)
})
