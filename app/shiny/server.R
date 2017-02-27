library(shiny)
library(shinyjs)
source("../core/visualisation.R")
source("../core/verification/read_data.R")
source("../core/verification/compare_distributions.R")
source("../core/drift_timeline/timeline.R")
source("../core/visualise_results/result_table.R")
source("../core/visualise_results/heatmaps.R")
source("../core/visualise_results/detailed_likelihood.R")
source("../core/visualise_results/detailed_posterior.R")

# Increase file size limit
options(shiny.maxRequestSize=100*1024^2)

# Variables persistent throughout application lifetime -------------------------

measurement = "StepSize"
folder_name <- "../data"
data.out.foler <- "./data_out"
data.all <- data.frame()
data.paths <- ""

# Actual Functions -------------------------------------------------------------

selectWavelengths <- function(spectra, wavelength.range = c(0, Inf)) {
  # Get wavelength in given range
  wavelengths <- as.numeric(names(spectra))
  col.min <- max(wavelength.range[1] - min(wavelengths) + 1, 1)
  col.max <- min(wavelength.range[2] - min(wavelengths) + 1, length(wavelengths))
  return(spectra[, col.min:col.max])
}

selectTests <- function(spectra, test.ids) {
  # Get tests given
  return(spectra[test.ids, ])
}

shinyServer(function(input, output) {
  data.table <- reactiveValues(data = NULL, name = NULL, paths = NULL)
      
  observe({
      output$chunk.attribute.num.vals <- renderText("0")
      shinyjs::toggle("timeline.type", condition = !is.null(data.table$data))
      shinyjs::toggle("timeline.panel", condition = !is.null(data.table$data))
      shinyjs::toggle("analyse.plot", condition = !is.null(data.table$data))
      shinyjs::toggle("chunk.attribute.num.vals", condition = grepl("_chunk", input$timeline.type))
      shinyjs::toggle("analyse.drift.type", condition = input$analyse.type == "analysis")
      # shinyjs::toggle("size.confirm", condition = grepl("_chunk", input$timeline.type))
      # shinyjs::toggle("size.reset", condition = grepl("_chunk", input$timeline.type))
      # shinyjs::toggle("size.recorded", condition = grepl("_chunk", input$timeline.type))
  })
  
  # ------- Data file chooser

  # Temporary load data function
  observeEvent(input$file.arff, {
      file.input <- input$file.arff
      data.table$data <- ReadArffFiles(file.input$datapath)
      data.table$paths <- paste0(file.input$datapath, collapse = " ")
      data.table$name <- gsub(".arff", "", paste0(file.input$name, collapse = "_", sep = ""))
      output$result.files <- renderDataTable({data.table$data})
  })
  
  # ------ Timeline Generator

  output$attribute.subset.length <- renderUI({
      min <- 1
      # one less for class
      max <- ncol(data.table$data) - 1
      if (is.null(data.table$data)) {
          return()
      }
      else if (grepl("_chunk", input$timeline.type)) {
          # 1 less for group attribute
          max <- max - 1
      }
      sliderInput("slider.attribute.subset.length", 
                  "Attribute Subset Length",
                  min = min,
                  max = max,
                  value = min,
                  step = 1)
  })
  
  output$chunk.attribute <- renderUI({
      if (is.null(data.table$data) || !grepl("_chunk", input$timeline.type)) {
          return()
      }
      selectInput("select.chunk.attribute",
                  "Chunk Attribute:",
                  names(data.table$data)[1:ncol(data.table$data) - 1])
  })
  
  output$chunk.input <- renderUI({
      if (is.null(data.table$data) || !grepl("_chunk", input$timeline.type)) {
          return()
      }
      numericInput("num.chunk.input", label = "Chunk Size to Add", min = 1, value = 1)
  })
  
  output$window.input <- renderUI({
      if (is.null(data.table$data) || input$timeline.type != "stream") {
          return()
      }
      numericInput("num.window.input", label = "Window Size to Add", min = 1, max = ncol(data.table$data), value = 1)
      #sliderInput("num.window.input", label = "Window Size to Add", min = 1, max = nrow(data.table$data), value = 1, step = 1)
  })
  
  subset.lengths <- reactiveValues(window.lengths = c(), chunk.lengths = c())
  sizes <- reactiveValues(chunk.sizes = c(), window.sizes = c())

  observeEvent(input$timeline.type, {
      if (input$timeline.type == "stream") {
          output$size.recorded <- renderText(paste("Window Sizes:", paste(sizes$window.sizes, collapse = ",")))
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$window.lengths, collapse = ",")))
      }
      else {
          output$size.recorded <- renderText(paste("Window Durations:", paste(sizes$chunk.sizes, collapse = ",")))
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$chunk.lengths, collapse = ",")))
      }
  })
  
  observeEvent(input$length.confirm, {
      if (input$timeline.type == "stream") {
          subset.lengths$window.lengths <- unique(c(subset.lengths$window.lengths, input$slider.attribute.subset.length))
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$window.lengths, collapse = ",")))
      }
      else {
          subset.lengths$chunk.lengths <- unique(c(subset.lengths$chunk.lengths, input$slider.attribute.subset.length))
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$chunk.lengths, collapse = ",")))
      }
  })
  
  observeEvent(input$length.reset, {
      if (input$timeline.type == "stream") {
          subset.lengths$window.lengths <- c()
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$window.lengths, collapse = ",")))
      }
      else {
          subset.lengths$chunk.lengths <- c()
          output$subset.length.recorded <- renderText(paste("Subset Lengths:", paste(subset.lengths$chunk.lengths, collapse = ",")))
      }
  })

  observeEvent(input$size.confirm, {
      if (input$timeline.type == "stream") {
          sizes$window.sizes <- unique(c(sizes$window.sizes, input$num.window.input))
          output$size.recorded <- renderText(paste("Window Sizes:", paste(sizes$window.sizes, collapse = ",")))
      }
      else {
          sizes$chunk.sizes <- unique(c(sizes$chunk.sizes, input$num.chunk.input))
          output$size.recorded <- renderText(paste("Chunk Sizes:", paste(sizes$chunk.sizes, collapse = ",")))
      }
  })
  
  observeEvent(input$size.reset, {
      if (input$timeline.type == "stream") {
          sizes$window.sizes <- c()
          output$size.recorded <- renderText(paste("Window Sizes:", paste(sizes$window.sizes, collapse = ",")))
      }
      else {
          sizes$chunk.sizes <- c()
          output$size.recorded <- renderText(paste("Chunk Sizes:", paste(sizes$chunk.sizes, collapse = ",")))
      }
  })
  
  observeEvent(input$run.timeline, {
      sizes.current <- if (input$timeline.type == "stream") paste0(sizes$window.sizes, collapse = ',') else paste((match(input$select.chunk.attribute, names(data.table$data)) - 1), paste0(sizes$chunk.sizes, collapse = ','), sep = ",")
      subset.lengths <- if (input$timeline.type == "stream") paste0(subset.lengths$window.lengths, collapse = ",") else paste0(subset.lengths$chunk.lengths, collapse = ",")
      withProgress(message = "Running Analysis...", {
          cmd <- paste("java", "-jar", "../MarTVarD.jar", input$timeline.type, 
                                   paste(subset.lengths, collapse = ","), sizes.current, 
                                   paste(data.out.foler, data.table$name, sep = "/"), data.table$paths)
          print(cmd)
          ret <- system2("java", c("-jar", "../MarTVarD.jar", input$timeline.type, 
                                   paste(subset.lengths, collapse = ","), sizes.current, 
                                   paste(data.out.foler, data.table$name, sep = "/"), data.table$paths))
      })
  })
  
  # --------- Timeline plotter
  
  output$timeline.plot.data <- renderUI({
      selectInput("select.timeline.plot.data", "Data Timeline Result", list.files(path = data.out.foler))
  })
  
  output$timeline.plot.type <- renderUI({
      if (input$select.timeline.plot.data == "") {
          return()
      }
      types <- list.files(path = paste(data.out.foler, input$select.timeline.plot.data, sep = "/"))
      types <- types[! types %in% "analyse"]
      selectInput("select.timeline.plot.type", "Timeline Type", types)
  })
  
  output$timeline.plot.drift.type <- renderUI({
      if (input$select.timeline.plot.type == "") {
          return()
      }
      selectInput("select.timeline.plot.drift.type", "Drift Measurement Type",
                  unique(sapply(
                      list.files(paste(data.out.foler,
                                       input$select.timeline.plot.data,
                                       input$select.timeline.plot.type, sep = "/"
                                       )),
                      function(x) strsplit(x, split = "_")[[1]][1])))
  })
  
  output$timeline.plot.sizes <- renderUI({
      if (input$select.timeline.plot.type == "") {
          return()
      }
      selectInput("select.timeline.plot.sizes", "Window/Chunk Size",
                  unique(sapply(
                      list.files(paste(data.out.foler,
                                       input$select.timeline.plot.data,
                                       input$select.timeline.plot.type, sep = "/"
                                       )),
                      function(x) strsplit(x, split = "_")[[1]][2])))
  })
  
  output$timeline.plot.subset.length <- renderUI({
      if (input$select.timeline.plot.type == "") {
          return()
      }
      selectInput("select.timeline.plot.subset.length", "Attribute Subset Length",
                  unique(sapply(
                      list.files(paste(data.out.foler,
                                       input$select.timeline.plot.data,
                                       input$select.timeline.plot.type, sep = "/"
                                       )),
                      function(x) strsplit(x, split = "[_.]")[[1]][3])),
                  multiple = TRUE)
  })
  
  observeEvent(input$timeline.plot.run, {
      output$timeline.plot.plot <- renderPlotly(PlotWindowSize(input$select.timeline.plot.drift.type, 
                                                             input$select.timeline.plot.sizes, 
                                                             input$select.timeline.plot.subset.length, 
                                                             directory = paste(data.out.foler, 
                                                                               input$select.timeline.plot.data, 
                                                                               input$select.timeline.plot.type, 
                                                                               sep = "/"),
                                                             c()))
  })
  
  # --------- Compare / Analyse
  output$start.index <- renderUI({
      if (is.null(data.table)) {
          return()
      }
      numericInput("numeric.start.index", "Split Start Index", value = 0, min = 0, max = nrow(data.table$data), step = 1)
  })
  
  output$middle.index <- renderUI({
      if (is.null(data.table)) {
          return()
      }
      numericInput("numeric.middle.index", "Split Middle Index", value = nrow(data.table$data) / 2, min = 0, max = nrow(data.table$data), step = 1)
  })
  
  output$end.index <- renderUI({
      if (is.null(data.table)) {
          return()
      }
      numericInput("numeric.end.index", "Split End Index", value = nrow(data.table$data) - 1, min = 0, max = nrow(data.table$data), step = 1)
  })
  
  observeEvent(input$analyse.plot.run, {
      if (is.null(data.table)) {
          return()
      }
      else if (input$analyse.type == "compare") {
          shinyjs::show("analysis.plot")
          output$analysis.plot <- renderPlot(Histogram(data.table$data[seq(input$numeric.start.index, (input$numeric.middle.index - 1)), ], 
                                                       data.table$data[seq(input$numeric.middle.index, input$numeric.end.index), ]))
      }
      else {
          shinyjs::hide("analysis.plot")
          analyse.folder <- paste(data.out.foler, data.table$name, "analyse", sep = "/")
          if (length(list.files(analyse.folder)) == 0) {
              withProgress(message = "Running Analysis...", {
                  ret <- system2("java", c("-jar", "../MarTVarD.jar", "analyse",
                                           "1,2", paste(input$numeric.start.index + 1, input$numeric.middle.index, input$numeric.end.index, sep = ","), 
                                           paste(data.out.foler, data.table$name, sep = "/"), data.table$paths))         
                  })
          }
          drift.type <- input$analyse.drift.type
          if (drift.type == "COVARIATE") {
              results.cov.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_covariate.csv", sep = "/"))
              results.cov.2 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/2-attributes_covariate.csv", sep = "/"))
              output$analysis.2att.plot <- renderPlotly(VisualPairAttributes(results.cov.1, results.cov.2, drift.type = "Covariate"))
          }
          else if (drift.type == "JOINT") {
              results.joint.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_joint.csv", sep = "/"))
              results.joint.2 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/2-attributes_joint.csv", sep = "/"))
              output$analysis.2att.plot <- renderPlotly(VisualPairAttributes(results.joint.1, results.joint.2, drift.type = "Joint"))
          }
          else if (drift.type == "POSTERIOR") {
              results.pos.d.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_posterior_detailed.csv", sep = "/"))
              results.pos.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_posterior.csv", sep = "/"))
              results.pos.2 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/2-attributes_posterior.csv", sep = "/"))
              output$analysis.2att.plot <- renderPlotly(VisualPairAttributes(results.pos.1, results.pos.2, drift.type = "Posterior"))
              output$analysis.detailed.plot <- renderPlotly(VisualSingleAttributeStructure(results.pos.d.1, "Posterior"))
          }
          else if (drift.type == "LIKELIHOOD") {
              results.like.d.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_likelihood_detailed.csv", sep = "/"))
              results.like.1 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/1-attributes_likelihood.csv", sep = "/"))
              results.like.2 <- ResultTable(paste(data.out.foler, data.table$name, "analyse/2-attributes_likelihood.csv", sep = "/"))
              output$analysis.2att.plot <- renderPlotly(VisualPairAttributes(results.like.1, results.like.2, drift.type = "Likelihood"))
              output$analysis.detailed.plot <- renderPlotly(VisualSingleLikelihoodAttribute(results.like.d.1))
          }
      }
  })
})


