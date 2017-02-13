library(shiny)
library(shinyjs)
source("../core/read_database.R")
source("../core/read_csv.R")
source("../core/multidimensional_scaling.R")
source("../core/compare_spectra.R")
source("../core/plot_spectra.R")

# Increase file size limit
options(shiny.maxRequestSize=100*1024^2)

# Variables persistent throughout application lifetime -------------------------

measurement = "StepSize"
folder_name <- "../data"

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
  # TODO: Use NULL instead of NA
  spectra <- reactiveValues(data = NULL, metadata = NULL, 
                            time.series.name = NULL, value.name = NULL)
  
  observe({
    shinyjs::toggleState("database.ip", input$is.local != TRUE)
    shinyjs::toggleState("database.name", input$is.local != TRUE)
    shinyjs::toggleState("user.name", input$is.local != TRUE)
    shinyjs::toggleState("user.pass", input$is.local != TRUE)
    shinyjs::toggle("file.databases", condition = input$input.type == "db")
    shinyjs::toggle("file.csv.data", condition = input$input.type == "csv")
    shinyjs::toggle("file.csv.metadata", condition = input$input.type == "csv")
    
    shinyjs::toggle("handoff.min", condition = input$input.type == "db")
    shinyjs::toggle("handoff.max", condition = input$input.type == "db")
    shinyjs::toggle("measure.type", condition = input$input.type == "db")
    shinyjs::toggle("value.type", condition = input$input.type == "db")
    shinyjs::toggleState("load.data", 
                         (!is.null(input$file.databases) && 
                            input$input.type == "db") ||
                           (!is.null(input$file.csv.data) && 
                              input$input.type == "csv")
                         )
    if (input$input.type == "db") {
      output$data.config.help <- renderText("*Wavelengths in this range will use 
                                            UV values")
    } else {
      output$data.config.help <- renderText("*No configuration needed 
                                            to load dataset")
    }
    shinyjs::toggle("value.y", condition = !is.null(spectra$data))
    shinyjs::toggle("value.colour", condition = !is.null(spectra$data))
  })

  output$connection.status <- renderText("No data loaded...")
  
  # Temporary load data function
  observeEvent(input$load.data, {
    if (input$input.type == "db") {
      file.input <- input$file.databases
      withProgress(message = "Loading Data...", {
      Sys.sleep(0.25)
      spectra$time.series.name <- "wavelength"
      spectra$value.name <- input$value.type
      spectra$metadata <- obtain_metadata(filepaths = file.input$datapath,
                                          measurement = measurement, 
                                          test.ids = file.input$name)
      incProgress(0.5, detail = "Metadata Loaded")
      spectra$data <- obtain_spectra(filepaths = file.input$datapath,
                                    test.ids = file.input$name,
                                    measurement = input$measure.type,
                                    value.type = input$value.type,
                                    handover.range = c(input$handoff.min, 
                                                        input$handoff.max)
                                    )
      setProgress(1, detail = "Spectra Loaded")
      output$connection.status <- renderText("Data Loaded.")
      })
    } else {
      file.input.data <- input$file.csv.data
      file.input.metadata <- input$file.csv.metadata
      withProgress(message = "Loading CSV file...", {
        Sys.sleep(0.25)
        spectra$time.series.name <- strsplit(extract_name(file.input.data$name),
                                             "_")[[1]][1]
        spectra$value.name <- "values"
        spectra$data <- loadDataFile(file.input.data$datapath)
        setProgress(0.5, detail = "Data Loaded")
        if (is.null(file.input.metadata)) {
          spectra$metadata <- data.frame(row.names = row.names(spectra$data))
        } else {
          spectra$metadata <- loadDataFile(file.input.metadata$datapath)
        }
        incProgress(1, detail = "Metadata Loaded")
        output$connection.status <- renderText("Data Loaded.")
      })
    }
    
  })
  

  # Slider creation in MDS page ------------------------------------------------
  output$slider.wl.mds <- renderUI({
    if (is.null(spectra$data)) {
      return("No data available...")
    }
    # Slider based on max and min of wavelength in spectra.all
    sliderInput("slider.wl", "Time Series Range",
                min = min(as.numeric(names(spectra$data))),
                max = max(as.numeric(names(spectra$data))),
                value = c(
                  min(as.numeric(names(spectra$data))),
                  max(as.numeric(names(spectra$data)))
                  ),
                step = 10
                )
    })
  
  # MDS type selection
  output$radio.type.mds <- renderUI({
    if (is.null(spectra$data) || is.null(spectra$metadata)) {
      return()
    }
    radioButtons("type.mds", "MDS Type", 
                 c("Categorical"="categ", 
                   "Color Scale"="scale", 
                   "Clustering"="clust")
                 )
  })
  
  # Selectize meta input creation in MDS page ----------------------------------
  output$selectize.meta.mds <- renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    # Selectize input based on metadata
    selectInput("select.metadata", "Metadata Available",
                c("row.id", names(spectra$metadata)), 
                multiple = FALSE, selectize = TRUE)
    })
  
  # Number clusters creation in MDS --------------------------------------------
  output$number.clusters.mds <- renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    numericInput("select.k", "Number of clusters", 
                 value = 2, min = 2, max = length(input$select.test) - 1)
  })
  
  # Hide or show clusters or meta input
  observeEvent(input$type.mds, {
    if (length(input$type.mds) == 0) {
      shinyjs::hide(id = "number.clusters.mds")
      shinyjs::hide(id = "selectize.meta.mds")
    } else if (input$type.mds == "clust") {
      shinyjs::show(id = "number.clusters.mds")
      shinyjs::hide(id = "selectize.meta.mds")
    } else {
      shinyjs::hide(id = "number.clusters.mds")
      shinyjs::show(id = "selectize.meta.mds")
    }
  })
  
  # Selectize test input creation ----------------------------------------------
  output$selectize.test.mds <- renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    # Selectize input based on available tests
    selectInput("select.test", "Select Rows (at least 3)", row.names(spectra$metadata),
                multiple = TRUE, selectize = TRUE
                )
  })
  
  # MDS plot creation ----------------------------------------------------------
  output$mds <- renderPlot({
    if (length(input$select.test) < 3) {
      return()
    } else if (input$type.mds == "clust") {
      if (!is.numeric(input$select.k)) {
        return("Please give number of clusters to divide data to")
      } else if (input$select.k < 1 || input$select.k > length(input$select.test) - 1) {
        return("Please give k in range of 1 and number of tests - 1")
      }
    }
    classical_MDS(
      spectra = selectTests(selectWavelengths(spectra$data, input$slider.wl),
                            input$select.test), 
      labels = (if(input$select.metadata == "row.id") input$select.test 
                else selectTests(spectra$metadata, 
                                 input$select.test)[, input$select.metadata]),
      cluster.number = input$select.k,
      type = input$type.mds
      )
    })
  
  # Compare spectra ------------------------------------------------------------
  # Wavelength Slider
  output$slider.wl.spec <- renderUI({
    if (is.null(spectra$data)) {
      return("No data available...")
    }
    # Slider based on max and min of wavelength in spectra.all
    sliderInput("slider.wl.spec", "Time Series Range",
                min = min(as.numeric(names(spectra$data))),
                max = max(as.numeric(names(spectra$data))),
                value = c(
                  min(as.numeric(names(spectra$data))),
                  max(as.numeric(names(spectra$data)))
                  ),
                step = 10
                )
    })
  
  # Base test slection
  output$selectize.test.base  <- renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    # Selectize input based on available tests
    selectInput("select.test.base", "Select Base Rows (at least 2)", 
                row.names(spectra$metadata),
                multiple = TRUE, selectize = TRUE
                )
  })
  
  output$selectize.test.query  <- renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    # Selectize input based on available tests
    selectInput("select.test.query", "Select Query Rows (at least 1)",
                row.names(spectra$metadata),
                multiple = TRUE, selectize = TRUE
                )
  })
  
  # Compare spectra plot
  output$comp.spec <- renderPlot({
    if (length(input$select.test.base) < 2 || length(input$select.test.query) < 1) {
      return()
    }
    plot_all(
      base.data = selectTests(
        selectWavelengths(spectra$data, input$slider.wl.spec),
        input$select.test.base
        ),
      query.data = selectTests(
        selectWavelengths(spectra$data, input$slider.wl.spec),
        input$select.test.query
        ),
      time.series.name = spectra$time.series.name,
      data.type = spectra$value.name,
      y.type = input$value.y,
      weight.type = input$value.colour
    )
    })
  
  # All spectra page -----------------------------------------------------------
  output$slider.wl.all.spec <- renderUI({
    if (is.null(spectra$data)) {
      return("No data available...")
    }
    # Slider based on max and min of wavelength in spectra.all
    sliderInput("slider.wl.all.spec", "Time Series Range",
                min = min(as.numeric(names(spectra$data))),
                max = max(as.numeric(names(spectra$data))),
                value = c(
                  min(as.numeric(names(spectra$data))),
                  max(as.numeric(names(spectra$data)))
                  ),
                step = 10
                )
    })
  
  output$selectize.test.spec <-  renderUI({
    if (is.null(spectra$metadata)) {
      return()
    }
    # Selectize input based on available tests
    selectInput("select.test.spec", "Select Rows", row.names(spectra$metadata),
                multiple = TRUE, selectize = TRUE
                )
  })
  
  output$spectra.plot <- renderPlot({
    if (length(input$select.test.spec) < 1) {
      return()
    }
    plotAllSpectra(
      spectra.df = selectTests(
      selectWavelengths(spectra$data, input$slider.wl.all.spec), 
      input$select.test.spec),
      data.name = spectra$value.name,
      time.series.name = spectra$time.series.name
      )
  })
  
})


