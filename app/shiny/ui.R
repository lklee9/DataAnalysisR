library(shiny)
library(shinyjs)
library(markdown)

databaseConnect <- sidebarLayout(
  sidebarPanel(
    useShinyjs(),
    selectInput("input.type", "Data Input Type:", 
                choices = c("Database Files"="db", "CSV Data File"="csv")),
    fileInput("file.databases", "Databse Files", multiple = TRUE, accept = ".db"),
    fileInput("file.csv.data", "CSV Data File", multiple = FALSE, accept = ".csv"),
    fileInput("file.csv.metadata", "CSV Metadata File", multiple = FALSE, accept = ".csv"),
  
    hr(),
    fluidRow(
      column(6, numericInput("handoff.min", "Handoff Min", 0, min = 0)),
      column(6, numericInput("handoff.max", "Handoff Max", 900, min = 0))
    ),
    textOutput("data.config.help"),
    selectInput("measure.type", "Measurement", c("StepSize", "Slope")),
    selectInput("value.type", "Type of Value", c("Transmission" = "tra", 
                                                 "Absorption" = "abs", 
                                                 "Reference" = "ref", 
                                                 "Sample" = "sam")),
    actionButton("load.data", "Load Data"),
    hr(),
    textOutput("connection.status")
    ),
  mainPanel(
    includeHTML("../Introduction.html")
    )
  )

loadDataFile <- sidebarLayout(
  sidebarPanel(
    useShinyjs(),
    fileInput("file.arff", "Arff data file", multiple = TRUE, accept = ".arff"),
  
    hr(),
    fluidRow(
      column(6, numericInput("handoff.min", "Handoff Min", 0, min = 0)),
      column(6, numericInput("handoff.max", "Handoff Max", 900, min = 0))
    ),
    textOutput("data.config.help"),
    selectInput("measure.type", "Measurement", c("StepSize", "Slope")),
    selectInput("value.type", "Type of Value", c("Transmission" = "tra", 
                                                 "Absorption" = "abs", 
                                                 "Reference" = "ref", 
                                                 "Sample" = "sam")),
    actionButton("load.data", "Load Data"),
    hr(),
    textOutput("connection.status")
    ),
  mainPanel(
    includeHTML("../Introduction.html")
    )
  )

mdsPage <- fluidPage(verticalLayout(
  useShinyjs(),
  plotOutput("mds", height = 500),
  hr(),
  wellPanel(
    uiOutput("slider.wl.mds"),
    fluidRow(
      column(8,
             uiOutput("selectize.test.mds")
             ),
      column(4,
             uiOutput("radio.type.mds"),
             uiOutput("selectize.meta.mds"),
             uiOutput("number.clusters.mds")
             )
      )
  )
))

comparePage <- fluidPage(
  verticalLayout(
  useShinyjs(),
  plotOutput("comp.spec", height = 500),
  hr(),
  wellPanel(
    uiOutput("slider.wl.spec"),
    fluidRow(
      column(6,
             selectInput("value.y", "y Value", 
                         c("p-value" = "pv", 
                           "standard deviation" = "sd",
                           "outlier score" = "os")),
             uiOutput("selectize.test.base")
             ),
      column(6,
             selectInput("value.colour", "Weight Value", 
                         c("p-value" = "pv", 
                           "standard deviation" = "sd", 
                           "outlier score" = "os")),
             uiOutput("selectize.test.query")
             )
      )
    )
  ))

spectraPage <- fluidPage(verticalLayout(
  useShinyjs(),
  plotOutput("spectra.plot", height = 500),
  hr(),
  wellPanel(
    uiOutput("slider.wl.all.spec"),
    uiOutput("selectize.test.spec")
  )
))

helpPage <- fluidPage(
  titlePanel("User Documentation"),
  mainPanel(
    includeHTML("../User Documentation.html")
  )
)

shinyUI(fluidPage(
  navbarPage("Spectra Analysis",
                   tabPanel("Data Selection", databaseConnect),
                   tabPanel("Tests Overview", mdsPage),
                   tabPanel("Comapre Spectra", comparePage),
                   tabPanel("View Spectra", spectraPage),
                   navbarMenu("More",
                              tabPanel("About", includeHTML("../About.html")),
                              tabPanel("Help", helpPage))
)
))


