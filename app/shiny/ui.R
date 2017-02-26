library(shiny)
library(shinyjs)
library(markdown)
library(plotly)

loadDataFile <- sidebarLayout(
  sidebarPanel(
    useShinyjs(),
    fileInput("file.arff", "Arff data file", multiple = TRUE, accept = ".arff") ,
    hr(),
    wellPanel(id = "timeline.panel",
        
    selectInput("timeline.type", 
                "Type of Drift Timeline", 
                c("Fixed Window Stream" = "stream", 
                  "Chunks" = "stream_chunk",
                  "Moving Chunk" = "moving_chunk")
                ),
    uiOutput("attribute.subset.length"),
    textOutput("timeline.description"),
    hr(),
    uiOutput("chunk.attribute"),
    textOutput("chunk.attribute.num.vals"),
    uiOutput("window.input"),
    hr(),
    uiOutput("chunk.input"),
    fluidRow(
        column(6, actionButton("size.confirm", label = "Add Size")),
        column(6, actionButton("size.reset", label = "Reset Sizes"))
        ),
    textOutput("size.recorded"),
    actionButton("run.timeline", label = "Run Timeline Analysis")
    )
    ),
  mainPanel(
      #includeHTML("../Introduction.html")
      dataTableOutput("result.files")
    )
  )

analysePage <- fluidPage(verticalLayout(
))

timelinePlotPage <- fluidPage(verticalLayout(
    useShinyjs(),
    wellPanel(id = "timeline.plot",
              fluidRow(
                  column(6, 
                         uiOutput("timeline.plot.data"),
                         uiOutput("timeline.plot.type")
                         ),
                  column(6,
                         uiOutput("timeline.plot.drift.type"),
                         uiOutput("timeline.plot.sizes"),
                         uiOutput("timeline.plot.subset.length"),
                         uiOutput("timeline.plot.attributes")
                         )
              ),
              actionButton("timeline.plot.run", "Plot Timeline")),
    plotlyOutput("timeline.plot.plot"),
    wellPanel(id = "analyse.plot",
              selectInput("analyse.type", 
                          "Select Analyse Type", 
                          c("Drift Analysis" = "analysis", "Compare Distributions" = "compare")),
              fluidRow(
                  column(4, uiOutput("start.index")),
                  column(4, uiOutput("middle.index")),
                  column(4, uiOutput("end.index"))
              ),
              selectInput("analyse.drift.type", 
                          "Select Drift Type",
                          c("COVARIATE", "JOINT", "LIKELIHOOD", "POSTERIOR")),
              actionButton("analyse.plot.run", "Run Analysis")),
    plotOutput("analysis.plot"),
    column(8,
    plotlyOutput("analysis.2att.plot"),
    plotlyOutput("analysis.detailed.plot")
    ),
    textOutput("debug")
))

shinyUI(fluidPage(
  navbarPage("Spectra Analysis",
             tabPanel("Data Selection", loadDataFile),
             tabPanel("Analyse", timelinePlotPage)
)
))


