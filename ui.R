library(shiny)
library(shinyjs)
library(shinycssloaders)

ui <- fluidPage(
  useShinyjs(),

  titlePanel("Alternative Splicing in Schinzel-Giedion Syndrome"),

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "gene",
        "Gene",
        choices = NULL,
        options = list(
          maxOptions = 5,
          maxItems = 1,
          placeholder = "Loading..."
        )
      ),
      shinyjs::hidden(
        selectizeInput(
          "splice_junction",
          "Splice Junction",
          choices = NULL,
          options = list(
            maxItems = 1,
            placeholder = "Loading..."
          )
        )
      )
    ),

    mainPanel(
      shinycssloaders::withSpinner(plotOutput("cellTypePlot"))
    )
  ),
  shinyjs::hidden(
    div(id = "geneExpressionPlots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtypeGeneExpressionPlot")
          ),
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutantGeneExpressionPlot")
          )
        )
      )
    )
  ),
  shinyjs::hidden(
    div(id = "spliceJunctionPlots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtypeSpliceJunctionPlot")
          )
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutantSpliceJunctionPlot")
          )
        )
      )
    )
  )
)
