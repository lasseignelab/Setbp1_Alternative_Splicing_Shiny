library(shiny)
library(shinyjs)
library(shinycssloaders)

welcome_about <- tabPanel(
  title = "Welcome and About",

  "Welcome!!"
)

plots <- tabPanel(
  title = "Gene Expression and Splice Junction Usage UMAPs",

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "gene",
        "Gene",
        choices = NULL,
        options = list(
          maxOptions = 20,
          maxItems = 1,
          placeholder = "Loading..."
        )
      ),
      helpText("Search for a gene to plot gene expression."),
      shinyjs::hidden(
        div(id = "spliceJunctionInput",
          selectizeInput(
            "splice_junction",
            "Splice Junction",
            choices = NULL,
            options = list(
              maxItems = 1,
              placeholder = "Loading..."
            )
          ),
          helpText(
            "Select a splice junction related to the selected gene to",
            "plot splice junction usage."
          )
        )
      ),
      actionButton("plot", "Show plots")
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

ui <- navbarPage(
  useShinyjs(),
  title = "Alternative Splicing in Schinzel-Giedion Syndrome",
  welcome_about,
  plots
)
