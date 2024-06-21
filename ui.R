ui <- fluidPage(
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
          )
        ),

        mainPanel(
          shinycssloaders::withSpinner(plotOutput("cellTypePlot")),
          shinycssloaders::withSpinner(plotOutput("wildtypeGeneExpressionPlot")),
          shinycssloaders::withSpinner(plotOutput("mutantGeneExpressionPlot"))
        )
    )
)
