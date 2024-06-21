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
          plotOutput("cellTypePlot"),
          plotOutput("wildtypeGeneExpressionPlot"),
          plotOutput("mutantGeneExpressionPlot")
        )
    )
)
