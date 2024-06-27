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
        div(id = "splice_junction_input",
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
      shinycssloaders::withSpinner(plotOutput("cell_type_plot"))
    )
  ),
  shinyjs::hidden(
    div(id = "gene_expression_plots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtype_gene_expression_plot")
          ),
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutant_gene_expression_plot")
          )
        )
      )
    )
  ),
  shinyjs::hidden(
    div(id = "splice_junction_plots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtype_splice_junction_plot")
          )
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutant_splice_junction_plot")
          )
        )
      )
    )
  )
)

as_gene_summary <- tabPanel(
  title = "Alternative Splicing Summaries",

  selectizeInput(
    "as_summary_gene",
    "Gene",
    choices = NULL,
    options = list(
      allowEmptyOption = TRUE,
      showEmptyOptionInDropdown = TRUE,
      maxItems = 1,
      placeholder = "Loading..."
    )
  ),
  uiOutput("as_gene_summary_image")
)

ui <- navbarPage(
  useShinyjs(),
  title = "Alternative Splicing in Schinzel-Giedion Syndrome",
  welcome_about,
  plots,
  as_gene_summary
)
