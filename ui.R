ui <- fluidPage(

    # Application title
    titlePanel("Alternative Splicing in Schinzel-Giedion Syndrom"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          "Sidebar Panel"
        ),

        # Show a plot of the generated distribution
        mainPanel(
           "Main Panel"
        )
    )
)
