print(paste("pid =", Sys.getpid()))

options(shiny.autoreload = TRUE)
options(shiny.trace=TRUE)
shiny::runApp()
