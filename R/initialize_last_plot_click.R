# Initialize the session variable to prevent NULL errors
initialize_last_plot_click <- function(session) {
  session$userData$last_plot_click <-
    first(c(session$userData$last_plot_click, ""))
}
