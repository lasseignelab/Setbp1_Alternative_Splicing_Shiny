initiate_splice_junction_plot <- function(session, plot_click, plot_type) {
  # Initialize the session variable to prevent NULL error
  session$userData$last_sj_plot_click <- first(c(session$userData$last_sj_plot_click, ""))

  new_plot_click <- plot_click != session$userData$last_sj_plot_click
  show_splice_junction_usage <- plot_type == "splice_junction_usage"
  if (new_plot_click & show_splice_junction_usage) {
    session$userData$last_sj_plot_click <- plot_click
    plot_click
  } else {
    NULL
  }
}
