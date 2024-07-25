initiate_splice_junction_plot <- function(session, plot_click, plot_type) {
  initialize_last_plot_click(session)

  new_plot_click <- plot_click != session$userData$last_plot_click
  show_splice_junction_usage <- plot_type == "splice_junction_usage"
  if (new_plot_click && show_splice_junction_usage) {
    session$userData$last_plot_click <- plot_click
    plot_click
  } else {
    NULL
  }
}
