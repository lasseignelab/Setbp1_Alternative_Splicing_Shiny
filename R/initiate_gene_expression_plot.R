initiate_gene_expression_plot <- function(session, plot_click, plot_type) {
  initialize_last_plot_click(session)

  new_plot_click <- plot_click != session$userData$last_plot_click
  show_gene_expression <- plot_type == "gene_expression"
  if (new_plot_click && show_gene_expression) {
    session$userData$last_plot_click <- plot_click
    plot_click
  } else {
    NULL
  }
}
