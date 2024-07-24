initiate_gene_expression_plot <- function(session, plot_click, plot_type) {
  # Initialize the session variable to prevent NULL error
  session$userData$last_gene_plot_click <- first(c(session$userData$last_gene_plot_click, ""))

  new_plot_click <- plot_click != session$userData$last_gene_plot_click
  show_gene_expression <- plot_type == "gene_expression"
  if (new_plot_click & show_gene_expression) {
    session$userData$last_gene_plot_click <- plot_click
    plot_click
  } else {
    NULL
  }
}
