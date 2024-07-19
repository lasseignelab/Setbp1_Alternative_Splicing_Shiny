gene_expression_plot_image <- function(data_file, data_name, gene) {
  gc()
  setbp1 <- readRDS(data_file)
  plot <- PlotValues.PCA.Gene.10x(
    MarvelObject=setbp1,
    gene_short_name=gene,
    color.gradient=viridis(5),
    log2.transform = FALSE,
    type="umap"
  )
  setbp1 <- NULL

  plot <- plot +
    labs(title = glue("{data_name} Gene Expression for {gene}")) +
    ggplotTheme()

  outfile <- tempfile(fileext = ".png")
  png(outfile, height = 5, width = 6, units = "in", res = 300)
  print(plot)
  dev.off()
  plot <- NULL
  gc()

  return(outfile)
}
