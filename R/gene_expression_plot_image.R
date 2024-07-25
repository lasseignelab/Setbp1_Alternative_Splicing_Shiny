################################################################################
# Create an image of a gene expression plot.
#
# In order to stay inside the shinyapps.io memory limit this function reads the
# data file, creates the plot as an image, and cleans everything up so that a
# minimal amount of memory is used.
#
# @param data_file Name of the file to create the plot from.
# @param data_name Name of the data to put in the plot title, i.e. Wildtype
# @param gene Gene name of filter the data for the plot.
# @return The name of the image file generated.
################################################################################

gene_expression_plot_image <- function(data_file, data_name, gene) {
  gc()
  setbp1 <- readRDS(data_file)
  plot <- PlotValues.PCA.Gene.10x(
    MarvelObject = setbp1,
    gene_short_name = gene,
    color.gradient = viridis(5),
    log2.transform = FALSE,
    type = "umap"
  )
  setbp1 <- NULL

  plot <- plot +
    labs(title = glue("{data_name} Gene Expression for {gene}")) +
    ggplot_theme()

  outfile <- tempfile(fileext = ".png")
  png(outfile, height = 5, width = 6, units = "in", res = 300)
  print(plot)
  dev.off()
  plot <- NULL
  gc()

  return(outfile)
}
