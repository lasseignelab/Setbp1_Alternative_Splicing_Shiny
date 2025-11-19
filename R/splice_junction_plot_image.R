################################################################################
# Create an image of a splice junction usage plot.
#
# In order to stay inside the shinyapps.io memory limit this function reads the
# data file, creates the plot as an image, and cleans everything up so that a
# minimal amount of memory is used.
#
# @param data_file Name of the file to create the plot from.
# @param data_name Name of the data to put in the plot title, i.e. Wildtype.
# @param tissue Tissue name for the data to plot.
# @param splice_junction Splice junction name to filter the data for the plot.
# @return The name of the image file generated.
################################################################################

splice_junction_plot_image <- function(data_file, data_name, tissue, splice_junction) {

  gc()
  setbp1 <- readRDS(data_file)
  plot <- PlotValues.PCA.PSI.10x(
    MarvelObject = setbp1,
    coord.intron = splice_junction,
    min.gene.count = 3,
    color.gradient = plasma(5),
    log2.transform = FALSE,
    type = "umap"
  )
  setbp1 <- NULL

  title_tissue <- toTitleCase(tissue)
  plot <- plot +
    labs(
      title = glue("{data_name} {title_tissue} SJU for {splice_junction}"),
      color = "SJU\nper\nCell"
    ) +
    ggplot_theme()

  outfile <- tempfile(fileext = ".png")
  png(outfile, height = 5, width = 6, units = "in", res = 300)
  print(plot)
  dev.off()
  plot <- NULL
  gc()

  return(outfile)
}
