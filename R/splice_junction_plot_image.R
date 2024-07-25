splice_junction_plot_image <- function(data_file, data_name, splice_junction) {

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

  plot <- plot +
    labs(
      title = glue("{data_name} SJU for {splice_junction}"),
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
