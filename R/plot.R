################################################################################
# This plotting script pre-renders the cell type plot.  The plot can be
# pre-rendered because it always has all the cells and is not filtered so it
# never changes.  It needed to be pre-rendered because it would exceed the
# shinyapps.io memory limit.
#
# The data transformation from data.R needs to be ran first to generate the
# setbp1_marvel_aligned_sans_gtf.rds file this script uses.
#
# Example usage in R console:
#   > source("R/plot.R")
#   > prerender_plots("brain", "original_data/setbp1_marvel_aligned_sans_gtf.rds")
#
################################################################################

library(tidyverse)
library(MARVEL)
library(here)
library(tools)

prerender_cell_type_plot <- function(tissue, data) {

  # The following cell_group_list code is based on code authored by Emma Jones.
  # 230926_EJ_Setbp1_AlternativeSplicing/src/marvel/03_analyze_de_genes.Rmd

  # Pull cell types and matching ids
  cell_group_list <- split(
    data$sample.metadata$cell.id,
    data$sample.metadata$cell_type
  )

  if (tissue == "brain") {
    # Set colors to match those used in the paper.
    cell_type_colors <- c(
      `Astrocytes` = "#6CA9E2",
      `Excitatory Neurons` = "#98D070",
      `Inhibitory Neurons` = "#DEE971",
      `Microglia` = "#B898E4",
      `Oligodendrocytes` = "#4AD8E6",
      `OPCs` = "#0A9A8D",
      `Vascular Cells` = "#E28C67"
    )
    point_size_legend = 7
    legend_text_size = 11
  } else {
    cell_type_colors <- grDevices::rainbow(length(cell_group_list))
    point_size_legend = 3
    legend_text_size = 7
  }

  png(
    paste0("www/", tissue, "_cell_types.png"),
    height = 4,
    width = 6,
    units = "in",
    res = 300
  )
  plot <- PlotValues.PCA.CellGroup.10x(
    MarvelObject = data,
    cell.group.list = cell_group_list,
    point.colors = cell_type_colors,
    point.size.legend = point_size_legend,
    legendtitle = "Cell group",
    type = "umap"
  )
  plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup +
    labs(title = toTitleCase(paste(tissue, "Cell Types"))) +
    theme(
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = legend_text_size),
      legend.key.size = unit(0.3, "lines"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.box = "vertical",
      legend.position = "right",
      axis.text = element_text(size = 11)
    )
  plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup +
    guides(
      color = guide_legend(
        ncol = 1,
        byrow = TRUE,
        override.aes = list(size = point_size_legend)
      ),
      fill = guide_legend(
        ncol = 1,
        byrow = TRUE,
        override.aes = list(size = point_size_legend)
      )
    )

  print(plot$adhocPlot$PCA$CellGroup)
  dev.off()
}

prerender_plots <- function(tissue, filename) {
  data <- readRDS(filename)
  prerender_cell_type_plot(tissue, data)
}
