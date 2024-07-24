library(tidyverse)
library(MARVEL)
library(here)

prerenderCellTypePlot <- function(data) {

  # The following cell_group_list code is based on code authored by Emma Jones.
  # 230926_EJ_Setbp1_AlternativeSplicing/src/marvel/03_analyze_de_genes.Rmd

  # Pull cell types and matching ids
  cell_group_list <- data$sample.metadata %>%
    group_split(cell_type, .keep = TRUE) %>%
    map(~ set_names(.$cell.id, .$cell_type[1]))

  # Name the cell groups
  cell_group_list <- set_names(cell_group_list, c(
    "Astrocytes", "Excitatory Neurons",
    "Inhibitory Neurons", "Microglia", "OPCs",
    "Oligodendrocytes", "Vascular Cells"
  ))

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

  png(
    paste0(here(), "/www/cell_types.png"),
    height = 4,
    width = 6,
    units = "in",
    res = 300
  )
  plot <- PlotValues.PCA.CellGroup.10x(
    MarvelObject = data,
    cell.group.list = cell_group_list,
    point.colors = cell_type_colors,
    point.size.legend = 7,
    legendtitle="Cell group",
    type = "umap"
  )
  plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup +
    labs(title = "Cell Types") +
    theme(
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 11)
    )
  print(plot$adhocPlot$PCA$CellGroup)
  dev.off()
}

prerenderPlots <- function(filename) {
  data <- readRDS(filename)
  prerenderCellTypePlot(data)
}
