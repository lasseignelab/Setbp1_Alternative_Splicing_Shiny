################################################################################
# This data processing script transforms a MARVEL data file from the paper
# into smaller files specific to the plots being generated.  This transformation
# was needed to overcome shinyapps.io limitations on disk space and memory.
#
# To run the data transformation, download the source files from the
# "/data/project/lasseigne_lab/DATASET_dir/setbp1_as_shiny/september2025"
# folder on Cheaha into the "original_data" folder in the project.  There
# are separate files for brain and kidney that will each need to be processed.
#
# Example usage in R console:
#   > source("R/data.R")
#   > prepare_data("brain", "original_data/setbp1_marvel_aligned.rds")
#
################################################################################

library(tidyverse)
library(plyr)
library(ggplot2)
library(Matrix)

metadata <- function(setbp1, output_file_base) {
  data <- setbp1
  data$sample.metadata <- NULL
  data$pca <- NULL
  data$gene.norm.matrix <- NULL
  data$gene.count.matrix <- NULL
  data$sj.count.matrix <- NULL
  saveRDS(data, file = paste0(output_file_base, "_metadata.rds"))

}

wildtype_setbp1 <- function(setbp1, output_file_base) {
  data <- setbp1
  data$sample.metadata <- data$sample.metadata %>%
    filter(seq_folder == "wildtype")
  data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
  data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
  data$gene.count.matrix <-
    data$gene.count.matrix[, data$sample.metadata$cell.id]
  data$sj.count.matrix <- data$sj.count.matrix[, data$sample.metadata$cell.id]

  data$sample.metadata <- NULL

  gene_data <- data
  gene_data$sj.count.matrix <- NULL
  gene_data$gene.count.matrix <- NULL
  gene_data$sj.metadata <- NULL
  saveRDS(gene_data, file = paste0(output_file_base, "_wildtype_gene.rds"))

  splice_junction_data <- data
  splice_junction_data$gene.norm.matrix <- NULL
  saveRDS(
    splice_junction_data,
    file = paste0(output_file_base, "_wildtype_sj.rds")
  )
}

mutant_setbp1 <- function(setbp1, output_file_base) {
  data <- setbp1
  data$sample.metadata <- data$sample.metadata %>%
    filter(seq_folder == "mutant")
  data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
  data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
  data$gene.count.matrix <-
    data$gene.count.matrix[, data$sample.metadata$cell.id]
  data$sj.count.matrix <- data$sj.count.matrix[, data$sample.metadata$cell.id]

  data$sample.metadata <- NULL

  gene_data <- data
  gene_data$sj.count.matrix <- NULL
  gene_data$gene.count.matrix <- NULL
  gene_data$sj.metadata <- NULL
  saveRDS(gene_data, file = paste0(output_file_base, "_mutant_gene.rds"))

  splice_junction_data <- data
  splice_junction_data$gene.norm.matrix <- NULL
  saveRDS(
    splice_junction_data,
    file = paste0(output_file_base, "_mutant_sj.rds")
  )
}

prepare_data <- function(tissue, filename) {
  message("*** Loading MARVEL data file.")
  setbp1 <- readRDS(filename)

  setbp1$gtf <- NULL

  message("*** Saving MARVEL data sans gtf for cell type plot rendering.")
  saveRDS(
    setbp1,
    file = paste0(dirname(filename), "/", tools::file_path_sans_ext(basename(filename)), "_sans_gtf.rds")
  )

  output_file_base <- paste0("data/setbp1_", tissue, "_marvel_aligned")

  message("*** Saving metadata file.")
  metadata(setbp1, output_file_base)

  message("*** Removing unused metadata.")
  setbp1$sj.metadata$gene_short_name.end <- NULL
  setbp1$sj.metadata$sj.type <- NULL
  setbp1$gene.metadata <- NULL

  message("*** Creating wild-type files.")
  wildtype_setbp1(setbp1, output_file_base)
  message("*** Creating mutant files.")
  mutant_setbp1(setbp1, output_file_base)

  message("*** Done.")
}
