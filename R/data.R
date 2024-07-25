library(tidyverse)
library(plyr)
library(ggplot2)
library(Matrix)

metadata <- function(setbp1) {
  data <- setbp1
  data$sample.metadata <- NULL
  data$pca <- NULL
  data$gene.norm.matrix <- NULL
  data$gene.count.matrix <- NULL
  data$sj.count.matrix <- NULL
  saveRDS(data, file = "data/setbp1_marvel_aligned_metadata.rds")

}

wildtype_setbp1 <- function(setbp1) {
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
  saveRDS(gene_data, file = "data/setbp1_marvel_aligned_wildtype_gene.rds")

  splice_junction_data <- data
  splice_junction_data$gene.norm.matrix <- NULL
  saveRDS(
    splice_junction_data,
    file = "data/setbp1_marvel_aligned_wildtype_sj.rds"
  )
}

mutant_setbp1 <- function(setbp1) {
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
  saveRDS(gene_data, file = "data/setbp1_marvel_aligned_mutant_gene.rds")

  splice_junction_data <- data
  splice_junction_data$gene.norm.matrix <- NULL
  saveRDS(
    splice_junction_data,
    file = "data/setbp1_marvel_aligned_mutant_sj.rds"
  )
}

prepare_data <- function(filename) {
  print("*** Loading MARVEL data file.")
  setbp1 <- readRDS(filename)

  setbp1$gtf <- NULL

  print("*** Saving MARVEL data sans gtf for cell type plot rendering.")
  saveRDS(
    setbp1,
    file = paste0(dirname(filename), "/setbp1_marvel_aligned_sans_gtf.rds")
  )

  print("*** Saving metadata file.")
  metadata(setbp1)

  print("*** Removing unused metadata.")
  setbp1$sj.metadata$gene_short_name.end <- NULL
  setbp1$sj.metadata$sj.type <- NULL
  setbp1$gene.metadata <- NULL

  print("*** Creating wild-type files.")
  wildtype_setbp1(setbp1)
  print("*** Creating mutant files.")
  mutant_setbp1(setbp1)

  print("*** Done.")
}
