library(shiny)
library(MARVEL)
library(tidyverse)

setbp1 <<- readRDS("./data/setbp1_marvel_aligned_reduced_size.rds")

gene_list <- c(c(""), setbp1$gene.metadata$gene_short_name %>% sort())

server <- function(input, output, session) {
  updateSelectizeInput(
    session,
    "gene",
    choices = gene_list,
    options = list(
      placeholder = "Select a gene"
    ),
    server = TRUE
  )

  output$cellTypePlot <- renderPlot({
    # The following cell_group_list code is based on code authored by Emma Jones.
    # 230926_EJ_Setbp1_AlternativeSplicing/src/marvel/03_analyze_de_genes.Rmd

    # Pull cell types and matching ids
    cell_group_list <- setbp1$sample.metadata %>%
      group_split(cell_type, .keep = TRUE) %>%
      map(~ set_names(.$cell.id, .$cell_type[1]))

    # Rename
    cell_group_list <- set_names(cell_group_list, c(
      "Astrocytes", "Excitatory Neurons",
      "Inhibitory Neurons", "Microglia", "OPCs",
      "Oligodendrocytes", "Vascular Cells"
    ))

    plot <- PlotValues.PCA.CellGroup.10x(
      MarvelObject = setbp1,
      cell.group.list = cell_group_list,
      legendtitle="Cell group",
      type = "umap"
    )
    plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup + labs(title = "Cell types")
    plot
  })

  wildtype_gene_expression <- reactive({
    data <- setbp1
    data$sample.metadata <- data$sample.metadata %>%
      filter(seq_folder == 'wildtype')
    data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
    data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
    data
  })

  output$wildtypeGeneExpressionPlot <- renderPlot({
    if (input$gene != "") {
      plot <- PlotValues.PCA.Gene.10x(
        MarvelObject=wildtype_gene_expression(),
        gene_short_name=input$gene,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
        labs(title = paste("Wildtype Gene Expression for", input$gene))
      plot
    }
  })

  mutant_gene_expression <- reactive({
    data <- setbp1
    data$sample.metadata <- data$sample.metadata %>%
      filter(seq_folder == 'mutant')
    data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
    data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
    data
  })

  output$mutantGeneExpressionPlot <- renderPlot({
    if (input$gene != "") {
      plot <- PlotValues.PCA.Gene.10x(
        MarvelObject=mutant_gene_expression(),
        gene_short_name=input$gene,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
        labs(title = paste("Mutant Gene Expression for", input$gene))
      plot
    }
  })
}
