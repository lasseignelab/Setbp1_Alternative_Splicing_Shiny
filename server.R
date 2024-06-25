library(shiny)
library(shinyjs)
library(shinycssloaders)
library(MARVEL)
library(tidyverse)

server <- function(input, output, session) {
  # Load data and split into wildtype and mutant.
  setbp1 <- reactiveVal(readRDS("./data/setbp1_marvel_aligned_reduced_size.rds"))

  wildtype_setbp1 <- reactive({
    data <- setbp1()
    data$sample.metadata <- data$sample.metadata %>%
      filter(seq_folder == 'wildtype')
    data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
    data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
    data$gene.count.matrix <- data$gene.count.matrix[, data$sample.metadata$cell.id]
    data$sj.count.matrix <- data$sj.count.matrix[, data$sample.metadata$cell.id]
    data
  })

  mutant_setbp1 <- reactive({
    data <- setbp1()
    data$sample.metadata <- data$sample.metadata %>%
      filter(seq_folder == 'mutant')
    data$pca <- data$pca[data$pca$cell.id %in% data$sample.metadata$cell.id, ]
    data$gene.norm.matrix <- data$gene.norm.matrix[, data$sample.metadata$cell.id]
    data$gene.count.matrix <- data$gene.count.matrix[, data$sample.metadata$cell.id]
    data$sj.count.matrix <- data$sj.count.matrix[, data$sample.metadata$cell.id]
    data
  })

  # Setup the gene and splice junction selectors.
  gene_list <- reactive(c(c(""), setbp1()$gene.metadata$gene_short_name %>% sort()))
  splice_junction_list <- reactive({
    splice_junctions <- setbp1()$sj.metadata %>% filter(gene_short_name.start == input$gene)
    c(c(""), splice_junctions$coord.intron %>% sort())
  })

  observe({
    updateSelectizeInput(
      session,
      "gene",
      choices = gene_list(),
      options = list(
        placeholder = "Select a gene"
      ),
      server = TRUE
    )
  })

  observeEvent(input$gene, {
    if (input$gene == "") {
      shinyjs::hide("splice_junction")
      shinyjs::hide("geneExpressionPlots")
      shinyjs::hide("spliceJunctionPlots")
      updateSelectizeInput(
        session,
        "splice_junction",
        choices = c(""),
        options = list(
          placeholder = "Loading..."
        )
      )
    } else {
      updateSelectizeInput(
        session,
        "splice_junction",
        choices = splice_junction_list(),
        options = list(
          placeholder = "Select a splice junction"
        )
      )
      shinyjs::show("splice_junction")
      shinyjs::show("geneExpressionPlots")
    }
  })

  observeEvent(input$splice_junction, {
    if (input$splice_junction == "") {
      shinyjs::hide("spliceJunctionPlots")
    } else {
      shinyjs::show("spliceJunctionPlots")
    }
  })

  # Create the plots.
  output$cellTypePlot <- renderPlot({
    # The following cell_group_list code is based on code authored by Emma Jones.
    # 230926_EJ_Setbp1_AlternativeSplicing/src/marvel/03_analyze_de_genes.Rmd

    # Pull cell types and matching ids
    cell_group_list <- setbp1()$sample.metadata %>%
      group_split(cell_type, .keep = TRUE) %>%
      map(~ set_names(.$cell.id, .$cell_type[1]))

    # Rename
    cell_group_list <- set_names(cell_group_list, c(
      "Astrocytes", "Excitatory Neurons",
      "Inhibitory Neurons", "Microglia", "OPCs",
      "Oligodendrocytes", "Vascular Cells"
    ))

    plot <- PlotValues.PCA.CellGroup.10x(
      MarvelObject = setbp1(),
      cell.group.list = cell_group_list,
      legendtitle="Cell group",
      type = "umap"
    )
    plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup + labs(title = "Cell types")
    plot
  })

  output$wildtypeGeneExpressionPlot <- renderPlot({
    if (input$gene != "") {
      plot <- PlotValues.PCA.Gene.10x(
        MarvelObject=wildtype_setbp1(),
        gene_short_name=input$gene,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
        labs(title = paste("Wildtype Gene Expression for", input$gene))
      plot
    }
  })

  output$mutantGeneExpressionPlot <- renderPlot({
    if (input$gene != "") {
      plot <- PlotValues.PCA.Gene.10x(
        MarvelObject=mutant_setbp1(),
        gene_short_name=input$gene,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
        labs(title = paste("Mutant Gene Expression for", input$gene))
      plot
    }
  })

  output$wildtypeSpliceJunctionPlot <- renderPlot({
    if (input$splice_junction != "") {
      plot <- PlotValues.PCA.PSI.10x(
        MarvelObject=wildtype_setbp1(),
        coord.intron=input$splice_junction,
        min.gene.count=3,
        log2.transform=FALSE,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
        labs(title = paste("Wildtype Splice Junction Usage for", input$splice_junction))
      plot
    }
  })

  output$mutantSpliceJunctionPlot <- renderPlot({
    if (input$splice_junction != "") {
      plot <- PlotValues.PCA.PSI.10x(
        MarvelObject=mutant_setbp1(),
        coord.intron=input$splice_junction,
        min.gene.count=3,
        log2.transform=FALSE,
        color.gradient=c("grey","cyan","green","yellow","red"),
        type="umap"
      )
      plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
        labs(title = paste("Mutant Splice Junction Usage for", input$splice_junction))
      plot
    }
  })
}
