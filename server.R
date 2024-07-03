library(shiny)
library(shinyjs)
library(shinycssloaders)
library(MARVEL)
library(tidyverse)
library(gh)
library(viridisLite)

server <- function(input, output, session) {

  # ****************************************************************************
  # Load MARVEL data and split into wildtype and mutant for separate plotting.
  # ****************************************************************************
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

  # ****************************************************************************
  # Load alternative splicing gene summary pre-rendered image information from
  # Github and setup gene selector with image display.
  # ****************************************************************************
  as_gene_summary_json <- reactive({
    gh("/repos/lasseignelab/230926_EJ_Setbp1_AlternativeSplicing/contents/results/as_gene_summaries/")
  })

  as_gene_summary_names <- reactive({
    names <- sapply(as_gene_summary_json(), function(file) file$name)
    gsub(".png", "", names) # Remove file name suffix, e.g. Son.png -> Son
  })

  as_gene_summary_urls <- reactive({
    sapply(as_gene_summary_json(), function(file) file$download_url)
  })

  observe({
    updateSelectizeInput(
      session,
      "as_summary_gene",
      choices = c("", as_gene_summary_names()),
      options = list(
        placeholder = "Select a gene"
      ),
      server = FALSE
    )
  })

  output$as_gene_summary_image <- renderUI({
    as_summary_gene_selected <- input$as_summary_gene != ""
    if (as_summary_gene_selected) {
      url <- as_gene_summary_urls()[which(as_gene_summary_names() == input$as_summary_gene)]
      mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
      selected_gene <- em(input$as_summary_gene)

      tagList(
        div(
          tags$img(src = url, width = "100%", height = "auto"),
          p(HTML(glue("
            (A) Transcript of all annotated transcripts of {selected_gene}.
            The color indicates transcript classification: indigo = transcripts
            flagged for nonsense-mediated decay (NMD), dark teal = protein-coding
            transcripts, turquoise = protein-coding transcripts, but coding
            sequence (CDS) is not defined, and green = transcripts with retained
            intron events. Arrows indicate the direction of transcription. (B)
            Split violin plots showing {selected_gene} expression per cell for
            all cell types, split by condition. (C) Heatmaps of the changes in
            normalized mean SJ expression (top) and usage (bottom) between
            {mouse_gene} mice and controls for all SJs of {selected_gene}. The top
            heatmap annotation indicates cell type. A positive delta indicates
            expression or usage was higher in {mouse_gene} mice than controls, and
            a negative indicates expression or usage was higher in controls
            compared to {mouse_gene} mice.
          ")))
        )
      )
    }
  })

  # ****************************************************************************
  # Setup the gene and splice junction selectors.
  # ****************************************************************************
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
        placeholder = "Search for a gene"
      ),
      server = TRUE
    )
  })

  observeEvent(input$gene, {
    gene_selected <- input$gene != ""
    if (gene_selected) {
      updateSelectizeInput(
        session,
        "splice_junction",
        choices = splice_junction_list(),
        options = list(
          placeholder = "Select a splice junction"
        )
      )
      shinyjs::show("splice_junction_input")
    } else {
      shinyjs::hide("splice_junction_input")
      updateSelectizeInput(
        session,
        "splice_junction",
        choices = c(""),
        options = list(
          placeholder = "Loading..."
        )
      )
    }
  })

  # ****************************************************************************
  # Respond to the plot button.
  # ****************************************************************************
  observeEvent(input$plot, {
    gene_selected <- input$gene != ""
    if (gene_selected) {
      shinyjs::show("gene_expression_plots")
    } else {
      shinyjs::hide("gene_expression_plots")
    }

    splice_junction_selected <- input$splice_junction != ""
    if (splice_junction_selected) {
      shinyjs::show("splice_junction_plots")
    } else {
      shinyjs::hide("splice_junction_plots")
    }
  })

  gene <- eventReactive(input$plot, {
      input$gene
  })

  splice_junction <- eventReactive(input$plot, {
    input$splice_junction
  })

  # ****************************************************************************
  # Set up the plots with caching.
  # ****************************************************************************
  ggplot_theme <- reactive({
    theme(
      plot.title = element_text(size = 16),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  })

  output$cell_type_plot <- renderCachedPlot(
    {
      # The following cell_group_list code is based on code authored by Emma Jones.
      # 230926_EJ_Setbp1_AlternativeSplicing/src/marvel/03_analyze_de_genes.Rmd

      # Pull cell types and matching ids
      cell_group_list <- setbp1()$sample.metadata %>%
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

      plot <- PlotValues.PCA.CellGroup.10x(
        MarvelObject = setbp1(),
        cell.group.list = cell_group_list,
        point.colors = cell_type_colors,
        point.size.legend = 7,
        legendtitle="Cell group",
        type = "umap"
      )
      plot$adhocPlot$PCA$CellGroup <- plot$adhocPlot$PCA$CellGroup +
        labs(title = "Cell Types") +
        ggplot_theme()
      plot
    },
    cacheKeyExpr = { TRUE }
  )

  output$wildtype_gene_expression_plot <- renderCachedPlot(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=wildtype_setbp1(),
          gene_short_name=gene(),
          color.gradient=viridis(5),
          type="umap"
        )
        plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
          labs(title = paste("Wildtype Gene Expression for", gene())) +
          ggplot_theme()
        plot
      }
    },
    cacheKeyExpr = { gene() }
  )

  output$mutant_gene_expression_plot <- renderCachedPlot(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=mutant_setbp1(),
          gene_short_name=gene(),
          color.gradient=viridis(5),
          type="umap"
        )
        plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
          labs(title = paste("Mutant Gene Expression for", gene())) +
          ggplot_theme()
        plot
      }
    },
    cacheKeyExpr = { gene() }
  )

  output$wildtype_splice_junction_plot <- renderCachedPlot(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=wildtype_setbp1(),
          coord.intron=splice_junction(),
          min.gene.count=3,
          log2.transform=FALSE,
          color.gradient=viridis(5),
          type="umap"
        )
        plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
          labs(title = paste("Wildtype Splice Junction Usage for", splice_junction())) +
          ggplot_theme()
        plot
      }
    },
    cacheKeyExpr = { splice_junction() }
  )

  output$mutant_splice_junction_plot <- renderCachedPlot(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=mutant_setbp1(),
          coord.intron=splice_junction(),
          min.gene.count=3,
          log2.transform=FALSE,
          color.gradient=viridis(5),
          type="umap"
        )
        plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
          labs(title = paste("Mutant Splice Junction Usage for", splice_junction())) +
          ggplot_theme()
        plot
      }
    },
    cacheKeyExpr = { splice_junction() }
  )
}
