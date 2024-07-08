library(shiny)
library(shinyjs)
library(shinycssloaders)
library(MARVEL)
library(tidyverse)
library(gh)
library(viridisLite)

source("R/helpers.R")
source("MARVEL/Script_DROPLET_07_ADHOC_PLOT_PCA_3_PlotValues_Gene.R")

server <- function(input, output, session) {

  # ****************************************************************************
  # Load MARVEL data and split into wildtype and mutant for separate plotting.
  # ****************************************************************************
  setbp1_metadata <- reactive({
    readRDS("./data/setbp1_marvel_aligned_metadata.rds")
  })
  wildtype_setbp1 <- reactive({
    readRDS("./data/setbp1_marvel_aligned_wildtype.rds")
  })
  mutant_setbp1 <- reactive({
    readRDS("./data/setbp1_marvel_aligned_mutant.rds")
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
  gene_list <- reactive(c(c(""), setbp1_metadata()$gene.metadata$gene_short_name %>% sort()))
  splice_junction_list <- reactive({
    splice_junctions <- setbp1_metadata()$sj.metadata %>% filter(gene_short_name.start == input$gene)
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
  output$wildtype_gene_expression_plot <- renderCachedPlot(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=wildtype_setbp1(),
          gene_short_name=gene(),
          color.gradient=viridis(5),
          log2.transform = FALSE,
          type="umap"
        )
        plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
          labs(title = paste("Wild-type Gene Expression for", gene())) +
          ggplotTheme()
        plot
      }
    },
    cacheKeyExpr = { gene() }
  )

  output$wildtype_gene_expression_legend <- renderUI({
    gene <- em(gene())
    HTML(glue("
      This UMAP displays the normalized and scaled gene expression values for
      {gene} in wild-type mouse cerebral cortex tissue cells. A brighter color
      indicates a higher expression level.
    "))
  })

  output$mutant_gene_expression_plot <- renderCachedPlot(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=mutant_setbp1(),
          gene_short_name=gene(),
          color.gradient=viridis(5),
          log2.transform = FALSE,
          type="umap"
        )
        plot$adhocPlot$PCA$Gene <- plot$adhocPlot$PCA$Gene +
          labs(title = bquote(Setbp1^S858R ~ "Gene Expression for" ~ .(gene()))) +
          ggplotTheme()
        plot
      }
    },
    cacheKeyExpr = { gene() }
  )

  output$mutant_gene_expression_legend <- renderUI({
    gene <- em(gene())
    mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
    HTML(glue("
      This UMAP displays the normalized and scaled gene expression values for
      {gene} in {mouse_gene} mouse cerebral cortex tissue cells. A brighter
      color indicates a higher expression level.
    "))
  })

  output$wildtype_splice_junction_plot <- renderCachedPlot(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=wildtype_setbp1(),
          coord.intron=splice_junction(),
          min.gene.count=3,
          color.gradient=plasma(5),
          log2.transform = FALSE,
          type="umap"
        )
        plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
          labs(
            title = paste("Wild-type Splice Junction Usage for", splice_junction()),
            color = "SJU\nper\nCell"
          ) +
          ggplotTheme()
        plot
      }
    },
    cacheKeyExpr = { splice_junction() }
  )

  output$wildtype_splice_junction_legend <- renderUI({
    gene <- em(gene())
    splice_junction <- splice_junction()
    HTML(glue("
      This UMAP displays the SJ usage (SJU) values for splice junction
      {splice_junction} from {gene} in wild-type mouse cerebral cortex tissue
      cells. A brighter color indicates a higher usage level. Please note that
      our manuscript does not use SJU values per cell, and SJU is a single
      number calculated for an entire population of cells, such as patient
      variant cells of a specific cell type.
    "))
  })

  output$mutant_splice_junction_plot <- renderCachedPlot(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=mutant_setbp1(),
          coord.intron=splice_junction(),
          min.gene.count=3,
          color.gradient=plasma(5),
          log2.transform=FALSE,
          type="umap"
        )
        plot$adhocPlot$PCA$PSI <- plot$adhocPlot$PCA$PSI +
          labs(
            title = bquote(Setbp1^S858R ~ "Splice Junction Usage for" ~ .(splice_junction())),
            color = "SJU\nper\nCell"
          ) +
          ggplotTheme()
        plot
      }
    },
    cacheKeyExpr = { splice_junction() }
  )

  output$mutant_splice_junction_legend <- renderUI({
    gene <- em(gene())
    splice_junction <- splice_junction()
    mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
    HTML(glue("
      This UMAP displays the SJ usage (SJU) values for splice junction
      {splice_junction} from {gene} in {mouse_gene} mouse cerebral cortex tissue
      cells. A brighter color indicates a higher usage level. Please note that
      our manuscript does not use SJU values per cell, and SJU is a single number
      calculated for an entire population of cells, such as patient variant cells
      of a specific cell type.
    "))
  })

}
