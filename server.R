library(shiny)
library(shinyjs)
library(shinycssloaders)
library(MARVEL)
library(tidyverse)
library(ggtext)
library(gh)
library(viridisLite)

source("R/helpers.R")
source("MARVEL/Script_DROPLET_07_ADHOC_PLOT_PCA_2_PlotValues_PSI.R")
source("MARVEL/Script_DROPLET_07_ADHOC_PLOT_PCA_3_PlotValues_Gene.R")

server <- function(input, output, session) {

  # ****************************************************************************
  # Load MARVEL data and split into wildtype and mutant for separate plotting.
  # ****************************************************************************
  setbp1_metadata <- reactive({
    readRDS("./data/setbp1_marvel_aligned_metadata.rds")
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
  #
  # *** WARNING*** The following plots are implemented in a way that reduces
  #                memory usage in order to conform to the shinyapps.io limit
  #                of 8Gb.  The limit is strictly enforced and memory would
  #                spike above the limit before garbage collection would release
  #                memory which resulted in the app process being killed.
  #
  # The following steps were taken to reduce memory usage. Please do not use
  # these unless it is absolutely necessary:
  #
  # 1) The data file is broken into a separate file for each plot with only
  #    the data needed for that plot. The file is read from disk as each plot
  #    is rendered.  This saves memory at the cost of performance.
  # 2) Garbage collection was encouraged before and after each plot by calling
  #    gc() to recover memory more quickly.
  # 3) The data and plot variables are explicitly set to NULL after they have
  #    been used so that garbage collection will release them more readily.
  # 4) renderImage was used instead of renderPlot so that no references to the
  #    data or plot objects would be held by Shiny beyond the output rendering.
  #    As a result, it is not possible to cache the plots which sacrifices
  #    performance to conserve memory.
  # ****************************************************************************

  output$wildtype_gene_expression_plot <- renderImage(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        gc()
        wildtype_setbp1 <- readRDS("./data/setbp1_marvel_aligned_wildtype_gene.rds")
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=wildtype_setbp1,
          gene_short_name=gene(),
          color.gradient=viridis(5),
          log2.transform = FALSE,
          type="umap"
        )
        wildtype_setbp1 <- NULL

        plot <- plot +
          labs(title = paste("Wild-type Gene Expression for", gene())) +
          ggplotTheme()

        outfile <- tempfile(fileext = ".png")
        png(outfile, height = 400, width = 500)
        print(plot)
        dev.off()
        plot <- NULL
        gc()

        list(src = outfile)
      }
    }, deleteFile = TRUE
  )

  output$wildtype_gene_expression_legend <- renderUI({
    gene <- em(gene())
    HTML(glue("
      This UMAP displays the normalized and scaled gene expression values for
      {gene} in wild-type mouse cerebral cortex tissue cells. A brighter color
      indicates a higher expression level.
    "))
  })

  output$mutant_gene_expression_plot <- renderImage(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        gc()
        mutant_setbp1 <- readRDS("./data/setbp1_marvel_aligned_mutant_gene.rds")
        plot <- PlotValues.PCA.Gene.10x(
          MarvelObject=mutant_setbp1,
          gene_short_name=gene(),
          color.gradient=viridis(5),
          log2.transform = FALSE,
          type="umap"
        )
        mutant_setbp1 <- NULL

        plot <- plot +
          labs(
            title = paste0("<i>Setbp1</i><sup>S858R</sup> Gene Expression for ", gene())
          ) +
          ggplotTheme()

        outfile <- tempfile(fileext = ".png")
        png(outfile, height = 400, width = 500)
        print(plot)
        dev.off()
        plot <- NULL
        gc()

        list(src = outfile)
      }
    }, deleteFile = TRUE
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

  output$wildtype_splice_junction_plot <- renderImage(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        gc()
        wildtype_setbp1 <- readRDS("./data/setbp1_marvel_aligned_wildtype_sj.rds")
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=wildtype_setbp1,
          coord.intron=splice_junction(),
          min.gene.count=3,
          color.gradient=plasma(5),
          log2.transform = FALSE,
          type="umap"
        )
        wildtype_setbp1 <- NULL

        plot <- plot +
          labs(
            title = paste("Wild-type Splice Junction Usage for", splice_junction()),
            color = "SJU\nper\nCell"
          ) +
          ggplotTheme()

        outfile <- tempfile(fileext = ".png")
        png(outfile, height = 400, width = 500)
        print(plot)
        dev.off()
        plot <- NULL
        gc()

        list(src = outfile)
      }
    }, deleteFile = TRUE
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

  output$mutant_splice_junction_plot <- renderImage(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        gc()
        mutant_setbp1 <- readRDS("./data/setbp1_marvel_aligned_mutant_sj.rds")
        plot <- PlotValues.PCA.PSI.10x(
          MarvelObject=mutant_setbp1,
          coord.intron=splice_junction(),
          min.gene.count=3,
          color.gradient=plasma(5),
          log2.transform=FALSE,
          type="umap"
        )
        mutant_setbp1 <- NULL

        plot <- plot +
          labs(
            title = paste0("<i>Setbp1</i><sup>S858R</sup> Splice Junction Usage for ", splice_junction()),
            color = "SJU\nper\nCell"
          ) +
          ggplotTheme()

        outfile <- tempfile(fileext = ".png")
        png(outfile, height = 400, width = 500)
        print(plot)
        dev.off()
        plot <- NULL
        gc()

        list(src = outfile)
      }
    }, deleteFile = TRUE
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
