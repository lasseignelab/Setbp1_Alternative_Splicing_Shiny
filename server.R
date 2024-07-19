library(shiny)
library(shinyjs)
library(shinycssloaders)
library(MARVEL)
library(tidyverse)
library(ggtext)
library(gh)
library(viridisLite)
library(fontawesome)

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
  gene_position <- reactive({
    splice_junctions <- as.data.frame(setbp1_metadata()$sj.metadata) %>%
      filter(gene_short_name.start == input$gene) %>%
      select(coord.intron) %>%
      arrange(coord.intron)
    beginning <- strsplit(splice_junctions$coord.intron[1], ":")[[1]]
    ending <- strsplit(splice_junctions$coord.intron[length(splice_junctions$coord.intron)], ":")[[1]]
    list(
      chromosome = beginning[1],
      beginning = beginning[2],
      ending = ending[3]
    )
  })

  splice_junction_list <- reactive({
    splice_junctions <- setbp1_metadata()$sj.metadata %>%
      filter(gene_short_name.start == input$gene)
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
      output$genome_browsers <- renderUI({
        gene_position <- paste0(
          gene_position()$chromosome,
          "%3A",
          gene_position()$beginning,
          "%2D",
          gene_position()$ending
        )
        div(
          h5("Genome Browsers", tags$small(input$gene)),
          p(
            a(
              href = glue("https://genome.ucsc.edu/cgi-bin/hgTracks?db=mm39&position={gene_position}"),
              target = "_blank",
              "UCSC Genome Browser",
              fa("external-link", "Launch"),
              tags$i(class = "fa fa-external-link")
            )
          ),
          p(
            a(
              href = glue("https://useast.ensembl.org/Mus_musculus/Gene/Summary?db=core;g={input$gene}"),
              target = "_blank",
              "Ensembl Genome Browser",
              fa("external-link", "Launch"),
              tags$i(class = "fa fa-external-link")
            )
          )
        )
      })
      shinyjs::show("genome_browsers")

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
      shinyjs::hide("genome_browsers")
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
    if (gene_selected & input$plot_type == "gene_expression") {
      shinyjs::show("gene_expression_plots")
    } else {
      shinyjs::hide("gene_expression_plots")
    }

    splice_junction_selected <- input$splice_junction != ""
    if (splice_junction_selected & input$plot_type == "splice_junction_usage") {
      shinyjs::show("splice_junction_plots")
    } else {
      shinyjs::hide("splice_junction_plots")
    }

    output$form_validation <- renderUI({
      if (!splice_junction_selected & input$plot_type == "splice_junction_usage") {
        div("Splice junction is required.", class = "alert alert-danger")
      }
    })
  })

  gene <- eventReactive(
    {
      # Initialize the session variable to prevent NULL error
      session$userData$last_gene_plot_click <- first(c(session$userData$last_gene_plot_click, ""))

      new_plot_click <- input$plot != session$userData$last_gene_plot_click
      show_gene_expression <- input$plot_type == "gene_expression"
      if (new_plot_click & show_gene_expression) {
        session$userData$last_gene_plot_click <- input$plot
        input$plot
      } else {
        NULL
      }
    },
    {
      input$gene
    }
  )

  splice_junction <- eventReactive(
    {
      # Initialize the session variable to prevent NULL error
      session$userData$last_sj_plot_click <- first(c(session$userData$last_sj_plot_click, ""))

      new_plot_click <- input$plot != session$userData$last_sj_plot_click
      show_splice_junction_usage <- input$plot_type == "splice_junction_usage"
      if (new_plot_click & show_splice_junction_usage) {
        session$userData$last_sj_plot_click <- input$plot
        input$plot
      } else {
        NULL
      }
    },
    {
      input$splice_junction
    }
  )

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
        image_file <- gene_expression_plot_image(
          "./data/setbp1_marvel_aligned_wildtype_gene.rds",
          "Wild-type",
          gene()
        )
        list(src = image_file, width = "100%", height = "auto")
      }
    }, deleteFile = TRUE
  )

  output$wildtype_gene_expression_legend <- renderUI({
    gene <- em(gene())
    tagList(
      HTML(glue("
        This UMAP displays the normalized and scaled gene expression values for
        {gene} in wild-type mouse cerebral cortex tissue cells. A brighter color
        indicates a higher expression level.
      ")),
      downloadLink(
        "wildtype_gene_expression_download",
        label = HTML(paste0("Download ", fa("download")))
      )
    )
  })

  output$wildtype_gene_expression_download <- downloadHandler(
    filename = function() {"plot.png"},
    content = function(file) {
      image_file <- gene_expression_plot_image(
        "./data/setbp1_marvel_aligned_wildtype_gene.rds",
        "Wild-type",
        gene()
      )
      file.copy(image_file, file)
      file.remove(image_file)
    }
  )

  output$mutant_gene_expression_plot <- renderImage(
    {
      gene_selected <- gene() != ""
      if (gene_selected) {
        image_file <- gene_expression_plot_image(
          "./data/setbp1_marvel_aligned_mutant_gene.rds",
          "<i>Setbp1</i><sup>S858R</sup>",
          gene()
        )
        list(src = image_file, width = "100%", height = "auto")
      }
    }, deleteFile = TRUE
  )

  output$mutant_gene_expression_legend <- renderUI({
    gene <- em(gene())
    mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
    tagList(
      HTML(glue("
      This UMAP displays the normalized and scaled gene expression values for
      {gene} in {mouse_gene} mouse cerebral cortex tissue cells. A brighter
      color indicates a higher expression level.
      ")),
      downloadLink(
        "mutant_gene_expression_download",
        label = HTML(paste0("Download ", fa("download")))
      )
    )
  })

  output$mutant_gene_expression_download <- downloadHandler(
    filename = function() {"plot.png"},
    content = function(file) {
      image_file <- gene_expression_plot_image(
        "./data/setbp1_marvel_aligned_mutant_gene.rds",
        "<i>Setbp1</i><sup>S858R</sup>",
        gene()
      )
      file.copy(image_file, file)
      file.remove(image_file)
    }
  )

  output$wildtype_splice_junction_plot <- renderImage(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        image_file <- splice_junction_plot_image(
          "./data/setbp1_marvel_aligned_wildtype_sj.rds",
          "Wild-type",
          splice_junction()
        )
        list(src = image_file, width = "100%", height = "auto")
      }
    }, deleteFile = TRUE
  )

  output$wildtype_splice_junction_legend <- renderUI({
    gene <- em(input$gene)
    splice_junction <- splice_junction()
    tagList(
      HTML(glue("
        This UMAP displays the SJ usage (SJU) values for splice junction
        {splice_junction} from {gene} in wild-type mouse cerebral cortex tissue
        cells. A brighter color indicates a higher usage level. Please note that
        our manuscript does not use SJU values per cell, and SJU is a single
        number calculated for an entire population of cells, such as patient
        variant cells of a specific cell type.
      ")),
      downloadLink(
        "wildtype_splice_junction_download",
        label = HTML(paste0("Download ", fa("download")))
      )
    )
  })

  output$wildtype_splice_junction_download <- downloadHandler(
    filename = function() {"plot.png"},
    content = function(file) {
      image_file <- splice_junction_plot_image(
        "./data/setbp1_marvel_aligned_wildtype_sj.rds",
        "Wild-type",
        splice_junction()
      )
      file.copy(image_file, file)
      file.remove(image_file)
    }
  )

  output$mutant_splice_junction_plot <- renderImage(
    {
      splice_junction_selected <- splice_junction() != ""
      if (splice_junction_selected) {
        image_file <- splice_junction_plot_image(
          "./data/setbp1_marvel_aligned_mutant_sj.rds",
          "<i>Setbp1</i><sup>S858R</sup>",
          splice_junction()
        )
        list(src = image_file, width = "100%", height = "auto")
      }
    }, deleteFile = TRUE
  )

  output$mutant_splice_junction_legend <- renderUI({
    gene <- em(input$gene)
    splice_junction <- splice_junction()
    mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
    tagList(
      HTML(glue("
        This UMAP displays the SJ usage (SJU) values for splice junction
        {splice_junction} from {gene} in {mouse_gene} mouse cerebral cortex tissue
        cells. A brighter color indicates a higher usage level. Please note that
        our manuscript does not use SJU values per cell, and SJU is a single number
        calculated for an entire population of cells, such as patient variant cells
        of a specific cell type.
      ")),
      downloadLink(
        "mutant_splice_junction_download",
        label = HTML(paste0("Download ", fa("download")))
      )
    )
  })

  output$mutant_splice_junction_download <- downloadHandler(
    filename = function() {"plot.png"},
    content = function(file) {
      image_file <- splice_junction_plot_image(
        "./data/setbp1_marvel_aligned_mutant_sj.rds",
        "<i>Setbp1</i><sup>S858R</sup>",
        splice_junction()
      )
      file.copy(image_file, file)
      file.remove(image_file)
    }
  )

}
