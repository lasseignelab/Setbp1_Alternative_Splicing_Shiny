library(shiny)
library(shinyjs)
library(shinycssloaders)
library(glue)

# Mouse and gene names with proper formatting.
mouse <- paste0("C57BL/6JSetbp1", tags$sup("em2Lutzy"), "/J")
mouse_gene <- paste0(em("Setbp1"), tags$sup("S858R"))
human_gene <- em("SETBP1")
son_gene <- em("Son")

# External links used throughout the app for references.
shiny_link <- external_link("https://shiny.posit.co/", "Shiny")
marvel_link <- external_link("https://github.com/wenweixiong/MARVEL", "MARVEL")
lasseigne_link <- external_link("https://www.lasseigne.org/", "Lasseigne Lab")
jones_paper_link <- external_link(
  "https://www.biorxiv.org/content/10.1101/2024.06.26.600823v1",
  "Jones et al., 2024"
)
whitlock_paper_link <- external_link(
  "https://doi.org/10.1111/jcmm.18001",
  "Whitlock et al., 2023"
)
medline_link <- external_link(
  "https://medlineplus.gov/genetics/condition/schinzel-giedion-syndrome/",
  "Medline"
)
gene_reviews_link <- external_link(
  "https://www.ncbi.nlm.nih.gov/books/NBK601394/",
  "GeneReviews"
)
omim_link <- external_link("https://omim.org/entry/269150", "OMIM")
geo_link <- external_link(
  "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE237816",
  "GEO"
)

################################################################################
# Welcome and about tab.
################################################################################
welcome_about <- tabPanel(
  title = "Welcome and About",

  p(
    HTML(glue("
      Welcome to the {shiny_link} application created by Anthony Crumley in
      the {lasseigne_link} to visualize gene expression and splice junction
      usage in the {mouse_gene} mouse brain!
    "))
  ),
  p(
    HTML(glue("
      This application accompanies our manuscript, Cell-type-specific
      alternative splicing in the cerebral cortex of a Schinzel-Giedion Syndrome
      patient variant mouse model, which is available on bioRxiv
      ({jones_paper_link}). In our manuscript, we quantified gene and splice
      junction (SJ) expression for 51,465 nuclei previously generated
      ({whitlock_paper_link}) from the cerebral cortex of atypical {mouse_gene}
      SGS patient variant mice (n = 3) and wild-type control mice (n = 3).
      After cell type annotation, we performed pseudobulk differential gene
      expression and SJ usage (SJU) analyses across cell types and conditions.
      We identified 34 genes with statistically significant alterations in SJU.
      Oligodendrocytes had the most genes with changes in SJU, followed by
      astrocytes, excitatory, and inhibitory neurons. One gene, {son_gene}, a
      splicing cofactor known to cause the neurodevelopmental disorder ZTTK
      Syndrome, had SJU changes in all six non-vascular cell types we measured
      in {mouse_gene} compared to controls. This is the first research to report
      cell-type-specific AS changes in the cerebral cortex of an SGS model and
      the first study to link SGS to perturbations in {son_gene}.
    "))
  ),
  img(src = "setbp1.png", width = "100%", height = "auto"),
  p(
    strong("Graphical Abstract."),
    HTML(glue("
      (A) Schematic overview of our processing and analysis
      pipeline. (B) We analyzed pseudobulk gene expression and calculated SJU
      for each cell type and condition. (C) We compared SJU values for each cell
      type using a permutation test to identify cell-type-specific differences
      in AS between {mouse_gene} and wild-type mouse brain tissue. (D) Next, we
      visualized all annotated transcripts and splice junction locations for
      each significant SJU gene. (E) Finally, we compared the genes and pathways
      identified through functional enrichment analysis that overlap between
      cell types and predict their biological relevance.
    "))
  )
)

################################################################################
# Gene expression and splice junction usage tab.
################################################################################
plots <- tabPanel(
  title = "Gene Expression and Splice Junction Usage UMAPs",

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "gene",
        "Gene",
        choices = NULL,
        options = list(
          maxOptions = 20,
          maxItems = 1,
          placeholder = "Loading..."
        )
      ),
      helpText("Search for a gene to plot gene expression."),
      htmlOutput("genome_browsers"),

      shinyjs::hidden(
        div(id = "splice_junction_input",
          selectizeInput(
            "splice_junction",
            "Splice Junction",
            choices = NULL,
            options = list(
              maxItems = 1,
              placeholder = "Loading..."
            )
          ),
          helpText(
            "Select a splice junction related to the selected gene to",
            "plot splice junction usage."
          ),

          radioButtons(
            "plot_type",
            "Choose data to plot",
            choiceNames = list(
              "Gene Expression",
              "Splice Junction Usage"
            ),
            choiceValues = list("gene_expression", "splice_junction_usage")
          )
        )
      ),
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')",
        actionButton("plot", "Show plots")
      ),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
        actionButton("", "Show plots", disabled = TRUE)
      ),
      htmlOutput("form_validation", class = "validation-messages")
    ),

    mainPanel(
      img(src = "cell_types.png", width = "100%"),
      p(
        "
          This UMAP displays the seven assigned cell types of all cells in our
          dataset. Cell color indicates cell type, following the colors in the
          figure legend.
        "
      )
    )
  ),
  shinyjs::hidden(
    div(id = "gene_expression_plots", class = "plot-pair",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(
              "wildtype_gene_expression_plot",
              width = "100%",
              height = "auto"
            )
          ),
          uiOutput("wildtype_gene_expression_legend")
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(
              "mutant_gene_expression_plot",
              width = "100%",
              height = "auto"
            )
          ),
          uiOutput("mutant_gene_expression_legend")
        )
      )
    )
  ),
  shinyjs::hidden(
    div(id = "splice_junction_plots", class = "plot-pair",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(
              "wildtype_splice_junction_plot",
              width = "100%",
              height = "auto"
            )
          ),
          uiOutput("wildtype_splice_junction_legend")
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(
              "mutant_splice_junction_plot",
              width = "100%",
              height = "auto"
            )
          ),
          uiOutput("mutant_splice_junction_legend")
        )
      )
    )
  )
)

################################################################################
# Alternative splicing summaries tab.
################################################################################
as_gene_summary <- tabPanel(
  title = "Alternative Splicing Summaries",

  p(HTML(glue("
    To determine significant changes in SJ usage (SJU), we performed a
    permutation analysis using the {marvel_link} R package for each cell type
    between {mouse_gene} and controls. We detected 34 genes with significant
    changes in SJU (permutation test, p < 0.05 and delta > 1). To explore the
    transcript structures, splice junction expression, and splice junction usage
    of these 34 significant SJU genes, please select a gene here! For more
    information, please refer to our manuscript ({jones_paper_link}).
  "))),
  selectizeInput(
    "as_summary_gene",
    "Gene",
    choices = NULL,
    options = list(
      allowEmptyOption = TRUE,
      showEmptyOptionInDropdown = TRUE,
      maxItems = 1,
      placeholder = "Loading..."
    )
  ),
  uiOutput("as_gene_summary_image")
)

################################################################################
# Tab for frequently asked questions.
################################################################################
faq <- tabPanel(
  title = "FAQ",

  tags$dl(
    tags$dt("What is SGS?"),
    tags$dd(
      tags$ul(
        tags$li(HTML(glue("
          Schinzel-Giedion Syndrome (SGS) is an ultra-rare Mendelian disorder
          caused by gain-of-function mutations in the {human_gene} gene.
        "))),
        tags$li("
          Symptoms of SGS include global neurodevelopmental impairment,
          progressive neurodegeneration, mild-to-profound intellectual
          disability, treatment-resistant seizures, distinctive craniofacial
          structure, muscle hypotonia/spasticity, hydronephrosis, and
          gastrointestinal problems.
        "),
        tags$li(HTML(glue("
          For more information on SGS, refer to {medline_link},
          {gene_reviews_link}, or {omim_link}.
        ")))
      )
    )
  ),
  tags$dl(
    tags$dt("How did we generate this data?"),
    tags$dd(
      tags$ul(
        tags$li(HTML(glue("
          We obtained right cerebral cortex hemispheres from three 6-week-old
          male {mouse} mice heterozygous for {mouse_gene}, an SGS-associated
          point mutation (JAX Stock #033235) and three wild-type (WT) age, and
          sex-matched C57BL6/J mice (JAX Stock #000664)
        "))),
        tags$li(HTML(glue("
          Our original manuscript, {whitlock_paper_link}, includes details on
          how we generated this data set.
        "))),
        tags$li(HTML(glue("
          This data is publicly accessible for download from {geo_link} at
          GSE237816.
        ")))
      )
    )
  ),
  tags$dl(
    tags$dt("What is alternative splicing?"),
    tags$dd(
      tags$ul(
        tags$li("
          Alternative splicing creates multiple mRNA isoforms of the same gene,
          potentially with varying functions.
        "),
        tags$li("
          When a gene is alternatively spliced, exons can be gained or lost with
          different functional domains. For example, losing an exon with a
          membrane-binding domain can make a membrane-embedded protein become
          cytosolic.
        ")
      )
    )
  ),
  tags$dl(
    tags$dt("What is a splice junction?"),
    tags$dd(
      tags$ul(
        tags$li("
          A splice junction is where two exons have been joined together as a
          product of constitutive or alternative splicing.
        ")
      )
    )
  ),
  tags$dl(
    tags$dt("What is splice junction usage?"),
    tags$dd(
      tags$ul(
        tags$li("
          Splice junction usage (SJU) is calculated by dividing the total number
          of sequencing reads for a splice junction by the total number of reads
          for that gene for a given cell type/condition.
        "),
        tags$li(img(src = "sj_usage.png", width = "50%", height = "auto"))
      )
    )
  ),
  tags$dl(
    tags$dt("How do I cite this tool?"),
    tags$dd(
      tags$ul(
        tags$li(HTML(glue("
          Please cite our bioRxiv manuscript ({jones_paper_link}).
        ")))
      )
    )
  ),
  tags$dl(
    tags$dt("Contact information"),
    tags$dd(
      tags$ul(
        tags$li("
          For questions about this Shiny application, please contact
          acrumley(at)uab(dot)edu
        "),
        tags$li("
          For questions about our manuscript, please contact
          efjones(at)uab(dot)edu or bnp0001(at)uab(dot)edu
        ")
      )
    )
  )
)

################################################################################
# Overall page layout.
################################################################################
ui <- navbarPage(
  includeCSS("www/styles.css"),
  useShinyjs(),

  title = "
    Visualizing Cell-Type-Specific Alternative Splicing in a Schinzel-Giedion
    Syndrome Mouse Model
  ",
  welcome_about,
  plots,
  as_gene_summary,
  faq,

  tags$footer(class = "footer",
    div(class = "container",
      p(class = "text-center",
        "Figures created with BioRender.com | Copyright 2024 by the ",
        HTML(glue("{lasseigne_link}")),
        a(
          href = "https://www.lasseigne.org/",
          target = "_blank",
          img(src = "logo_only.png", height = "50px")
        )
      )
    )
  )
)
