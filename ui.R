library(shiny)
library(shinyjs)
library(shinycssloaders)
library(glue)

mouse <- paste("C57BL/6JSetbp1", tags$sup("em2Lutzy"), "/J")
mouse_gene <- paste(em("Setbp1"), tags$sup("S858R"))
human_gene <- em("SETBP1")
shiny_link <- a(href = "https://shiny.posit.co/", target = "_blank", "Shiny")
lasseigne_link <- a(href = "https://www.lasseigne.org/", target = "_blank", "Lasseigne Lab")
jones_paper_link <- a(href = "https://www.biorxiv.org/content/10.1101/2024.06.26.600823v1", target = "_blank", "Jones et al., 2024")
whitlock_paper_link <- a(href = "https://doi.org/10.1111/jcmm.18001", target = "_blank", "Whitlock et al., 2023")
medline_link <- a(href = "https://medlineplus.gov/genetics/condition/schinzel-giedion-syndrome/", target = "_blank", "Medline")
gene_reviews_link <- a(href = "https://www.ncbi.nlm.nih.gov/books/NBK601394/", target = "_blank", "GeneReviews")
OMIM_link <- a(href = "https://omim.org/entry/269150", target = "_blank", "OMIM")
GEO_link <- a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE237816", target = "_blank", "GEO")

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
      This application accompanies our manuscript, Cell-type-specific alternative
      splicing in the cerebral cortex of a Schinzel-Giedion Syndrome patient
      variant mouse model, which is available on bioRxiv ({jones_paper_link}). In our
      manuscript, we quantified gene and splice junction (SJ) expression for
      51,465 nuclei previously generated ({whitlock_paper_link}) from the cerebral
      cortex of atypical {mouse_gene} SGS patient variant mice (n = 3) and
      wild-type control mice (n = 3). After cell type annotation, we performed
      pseudobulk differential gene expression and SJ usage (SJU) analyses across
      cell types and conditions. We identified 34 genes with statistically
      significant alterations in SJU. Oligodendrocytes had the most genes with
      changes in SJU, followed by astrocytes, excitatory, and inhibitory neurons.
      One gene, Son, a splicing cofactor known to cause the neurodevelopmental
      disorder ZTTK Syndrome, had SJU changes in all six non-vascular cell types
      we measured in {mouse_gene} compared to controls. This is the first research
      to report cell-type-specific AS changes in the cerebral cortex of an SGS
      model and the first study to link SGS to perturbations in Son.
    "))
  ),
  img(src = "setbp1.png", width = "100%", height = "auto"),
  p(
    strong("Graphical Abstract."),
    HTML(glue("
      (A) Schematic overview of our processing and analysis
      pipeline. (B) We analyzed pseudobulk gene expression and calculated SJU for
      each cell type and condition. (C) We compared SJU values for each cell type
      using a permutation test to identify cell-type-specific differences in AS
      between {mouse_gene} and wild-type mouse brain tissue. (D) Next, we visualized
      all annotated transcripts and splice junction locations for each significant
      SJU gene. (E) Finally, we compared the genes and pathways identified through
      functional enrichment analysis that overlap between cell types and predict
      their biological relevance.
    "))
  )
)

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
          )
        )
      ),
      actionButton("plot", "Show plots")
    ),

    mainPanel(
      shinycssloaders::withSpinner(plotOutput("cell_type_plot"))
    )
  ),
  shinyjs::hidden(
    div(id = "gene_expression_plots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtype_gene_expression_plot")
          ),
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutant_gene_expression_plot")
          )
        )
      )
    )
  ),
  shinyjs::hidden(
    div(id = "splice_junction_plots",
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("wildtype_splice_junction_plot")
          )
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput("mutant_splice_junction_plot")
          )
        )
      )
    )
  )
)

as_gene_summary <- tabPanel(
  title = "Alternative Splicing Summaries",

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
          progressive neurodegeneration, mild-to-profound intellectual disability,
          treatment-resistant seizures, distinctive craniofacial structure,
          muscle hypotonia/spasticity, hydronephrosis, and gastrointestinal
          problems.
        "),
        tags$li(HTML(glue("
          For more information on SGS, refer to {medline_link},
          {gene_reviews_link}, or {OMIM_link}.
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
          This data is publicly accessible for download from {GEO_link} at GSE237816.
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
        tags$li(img(src = "sj_usage.png"))
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
          efjones(at)uab(dot)edu or bnp0001(at)uab(edu)edu
        ")
      )
    )
  )
)


ui <- navbarPage(
  includeCSS("www/styles.css"),
  useShinyjs(),

  title = "Visualizing Cell-Type-Specific Alternative Splicing in an Schinzel-Giedion Syndrome Mouse Model",
  welcome_about,
  plots,
  as_gene_summary,
  faq,

  tags$footer(class = "footer",
    div(class = "container",
      p(class = "text-center",
        HTML(glue("Figures created with BioRender.com | Copyright 2024 by the {lasseigne_link}"))
      )
    )
  )
)
