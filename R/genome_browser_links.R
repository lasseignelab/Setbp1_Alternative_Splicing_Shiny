genome_browser_links <- function(setbp1_metadata, gene) {
  position <- gene_position(setbp1_metadata, gene)
  url_encoded_position <- paste0(
    position$chromosome,
    "%3A",
    position$beginning,
    "%2D",
    position$ending
  )

  div(
    h5("Genome Browsers", tags$small(gene)),
    p(
      external_link(
        glue(
          "https://genome.ucsc.edu/cgi-bin/hgTracks?db=mm39&position=",
          "{url_encoded_position}"
        ),
        "UCSC Genome Browser"
      )
    ),
    p(
      external_link(
        glue(
          "https://useast.ensembl.org/Mus_musculus/Gene/Summary?db=core;g=",
          "{gene}"
        ),
        "Ensembl Genome Browser"
      )
    )
  )
}
