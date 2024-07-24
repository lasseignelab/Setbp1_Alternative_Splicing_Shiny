gene_position <- function(setbp1_metadata, gene) {
  splice_junctions <- as.data.frame(setbp1_metadata$sj.metadata) %>%
    filter(gene_short_name.start == gene) %>%
    select(coord.intron) %>%
    arrange(coord.intron)
  beginning <- strsplit(splice_junctions$coord.intron[1], ":")[[1]]
  ending <- strsplit(splice_junctions$coord.intron[length(splice_junctions$coord.intron)], ":")[[1]]

  list(
    chromosome = beginning[1],
    beginning = beginning[2],
    ending = ending[3]
  )
}
