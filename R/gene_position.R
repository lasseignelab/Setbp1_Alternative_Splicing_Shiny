################################################################################
# The position of a gene within the genome.
#
# When linking to the UCSC Genome Browser for a gene the position of the gene
# must be passed in the URL including the chromosome, beginning location, and
# ending location.
#
# The location information is stored in the format "gene:beginning:ending" for
# each intron in a gene, i.e. "chr1:3277541:3283661".  This function retrieves
# all the introns for a gene, finds the beginning and ending location and
# creates a position for the gene's overall location.
#
# @param setbp1_metadata MARVEL object with splice junction metadata.
# @param gene The name of the gene to calculate a position for.
# @return A list with chromosome, beginning, and ending attributes.
#
################################################################################

gene_position <- function(setbp1_metadata, gene) {
  splice_junctions <- as.data.frame(setbp1_metadata$sj.metadata) %>%
    filter(gene_short_name.start == gene) %>%
    select(coord.intron) %>%
    arrange(coord.intron)
  beginning <- strsplit(
    splice_junctions$coord.intron[1],
    ":"
  )[[1]]
  ending <- strsplit(
    splice_junctions$coord.intron[length(splice_junctions$coord.intron)],
    ":"
  )[[1]]

  list(
    chromosome = beginning[1],
    beginning = beginning[2],
    ending = ending[3]
  )
}
