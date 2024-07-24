library(fontawesome)

external_link <- function(href, text) {
  a(href = href, target = "_blank", text, fa("external-link"))
}
