##helper funciton to extract lag - if var name ends in _(t) lag is 0, else it is the value of x in _(t-x)
extract_lag <- function(label) {
  suppressWarnings(
    ifelse(grepl("_\\(t\\)$", label),0,
           as.integer(sub(".*_\\(t-([0-9]+)\\)$", "\\1", label)))
  )
}
