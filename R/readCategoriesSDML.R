readCategoriesSDML <- function (x)
{
  if (is.null(x)) return(NULL)
  labels <- NULL
  if (xmlName(x) == "nominalCategories") {
    for (k in 1:xmlSize(x)) {
      code <- as.integer( xmlAttrs(x[[k]])["code"] )
      lab <- xmlChildren(x[[k]])
      labels[code] <- if (is.null(lab)) k else xmlValue(lab[[1]])
    }
  }
  labels
}
