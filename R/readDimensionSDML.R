readDimensionSDML <- function(x)
{
    dim <- NULL
    dimnames <- NULL
    names <- list()
    if (xmlName(x) == "dimension") {
      if (xmlSize(x)) {
        for (k in 1:xmlSize(x)) {
          dim[k]     <- xmlAttrs(x[[k]])["size"]
          if (!is.na(xmlAttrs(x[[k]])["name"]))
              dimnames[k] <- xmlAttrs(x[[k]])["name"]
          names[[k]] <- getDataSDML(xmlChildren(x[[k]]))
        }
      } else dim <- 0
    }
    mode(dim) <- "integer"
    if (!is.null(dimnames)) names(names) <- dimnames
    list(dim=dim, names=names)
}
