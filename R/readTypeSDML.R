readType <- function(x)
{
  ret <- list()
  ret$type <- "character"
  ret$mode <- NULL
  ret$labels <- NULL
  ret$min <- -Inf
  ret$max <- +Inf
  
  if (xmlName(x) == "type") {
    x <- x[[1]]
    ret$type <- xmlName(x)

    if (ret$type == "categorical") {
      ## mode
      ret$mode <- xmlAttrs(x)["mode"]
      
      ## labels
      for (k in 1:xmlSize(x)) {
        code <- as.integer(xmlAttrs(x[[k]])["code"])
        lab <- xmlChildren(x[[k]])
        ret$labels[code] <- if (is.null(lab)) k else xmlValue(lab[[1]])
      }
    }

    if (ret$type == "numeric") {
      if (!length(xmlChildren(x)))
        ret$mode <- "real"
      else {
        x <- x[[1]]
        ret$mode <- xmlName(x)
        l <- length(xmlChildren(x))
        if (ret$mode %in% c("integer", "real") && l)
          for (i in 1:l)
            ret[[xmlName(x[[i]])]] <- getDataSDML(x[i])
      }
    }
  }

  ret
}
