writeListArraySDML <- function(x,
                               file,
                               textdata,
                               sep,
                               na.string,
                               null.string,
                               posinf.string,
                               neginf.string,
                               nan.string,
                               true,
                               false)
{
  if (is.null(x))
    catSDML("\<empty/>\n", file = file)
  else if (is.recursive(x)) {	
    catSDML("\<list>\n", file = file)
    writeDimensionSDML(x, file = file)
    writePropertiesSDML(attributes(x), file = file)
                            
    catSDML("\<listdata>\n", file = file)		
    lapply(x, writeListArraySDML, file = file, textdata = textdata,
           sep = sep, na.string = na.string, null.string = null.string,
           posinf.string = posinf.string, neginf.string = neginf.string,
           nan.string = nan.string, true = true, false = false)
    catSDML("\</listdata>\n", file = file)	
    catSDML("\</list>\n", file = file)
  } else {
    catSDML("\<array>\n", file = file)

    ## dimension tag
    writeDimensionSDML(x, file = file)

    ## properties tag
    ## (remove factor/POSIXxx attributes information first)
    xtmp <- x
    if (is.factor(x)) {
      attr(xtmp, "levels") <- NULL
      class(xtmp) <- class(xtmp)[!class(xtmp) %in% c("ordered","factor")]
    }
    if (inherits(x, "POSIXt"))
      class(xtmp) <- class(xtmp)[!class(xtmp) %in% c("POSIXt","POSIXct","POSIXlt")]
    writePropertiesSDML(attributes(xtmp), file = file)

    ## nominalCategories tag
    if (is.factor(x)) writeCategoriesSDML(x, file = file)
    
    ## check the datatype
    mode <- NULL
    if (inherits(x, "POSIXt")) {
      type <- "datetime"
      x <- as.character(x, format="%Y-%m-%dT%H:%M:%S")
    } else {
      type <- "numeric"
      if (is.integer(x)) mode <- "integer"
      if (is.real(x)) mode <- "real"
      if (is.complex(x)) mode <- "complex"
      if (is.character(x)) type <- "character"
    }
    if (is.logical(x)) {
      type <- "logical"
      x <- ifelse(x, true, false)
    }
    if (is.factor(x)) {
      type <- "nominal"
      mode <- if (is.ordered(x)) "ordered" else "unordered"
      x <- codes(x)
    }
    if (is.null(textdata))
      textdata <- type != "character" && type != "datetime"
    
    if (is.character(x)) {
      x <- gsub("&", "&amp;", x)
      x <- gsub("<", "&lt;", x)
      x <- gsub(">", "&gt;", x)
    }

    if (textdata) {
      ## textdata tag
      catSDML("\<textdata type=\"", type, "\"",
              " sep=\"", sep, "\"",
              if (!is.null(mode))
              paste(" mode=\"", mode, "\"", sep = ""),
              if (any(is.na(x[!is.nan(x)])))
              paste(" na.string=\"", na.string, "\"", sep = ""),
              if (type == "character")
              paste(" null.string=\"", null.string, "\"", sep = ""),
              if (type == "numeric") paste(
                    if (any(is.nan(x)))
                    paste(" nan.string=\"", nan.string, "\"", sep = ""),
                    if (any(x == Inf, na.rm = TRUE))
                    paste(" posinf.string=\"", posinf.string, "\"", sep = ""),
                    if (any(x == -Inf, na.rm = TRUE))
                    paste(" neginf.string=\"", neginf.string, "\"", sep = ""),
                    sep = ""),
              if (type == "logical")
              paste(
                    if (any(x == true, na.rm = TRUE))
                    paste(" true=\"", true, "\"", sep=""),
                    if (any(x == false, na.rm = TRUE))
                    paste(" false=\"", false, "\"", sep=""),
                    sep=""
                    ),
              ">",
              file = file
              )
      if (length(x)) {
        catSDML("\n", file = file)
      
        ## replacements
        x <- as.character(x)

        x[is.na(x)] <- na.string
        x[x == ""] <- null.string
      
        if (type == "numeric") {
          x <- gsub("NaN", nan.string, x)
          x <- gsub("^Inf", posinf.string, x)
          x <- gsub("-Inf", neginf.string, x)
        }

        if (type == "logical") {
          x[x == "TRUE"] <- true
          x[x == "FALSE"] <- false
        }

        x <- gsub("&", "&amp;", x)
        x <- gsub("<", "&lt;", x)
        x <- gsub(">", "&gt;", x)
        
        ## write data
        cat(x, sep=c(rep(substr(sep, 1, 1), 9), "\n"), file = file, append = TRUE)
      }
      
      catSDML("</textdata>\n", file = file)
    } else {
      ## data tag
      catSDML("\<data type=\"", type, "\"",
              if (!is.null(mode))
              paste(" mode=\"", mode, "\"", sep = ""),
              if (type == "logical")
              paste(
                    if (any(x == true, na.rm = TRUE))
                    paste(" true=\"", true, "\"", sep=""),
                    if (any(x == false, na.rm = TRUE))
                    paste(" false=\"", false, "\"", sep=""),
                    sep=""
                    ),
              ">",
              file = file)

      if (length(x)) {
        catSDML("\n", file = file)
      
        ## write data
        if (type == "numeric" && !is.null(mode) && mode == "complex")
          cetagsSDML(x, file = file)
        else
          etagsSDML(x, file = file)
        
        catSDML("\n", file = file);
      }
      catSDML("</data>\n", file = file)
    }
    catSDML("\</array>\n", file=file)
  }
}

writeDimensionSDML <- function(x, file = "") 
{
  ## make dataframes behave themselves
  x <- unclass(x)
  
  catSDML("\<dimension>", file = file)
  if (length(x)) catSDML("\n", file = file)
  
  if (!is.null(dim(x))) {
    for (i in 1:length(dim(x))) {	
      catSDML("\<dim size=\"", dim(x)[i], "\"",
              if (!is.null(names(dimnames(x))[i]))
              paste(" name=\"", names(dimnames(x))[i], "\"", sep = ""),
              "\>", file = file)
      
      if (!is.null(dimnames(x)[[i]]))
        etagsSDML(dimnames(x)[[i]], file = file)
      
      catSDML("\</dim>\n", file = file)
    }
  } else if (length(x)) {
    catSDML("\<dim size=\"", length(x), "\"\>", file = file)
    
    if (!is.null(names(x)))
      etagsSDML(names(x), file = file)
    
    catSDML("\</dim>\n", file = file)
  }
  
  catSDML("\</dimension>\n", file = file)	
}

writePropertiesSDML <- function(attrib, file)
{
  attrib[["dim"]] <- NULL
  attrib[["names"]] <- NULL
  attrib[["dimnames"]] <- NULL
  attrib[["length"]] <- NULL
  
  if (!is.null(attrib) && length(attrib) > 0) {
    catSDML("\<properties>\n", file = file)
    
    writeListArraySDML(attrib, file = file, textdata = FALSE)
    
    catSDML("\</properties>\n", file = file)	
  }
}

writeCategoriesSDML <- function(x, file)
{
  catSDML("\<nominalCategories>\n", file = file)

  for (i in 1:length(levels(x)))
    catSDML("\<label code=\"", i, "\">", levels(x)[i], "\</label\>\n", file = file)

  catSDML("\</nominalCategories>\n", file = file)	
}
