readArraySDML <- function(x)
{
  if (is.null(x)) return(NULL)
    
  ## parse dimension
  dimension <- readDimensionSDML(x[["dimension"]])

  ## parse properties
  attrib <- readProperties(x[["properties"]])

  ## parse categories
  labels <- readCategoriesSDML(x[["nominalCategories"]])
  
  if (!is.null(x[["data"]]) || !is.null(x[["textdata"]])) {
    
    if(!is.null(x[["data"]])) {
      attribs <- getAttrSDML(x[["data"]])
      type <- attribs[["type"]]
      mode <- default(attribs, "mode", NULL)
      vals <- if (!is.null(type) && type == "numeric" && mode == "complex")
          getComplexDataSDML(xmlChildren(x[["data"]]))
      else
        getDataSDML(xmlChildren(x[["data"]]))
    } else {
      attribs <- getAttrSDML(x[["textdata"]])
      type <- attribs[["type"]]
      mode <- default(attribs, "mode", NULL)
      vals <- if (is.null(xmlChildren(x[["textdata"]])))
        character()
      else
        getTextDataSDML(xmlChildren(x[["textdata"]])[[1]], attribs, type)
    }

    if (length(vals) != prod(dimension$dim))
      stop(paste("Wrong dimension !", paste(dimension$dim, collapse=", "), length(vals), collapse= " "))
    
    if (length(dimension$dim) > 1) {
      vals <- array(vals, dim=dimension$dim, dimnames=dimension$names)
    } else {
      if (!length(vals))
        vals <- vector()
      else
        if (length(dimension$names[[1]]))
          names(vals) <- dimension$names[[1]]
    }

    ## handle types
    if (!is.null(type)) {
      
      if (type == "logical") {
        tr <- as.character(default(attribs, "true", "1"))
        fa <- as.character(default(attribs, "false", "0"))
        vals1 <- vals == tr
        vals2 <- !(vals == fa)
        vals <- vals1
        vals[vals != vals2] <- NA
      }
      
      if (type == "numeric") {
        if (mode %in% c("integer", "real", "complex"))
          mode(vals) <- mode
        else ## default mode if none
          mode(vals) <- "double"
      }
      
      if (type == "nominal") {
        if (mode == "ordered")
          vals <- ordered(as.integer(vals), labels = labels)
        else
          vals <- factor(as.integer(vals), labels = labels)
      }
      
      if (type == "datetime")
        vals <- as.POSIXct(strptime(vals, format="%Y-%m-%dT%H:%M:%S"))

      if (type == "character")
        vals <- as.character(vals)
    }
    
    atvals <- attributes(vals)
    if (!is.null(atvals)) attrib <- c(attrib, atvals)
    if (!is.null(attrib)) attributes(vals) <- attrib
    
    return(vals)
  }
}

getComplexDataSDML <- function(y)
  as.complex(sapply(y, function(x) {
    if (x$name=="na") return(NA)
    cs <- getDataSDML(xmlChildren(x))
    complex(1, as.double(cs[1]), as.double(cs[2]))
  }))

getDataSDML <- function(y) 
{
    w <- sapply(y,
                function(x) ifelse(x$name=="na", NA,
                                   if (is.null(x[[1]])) "" else 
                                   switch(x[[1]]$name,
                                          posinf = +Inf,
                                          neginf = -Inf,
                                          nan    = NaN,
                                          xmlValue(x[[1]])
                                          )
                                   )
                )
    
    if(is.character(w)) {
        w <- gsub("&amp;", "&", w)
        w <- gsub("&lt;", "<", w)
        w <- gsub("&gt;", ">", w)
    }

    attributes(w) <- NULL
    w
}

getTextDataSDML <- function(y, attribs, type) 
{
  y <- gsub("&amp;", "&", y)
  y <- gsub("&lt;", "<", y)
  y <- gsub("&gt;", ">", y)
  y[y == attribs[["na.string"]]] <- NA
  if (type == "character")
    y[y == attribs[["null.string"]]] <- ""
  if (type == "numeric") {
    y <- gsub(attribs[["neginf.string"]], "-Inf", y)
    y <- gsub(attribs[["posinf.string"]], "Inf", y)
    y <- gsub(attribs[["nan.string"]], "NaN", y)
  }
  if (type == "logical") {
    y[y == attribs[["true"]]] <- "1"
    y[y == attribs[["false"]]] <- "0"
  }
  y
}

default <- function (attr, name, defval) 
  if (name %in% names(attr)) attr[name] else defval
