readArraySDML <- function(x)
{
#    x$name <- NULL
    
    if (is.null(x)) return(NULL)
    
    dimension <- readDimensionSDML(x[["dimension"]])

    attrib <- NULL
    if (!is.null(x[["properties"]]))
        attrib <- readListSDML(x[["properties"]][["list"]])	
    
    if (!is.null(x[["data"]]) |
        !is.null(x[["textdata"]]))
    {
        attribs <- getAttrSDML(x[["data"]])

        if(!is.null(x[["data"]])){
            vals <- getDataSDML(xmlChildren(x[["data"]]))
            mode <- xmlAttrs(x[["data"]])["mode"]
        }
        else{
            vals <- x[["textdata"]][["value"]]
            mode <- x[["textdata"]][["mode"]]
        }

        if (length(vals) != prod(dimension$dim))
            stop(paste("Wrong dimension !", paste(dimension$dim, collapse=", "), length(vals), collapse= " "))
            
        if (length(dimension$dim) > 1){
            vals <- array(vals, dim=dimension$dim, dimnames=dimension$names)
        }
        else{
            if(length(vals)==0)
                vals <- vector()
            else{
                if(length(dimension$names[[1]]))
                    names(vals) <- dimension$names[[1]]
            }
        }
        

        if(!is.null(mode)){
            if(mode %in% c("logical","integer","real","complex"))
                mode(vals) <- mode
        }
        
        atvals <- attributes(vals)
        if(!is.null(atvals)) attrib <- c(attrib, atvals)
        if(!is.null(attrib)) attributes(vals) <- attrib
        
        
        return(vals)
    }
    
    
}
	
getDataSDML <- function(y) 
{
    w <- sapply(y, function(x) ifelse(x$name=="na",
                                      NA, xmlValue(x[[1]])))
    if(length(w)>0){
        w <-gsub("&amp;", "&", w)
        w <-gsub("&lt;", "<", w)
        w <-gsub("&gt;", ">", w)
    }
    w
}
