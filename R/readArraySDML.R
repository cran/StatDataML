readArraySDML <- function(x)
{
    x$name <- NULL
    
    if (is.null(x)) return(NULL)
    
    dimension <- readDimensionSDML(x$children[["dimension"]])
    
    attrib <- NULL
    if (!is.null(x$children[["properties"]]))
        attrib <- readListSDML(x$children[["properties"]]$children[["list"]])	
    
    if (!is.null(x$children[["data"]]) |
        !is.null(x$children[["textdata"]]))
    {
        attribs <- getAttrSDML(x$children[["data"]])

        if(!is.null(x$children[["data"]])){
            vals <- getDataSDML(x$children[["data"]]$children)
            mode <- x$children[["data"]]$attributes["mode"]
        }
        else{
            vals <- x$children[["textdata"]][["value"]]
            mode <- x$children[["textdata"]][["mode"]]
        }
        if (length(vals) != prod(dimension$dim))
            stop("Wrong dimension!")
            
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
                                      NA, x$children$text$value))
    if(length(w)>0){
        w <-gsub("&amp;", "&", w)
        w <-gsub("&lt;", "<", w)
        w <-gsub("&gt;", ">", w)
    }
    w
}
