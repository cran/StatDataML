readListSDML <- function(x)
{
    if (is.null(x)) return(NULL)
    
    if (x$name == "list")
    {
        dimension <- readDimensionSDML(x$children[["dimension"]])
        
        attrib <- NULL
        if (!is.null(x$children[["properties"]]))
            attrib <- readListSDML(x$children[["properties"]]$children[["list"]])
        
        thislist <- lapply(x$children[["listdata"]]$children, readListSDML)
        
        if(names(thislist) =="array") names(thislist) <- NULL
        
        if(length(dimension$dim)>1){
            dim(thislist) <- dimension$dim
            dimnames(thislist) <- dimension$names
        }
        else
            if(length(dimension$names[[1]])>0)
                names(thislist) <- dimension$names[[1]]

        atL <- attributes(thislist)
        if (!is.null(atL)) attrib <- c(attrib, atL)
        if (!is.null(attrib)) attributes(thislist) <- attrib
       
        return(thislist)
    }
    
    if (x$name =="array")
    {
        return(readArraySDML(x))
    }
}
