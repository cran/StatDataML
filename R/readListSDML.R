readListSDML <- function(x)
{
    if (is.null(x)) return(NULL)
    
    if (xmlName(x) == "list")
    {
        dimension <- readDimensionSDML(x[["dimension"]])
        
        attrib <- NULL
        if (!is.null(x[["properties"]]))
            attrib <- readListSDML(x[["properties"]][["list"]])
        
        thislist <- lapply(xmlChildren(x[["listdata"]]), readListSDML)
        
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
    
    if (xmlName(x) =="array")
    {
        return(readArraySDML(x))
    }
}
