readDatasetSDML <- function(x)
{
	
    ## a dataset contains either a list or an array 
	
    if(!is.null(x$children[["list"]]))
    { 
        return(readListSDML(x$children[["list"]]))
    }
    if(!is.null(x$children[["array"]]))
    { 
        return(readArraySDML(x$children[["array"]]))
    }
}
