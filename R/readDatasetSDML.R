readDatasetSDML <- function(x)
{
	
    ## a dataset contains either a list or an array 
	
    if(!is.null(x[["list"]]))
    { 
        return(readListSDML(x[["list"]]))
    }
    if(!is.null(x[["array"]]))
    { 
        return(readArraySDML(x[["array"]]))
    }
}
