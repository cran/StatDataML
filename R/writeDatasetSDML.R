writeDatasetSDML <- function(x, file, textdata,
                             sep, na.string, null.string)
{
    ## 2nd level function: responsible for the correct 
    ## choice of StatDataML objects, currently only lists and arrays

    if (is.null(x)) return(NULL)
    
    cat("<dataset>\n", file=file, append=TRUE, sep="")
    
    writeListArraySDML(x, file=file, textdata=textdata,
                       sep=sep, na.string=na.string,
                       null.string=null.string)
    
    cat("</dataset>\n", file=file, append=TRUE, sep="")
}
