readSDML <- function(file="", text=NULL, validate=FALSE,
                     read.description=FALSE)
{
    if (is.null(text))
    {
        tree <- xmlTreeParse(file=file, asText=FALSE, validate=validate,
                             handler=handlersSDML(), asTree=T)
    } else { 
        tree <- xmlTreeParse(file=text, asText=TRUE, validate=validate,
                             handler=handlersSDML(), asTree=T)
    }
    
    ## is this file a StatDataML file ?    
    if (xmlName(xmlRoot(tree)) == "StatDataML")
    {
        statxml <- xmlRoot(tree)
        if (!is.null(statxml[["description"]]) && read.description)
        {
            description <-
                readDescriptionSDML(statxml[["description"]])
        }
        if (!is.null(statxml[["dataset"]]))
        {
            dataset <- readDatasetSDML(statxml[["dataset"]])
        }
    }
    else
        stop("This is no StatDataML file");
    
    if (read.description)
        attr(dataset,"SDMLdescription") <- description
    return(dataset)
}
 
