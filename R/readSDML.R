readSDML <- function(file="", text=NULL, validate=FALSE,
                     read.description=FALSE)
{
    if (is.null(text))
    {
        tree <- xmlTreeParse(file=file, asText=FALSE, validate=validate,
                             handler=handlersSDML)
    } else { 
        tree <- xmlTreeParse(file=text, asText=TRUE, validate=validate,
                             handler=handlersSDML)
    }
    
    ## is this file a StatDataML file ?    
    if (!is.null(tree$doc$children[["StatDataML"]]))
    {
        statxml <- tree$doc$children[["StatDataML"]]
        if (!is.null(statxml$children[["description"]]) && read.description)
        {
            description <-
                readDescriptionSDML(statxml$children[["description"]])
        }
        if (!is.null(statxml$children[["dataset"]]))
        {
            dataset <- readDatasetSDML(statxml$children[["dataset"]])
        }
    }
    else
        stop("This is no StatDataML file");
    
    if (read.description)
        attr(dataset,"SDMLdescription") <- description
    return(dataset)
}
 
