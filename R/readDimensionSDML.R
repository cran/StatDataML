readDimensionSDML <- function(x)
{
    dim <- NULL
    names <- list()
    if(xmlName(x) =="dimension"){
        if(xmlSize(x)){
            for(k in 1:xmlSize(x)){
                dim[k] <- xmlAttrs(x[[k]])["size"]
                names[[k]] <- getDataSDML(xmlChildren(x[[k]]))
            }
        }
        else
            dim <- 0
    }
    mode(dim) <- "integer"
    list(dim=dim, names=names)
}
