readDimensionSDML <- function(x)
{
    dim <- NULL
    names <- list()
    if(x$name =="dimension"){
        if(length(x$children)){
            for(k in 1:length(x$children)){
                dim[k] <- x$children[[k]]$attributes["size"]
                names[[k]] <- getDataSDML(x$children[[k]]$children)
            }
        }
        else
            dim <- 0
    }
    mode(dim) <- "integer"
    list(dim=dim, names=names)
}
