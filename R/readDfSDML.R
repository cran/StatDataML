readDfSDML <-function(file="", text=NULL, validate=F)
{
    dataf <- readSDML(file=file, text=text, validate=validate, read.description=T)
    
    if (is.data.frame(dataf)) return(dataf)
    else {
        if (!(attributes(dataf)$SDMLdescription$SDMLclass == "data.frame"))
        {
            stop("This file does not includes a data.frame object!")
        } else {
            if (!is.list(dataf)) stop("Not a correct data.frame object")
            numberrows <- sapply(dataf, length)
            error <- all(numberrows == numberrows[1])
            if (!error) stop("Not a correct data.frame object")
            return(as.dataframe(dataf))
        }
    }
}

