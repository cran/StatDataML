writeListArraySDML <- function(x, file, textdata, sep,
                               na.string, null.string)
{	
    if(is.recursive(x))
    {	
        catSDML("\<list>\n", file=file)
        writeDimensionSDML(x, file=file)
        writePropertiesSDML(attributes(x), file=file)
                            
        catSDML("\<listdata>\n", file=file)		
        lapply(x,writeListArraySDML, file=file, textdata=textdata,
               sep=sep, na.string=na.string, null.string=null.string)
        catSDML("\</listdata>\n", file=file)	
        catSDML("\</list>\n", file=file)
    }
    else {
        catSDML("\<array>\n", file=file)
        writeDimensionSDML(x, file=file)
        writePropertiesSDML(attributes(x), file=file)
        
        ## check the datatype
        mode <- ""
        if (is.integer(x)) mode <- "integer"
        if (is.real(x)) mode <- "real"
        if (is.complex(x)) mode <- "complex"
        if (is.logical(x)){
            mode <- "logical"
            mode(x) <- "integer"  # write 0/1 instad of FALSE/TRUE
        }
        if (is.character(x)) mode <- "character"
        if(is.null(textdata)){
            textdata <- ifelse(mode=="character", FALSE, TRUE)
        }
        
        if(length(x))
        {
            if(is.character(x)){
                x <- gsub("&", "&amp;", x)
                x <- gsub("<", "&lt;", x)
                x <- gsub(">", "&gt;", x)
            }
            if(textdata){
                x[is.na(x)] <- na.string
                if(is.factor(x)){
                    x <- as.character(codes(x))
                    mode <- "integer"
                }
                else
                    x <- as.character(x)
                
                catSDML("\<textdata mode=\"", mode,
                        "\" na.string=\"", na.string, "\">\n", file=file)
                cat(x, sep=c(rep(" ", 9), "\n"), file=file, append=TRUE)
                catSDML("</textdata>\n", file=file)
            }
            else{
                catSDML("\<data mode=\"", mode, "\">\n", file=file) 
                etagsSDML(x, file=file)           
                catSDML("\n</data>\n", file=file)
            }
        }
        catSDML("\</array>\n", file=file)
    }
}

writeDimensionSDML <- function(x, file="") 
{
    catSDML("\<dimension>\n", file=file)

    ## make dataframes behave themselves
    x <- unclass(x)

    if (!is.null(dim(x)))
    {
        for(i in 1:length(dim(x)))
        {	
            catSDML("\<dim size=\"", dim(x)[i], "\"\>", file=file)
            if(!is.null(dimnames(x)[[i]])){
                etagsSDML(dimnames(x)[[i]], file=file)
            }
            catSDML("\</dim>\n", file=file)
        }
    }
    else if (length(x))
    {
        catSDML("\<dim size=\"", length(x), "\"\>", file=file)
        if(!is.null(names(x))){
            etagsSDML(names(x), file=file)
        }
        catSDML("\</dim>\n", file=file)
    }
    catSDML("\</dimension>\n", file=file)	
}



writePropertiesSDML <- function(attrib, file)
{
    attrib[["dim"]] <- NULL
    attrib[["names"]] <- NULL
    attrib[["dimnames"]] <- NULL
    attrib[["length"]] <- NULL
    if (!is.null(attrib) && length(attrib) > 0)
    {
        catSDML("\<properties>\n", file=file)
        writeListArraySDML(attrib, file=file, textdata=FALSE)
        catSDML("\</properties>\n", file=file)	
    }
}

