catSDML <- function(...) cat(..., append=TRUE, sep="")

etagsSDML <- function(x, ...)
{
    for(i in 1:length(x)){
        if(is.na(x[i]))
            catSDML("\<na/>", ...)
        else
            catSDML("\<e>", x[i], "\</e>", ...)
        
        if((i!=length(x)) & ((i %% 5)==0))
            catSDML("\n", ...)
    }
}

getAttrSDML <- function(x)
{
	if (!is.null(x$attributes))
	{
		return(x$attributes)
	}
	return(NULL)
}
