catSDML <- function(...) cat(..., append=TRUE, sep="")

etagsSDML <- function(x, ...)
{
  for (i in 1:length(x)) {
    if (is.na(x[i]) && !is.nan(x[i]))
      catSDML("\<na/>", ...)
    else
      tags(x[i], "e", ...)
    
    if((i!=length(x)) & ((i %% 5)==0))
      catSDML("\n", ...)
  }
}

cetagsSDML <- function(x, ...)
{
  for (i in 1:length(x)) {
    if (is.na(x[i]) && !is.nan(x[i]))
      catSDML("\<na/>", ...)
    else {
      catSDML("\<ce>", ...)
      tags(Re(x[i]),"r", ...)
      tags(Im(x[i]),"i", ...)
      catSDML("\</ce>", ...)
    }
    
    if((i!=length(x)) & ((i %% 3)==0))
      catSDML("\n", ...)
  }
}

tags <- function(x, s, ...) {
  if (is.nan(x))
    catSDML("\<",s,"><nan/></",s,">", ...)
  else if (x == Inf)
    catSDML("\<",s,"><posinf/></",s,">", ...)
  else if (x == -Inf)
    catSDML("\<",s,"><neginf/></",s,">", ...)
  else
    catSDML("\<",s,">", x, "\</",s,">", ...)
}

getAttrSDML <- function(x)
{
	if (!is.null(x$attributes))
	{
		return(x$attributes)
	}
	return(NULL)
}
