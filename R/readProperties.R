readProperties <- function(x)
{
	return(readListSDML(x$children[["list"]], readProp=F))
}
