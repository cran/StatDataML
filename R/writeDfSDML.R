writeDfSDML <- function(x, file="")
{
		# writes a dataframe.dtd compatible SDML object

		if(!is.data.frame(x))
			stop("x is not a data.frame")

		writeSDML(x, file=file, dtd=system.file("dtd/dataframe.dtd", package="StatDataML"), SDMLclass="dataframe")
}
