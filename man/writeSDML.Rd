\name{writeSDML}
\alias{writeSDML}
\title{Write Data in StatDataML Format}
\description{
  Write data in StatDataML format, either in a file or to
  standard output
}
\usage{
writeSDML(x, file = "", textdata = NULL, dtd = NULL, sep = " \\n",
na.string = "NA", null.string = "NULL", title = deparse(substitute(x)),
source = "R", version = " ", date = NULL, comment = " ", class = "") 
}
\arguments{
  \item{x}{a data object.}
  \item{file}{the name of the file to write to. }
  \item{textdata}{save array elements as \code{textdata} blocks instead
    of \code{data}?   Numeric arrays are by default
    (\code{textdata=NULL}) saved in \code{textdata} blocks, character
    arrays in \code{data} blocks.}
  \item{dtd}{location of the StatDataML DTD.}
  \item{sep}{field separator for \code{textdata} blocks.}
  \item{na.string}{the string which should be interpreted as NA element.}
  \item{null.string}{the string which should be interpreted as NULL element.}
  \item{title}{the title of the data being saved (string).}
  \item{source}{the source of the data being saved (string).}
  \item{version}{the version of the data being saved (string).}
  \item{comment}{a free form commentary for the data being saved (string).}
  \item{date}{a free form date element, date() by default.}
  \item{class}{data class (provided for future enhancements).}
}
\details{For details on the \code{StatDataML} format see the proposal.
}
\author{Torsten Hothorn <Torsten.Hothorn@rzmail.uni-erlangen.de> and Friedrich
  Leisch<leisch@ci.tuwien.ac.at>} 
\seealso{\code{\link{readSDML}}}

\examples{
A <- matrix(1:16, ncol=4)
rownames(A) <- paste("row", 1:4, sep="")
colnames(A) <- paste("col", 1:4, sep="")
writeSDML(A, "testmat.sdml")
}
\keyword{file}
