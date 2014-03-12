\name{writeSDML}
\alias{writeSDML}
\title{Write Data in StatDataML Format}
\description{
  Write data in StatDataML format, either in a file or to
  standard output
}
\usage{
writeSDML(x, file = "", textdata = NULL, dtd = NULL, sep = " &#x000A;&#x000D;",
na.string = "NA", null.string = "NULL", posinf.string = "+Inf",
neginf.string = "-Inf", nan.string = "NaN", true = "1", false = "0",
title = deparse(substitute(x)), source = "R", version = " ",
date = NULL, comment = " ", properties = NULL) 
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
  \item{na.string}{the string which should be interpreted as \code{NA} element.}
  \item{null.string}{the string which should be interpreted as NULL
    string.}
  \item{posinf.string}{the string which should be interpreted as \code{+Inf}.}
  \item{neginf.string}{the string which should be interpreted as \code{-Inf}.}
  \item{nan.string}{the string which should be interpreted as
    \code{NaN}.}
  \item{true, false}{the strings which should be interpreted as \code{TRUE}/\code{FALSE}.}
  \item{title}{the title of the data being saved (string).}
  \item{source}{the source of the data being saved (string).}
  \item{version}{the version of the data being saved (string).}
  \item{comment}{a free form commentary for the data being saved (string).}
  \item{date}{a free form date element, date() by default.}
  \item{properties}{an arbitrary list or array.}
}
\details{\code{info} attributes of arrays are used for the 
  \code{info} attributes of the \code{e} / \code{ce} / \code{na} tags
  in StatDataML. For further details on the \code{StatDataML} format see the proposal.
}
\author{David.Meyer@R-Project.org} 
\seealso{\code{\link{readSDML}}}

\examples{
A <- matrix(1:16, ncol=4)
rownames(A) <- paste("row", 1:4, sep="")
colnames(A) <- paste("col", 1:4, sep="")
writeSDML(A, "testmat.sdml")

I <- letters[1:16]
attr(A, "info") <- I
writeSDML(A, "testmat2.sdml", textdata = FALSE)
}
\keyword{file}
