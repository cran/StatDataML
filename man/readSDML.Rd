\name{readSDML}
\alias{readSDML}
\title{Read StatDataML Files}
\description{
  Read a StatDataML file and create a corresponding R object.
}
\usage{
readSDML(file="", text=NULL, validate=FALSE, read.description=FALSE, \dots)
}
\arguments{
  \item{file}{the StatDataML file to be read.}
  \item{text}{a string containing StatDataML code (if no file is specified).}
  \item{validate}{logical, should \code{file} be validated using the DTD
    specified in \code{file}?} 
  \item{read.description}{logical, should the \code{description} tag in
    \code{file} be read?}
  \item{\dots}{arguments passed to \code{\link[XML]{xmlTreeParse}}}
}
\details{For details on the \code{StatDataML} format see the proposal.
}
\value{
  a data object with an additional \code{SDMLdescription} attribute}
\author{David.Meyer@R-Project.org}
\seealso{see also \code{\link{writeSDML}}}

\examples{
library(XML)

TEST <-
    function(x) identical(readSDML(text = capture.output(writeSDML(x))), x)

# write/read vector with names
a <- 1:15
names(a) <- paste("n", 1:15, sep="")
stopifnot(TEST(a))

# write/read a matrix
A <- matrix(1:16, ncol=4)
rownames(A) <- paste("row", 1:4, sep="")
colnames(A) <- paste("col", 1:4, sep="")
stopifnot(TEST(A))

# write/read a data.frame
data(iris)
stopifnot(TEST(iris))

# write/read a ts object
data(airmiles)
stopifnot(TEST(airmiles))

# write/read the islands data
data(islands)
stopifnot(TEST(islands))

}
\keyword{file}
