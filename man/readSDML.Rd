\name{readSDML}
\alias{readSDML}
\title{Read StatDataML Files}
\description{
  Read a StatDataML file and create a corresponding R object.
}
\usage{
readSDML(file="", text=NULL, validate=FALSE, read.description=FALSE)
}
\arguments{
  \item{file}{the StatDataML file to be read.}
  \item{text}{a string containing StatDataML code (if no file is specified).}
  \item{validate}{logical, should \code{file} be validated using the DTD
    specified in \code{file}?} 
  \item{read.description}{logical, should the \code{description} tag in
    \code{file} be read?} 
}
\details{For details on the \code{StatDataML} format see the proposal.
}
\value{
  a data object with an additional \code{SDMLdescription} attribute}
\author{Torsten Hothorn <hothorn@ci.tuwien.ac.at> and Friedrich Leisch
  <leisch@ci.tuwien.ac.at>}
\seealso{see also \code{\link{writeSDML}}}

\examples{
library(XML)

# write/read vector with names
a <- 1:15
names(a) <- paste("n", 1:15, sep="")
writeSDML(a, "testvec.xml")
b <- readSDML("testvec.xml")
cat(" a is equal b :", all.equal(a,b), "\n")


# write/read a matrix
A <- matrix(1:16, ncol=4)
rownames(A) <- paste("row", 1:4, sep="")
colnames(A) <- paste("col", 1:4, sep="")
writeSDML(A, "testmat.sdml")
B <- readSDML("testmat.sdml")
cat(" A is equal B :", all.equal(A,B), "\n")

# write/read a data.frame
data(iris)
writeSDML(iris, "iris.sdml")
irisSDML <- readSDML("iris.sdml")
cat(" iris is equal irisSDML: ", all.equal(iris,irisSDML), "\n")

# write/read a ts object
data(airmiles)
writeSDML(airmiles, "air.sdml")
airSDML <- readSDML("air.sdml")
cat(" airmiles is equal airSDML: ", all.equal(airmiles,airSDML), "\n")

# write/read the islands data
data(islands)
writeSDML(islands, "islands.sdml")
islandsSDML <- readSDML("islands.sdml")
cat(" islands is equal islandsSDML: ", all.equal(islands,islandsSDML), "\n")

}
\keyword{file}
