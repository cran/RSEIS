\name{meshgrid}
\alias{meshgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a mesh grid like in Matlab}
\description{
  Creates 2D matrices for accessing images and 2D matrices
}
\usage{
meshgrid(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{x vector components}
  \item{b}{y vector components}
}
\details{
  returns outer product of x-compnents and y-components for
  use as index arrays
}
\value{
 
  \item{x}{length(y) by length(x) matrix of x indicies}
  \item{y}{length(y) by length(x) matrix of y indicies}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

meshgrid(1:5, 1:3)


}

\keyword{misc}
