\name{gpoly}
\alias{gpoly}

\title{Convert Poles and Zeros to  Polynomial}
\description{
  Get Polynomial from Poles and Zeros
}
\usage{
gpoly(x)
}

\arguments{
  \item{x}{complex vector of poles or zeros}
}

\value{
 vector of coefficients
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

K <- PreSet.Instr()

##  convert zeros to polynomial coefficients

gpoly(K[[1]]$zeros)

}

\keyword{misc}
