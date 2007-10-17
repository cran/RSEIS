\name{fixcompname}
\alias{fixcompname}

\title{Fix component names for uniformity}
\description{
  Fix component names for uniformity
}
\usage{
fixcompname(comp)
}

\arguments{
  \item{comp}{4, "SHV"}
}
\details{
  Translate the component names to
  something uniform that can be used for sorting and other
  functions.
}
\value{
 one of "V", "N", "E" 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

fixcompname("SHV")
fixcompname("SHE")

}

\keyword{misc}
