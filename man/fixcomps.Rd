\name{fixcomps}
\alias{fixcomps}

\title{Fix Station Component Names}
\description{
  Convert components to common names: V N E
}
\usage{
fixcomps(oldcomps, SEGY = FALSE)
}

\arguments{
  \item{oldcomps}{vector of compnents}
  \item{SEGY}{logical, TRUE= segy data with compnents 4,5,6 or 1,2,3}
}
\details{
 Attemps to convert irregular component names to common format for later
 processing.
}
\value{
  character vector
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{fixcompname }
\examples{
comp <- c("v", "e")
fixcomps(comp)
}


\keyword{misc}