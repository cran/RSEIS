\name{SEARCHPIX}
\alias{SEARCHPIX}

\title{Search Pix}
\description{
Search through pick strucutre to select phase arrivals
}
\usage{
SEARCHPIX(KPX, IPX, tol = 0.5)
}

\arguments{
  \item{KPX}{user locator pix}
  \item{IPX}{ set of pix in memory}
  \item{tol}{tolerance, s}
}
\details{
  returns index vector of picks that satisfy:
   wn = which(  abs(t2-t1) < tol)
}
\value{
index vector
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

\dontrun{SEARCHPIX(KPX, IPX, tol = 0.5) }


}

\keyword{misc}
