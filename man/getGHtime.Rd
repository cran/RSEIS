\name{getGHtime}
\alias{getGHtime}
\title{Get Seismic reference time}
\description{
  Extract the times of all traces relative to a reference
  trace on a seismic RSEIS list.
}
\usage{
getGHtime(GH, wi = 1, pix = NULL)
}
\arguments{
  \item{GH}{RSEIS seismic data list}
  \item{wi}{which event to use as a reference baseline}
  \item{pix}{list of time to difference}
}

\value{
  list:  times relative to reference time:
  \item{yr}{year}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
  \item{spix}{seconds after reference}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{secdifL, secdif}
\examples{
 data(GH)

getGHtime(GH)

}
\keyword{misc}
