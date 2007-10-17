\name{getHcard}
\alias{getHcard}

\title{Parse UW Hires location Card}
\description{
  Extract High resolution information from H-card
}
\usage{
getHcard(hcard)
}

\arguments{
  \item{hcard}{ascii h-card}
}

\value{
List:
\item{yr}{Year}
\item{mo}{Month}
\item{dom}{Day of Month}
\item{hr}{Hour}
\item{mi}{minute}
\item{sec}{second}
  \item{lat}{latitude}
  \item{lon}{longitude}
 \item{z}{depth}
  \item{mag}{magnitude}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{EmptyPickfile}

\keyword{misc}