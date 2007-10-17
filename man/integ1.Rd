\name{integ1}
\alias{integ1}

\title{Integrate seismogram}
\description{
  integrate under the curve of a pulse
}
\usage{
integ1(x, y, dm = -Inf, hm = +Inf)
}

\arguments{
  \item{x}{x-axis vector}
  \item{y}{y-axis vector}
  \item{dm}{lower bound}
  \item{hm}{upper bound}
}

\value{
 vector: c(osum,cista)
  one with the bottom triangle included one without
}

\author{Jonathan M. Lees<jonathan.lees.edu>}



\keyword{misc}
