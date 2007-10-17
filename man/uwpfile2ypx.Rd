\name{uwpfile2ypx}
\alias{uwpfile2ypx}

\title{UW pickfile to pphase pick data.frame}
\description{
 Read in ASCII version of pickfile.
}
\usage{
uwpfile2ypx(P)
}

\arguments{
  \item{P}{pickfile}
}

\value{
  list:
  \item{STAS}{input structure}
  \item{yr}{year}
  \item{mo}{month}
  \item{dom}{day of month}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
  \item{col}{color}
  \item{onoff}{logical, TRUE plot trace}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
