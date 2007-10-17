\name{unpackAcard}
\alias{unpackAcard}

\title{Parse Acard from UW-format pickfile}
\description{
 Parse Acard from UW-format pickfile
}
\usage{
unpackAcard(AC)
}

\arguments{
  \item{AC}{ascii acard}
}
\details{
  Reads and Parses A-cards from UW foprmatted data.
  
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
    \item{gap}{gap in station coverage}
    \item{delta}{distance to nearest station}
    \item{rms}{root mean square residual}
   \item{ hozerr}{horizontal error}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}