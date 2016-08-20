\name{showdatetime}
\alias{showdatetime}
\title{Print Date/TIME}
\description{
  Print Date and Time as yyyy-mo-do hr:mi:se msec
}
\usage{
showdatetime(rd, AMPM = FALSE, verbose=TRUE)
}
\arguments{
  \item{rd}{ date time list, jd  hr  mi sec yr }
  \item{AMPM}{24 hour time (AMPM=FALSE) or 12 hour clock (AMPM=TRUE)  }
    \item{verbose}{logical, print information to screen, default=TRUE
}

}

\value{
  Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{
hrs <- seq(from=-36, to=36, by=2)

rd <- recdate(jd=1, hr=hrs, mi=34,
         sec=23+runif(n=length(hrs), 0, 59) , yr=2009)

showdatetime(rd)

showdatetime(rd, AMPM=TRUE)


}
\keyword{misc}
