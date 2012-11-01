\name{recdate}
\alias{recdate}
\alias{recdatel}
\title{Rectify Date }
\description{
 Rectify a date that may be out of wack.
}
\usage{
recdate(jd=0, hr=0, mi=0, sec=0, yr=0)
recdatel(X)

}

\arguments{
  \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}
  \item{yr}{year}

  or
  \item{X}{list of date}

  
}
\details{
  Returns date with correct numbers. So if number of seconds is
  greater than 60, will add to minutes...
}
\note{
  Default value for jd is 1, the rest are 0. This function now
  should successfully span year breaks.  Leap years are correctly
  accounted for too.
  }
\value{
 \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}
    \item{yr}{year }

}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{showdatetime, DAYSperYEAR, fromjul,getjul, tojul, getmoday }
\examples{

recdate(76, 23, 22, yr=2000)

###########  example spanning leap year
##  start on Day 1, but subtract 36 hours and proceed to plus 36 hours
hrs <- seq(from=-36, to=36, by=2)
rd <- recdate(jd=1, hr=hrs, mi=34,
       sec=23+runif(n=length(hrs), 0, 59) , yr=2009)
write.table(data.frame(rd))

###########  example spanning non-leap year
rd2 <- recdate(jd=1, hr=hrs, mi=34,
     sec=23+runif(n=length(hrs), 0, 59) , yr=2008)
write.table(data.frame(rd2))


}

\keyword{misc}
