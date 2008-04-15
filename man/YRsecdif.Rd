\name{YRsecdif}
\alias{YRsecdif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return difference in seconds}
\description{
 Difference between two Date/Times (Julian Day)
}
\usage{
YRsecdif(jd1, hr1, mi1, sec1, jd2, hr2, mi2, sec2, yr1 = 0, yr2 = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{jd1}{Julian Day}
  \item{hr1}{hour}
  \item{mi1}{minute}
  \item{sec1}{second}
  \item{jd2}{Julian Day}
  \item{hr2}{ hour }
  \item{mi2}{ minute }
  \item{sec2}{second}
  \item{yr1}{year 1}
  \item{yr2}{ year 2}
}
\details{
  Returns T2-T1, year is used.
}
\value{
  \item{numeric}{seconds}
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{secdifL, secdif }
\examples{


T1 = list(jd=12, hr=13, mi=23, sec=21)
T2 = list(jd=14, hr=23, mi=23, sec=2)

YRsecdif(T1$jd, T1$hr, T1$mi, T1$sec,  T2$jd, T2$hr, T2$mi, T2$sec,
1964, 1976)


}
\keyword{misc}
