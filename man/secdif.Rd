\name{secdif}
\alias{secdif}

\title{Return difference in seconds}
\description{
  Difference between two Date/Times (Julian Day)
}
\usage{
secdif(jd1, hr1, mi1, sec1, jd2, hr2, mi2, sec2)
}

\arguments{
  \item{jd1}{Julian Day}
  \item{hr1}{hour}
  \item{mi1}{minute}
  \item{sec1}{second}
  \item{jd2}{Julian Day}
  \item{hr2}{ hour }
  \item{mi2}{ minute }
  \item{sec2}{second}
}
\details{Returns T2-T1.  Year is not included.
}
\value{
 \item{numeric}{seconds}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{secdifL }
\examples{
T1 <- list(jd=12, hr=13, mi=23, sec=21)
T2 <- list(jd=14, hr=23, mi=23, sec=2)
secdif(T1$jd, T1$hr, T1$mi, T1$sec,  T2$jd, T2$hr, T2$mi, T2$sec)

}

\keyword{misc}
