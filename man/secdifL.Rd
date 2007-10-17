\name{secdifL}
\alias{secdifL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Seconds Difference}
\description{
  Given two date/time lists, return seconds diffrence
}
\usage{
secdifL(T1, T2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T1}{list(jd, hr, mi, sec)}
  \item{T2}{list(jd, hr, mi, sec)}
}
\details{
  Year is not included in this calculation.
}
\value{
  
  \item{numeric}{seconds}
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{secdif}
\examples{
T1 = list(jd=12, hr=13, mi=23, sec=21)
T2 = list(jd=14, hr=23, mi=23, sec=2)
secdifL(T1, T2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
