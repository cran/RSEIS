\name{secdifv}
\alias{secdifv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Seconds Difference}
\description{
  Given two date/time vectors, return seconds diffrence
}
\usage{
secdifv(T1, T2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T1}{c(jd, hr, mi, sec)}
  \item{T2}{c(jd, hr, mi, sec)}
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
T1 = c(12, 13, 23, 21)
T2 = c(14, 23, 23, 2)
secdifv(T1, T2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
