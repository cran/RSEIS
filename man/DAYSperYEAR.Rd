\name{DAYSperYEAR}
\alias{DAYSperYEAR}
\title{Days per Year}
\description{
  Calculate the number of days per calendar year
}
\usage{
DAYSperYEAR(yr)
}
\arguments{
  \item{yr}{year}
}

\value{
    \item{days}{integer number of days for a given year}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

years = seq(from=1850, to=2010, by=1)

DAYSperYEAR(years)


}
\keyword{misc}
