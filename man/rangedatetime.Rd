\name{rangedatetime}
\alias{rangedatetime}
\title{Range of Date Time}
\description{
  Return the range of dates and times for any list with a date/time list
}
\usage{
rangedatetime(D)
}
\arguments{
  \item{D}{info list from RSEIS seismic data list}
}

\value{
  \item{min}{date time list}
  \item{max}{date time list}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
data(GH)

rangedatetime(GH$info)


}
\keyword{misc}
