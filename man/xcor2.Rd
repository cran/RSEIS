\name{xcor2}
\alias{xcor2}

\title{Cross Correlation}
\description{
Cross correlation of two signals
}
\usage{
xcor2(a1, a2, DT, PLOT = FALSE, LAG = 100)
}

\arguments{
  \item{a1}{input signal 1}
  \item{a2}{input signal 1}
  \item{DT}{deltaT in seconds}
  \item{PLOT}{logical TRUE=plot}
  \item{LAG}{ time lag for correlation function }
}
\details{
 Illustrates the cross correlation of two time series.
}
\value{
  \item{ccf}{Return list from function ccf}
  \item{mlag}{maximum lag in time}
  \item{mccx}{value of ccf at max lag mlag}
  \item{mlag2}{maximum absolute value lag }
  \item{mccx2}{value of ccf at mlag2}
}
\seealso{ccf}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\examples{

data(CE1)

ts1 <- CE1$y[CE1$x>5.443754 & CE1$x<5.615951]

ts2 <- CE1$y[CE1$x>5.760959]
ts2 <- ts2[1:length(ts1)]

ts1 <- ts1-mean(ts1)
ts2 <- ts2-mean(ts2)

xc <- xcor2(ts1, ts2, CE1$dt , PLOT = TRUE)

}

\keyword{misc}
