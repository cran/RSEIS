\name{GAZI}
\alias{GAZI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get azimuthal particle motion}
\description{
Do particle motion analysis
}
\usage{
GAZI(ADAT, dt = 1, ex = seq(0, 100), comp = c(4, 5, 6), sta = "ZZZ", az = 0, len = 50, shift = 10, prev = 1, fileid = "", picks = NA, labs = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ADAT}{Matrix of 3 component seismic data}
  \item{dt}{delta T (s)}
  \item{ex}{x-axis vector}
  \item{comp}{component names}
  \item{sta}{station name}
  \item{az}{azimuth of station orientation}
  \item{len}{length of time series}
  \item{shift}{amount to shift per window}
  \item{prev}{length of buffer at beginning of trace}
  \item{fileid}{character string to put on plot}
  \item{picks}{arrival times for annotation}
  \item{labs}{labels for arrival times for annotation}
}

\value{
 list(aex=aex[1:jall], rateig=rateig[1:jall], aaz=aaz[1:jall], ai=ai[1:jall], figaz=figaz, azpar=azpar, incpar=incpar )
}


\examples{

data("GH")


temp = cbind(GH$JSTR[[4]], GH$JSTR[[5]], GH$JSTR[[6]])

pmolabs=c("Vertical", "North", "East")


 G  = GAZI(temp, dt =GH$dt[4]  , comp = pmolabs, sta = GH$STNS[4] , az = 0, len =75, shift = 10, prev = 1)

}

\keyword{hplot}
