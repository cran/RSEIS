\name{prep1wig}
\alias{prep1wig}

\title{Prepare structure for RSEIS}
\description{
  Takes list of traces and prepares new list
  for analysis in RSEIS
}
\usage{
prep1wig(wig=vector(), dt=1, sta="STA", comp="CMP", units="UNITS")
}

\arguments{
  \item{wig}{vector of time series}
 \item{dt}{sample interval}
 \item{sta}{character, station name}
 \item{comp}{character,component name}
 \item{units}{character, units of signal}
}
\details{
  prep1wig is offered to reformat a time series
 
  for input to program PICK.GEN()
}
\value{
  Rsac output list
  \item{amp}{amplitude}
  \item{dt}{sample rate}
  \item{nzyear}{year}
  \item{nzhour}{hour}
  \item{nzmin}{minutes}
  \item{nzsec}{seconds}
  \item{nzmsec}{msec}
  \item{b}{sac stuff}
  \item{e}{sac stuff}
  \item{o}{sac stuff}
  \item{fn}{character, file name}
  \item{sta}{character}
  \item{comp}{character}
  \item{DATTIM}{list of date and time}
  \item{N}{number of points}
  \item{units}{character}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN, prepSEIS}
\examples{

data(sunspots)

ES = prep1wig(wig=sunspots, dt=1/12, sta="STA", comp="CMP", units="UNITS"    )

EH=prepSEIS(ES)

STDLAB = c("DONE",  "zoom out", "refresh", "restore", "XTR", "SPEC", "SGRAM" ,"WLET")

xx =  PICK.GEN( EH, STDLAB = STDLAB)


}

\keyword{misc}
