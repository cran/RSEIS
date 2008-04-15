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
####################
####################
####################   example with multiple signals

dt = 0.001
t = seq(0, 6, by=0.001)
######   % sample rate = 1000 Hz, 0.001 seconds 601 samples

###  set up the fequencies and amplitudes for signals that have 2 frequencies
afreqs1 = c(50, 40,10, 5 )
amps1 = c(6, 2,3, 2 )
####
afreqs2 = c(120,30,20, 30  )
amps2 = c(10,5, 9, 2 )


 x = cbind( amps1[1]*sin(2*pi*afreqs1[1]*t) + amps2[1]* sin(2*pi*afreqs2[1]*t),
amps1[2]*sin(2*pi*afreqs1[2]*t) + amps2[2]* sin(2*pi*afreqs2[2]*t),
amps1[3]*sin(2*pi*afreqs1[3]*t) + amps2[3]* sin(2*pi*afreqs2[3]*t),
amps1[4]*sin(2*pi*afreqs1[4]*t) + amps2[4]* sin(2*pi*afreqs2[4]*t))

d = dim(x)

########  names of signals
mysta=c("R1", "R2", "R3", "R4")

MYLIST = list()

############  set up the initial list of wiggles
for(i in 1:d[2])
{

A = prep1wig(wig =x[,i], sta=mysta[i], dt=dt, comp="DO", units="amp") 
A[[1]]$DATTIM$yr=2000
MYLIST = c(MYLIST, A)

}

###  reorganize into RSEIS format:
PH1=prepSEIS(MYLIST)

STDLAB = c("DONE",  "zoom out", "refresh", "restore", "XTR", "SPEC", "SGRAM" ,"WLET")

PICK.GEN(PH1, STDLAB = STDLAB)



}

\keyword{misc}
