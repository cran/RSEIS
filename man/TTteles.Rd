\name{TTteles}
\alias{TTteles}
\title{Read output of ttBK}
\description{
 This routine assumes you have a program called ttimes,
  or some equivalent.  The output of that routine
  is dumped to a file by ttBK a routine I wrote,
  and R then reads this in.
}
\usage{
TTteles(EQ, staLLZ, ista = c(1), IASPHOME = "")
}
\arguments{
  \item{EQ}{earthquake list with time, lat, lon, z}
  \item{staLLZ}{station list with lat lon z}
  \item{ista}{index vector of stations to use}
  \item{IASPHOME}{location of files for ttBK (already created)}
}
\details{
  Requires external program ttBK written by J. M. Lees.  The
  binary input files to ttBK are iasp91.hed and iasp91.tbl.
  These must be created before using this routine.  The R code
  links these files to the local directory and then executes
  an external fortran program to run the tau-p calculation.
}
\value{
Returns a list, which each station, 
\item{num}{index number}
\item{phase}{phase name}
\item{sec}{seconds}
\item{tmin}{minutes}
\item{tsec}{fractional seconds after minutes}
\item{dtdd}{derivative of time w.r.t distance dt/dd}
\item{dtdh}{derivative of time w.r.t depth dt/dh}
\item{dddp}{derivative of distance w.r.t ray parameter dd/dp (i think)}

}
\references{
Buland and Chapman (1983) in "The Computation of
Seismic Travel Times", BSSA, v. 73, pp. 1271-1302

}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Requires external program ttBK written by J. M. Lees.
  Requires that the working directory be read/write because
  files are created outside of R.
}
\seealso{}
\examples{

\dontrun{
###  The station:

PEN = list(name="PEN", lat=35+56/60+4.90/(3600)  ,
lon=-(82+7/60+8.71/3600), z= .817)

###  read in earthquake catalog, and reformat:
geteq5 = scan(file="tempEQ", what="", sep="\n")

glist = list()
for(i in 1:length(geteq5))
{
z1 = unlist(strsplit(geteq5[i], split="\t"))

z1 = z1[z1!=""]


yr = as.numeric(substr(z1[1], 1,4))
mo = as.numeric(substr(z1[1], 6,7))
dom = as.numeric(substr(z1[1], 9 ,10))

hr = as.numeric(substr(z1[1], 12,13))
mi = as.numeric(substr(z1[1], 15,16))
sec = as.numeric(substr(z1[1], 18,24))
lat=as.numeric(z1[2])
lon=as.numeric(z1[3])
depth=as.numeric(z1[4])
mag=as.numeric(z1[5])

jd = getjul(yr, mo, dom)
glist[[i]] = list(yr=yr, jd=jd, mo=mo, dom=dom, hr=hr, mi=mi, sec=sec, lat=lat, lon=lon, z=depth, mag=mag)

}

###  now you have a list of stations and earthquakes, can run ttimes
###  (ttBK) and reformat output

Aout = TTteles(glist[[1]], PEN, ista = 1, IASPHOME = "/home/lees/Progs/TTIMES")

}

}
\keyword{misc}
