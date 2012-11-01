\name{OH}
\alias{OH}
\docType{data}
\title{Delta-O18 isotpe record}
\description{
  Data from Delta-O18 Isotope record of climate change.
  Periodicities of this data show the Milancovic cycles.
}
\usage{ data(OH) }
\format{
OH = 
list( LOC=list(yr=0, jd=0, mo=0, dom=0, hr=0, mi=0, sec=0,
lat=0, lon=0, z=0, mag=0, gap=0,
   delta=0 , rms=0, hozerr=0),
   MC=list(az1=0, dip1=0, az2=0, dip2=0,  dir=0, rake1=0,
   dipaz1=0, rake2=0, dipaz2=0,
   F=list(az=0, dip=0), G=list(az=0, dip=0), U=list(az=0, dip=0),
V=list(az=0, dip=0), P=list(az=0, dip=0), T=list(az=0,
dip=0),sense=0,M=list( az1=0, d1=0,  az2=0, d2=0, uaz=0,
ud=0, vaz=0,
vd=0, paz=0, pd =0, taz=0, td=0), UP=TRUE, icol=1, ileg="",
fcol='red', CNVRG="", LIM =c(0,0,0,0)
),

STAS=list(tag="", name="", comp="", c3="", phase="",
sec=0, err=0, pol="", flg=0 , res=0),

LIP=vector(length=6),

H=list(yr=0,mo=0,dom=0,hr=0,mi=0,sec=0,lat=0,lon=0,z=0,mag=0),

N=list(name=""),

E=list(rms=0,meanres=0,sdres=0,sdmean=0,sswres=0,ndf=0,
fixflgs=0,sterrx=0,sterry=0,sterrz=0,sterrt=0,mag=0,sterrmag=0),

filename="",

PICKER="",
UWFILEID="",winID1="",comments="", OSTAS="")

}
\note{
  The sample unit here is set to 0.3 which
  is 10000 times the correct sample rat.

  }
\references{
Lees, J. M. and J. Park (1995): Multiple-taper spectral analysis: A stand-alone C-subroutine: Computers & Geology: 21, 199-236.

}
\examples{
data(OH)
xx <-  swig( OH, sel=which(OH$COMPS == "V"))

}
\keyword{datasets}
