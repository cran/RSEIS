\name{GH}
\alias{GH}
\docType{data}
\title{Earthquake Seismic Data}
\description{
Example of seismic data structure. Geothermal Earthquake.
}
\usage{ data(GH) }
\format{
  List, consisting of:
  \describe{
    \item{JSTR}{list of digital seismic data traces}
    \item{STNS}{vector of stations}
    \item{dir}{directory}
    \item{ifile}{original file names}
    \item{COMPS}{Component names, V N E, e.g.}
    \item{OCOMPS}{Old Component names}
    \item{dt}{vector of delta-t, sampling time intervals}
    \item{KNOTES}{Notes for plotting on panels}
    \item{info}{List, detailed information about traces, including}
    \item{dat}{not used}
    \item{nn}{Number of traces}
    \item{ex}{time axis for plotting}
    \item{pcol}{colors for plotting}
    \item{ok}{which traces are okay}
    \item{wintim}{window span time, seconds}
    \item{ftime}{alphanumeric time stamp}
    \item{pickfile}{pickfile, see below}
    \item{velfile}{velocity model list}
    \item{stafile}{station information list including lat, lon, z}
    \item{aname}{source name for loading}
    \item{UWFILEID}{event ID number}
  }
  
  The info list consists of:
      \describe{
      \item{fn}{file name}
      \item{name}{identification name}
      \item{yr}{start year}
      \item{jd}{start julianday}
      \item{mo}{month}
      \item{dom}{day of month}
      \item{hr}{hour}
      \item{mi}{minute}
      \item{sec}{second}
      \item{msec}{millisecond}
      \item{dt}{delta-t}
      \item{t1}{time 1}
      \item{t2}{time 2}
      \item{off}{offset}
      \item{n1}{number of samples}
      \item{n2}{not used}
      \item{n3}{not used}
      \item{n}{number of samples}
      }
    
  The pickfile consists of:
  \describe{
    \item{LOC}{list(yr, jd, mo, dom, hr, mi,
      sec, lat, lon, z, mag, gap,
      delta , rms, hozerr)}
    \item{MC}{list(az1, dip1, az2, dip2,  dir, rake1,
      dipaz1, rake2, dipaz2,
      F=list(az, dip), G=list(az, dip), U=list(az, dip),
      V=list(az, dip), P=list(az, dip),
      T=list(az,dip),sense,M=list( az1, d1,  az2, d2,
      uaz, ud, vaz, vd, paz, pd , taz, td), UP=TRUE,
      icol=1, ileg,  fcol='red', CNVRG, LIM =c(0,0,0,0))}
    \item{STAS}{list(tag, name, comp, c3, phase, sec, err, pol, flg , res)}
    \item{LIP}{vector, length=6}
    \item{H}{list(yr,mo,dom,hr,mi,sec,lat,lon,z,mag)}
    \item{N}{name card}
    \item{E}{list(rms,meanres,sdres,sdmean,
      sswres,ndf,fixflgs,sterrx,sterry,sterrz,sterrt,mag,sterrmag)}
    \item{filename}{file name}
    \item{PICKER}{Name of Picker}
    \item{UWFILEID}{numeric ID}
    \item{winID1}{win format ID}
    \item{comments}{Vector of comments}
    \item{OSTAS}{Old station names}
    }
}

\references{
Lees, J.M., 2004. Scattering from a fault interface in the Coso geothermal field. Journal of Volcanology and Geothermal Research, 130(1-2):
61-75.
 
}
\examples{
data(GH)

}
\keyword{datasets}
