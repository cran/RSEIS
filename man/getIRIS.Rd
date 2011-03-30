\name{getIRIS}
\alias{getIRIS}
\alias{getANSS}
\title{get Hypocenters from IRIS web site
}
\description{
  Convert hypocenters from the
  IRIS website and prepare for plotting in GEOmap
}
\usage{
getIRIS(fn, skip=0)
getANSS(fn, skip=2)
}
\arguments{
  \item{fn}{character, file path name
  }
  \item{skip}{numeric, number of lines to skip (e.g. for the header)
  }

  
}
\details{Reads in a file dumped out
  by the website selection box.
  
}
\value{list:
  \item{yr}{vector year}
\item{dom}{vector, day of month}
\item{mo}{vector, mo}
\item{hr}{vector, hour}
\item{mi}{vector, minute}
\item{sec}{vector, sec}
\item{lat}{vector, latitude}
\item{lon}{vector, longitude}
\item{z}{vector, depth}
\item{mag}{vector, magnitude}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Be careful about headers and lines that need to be skipped.


  
for  IRIS:
  http://www.iris.washington.edu/data/event/eventsearch.htm

For   ANSS:
http://www.quake.geo.berkeley.edu/anss/catalog-search.html

For NEIC (yet to be added)
http://earthquake.usgs.gov/earthquakes/eqarchives/epic/epic_global.php


  
}


\seealso{getjul
}
\examples{
\dontrun{
fn = "/home/lees/WORK/SENDAI.EVENT/catsearch.8757"

g = getANSS(fn)
g$jd = getjul(g$yr, g$mo, g$dom)
}

}
\keyword{misc}
