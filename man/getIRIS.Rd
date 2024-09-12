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

fn <- tempfile()
K = c(
'Date       Time             Lat       Lon  Depth   Mag Magt  Nst Gap  Clo  RMS  SRC   Event ID',
'----------------------------------------------------------------------------------------------',
'1994/09/06 09:37:36.48  40.1330  144.6240  33.40  4.60   Mb   28          1.22  NEI 199409064025',
'1994/09/06 10:00:02.97  36.4840  140.5730  66.60  4.90   Mb   39          0.88  NEI 199409064028',
'1994/09/06 10:07:16.53  40.1700  144.5890  33.00  4.70   Mb   49          1.09  NEI 199409064029',
'1994/09/06 17:31:52.27  42.6220  142.7000  33.00  5.00   Mb   13          0.54  NEI 199409064042')

cat(file=fn, K, sep='\n')

### check: z = scan(file=fn, what='', sep='\n')

g <- getANSS(fn, skip=2)
g$jd <- getjul(g$yr, g$mo, g$dom)


}
\keyword{misc}
