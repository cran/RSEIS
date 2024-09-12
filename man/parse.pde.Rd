\name{parse.pde}
\alias{parse.pde}
\title{Parse PDE file
}
\description{Parse and Extact information from a screen dump of
  PDE (preliminary earthquake estimates) from the internet,
}
\usage{
parse.pde(card)
}
\arguments{
  \item{card}{character, one line from the PDE file
}
}
\details{Parsing is done by column specification.  Uses screen dump format.
  see http://neic.usgs.gov/neis/epic/epic.html
  
}
\value{
  Time, Location and Magnitude:
  list(yr, jd, mo, dom, hr, mi, sec,  lat, lon, depth, z, mag)  
}
\references{http://neic.usgs.gov/neis/epic/epic.html
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\note{
  May try using the CSV version of the dump.
}
\seealso{getPDEcsv, getPDEscreen
}


\examples{
######  copy/paste  from the screen dump at the NEIC web site


K = c(
' PDE-Q  2008  12 31 053408.80  40.11  -77.00   1  2.4 LgGS    ... ....... ',      
' PDE-Q  2008  12 31 084757.50  46.75  154.41  14  4.9 mbGS    ... ....... ')      

G = parse.pde(K[1])

}

\keyword{misc}
