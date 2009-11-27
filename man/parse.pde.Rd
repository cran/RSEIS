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

\keyword{misc}
