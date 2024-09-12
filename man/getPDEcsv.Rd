\name{getPDEcsv}
\alias{getPDEcsv}
\alias{getPDEscreen}

\title{Unpack PDE file
}
\description{Unpack PDE file as CSV file or ascii screen dump
}
\usage{
getPDEcsv(pde = 'filename')
getPDEscreen(pde = 'filename' )

}
\arguments{
  \item{pde}{character, file name
}
}
\details{Download pde from: http://neic.usgs.gov/neis/epic/epic.html.
  csv version uses comma separated values.  screen versions uses the
  screen dump and a parser
}
\value{list of locations, times and magnitude
}
\references{http://neic.usgs.gov/neis/epic/epic.html
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  if using screen dump, may need to clean up file a bit first.
}



\examples{
######  copy/paste  from the screen dump at the NEIC web site

fn <- tempfile()
K = c(
' PDE-Q  2008  12 31 053408.80  40.11  -77.00   1  2.4 LgGS    ... ....... ',      
' PDE-Q  2008  12 31 084757.50  46.75  154.41  14  4.9 mbGS    ... ....... ',       
' PDE-Q  2008  12 31 090228     44.53 -110.36   4  3.6 MLSLC   ... ....... ',      
' PDE-Q  2008  12 31 110505     33.94 -118.78  14  3.1 MLPAS   2F. ....... ',       
' PDE-Q  2008  12 31 113957.56   4.91  127.43  77  5.4 MwGS    ..M ....... ',       
' PDE-Q  2008  12 31 140227.55 -25.35 -177.61 154  5.3 MwGS    ..M ....... ')      

cat(file=fn, K, sep='\n')

### check: z = scan(file=fn, what='', sep='\n')

 
g <- getPDEscreen(pde = fn)
   

}
\keyword{misc}
