\name{getPDEcsv}
\alias{getPDEcsv}
\alias{getPDEscreen}

\title{Unpack PDE file
}
\description{Unpack PDE file as CSV file or ascii screen dump
}
\usage{
getPDEcsv(pde = "/home/lees/Site/Santiaguito/pdq.eqs")
getPDEscreen(pde = "/home/lees/Site/Santiaguito/pdq.eqs")

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

\dontrun{


eq1 = getPDEcsv(pde = "/home/lees/Site/Santiaguito/pdq.eqs")

}


}
\keyword{misc}
