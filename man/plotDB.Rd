\name{plotDB}
\alias{plotDB}
\title{Plot a time line of a DB set in RSEIS}
\description{
  makes a plot of the data base files stored on disk.
}
\usage{
plotDB(DB)
}
\arguments{
  \item{DB}{List, Data Base created by makeDB or setupDB}
}

\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{makeDB, setupDB}
\examples{

\dontrun{

  path <- '/home/lees/Site/Santiaguito/SG09'
     pattern <- "R0*"

     ###   get DB information
     XDB  <-  makeDB(path, pattern, kind =1)

plotDB(DB)

}

}
\keyword{misc}
