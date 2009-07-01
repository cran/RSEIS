\name{infoDB}
\Rdversion{1.1}
\alias{infoDB}
\title{Print information about the seismic database
}
\description{Print information about the seismic database
}
\usage{
infoDB(DB)
}
\arguments{
  \item{DB}{Database list
}
}

\value{
  list(
  \item{usta}{Unique station names}
  \item{ucomp}{Unique component names}
  \item{start}{starting date}
  \item{end}{ending date}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{makeDB
}
\examples{

\dontrun{

 path = '/home/lees/Site/Santiaguito/SG09'
     pattern = "R0*"

     ###   get DB information
     XDB  =  makeDB(path, pattern, kind =1)

infoDB(XDB)


}



}
\keyword{misc}
