\name{insertNAs}
\alias{insertNAs}
\title{Insert NA in a vector at given break points
}
\description{Insert NA in a vector at given break points
}
\usage{
insertNAs(v, w)
}
\arguments{
  \item{v}{original vector
}
  \item{w}{break points
}
}

\value{ vector with NA inserted
}
\details{ Used for plotting lines that
  wrap around.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\examples{

a  <-  1:20
b  <-  insertNAs(a, c(5, 12))
b 


}
\keyword{misc}
