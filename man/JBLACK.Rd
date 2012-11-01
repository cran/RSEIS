\name{JBLACK}
\alias{JBLACK}

\title{Gray scale Color Palette}
\description{
  generate a gray scale color palette
}
\usage{
JBLACK(n, acol=rgb(0,0,0))
}

\arguments{
  \item{n}{number of colors to produce }
  \item{acol}{RGB color }
  
}
\details{
  Creates a black color palette suitable for
  replacing rainbow for B/W color plots.  This is inserted in case user
  needs to completely elliminate color from a plot that uses
  color palettes for fixing colors.
}
\value{
  n characters used for color palette
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{shade.col, rainbow, colors }
\examples{
pal <- JBLACK(100)

}

\keyword{misc}
