\name{JGRAY}
\alias{JGRAY}

\title{Gray scale Color Palette}
\description{
  generate a gray scale color palette
}
\usage{
JGRAY(n)
}

\arguments{
  \item{n}{number of colors to produce }
}
\details{
  Creates a grey scale color palette suitable for
  replacing rainbow for grey shade plots.
}
\value{
 n characters used for color palette
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{shade.col, rainbow, colors }
\examples{
pal = JGRAY(100)
data(volcano)
image(volcano, col=pal)

}

\keyword{misc}
