\name{Gcols}
\alias{Gcols}

\title{Get Color Palette }
\description{
 Get Color Palette
}
\usage{
Gcols(plow = 10, phi = 10, N = 100, pal = "rainbow", mingray = 0.5)
}

\arguments{
  \item{plow}{lowest number for color selection}
  \item{phi}{highest number for color selection}
  \item{N}{number of colors}
  \item{pal}{color palette name}
  \item{mingray}{lower end is blanked out and replaced by gray}
}

\value{
  c(LOW  , Z, HI)
  color palette
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{tomo.colors, shade.col}
\examples{

TPALS = c("rainbow", "topo.colors", "terrain.colors", "heat.colors", "tomo.col")

 pal = Gcols(plow=5, phi=0,  N=100, pal=TPALS[3])

}

\keyword{misc}
