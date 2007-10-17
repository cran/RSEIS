\name{shade.col}
\alias{shade.col}

\title{Shaded Color Palette}
\description{
 Create a color palette with two end member colors
}
\usage{
shade.col(n, acol = c(1, 0, 0), bcol = c(1, 1, 1))
}

\arguments{
  \item{n}{ number of desired colors }
  \item{acol}{ rgb, starting color}
  \item{bcol}{rgb, ending color }
}
\details{
Linear interpolation from color1 to color 2.
}
\value{
  color vector
}

\author{Jonathan M. Lees<jonathan.lees.edu> }

\seealso{rainbow, tomo.col}
\examples{
##  color palette from red to white
shade.col(100, acol = c(1, 0, 0), bcol = c(1, 1, 1))
}

\keyword{misc}
