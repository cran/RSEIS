\name{TOCART}
\alias{TOCART}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert to Cartesian coordinates}
\description{
  Convert to cartesian coordinates
}
\usage{
TOCART(az, nadir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{degrees, azimuth }
  \item{nadir}{degrees, dip }
}

\value{
 LIST
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
  \item{z}{z-coordinate}
  \item{az}{degrees, azimuth }
  \item{nadir}{degrees, dip }

}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{tocartL}
\examples{
TOCART(132, 69)
}

\keyword{misc}
