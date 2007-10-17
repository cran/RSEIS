\name{circle}
\alias{circle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{circle coordinates}
\description{
 generate circle coordinates for plotting
}
\usage{
circle(n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of points}
}

\value{
 List
  \item{x}{coordinates}
  \item{y}{coordinates}

}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{

j = circle(26)
plot(j)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
