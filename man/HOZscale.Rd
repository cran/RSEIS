\name{HOZscale}
\alias{HOZscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{add horizontal color scale}
\description{
 Add horizontal color scale to existing plot.
}
\usage{
HOZscale(z, col, units = "", SIDE = 1, s1 = 0.4, s2 = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{image matrix}
  \item{col}{color palette}
  \item{units}{character string, units}
  \item{SIDE}{Side of the plot}
  \item{s1}{percent of margin for bottom}
  \item{s2}{percent of margin for top}
}

\value{
 Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
data(volcano)
image(volcano, col=terrain.colors(100))

HOZscale(volcano,terrain.colors(100) , units = "", SIDE = 1, s1 = 0.4, s2 = 0.95)


}

\keyword{hplot}
