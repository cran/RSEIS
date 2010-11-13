\name{NEWPLOT.WPX}
\alias{NEWPLOT.WPX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Window Picks (WPX) }
\description{
adds picks to existing seismic section
}
\usage{
NEWPLOT.WPX(t0, STNS, COMPS, YPX, FILL = FALSE, FORCE = TRUE, cex = cex, srt = srt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t0}{starting time for window}
  \item{STNS}{stations to match}
  \item{COMPS}{components to match}
  \item{YPX}{list of picks}
  \item{FILL}{fill color}
  \item{FORCE}{logical, TRUE=plot picks on all traces}
  \item{cex}{character expansion}
  \item{srt}{string rotation angle}
}
\details{
  Used in conjunction with swig program
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{
########  no example available now
}

\keyword{misc}
\keyword{aplot}