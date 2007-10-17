\name{PICK.GEN}
\alias{PICK.GEN}

\title{Seismic Analysis }
\description{
Main INteractive Program for plotting and analyzing seismic waveform data.
}
\usage{
PICK.GEN(GH, sel = 1:length(GH$dt), ORD = NULL, WIN = NULL, APIX = NULL, PHASE = NULL, STDLAB = NULL, PADDLAB = NULL, SHOWONLY = FALSE, CHOP = FALSE, TIT = "", pts = FALSE, forcepix = FALSE, SCALE = 1, velfile = "", stafile = "", LOC = NULL, FILT=list(fl=.2, fh=15,  type="HP", proto="BU"))
}

\arguments{
  \item{GH}{Seismic data structure}
  \item{sel}{selection of traces from structure }
  \item{ORD}{order to plot traces }
  \item{WIN}{vector c(t1, t2) for window of traces to be shown }
  \item{APIX}{structure of arrival time picks }
  \item{PHASE}{phase to display, "P", "S", etc }
  \item{STDLAB}{label of buttons}
  \item{PADDLAB}{label of phase-pick buttons}
  \item{SHOWONLY}{logical, TRUE=non-interactive }
  \item{CHOP}{whether to chop the signal }
  \item{TIT}{title for the top of plot}
  \item{pts}{whether to plot specific points on the plot}
  \item{forcepix}{logical, force all phase picks to be shown on all traces }
  \item{SCALE}{flag, 1,0= scale according to window or trace }
  \item{velfile}{velocity structure or file name }
  \item{stafile}{station structure or file name }
  \item{LOC}{source location structure (lat, lon, depth) }
  \item{FILT}{default filter definition list(fl=.2, fh=15,  type="HP", proto="BU")}
  
}
\details{
  This is hte main program that drives the other analysis in RSEIS.
}
\value{
  various strucutres are returned
  based on interactive selections of the user.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{RPGM}
\examples{
data("GH")
###  This loads a structure
PICK.GEN(GH)
}

\keyword{iplot}
