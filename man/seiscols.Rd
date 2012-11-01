\name{seiscols}
\alias{seiscols}
\title{Set colors for seismic display}
\description{
  Given an RSEIS list of seismic data return a set of
  colors associated with the structure that colors each trace
  and its components the same color.
}
\usage{
seiscols(GH, acols="black", M="STNS")
}
\arguments{
  \item{GH}{Seismic RSEIS list}
  \item{acols}{vector of colors to choose from}
  \item{M}{character, "STNS" = stations, "COMPS" = components}
  
}

\value{
  \item{colors}{alpha/numeric vector of colors  }
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

data(GH)

GH$pcol <- seiscols(GH)
swig(GH, sel=which(GH$COMPS=="V"),    WIN=c(3, 10), SHOWONLY=TRUE)

xcol <-  seiscols(GH, acols=c("black", "darkmagenta", "forestgreen") )

GH$pcol <- xcol

swig(GH, sel=which(GH$COMPS=="V"), , SHOWONLY=TRUE)


}
\keyword{misc}
