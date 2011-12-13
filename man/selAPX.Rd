\name{selAPX}
\alias{selAPX}
\alias{selWPX}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Select Picks}
\description{
select a subset of picks from a larger data base
}
\usage{
selAPX(APX, ista = NULL, icomp = c("V", "N", "E"))
selWPX(APX, ista = NULL, icomp = c("V", "N", "E"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{APX}{Pick Data Frame}
  \item{ista}{vector of stations to select}
  \item{icomp}{vector of components}
}

\value{
 returns subset list 
}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
