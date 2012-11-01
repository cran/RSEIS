\name{selAPX}
\alias{selAPX}
\alias{selWPX}


\title{Select Picks}
\description{
select a subset of picks from a larger data base
}
\usage{
selAPX(APX, ista = NULL, icomp = c("V", "N", "E"))
selWPX(APX, ind=NULL, ista = NULL, icomp = c("V", "N", "E"))

}

\arguments{
  \item{APX}{Pick Data Frame}
  \item{ista}{vector of stations to select}
  \item{icomp}{vector of components}
  \item{ind}{index of picks to select (negitive values imply omission) }
  
}

\value{
 returns subset list 
}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
