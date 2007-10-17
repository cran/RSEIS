\name{setLQUAKE}
\alias{setLQUAKE}

\title{Set Up data for earthquake location}
\description{
  Create structure for earthquake location
}
\usage{
setLQUAKE(velfile = "", stafile = "", delfile = "", fout = "")
}

\arguments{
  \item{velfile}{velocity file/list}
  \item{stafile}{station file/list}
  \item{delfile}{station delay file}
  \item{fout}{output ascii file}
}
\details{
  function relies on program lquake.
  This wil be replaced at a later date by an internal
  function for calling event location in R.
}
\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
