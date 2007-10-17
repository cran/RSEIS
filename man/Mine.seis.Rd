\name{Mine.seis}
\alias{Mine.seis}

\title{Mine a seismic data base to extract secions of time limited data}
\description{
 Mine a seismic data base to extract secions of time limited data
}
\usage{
Mine.seis(at1, at2, DB, grepsta, grepcomp, kind = 1)
}

\arguments{
  \item{at1}{time 1 in julian days}
  \item{at2}{time 2 in julian days}
  \item{DB}{data base structure to searcth through that provides the
    files where data is to extracted from}
  \item{grepsta}{which stations to extract}
  \item{grepcomp}{which components to extract}
  \item{kind}{kind of data, segy=1, sac=2}
}
\details{
  The data base is a list or dataframe containing the files names, the
  beginning time (t1) and ending time (t2) for each file int eh data base
}
\value{
list of seismograms cut from the database
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{GLUEseisMAT}

\keyword{misc}
