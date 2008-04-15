\name{recdate}
\alias{recdate}
\alias{recdatel}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rectify Date }
\description{
 Rectify a date that may be out of wack.
}
\usage{
recdate(jd=0, hr=0, mi=0, sec=0, yr=0)
recdatel(X)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}
  \item{yr}{year (not used) }

  or
  \item{X}{list of date}

  
}
\details{
  Returns date with correct numbers. So if number of seconds is
  greater than 60, will add in to minutes...
}
\note{
  Year is currently not used in calculations, so it may be omitted.
  Default value for jd is 1, the rest are 0.
  

  }
\value{
 \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}
    \item{yr}{year }

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

recdate(76, 23, 22, 2000)


}

\keyword{misc}
