\name{recdate}
\alias{recdate}
\alias{recdatel}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rectify Date }
\description{
 Rectify a date that may be out of wack.
}
\usage{
recdate(jd, hr, mi, sec)
recdatel(X)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}

  or
  \item{X}{list of date}

  
}
\details{
  Returns date with correct numbers. So if number of seconds is
  greater than 60, will add in to minutes...
}
\value{
 \item{jd}{Julian Day}
  \item{hr}{hours}
  \item{mi}{minutes}
  \item{sec}{seconds}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

recdate(76, 23, 22, 2000)


}

\keyword{misc}
