\name{fmod}
\alias{fmod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Floating point remainer function}
\description{
 extract remainder for floating point numbers
}
\usage{
fmod(k, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{floating point number}
  \item{m}{divisor number}
}

\value{
  returns remainder after dividing out the divisor part:\cr
  \code{j = floor(k/m)}\cr
    \code{a = k-m*j}\cr
   \code{ return(a)}\cr
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{

###  degrees after removing extraneous 2*pi
j = 540.23
fmod(j, 360)

}

\keyword{misc}
