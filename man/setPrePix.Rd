\name{setPrePix}
\alias{setPrePix}
\title{Set list of arrival times for swig.
}
\description{Prepare a set of arrival picks for swig plotting.
}
\usage{
setPrePix(R1, tt, name, flag = "K", col = "blue")
}
\arguments{
  \item{R1}{Location and time of event source. (list)
}
  \item{tt}{Vector of travel times, seconds.
}
  \item{name}{Station names
}
  \item{flag}{Phase Identifier, character
}
  \item{col}{Color
}
}

\value{ List of picks suitable for swig plotting.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{R1 should have yr, jp, hr, mi, sec at the least.
}


\seealso{setWPX
}
\examples{

T1 = as.POSIXct("2020-08-20 06:30:17.15 UTC", "UTC")

     R1 = posix2RSEIS(T1)

name = c("MERT", "KRN", "KUA")
tt = c(1,2,3)
wpx = setPrePix(R1, tt, name, flag = "K", col = "blue") 


}
\keyword{misc}
