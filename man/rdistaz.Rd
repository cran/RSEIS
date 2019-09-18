\name{rdistaz}
\alias{rdistaz}
\title{Distance and Azimuth from two points }
\description{
  Calculate distance, Azimuth and Back-Azimuth from two points on Globe.
}
\usage{
rdistaz(olat, olon, tlat, tlon)
}
\arguments{
  \item{olat}{origin latitude, degrees }
  \item{olon}{origin longitude, degrees }
  \item{tlat}{target latitude, degrees }
  \item{tlon}{target longitude, degrees  }
}

\value{
  List:
  \item{del}{Delta, angle in degrees}
  \item{az}{Azimuth, angle in degrees}
  \item{baz}{Back Azimuth, angle in degrees from target to origin}
  \item{dist}{Distance in km}
  \item{err}{0 or 1, error flag.  0=error, 1=no error, see details}
  
}
\details{

  
  Program is set up for one origin (olat, olon) pair and many
  target (tlat, tlon) pairs given as vectors.

  If multiple olat and olon are given, the program returns a list
  of outputs for each.

  If olat or any tlat is greater than 90 or less than -90,  NA is returned
  and error flag is 0.

  If any tlat and tlon is equal to olat and olon, the points are
  coincident.
  In that case the distances are set to zero, but the az and baz are NA,
  and the error flag is set to 0.
  
  }

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{along.great, getgreatarc}
\examples{


####  one point
d <- rdistaz(12, 23, -32,    -65)
d

####  many random target points
org <- c(80.222, -100.940)
targ <- cbind(runif(10, 10, 50), runif(10, 20, 100))


rdistaz(org[1], org[2], targ[,1], targ[,2])

############  if origin and target are identical
#####  the distance is zero, but the az and baz are not defined
rdistaz(80.222, -100.940, 80.222, -100.940)


########################   set one of the targets equal to the origin
targ[7,1] <- org[1]
targ[7,2] <- org[2]

rdistaz(org[1], org[2], targ[,1], targ[,2])

####  put in erroneous latitude data

targ[3,1] <-  -91.3


rdistaz(org[1], org[2], targ[,1], targ[,2])


}
\keyword{misc}
