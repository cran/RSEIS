\name{gwpix2ypx}
\alias{gwpix2ypx}
\title{Convert a teleseismic pickfile to structure usefule for PICK.GEN}
\description{
  Takes as input the output file of program ttimes, by Brian Kennett,
  which uses the TauP method.
}
\usage{
gwpix2ypx(wpix, LOC, sta = sta, comp = comp, PH = c(1, 2, 3, 5, 9, 13, 16, 41, 44, 25))
}
\arguments{
  \item{wpix}{pickfile data frame}
  \item{LOC}{location}
  \item{sta}{station}
  \item{comp}{component}
  \item{PH}{phase}
}

\value{
 Pickfile structure suitable for input to PICK.GEN
}
\references{
Buland and Chapman (1983) in "The Computation of
Seismic Travel Times", BSSA, v. 73, pp. 1271-1302

}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{uwpfile2ypx, PICK.GEN}

\examples{}

\keyword{misc}
