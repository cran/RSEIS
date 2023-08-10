\name{repairWPX}
\alias{repairWPX}
\title{Repair WPX
}
\description{
Repair a WPX list that may be deficient 
in one or more of its components.
}
\usage{
repairWPX(wpx)
}
\arguments{
  \item{wpx}{Pick information, dataframe
}
}
\details{
Program checks a few of the elelments
and tries to fix potential problems.
}
\value{WPX dataframe
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{
addWPX,	catWPX,	checkWPX,cleanWPX,clusterWPX,saveWPX, setWPX
}
\examples{

s1 <- setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s1$col <- NULL

s2 <- repairWPX(s1)




}
\keyword{misc}
