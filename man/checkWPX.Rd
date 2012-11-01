\name{checkWPX}
\alias{checkWPX}
\title{Check WPX
}
\description{
Check and verify WPX list for compliance.
}
\usage{
checkWPX(wpx)
}
\arguments{
  \item{wpx}{WPX list
}
}
\details{
Perform several checks on completeness, length of components,
station names, component names and date-times 
of the WPX lists.
}
\value{
\itemize{
\item 0 no problems
\item 1 list incomplete
\item 2 names incomplete
\item 3 components incomplete
\item 4 dates incomplete
}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
No action taken in the event an error occurs - 
see repairWPX to fix problems.
}

\seealso{addWPX,catWPX,	saveWPX,cleanWPX,clusterWPX,repairWPX,setWPX
}
\examples{

s1 <- setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s1$col=NULL



}
\keyword{misc}
