\name{catWPX}
\alias{catWPX}
\title{
Concatenate two WPX lists
}
\description{
Concatenate (combine) two WPX lists.
}
\usage{
catWPX(WPX, ppx)
}
\arguments{
  \item{WPX}{WPX list
}
  \item{ppx}{WPX list
}
}
\details{
Adds second list to the end of the first list.
}
\value{
WPX list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{
addWPX,	setWPX,	checkWPX,	cleanWPX,	clusterWPX,	repairWPX,	saveWPX
}
\examples{

s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s2 = setWPX(name="BYE", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s3 = catWPX(s1, s2)

}
\keyword{misc}
