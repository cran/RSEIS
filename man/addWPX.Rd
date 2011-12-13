\name{addWPX}
\alias{addWPX}
\title{
Add one pick to WPX file
}
\description{Add one pick to WPX file
}
\usage{
addWPX(WPX, ppx)
}
\arguments{
  \item{WPX}{WPX list
}
  \item{ppx}{WPX list
}
}
\details{Adds one pick to end of list.
}
\value{WPX list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Uses, the last pick as a reference.
}
\seealso{catWPX
}
\examples{
s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 
s2 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(1)) 

s3 = addWPX(s1, s2)



}
\keyword{misc}
