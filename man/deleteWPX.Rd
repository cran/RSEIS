\name{deleteWPX}
\alias{deleteWPX}
\title{
Delete  picks to WPX file
}
\description{Delete pick to WPX file
}
\usage{
deleteWPX(WPX, ind=1)
}
\arguments{
  \item{WPX}{WPX list
}
  \item{ind}{integer, index to delete
}
}
\details{Deletes one pick to end of list.
}
\value{WPX list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Uses, the last pick as a reference.
}
\seealso{addWPX, catWPX
}
\examples{
s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 
s2 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(1)) 

s3 = addWPX(s1, s2)

s4 = deleteWPX(s3, ind=2:3)

}
\keyword{misc}
