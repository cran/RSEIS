\name{replaceWPX}
\alias{replaceWPX}
\title{
Replace  picks in WPX file
}
\description{Replace pick in WPX file
}
\usage{
replaceWPX(WPX, onepx , ind=1)
}
\arguments{
  \item{WPX}{WPX list
  }
  \item{onepx}{WPX list with one pick
}  
  \item{ind}{integer, index to replace
}
}
\details{Replaces one pick at index provided.
}
\value{WPX list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Replaces in the location provided. No test is made to
  determine if there is a pick already there.
  Maybe future versions will allow multiple replacements.
}
\seealso{addWPX, catWPX, deleteWPX, selWPX
}
\examples{
s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 
s2 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(1)) 


s4 = replaceWPX(s1,s2, ind=4)

}
\keyword{misc}
