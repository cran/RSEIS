\name{saveWPX}
\alias{saveWPX}
\title{Save WPX 
}
\description{
Save a WPX list to a file on the local file system.
}
\usage{
saveWPX(twpx, destdir = ".")
}
\arguments{
  \item{twpx}{WPX list
}
  \item{destdir}{character, destination directory, default=getwd() 
}
}
\details{
Creates a file with the list as in native
binary format.  This file can
be loaded with the standard load function in R.
The name of the file is 
created by using the minimum time extracted from the WPX list.
The suffix on the file name is RDATA.
When reading in, the object created is named "twpx"
for further processing.
}
\value{Side effects on file system.  The name of the output file is returned.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{User must have write access to the destination directory.

}


\seealso{
addWPX,	catWPX,	checkWPX,	cleanWPX,	clusterWPX,	repairWPX,	setWPX
}
\examples{

\dontrun{
s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 
hh = saveWPX(s1)

###   read in the data


load(hh)

data.frame(twpx)



}

}
\keyword{misc}
