\name{ruler}
\Rdversion{1.1}
\alias{ruler}
\title{Column Ruler
}
\description{Column Ruler for determining columns to read.
}
\usage{
ruler(a = "")
}
\arguments{
  \item{a}{character string, optional}
}
\details{
  This routine is set up to help get the columns
  for specific column oriented data.  The ruler is
  dumped out below the character string for comparison.
  If no string is provided, just the rule is dumped.
  Use routine substr to extract the data from the columns.
}
\value{Side effects}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\seealso{substr }
  
\examples{

\dontrun{
aa = paste(runif(n=5), collapse='-')
ruler(aa)
}

}
\keyword{misc}
