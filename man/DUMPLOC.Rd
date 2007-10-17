\name{DUMPLOC}
\alias{DUMPLOC}

\title{Display contents of locator list}
\description{
  Display the results of locator in a form useful for saving.
}
\usage{
DUMPLOC(zloc)
}

\arguments{
  \item{zloc}{x,y list from locator }
}
\details{
  This is a useful routine for locating
  specific points and saving them for later use in code.
}
\value{
 Print to console.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{locator}
\examples{
L = locator(4)
DUMPLOC(L)

}

\keyword{misc}
