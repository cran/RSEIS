\name{plocator}
\alias{plocator}

\title{Specialized Locator function}
\description{
Locator function with set parameters
}
\usage{
plocator(COL = 1, NUM = FALSE, YN = NULL, style = 0)
}

\arguments{
  \item{COL}{color}
  \item{NUM}{number of points }
  \item{YN}{number of windows to span for lines}
  \item{style}{0,1,2 for differnt style of plotting vertical lines}
}
\details{
  if the window is divided into YN horizontal regions,
  style =2 will plot segments only within regions
  based on y-value of locator().
  
}
\value{
list:
  \item{x}{x-locations}
  \item{y}{y-locations}
   \item{n}{number of points}
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{locator }
\examples{
plot(c(0,1), c(0,1), type='n')
for(i in 1:5) { abline(h=i/6) }

plocator(COL = 1, NUM = 4, YN = 6, style = 2)

}

\keyword{iplot}
