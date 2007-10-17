\name{ymarginfo}
\alias{ymarginfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get information on Y-margin for plotting }
\description{
 Get information on Y-margin for plotting
}
\usage{
ymarginfo(SIDE = 1, s1 = 0.1, s2 = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIDE}{plotting side 1,2,3,4 }
  \item{s1}{lower percent of margin to return}
  \item{s2}{upper percent of margin to return}
}
\details{
  Function uses par to help determine how to plot
  objects in the margins.
}
\value{
  vector c(a, b)
  giving coordinates in margin worth plotting.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{par}
\examples{

plot(c(0,1), c(0,1), type='n')
s1=0.4
s2=0.95
ym = ymarginfo(SIDE=1, s1=s1, s2=s2)


}

\keyword{misc}
