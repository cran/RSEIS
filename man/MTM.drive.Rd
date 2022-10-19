\name{MTM.drive}
\alias{MTM.drive}
\title{Interactive MTM driver}
\description{
 MTM analysis of signals
}
\usage{
MTM.drive(a, f1 = f1, f2 = f2, len2 = 1024, COL = 2, PLOT = FALSE,
PADDLAB = NULL, GUI = TRUE)
}

\arguments{
  \item{a}{list(y=time series amp, dt=delta-ts, stamps=text stamps) }
  \item{f1}{low frequency }
  \item{f2}{high frequency }
  \item{len2}{power of two length }
  \item{COL}{colors }
  \item{PLOT}{logical PLOT=TRUE }
  \item{PADDLAB}{vector of buttons}
  \item{GUI}{Whether to be in GUI (interactive) mode}
}

\value{
 Graphical Side effect
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A stand-alone C-subroutine, \emph{Computers and Geology}, 21(2), 199-236.}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plt.MTM0}
\examples{

data("GH")
sel <-  which(GH$COMPS=="V")

amp  <-  list()
dees  <-  list()
stamps  <-  list()

for( i in 1:3)
{
amp[[i]]  <-  GH$JSTR[[sel[i]]]
dees[i]  <-  GH$dt[sel[i]]
stamps[i]  <-  paste(GH$STNS[sel[i]], GH$COMPS[sel[i]])
}

 a  <-  list(y=amp, dt=dees, stamps=stamps)


 f1  <-  0.1

f2  <-  floor(0.33*(1/a$dt[[1]]))

speccol   <-  c('red', 'blue', 'purple')


MTM.drive(a, f1, f2, COL=speccol, PLOT=TRUE)


}

\keyword{misc}
\keyword{iplot}
