\name{get.corner}
\alias{get.corner}

\title{Get Corner Frequency: Linear Model}
\description{
 Search for low frequency asymptote, corner frequency, and fall off
 slope of seismic spectrum.
}
\usage{
get.corner(INfreq, INspec, dt, f1, f2, PLOT = FALSE, VERBOSE = FALSE)
}

\arguments{
  \item{INfreq}{frequency vector}
  \item{INspec}{spectrum}
  \item{dt}{deltaT}
  \item{f1}{low frequency for modeling, Hz}
  \item{f2}{High frequency for modeling, Hz}
  \item{PLOT}{logical, TRUE=plot  }
  \item{VERBOSE}{TRUE=diagnostics}
}
\details{
  This routine does not assume any particular mathematical model.
  It searches for a three parameters that describe
  two lines that mimic the displacement spectrum.
  The search is done via least squares.
}
\value{
  Model of 3 parameters, best fit.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{brune.doom}
\examples{

data(CE1)

####   set frequency range for modeling - for this high frequency data
####   we use f2 = 50, but for volcano data should be f2<15

f1 = 0.01
f2 = 50.0  

##  set up data and parameters
amp =  CE1$y
len2 = 2*next2(length(amp))
a = list(y=amp, dt=CE1$dt)

Spec =MTMdisp(a, f1=f1, f2=f2, len2=len2, PLOT=FALSE )

lspec = Spec$displ

###  get initial estimate of parameters
xc = get.corner(  Spec$f , lspec, CE1$dt, f1, f2, PLOT=FALSE)



}

\keyword{misc}
\keyword{hplot}