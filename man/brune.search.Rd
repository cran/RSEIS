\name{brune.search}
\alias{brune.search}

\title{Search for Brune fit to displacement spectrum }
\description{
  Model of the spectrum of a seismic arrival.  Uses Brune's Model.
}
\usage{
brune.search(infreq, inspec, f1, f2, omega0, fcorn, tstar0, gamma)
}

\arguments{
  \item{infreq}{vector of frequencies}
  \item{inspec}{spectrum}
  \item{f1}{low frequency, Hz}
  \item{f2}{ high frequency, Hz}
  \item{omega0}{initial starting low frequency asymptote}
  \item{fcorn}{ initial starting corner frequency}
  \item{tstar0}{initial starting t* }
  \item{gamma}{initial starting gamma}
}
\details{
 see paper by Lees and Lindley
}
\value{
  list(omega0=omega0,tstar0=tstar3[3]  , fc=fcorn,  alpha=0,
  gamma=gam3[3])

  \item{omega0}{ low frequency asymptote}
  \item{fc}{  corner frequency}
  \item{tstar0}{  t* }
  \item{gamma}{ gamma}
  
  \item{alpha}{alpha parameter}
  
}
\references{Lees and Lindley}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{MTM}
\examples{


data(CE1)

####   set frequency range for modeling
f1 = 0.01
f2 = 14.0

##  set up data and parameters
amp =  CE1$y
len2 = 2*next2(length(amp))
a = list(y=amp, dt=CE1$dt)

Spec =MTMdisp(a, f1=f1, f2=f2, len2=len2, PLOT=FALSE )
 
lspec = Spec$displ

###  get initial estimate of parameters
xc = get.corner(  Spec$f , lspec, dt, f1, f2, PLOT=FALSE)

jmod = brune.search(Spec$f, lspec, f1, f2,  xc$omega0, xc$corn, xc$tstar0, 2.0)

}

\keyword{misc}
