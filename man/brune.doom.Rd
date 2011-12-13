\name{brune.doom}
\alias{brune.doom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Brune Modeling}
\description{
  Modeling the Brune spectrum with Graphical Diagnostics
}
\usage{
brune.doom(amp, dt = 1, f1 = 0.01, f2 = 15, PLOTB = FALSE, tit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amp}{signal }
  \item{dt}{deltaT }
  \item{f1}{low frequency for modeling}
  \item{f2}{ high  frequency for modeling }
  \item{PLOTB}{logical, TRUE=show diagnostic plots }
  \item{tit}{title for plot}
}

\value{
  List:
\item{ SUCCESS}{(0,1) for success or failure of modeling}
 \item{ WARN }{flag = "OK"}
  \item{tstar0}{ tstar0}
  \item{gamma}{ gamma}
  \item{omega0}{omega0}
  \item{fc}{ fc}
  \item{alpha}{alpha}
   \item{chisqrd}{chi-squared misfit over region of fitting}

   
}
\references{
  Lees, J. M. and G. T. Lindley  (1994): Three-dimensional Attenuation
  Tomography at Loma Prieta:Inverting t* for Q, J. Geophys. Res., 99(B4), 6843-6863.
}
\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

data(CE1)
plot(CE1$x, CE1$y, type='l')
Xamp = CE1$y[CE1$x>5.443754 & CE1$x<5.615951]

BF = brune.doom( Xamp, CE1$dt ,f1=.5, f2=12 ,  PLOTB = TRUE)

}

\keyword{misc}
\keyword{hplot}
