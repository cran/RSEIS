\name{INSTFREQS}
\alias{INSTFREQS}

\title{Instrument Frequencies}
\description{
  Vector of frequencies
}
\usage{
INSTFREQS(b, a, w)
}

\arguments{
  \item{b}{numerator, zeros}
  \item{a}{denominator, poles}
  \item{w}{frequency}
}
\details{
 h = jpolyval(b,s) / jpolyval(a,s)

}
\value{
 h = jpolyval(b,s) / jpolyval(a,s)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

K  <-  PreSet.Instr()
b  <-  K[[1]]$zeros
a  <-  K[[1]]$poles
INSTFREQS(b, a, 2*pi*12)



}

\keyword{misc}
