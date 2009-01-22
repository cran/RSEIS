\name{PreSet.Instr}
\alias{PreSet.Instr}

\title{Set up Standard Instrument Responses}
\description{
  A set of standard known instrument responses.
}
\usage{
PreSet.Instr()
}

\value{
  List of instrument responses. Each is a list:
 \item{np}{Number of poles}
  \item{poles}{complex vector of poles}
  \item{nz}{number of zeros}
  \item{zeros}{complex vector of zeros}
  \item{Knorm}{normalization factor}
  \item{Sense}{sensitivity factor }
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{ReadSet.Instr}
\examples{
MYset = PreSet.Instr()
MYset[[1]]
}

\keyword{misc}
