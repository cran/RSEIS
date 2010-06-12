\name{P2GH}
\alias{P2GH}
\title{XTR button to RSEIS
}
\description{
Convert output of XTR button to RSEIS list.
}
\usage{
P2GH(P1)
}

\arguments{
  \item{P1}{
Output of PICK.GEN after clicking XTR
}
}
\details{
  Running PICK.GEN out after a selection of a window and
  the XTR button, one can create an RSEIS structure for further use
  in PICK.GEN.
}
\value{
RSEIS list
}

\author{Jonathan M. Lees<jonathan.lees.edu>

}

\seealso{PICK.GEN,
  prepSEIS
}
\examples{

\dontrun{

P1 = PICK.GEN(GH)
LH = P2GH(P1)

}

}

\keyword{MISC}
