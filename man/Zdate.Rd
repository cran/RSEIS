\name{Zdate}
\alias{Zdate}

\title{Date functions}
\description{
 Make character vector from dates
}
\usage{
Zdate(info, sel, t1)
}

\arguments{
  \item{info}{info structure from trace structure}
  \item{sel}{selection of which ones to extract}
  \item{t1}{ time offset, seconds }
}
\details{
 Format date stamp for plotting and identification. Used for STAMP.
}
\value{
 character strings
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN}
\examples{
data("GH")

sel= which(GH$COMPS == "V")

ftime = Zdate(GH$info, sel[1:5], 1)

}

\keyword{misc}
