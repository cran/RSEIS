\name{Zdate}
\alias{Zdate}
\alias{dateList}
\alias{dateStamp}


\title{Date functions}
\description{
 Make character vector from dates
}
\usage{
Zdate(info, sel=1, t1=0)
dateList(datevec)
dateStamp(datelist)
}

\arguments{
  \item{info}{info structure from trace structure}
  \item{sel}{selection of which ones to extract,
    default=1:length(info$jd) }
  \item{t1}{ time offset, seconds, default=0 }

  \item{datevec}{ vector with yr, jd, mo, day, hr, mi, sec  }
   \item{ datelist}{ output of dateList }

}
\details{
 Format date stamp for plotting and identification. Used for STAMP.
}
\value{
 character strings
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{
data("GH")

sel <-  which(GH$COMPS == "V")

ftime  <-  Zdate(GH$info, sel[1:5], 1)

dvec  <-  c(2009, 134, 5, 14, 10, 32, 24.5, 0)
A  <-  dateList(dvec)
dateStamp(A)

}

\keyword{misc}
