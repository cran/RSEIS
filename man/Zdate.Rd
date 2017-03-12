\name{Zdate}
\alias{Zdate}
\alias{dateList}
\alias{dateStamp}


\title{Date functions}
\description{
 Make character vector from dates
}
\usage{
Zdate(info, sel=1, t1=0, sep=':')
dateList(datevec)
dateStamp(datelist, sep=':')
}

\arguments{
  \item{info}{info structure from trace structure}
  \item{sel}{selection of which ones to extract,
    default=1:length(info$jd) }
  \item{t1}{ time offset, seconds, default=0 }
  \item{sep}{ character for separating the components in the string, default=":" }

  \item{datevec}{ vector with yr, jd, mo, day, hr, mi, sec  }
   \item{ datelist}{ output of dateList }

}
\details{
 Format date stamp for plotting and identification. Used for STAMP.
}
\value{
 character strings
}

\note{
If using Zdate to create a file name, becareful about the separator.  A colon
in the file name on PC and MAC systems can be confusing for the OS.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig, dateStamp, ghstamp, filedatetime}
\examples{
data("GH")

sel <-  which(GH$COMPS == "V")

ftime  <-  Zdate(GH$info, sel[1:5], 1)

dvec  <-  c(2009, 134, 5, 14, 10, 32, 24.5, 0)
A  <-  dateList(dvec)
dateStamp(A)

dateStamp(A, sep="_")



}

\keyword{misc}
