\name{local.file}
\alias{local.file}

\title{Get name for a Local file}
\description{
  Get a name for a local file for writing
  ascii files or postscript output.  This code checks
  to see if file exists and if so it increments
  a counter int he name.
}
\usage{
local.file(pref, suf)
}

\arguments{
  \item{pref}{prefix for file name}
  \item{suf}{suffix for file name }
}
\details{
 File name is located in the current directory.
}
\value{
 character string for new file name
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

psfile = local.file("JML", "eps")


}

\keyword{misc}
