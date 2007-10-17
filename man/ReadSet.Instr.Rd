\name{ReadSet.Instr}
\alias{ReadSet.Instr}

\title{Read Instrument Response file}
\description{
  Read in an instrument response file, or 
}
\usage{
ReadSet.Instr(file)
}

\arguments{
  \item{file}{name of file to read, or vector of character strings from
    the file }
}
\details{
  If file is a path to a file it is read in and processed.  If file is a
  vector of character strings from a file that has already been read in,
  the file is processed directly.
}
\value{
  list:
  \item{np}{ NUmber of poles}
  \item{poles} {complex vector of poles}
  \item{nz}{number of zeros}
  \item{zeros}{complex vector of zeros}
  \item{Knorm}{normalization factor}
  \item{Sense}{ sensitivity factor }

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{}
\examples{

###  in this case a file has already been read in:

CMG=c(
      "ZEROS 2",
      "0.0000E+00 0.0000E+00",
      "0.0000E+00 0.0000E+00",
      "POLES 3",
      "-0.1480E+00 0.1480E+00",
      "-0.1480E+00 -0.1480E+00",
      "-50.0 0.0",
      "CONSTANT 1.0",
      "SENSE 800")

ReadSet.Instr(CMG)

\dontrun{
RL28 = ReadSet.Instr('/pathto/Equipment/Sensors/L28.inst.response')
}


}


\keyword{misc}
