\name{ReadInstr}
\alias{ReadInstr}
\title{Read Instrument Response in  IRIS SEED format
}
\description{Read Instrument Response, poles and zeros, in  IRIS SEED
  format.
}
\usage{
ReadInstr(fn)
}
\arguments{
  \item{fn}{File name with Poles and Zeros
}
}
\details{
  RSEIS currently has a function (ReadSet.Instr) to read pole/zero files,
  but it seems to expect a format different from what one gets from
  IRIS.  
  This one is compatible with pole/zero files produced by rdseed
  when converting seed files from the DMC to SAC files.
}
\value{List of poles and zeros compatible for swig decon
}

\author{
  Jake Anderson<ajakef@gmail.com>
}

\seealso{ReadSet.Instr
}
\examples{

######  create a SAC format response file:
temp.file= tempfile("PZ")
cat(file=temp.file, c(
"ZEROS 4",
"-999.0260  0.0000",
"POLES 6",
"-0.1480  0.1480",
"-0.1480  -0.1480",
"-314.1600  0.0000",
"-9904.8000  3786.0000",
"-9904.8000  -3786.0000",
"-12507.0000  0.0000",
"CONSTANT 4.540182e+20"),  sep='\n')


RESP <- ReadInstr(temp.file)

}
\keyword{misc}
