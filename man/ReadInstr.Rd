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

\dontrun{
library(RSEIS)
source('ReadInstr.R')
S  <-  JSAC.seis('2010.012.21.54.46.6195.IU.OTAV.00.BHZ.R.SAC', BIGLONG = FALSE)
GH  <-  prepSEIS(S)
swig(GH)
RESP <-
ReadInstr('SAC_PZs_IU_OTAV_BHZ_00_2009.091.00.00.00.0000_2010.136.22.12.60.99999')
GHV  <-  VELOCITY.SEISN(GH, Kal = RESP,  FILT = list(ON = TRUE, fl = 1/160,
fh = 7, type = "HP", proto = "BU"))
swig(GHV)
GHD  <-  DISPLACE.SEISN(GH, Kal = RESP,  FILT = list(ON = TRUE, fl = 1/160,
fh = 7, type = "HP", proto = "BU"))
swig(GHD)


}


}
\keyword{misc}
