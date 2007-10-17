\name{prepSEIS}
\alias{prepSEIS}

\title{Prepare structure for RSEIS}
\description{
  Takes list of traces and prepares new list
  for analysis in RSEIS
}
\usage{
prepSEIS(GG)
}

\arguments{
  \item{GG}{Output list of Rsac function GET.seis }
}
\details{
  prepSEIS is offered to reformat the
  output of a list of seismic traces (or other time series)
  for inpout to program PICK.GEN()
}
\value{
 RSEIS list
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN, GET.seis}
\examples{

data(sunspots)

ES = prep1wig(wig=sunspots, dt=1/12, sta="STA", comp="CMP", units="UNITS"    )

EH=prepSEIS(ES)

STDLAB = c("DONE",  "zoom out", "refresh", "restore", "XTR", "SPEC", "SGRAM" ,"WLET")

xx =  PICK.GEN( EH, STDLAB = STDLAB)

\dontrun{
######  this example shows how to read data from SEGY files using Rsac
#####  and entering RSEIS
KG4 = JGET.seis(c(fnames), kind = 1, PLOT = FALSE)

KH=prepSEIS(KG4)

STDLAB = c("DONE",  "zoom out", "refresh", "restore", "XTR", "SPEC", "SGRAM" ,"WLET")

xx =  PICK.GEN( KH, sel=which(KH$COMPS == "V"), STDLAB = STDLAB)

}

}

\keyword{misc}
