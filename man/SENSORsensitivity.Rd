\name{SENSORsensitivity}
\alias{SENSORsensitivity}

\title{Sensor Sensitivity from a known set of seismo/acoustic sensor}
\description{
 From published sensitivities of seismic and acoustic sensors.
}
\usage{
SENSORsensitivity(K = 1)
}

\arguments{
  \item{K}{number of sensor from list}
}

\value{
 Sensitivity
}


 \note{
Current choices are:
c("40T", "3T",  "L28", "LD",    "EL", "MC", "EL(SANGAY)")


 }

 


 \references{
  Johnson, J.B., R.C. Aster, M.C. Ruiz, S.D. Malone,
  P.J. McChesney,J.M. Lees, and P.R. Kyle, Interpretation and utility of
  infrasonic records from erupting volcanoes, J. Volc. Geoth. Res., 121 (1-2), 15-63, 2003. 

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{
SENSORsensitivity(3)
SENSORsensitivity(5)

}

\keyword{misc}
