\name{rseis2segy}
\alias{rseis2segy}
\alias{rseis2sac}

\title{Convert RSEIS to SEGY/SAC format
}
\description{Convert RSEIS to SEGY/SAC format
}
\usage{
rseis2segy(GH, sel = 1, win = c(0, 1), path = ".", BIGLONG = FALSE)
rseis2sac(GH, sel = 1, win = c(0, 1), path = ".", BIGLONG = FALSE)

}
\arguments{
  \item{GH}{RSEIS format list
}
  \item{sel}{ select traces to convert
}
  \item{win}{ vector, t1 and t2  window each trace
}
  \item{path}{path to directory where files are created
}
  \item{BIGLONG}{logical, indicating whether long is 8 or 4 bytes.
}
}
\details{This is the converse of the segy2rseis routine.

  Segy format files are in integer format.
  The time series ususally represents counts
  recorded in a data acquisition system.  The header includes
  meta-data and other identifying information.
}
\value{Side effects in file system
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
   The Endian-ness of the output file will
  be the native endian-ness of the system.
}


\seealso{write1segy, write1sac, read1sac,
  read1segy, sac2rseis, segy2rseis
}
\examples{
\dontrun{

rseis2segy(GH,  path="./MYSEGFILES", BIGLONG=FALSE )

}

}
\keyword{misc}
