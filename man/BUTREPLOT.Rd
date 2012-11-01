\name{BUTREPLOT}
\alias{BUTREPLOT}
\title{Replot Function for SELBUT
}
\description{Replot Function for SELBUT
}
\usage{
BUTREPLOT(opts, ncol = 5, HOZ = TRUE, TOP = TRUE,
 cols = "white", main = "", xlim = c(0, 1),
        ylim = c(0, 1), newplot = TRUE)
}
\arguments{
  \item{opts}{character list of options
}
  \item{ncol}{number of columns
}
  \item{HOZ}{logical, TRUE=plot horizontally
}
  \item{TOP}{logical, TRUE=plot top-down
}
  \item{cols}{colors
}
  \item{main}{character title
}
  \item{xlim}{x-limits in plotting region (user coordinates)
}
  \item{ylim}{y-limits in plotting region (user coordinates)
}
  \item{newplot}{logical, new plot?
}
}
\details{Used internally in SELBUT as a replotting function
}
\value{list
  \item{ M}{x,y matrix of grid}
  \item{dx}{delta x}
  \item{dy}{delta y}
  \item{rx}{range of x}
  \item{ry}{range of y}
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{SELBUT, swig
}
\examples{


STDLAB <- c("DONE", "QUIT", "zoom.out", "zoom.in",
  "SELBUT", "FILT","UNFILT", "PSEL", "SGRAM",
       "WLET", "SPEC", "XTR" )
BUTREPLOT(STDLAB)


}
\keyword{misc}
