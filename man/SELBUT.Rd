\name{SELBUT}
\alias{SELBUT}
\title{Select Buttons
}
\description{Select buttons interactively.
}
\usage{
SELBUT(OPTS, onoff = 1, ocols = "white", default = "opt")
}
\arguments{
  \item{OPTS}{character list of buttons
}
  \item{onoff}{which buttons are active
}
  \item{ocols}{colors for plotting
  }
   \item{default}{default list of buttons
  }
 
}
\details{Used in swig.
  OPtions can be added, subtracted, deleted, or completely
  filled out based on interactive choice.
}
\value{character list of chosen options.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig
}
\examples{

\dontrun{
STDLAB=c("DONE", "QUIT", "zoom.out", "zoom.in", "SELBUT", "FILT","UNFILT", "PSEL", "SGRAM", "WLET", "SPEC", "XTR" )
onoff = rep(0, length(STDLAB))
onoff[1:5] = 1
SELBUT(STDLAB, onoff=onoff)

}

}
\keyword{misc}
