\name{YPIX}
\alias{YPIX}
\alias{WPIX}
\alias{NOPIX}
\alias{REPIX}
\alias{PickWin}
\alias{pADDPIX}
\alias{Ppic}
\alias{Spic}
\alias{Apic}
\alias{POLSWITCH}
\alias{Pup}
\alias{Pnil}
\alias{Pdown}

\title{PICK Buttons for swig
}
\description{defining functions for swig
}
\usage{
YPIX(nh, g)
WPIX(nh, g)
NOPIX(nh, g)
REPIX(nh, g)
PickWin(nh, g)
pADDPIX(nh, g, phase)
Ppic(nh, g)
Spic(nh, g)
Apic(nh, g)
POLSWITCH(nh, g, dir)
Pup(nh, g)
Pnil(nh, g)
Pdown(nh, g)

}
\arguments{
  \item{nh}{waveform list for RSEIS
}
  \item{g}{plotting parameter list for interactive program
}
  \item{phase}{phase name (P, S, A, etc...)
}
  \item{dir}{vertical up, down or nil
}




}
\details{

  Buttons can be defined on the fly.

  
}
\value{The return value depends on the nature of the function
  as it is returned to the main code swig.
Choices for returning to swig are: break, replot, revert, replace,
donothing, exit.


  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, XTR
}
\examples{

\dontrun{

MYFUNC<-function(nh, g)
  {
    print("pressed MYFUNC")
    d = data.frame(list(stations=nh$STNS, components=nh$COMPS))
print(d)        
    g$action = "replot"
    invisible(list(global.vars=g))	
  }

STDLAB=c("DONE", "QUIT", "SELBUT" , "MYFUNC" )
data(GH)
JJ = swig(GH, sel=1:10, STDLAB=STDLAB)

}


}
\keyword{misc}
