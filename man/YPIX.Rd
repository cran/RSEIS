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
\alias{FILLPIX}
\alias{RIDPIX}
\alias{SEEPIX}
\alias{ROT.RT}
\alias{JustV}
\alias{JustE}
\alias{JustN}
\alias{iNEXT}

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
FILLPIX(nh, g)
RIDPIX(nh, g)
SEEPIX(nh, g)
ROT.RT(nh, g)
JustV(nh, g)
JustE(nh, g)
JustN(nh, g)

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
  \describe{
    \item{YPIX}{Multiple picks on a panel}
    \item{WPIX}{window picks (start and end)}
    \item{NOPIX}{remove the picks}
    \item{REPIX}{un-remove the picks}
    \item{PickWin}{Pick window for 3 component picking}
    \item{pADDPIX}{add picks}
    \item{Ppic}{P-wave arrival (only one per station) }
    \item{Spic}{S-wave arrival (only one per station)}
    \item{Apic}{acoustic-wave arrival (only one per station)}
    \item{POLSWITCH}{flip polarity}
    \item{Pup}{Polarity Up}
    \item{Pnil}{Polarity nil}
    \item{Pdown}{Polarity down}
    \item{FILLPIX}{Fill the pick from bottom to top of panel}
    \item{RIDPIX}{remove pick}
    \item{SEEPIX}{print current picks to screen}
    \item{ROT.RT}{Rotate to radial and transverse (need event and
      station locations}
    \item{JustV}{Display only vertical components}
    \item{JustE}{Display only east components}
    \item{JustN}{Display only north components}
    \item{iNEXT}{Used internally in PickWin to move to next station}


}
  
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
