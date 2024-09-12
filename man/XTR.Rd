\name{XTR}
\alias{XTR}
\alias{NEXT}
\alias{PREV}
\alias{HALF}
\alias{MARK}
\alias{DOC}
\alias{REFRESH}
\alias{RESTORE}
\alias{ZOOM.out}
\alias{ZOOM.in}
\alias{LEFT}
\alias{RIGHT}
\alias{SCALE}
\alias{PSEL}
\alias{FLIP}
\alias{PTS}
\alias{FILT}
\alias{UNFILT}
\alias{SPEC}
\alias{WWIN}
\alias{SGRAM}
\alias{WLET}
\alias{XTR}
\alias{Pinfo}
\alias{TSHIFT}
\alias{RMS}
\alias{LocStyle}
\alias{CENTER}
\alias{fspread}
\alias{Xwin}


\title{Buttons for swig
}
\description{defining functions for swig
}
\usage{
XTR(nh, g)
NEXT(nh, g)
PREV(nh, g)
HALF(nh, g)
MARK(nh, g)
DOC(nh, g)
REFRESH(nh, g)
RESTORE(nh, g)
ZOOM.out(nh, g)
ZOOM.in(nh, g)
RIGHT(nh, g)
LEFT(nh, g)
SCALE(nh, g)
PSEL(nh, g)
FLIP(nh, g)
PTS(nh, g)
FILT(nh, g)
UNFILT(nh, g)
SPEC(nh, g)
WWIN(nh, g)
SGRAM(nh, g)
WLET(nh, g)
XTR(nh, g)
Pinfo(nh, g)
TSHIFT(nh, g)
RMS(nh, g)
LocStyle(nh, g)
CENTER(nh, g)
fspread(nh, g)
Xwin(nh, g)

}
\arguments{
  \item{nh}{waveform list for RSEIS
}
  \item{g}{plotting parameter list for interactive program
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


\seealso{swig
}
\examples{

if(interactive()){

MYFUNC<-function(nh, g)
  {
    print("pressed MYFUNC")
    g$sel
    d  <-  data.frame(list(stations=nh$STNS[g$sel],
                   components=nh$COMPS[g$sel]))
     print(d)        
    g$action  <-  "replot"
    invisible(list(global.vars=g))	
  }

STDLAB <- c("DONE", "QUIT", "SELBUT" , "PSEL", "MYFUNC" )
data(GH)
JJ  <-  swig(GH, sel=1:10, STDLAB=STDLAB)

}


}
\keyword{misc}
