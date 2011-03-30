\name{swig}
\alias{swig}
\alias{PICK.GEN}
\title{Seismic Analysis }
\description{
Main Interactive Program for plotting and analyzing seismic waveform data.
}
\usage{
swig(GH, sel = 1:length(GH$dt), ORD = NULL, WIN = NULL, APIX = NULL,
PHASE = NULL,
STDLAB = NULL, PADDLAB = NULL, TEMPBUT=NULL,
SHOWONLY = FALSE, CHOP = FALSE, TIT = "",
pts = FALSE, forcepix = FALSE, pcex=0.7, SCALE = 1, ilocstyle=1,
velfile = "", stafile = "", LOC = NULL,
prefilt=list(fl=.2, fh=15,  type="HP", proto="BU"), filters=NULL)
}

\arguments{
  \item{GH}{Seismic data structure}
  \item{sel}{selection of traces from structure }
  \item{ORD}{order to plot traces }
  \item{WIN}{vector c(t1, t2) for window of traces to be shown }
  \item{APIX}{structure of arrival time picks }
  \item{PHASE}{phase to display, "P", "S", etc }
  \item{STDLAB}{label of buttons}
  \item{PADDLAB}{label of phase-pick buttons}
  \item{TEMPBUT}{temporary, user defined buttons}
  \item{SHOWONLY}{logical, TRUE=non-interactive }
  \item{CHOP}{whether to chop the signal }
  \item{TIT}{title for the top of plot}
  \item{pts}{whether to plot specific points on the plot}
  \item{forcepix}{logical, force all phase picks to be shown on all
    traces }
  \item{pcex}{ Pick label size expansion (cex), default=0.7 }
  \item{SCALE}{flag, 1,0= scale according to window or trace }
  \item{ilocstyle}{integer, style of click graphic, one of -1, 0, 1, 2,
    3, indicating: 
    points, abline, segs, segs+abline,
    segs+long-abline  , default=1  }
  \item{velfile}{velocity structure or file name }
  \item{stafile}{station structure or file name }
  \item{LOC}{source location structure (lat, lon, depth) }
  \item{prefilt}{default filter definition list(fl=.2, fh=15,  type="HP",
    proto="BU")}
  \item{filters}{ a list of filters for choosfilt, the list consists of
    3 vectors: flo, fhi and type defining the filter choices.}

  
}
\details{
  This is the main program that drives the other analysis in RSEIS.
  GH is a list consisting of header (meta-data) and time series
  information.  See documentation on GH to get complete description.

  A set of filters can be defined by the user, see choosfilt
  
  Default Buttons, can be created by:
  STDLAB = c("DONE", "QUIT","zoom out", "zoom in", "Left", "Right", "restore", "Pinfo","WINFO",
  "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT", "SCALE",
  "Postscript")

  If the user has defined STDLAB.DEFAULT and PADDLAB.DEFAULT
  in the .Rprofile or .First commands, these will override
  the default in the function definition.


  
}
\value{
  Various structures are returned
  based on interactive selections of the user.

  Howeverr, the default return list:
  \item{but}{last button pushed}
  \item{sloc}{location of last set of clicks}
  \item{WPX}{set of saved WPIX (window picks}
  \item{BRUNINFO}{Brune Model information}
  \item{DETLINFO}{Detailed information about traces}
  \item{mark}{mark (MARK button was pressed}
  \item{PUSHED}{list of all buttons pressed prior to exit}

}


\note{
  If using the filters for button FILT, it is useful to have a "None"
  in case no filter is desired (i.e. user changes mind).
  

  }

\author{Jonathan M. Lees<jonathan.lees.edu>}



\seealso{PICK.DOC, GH, RPGM, choosfilt}
\examples{
data("GH")
###  This loads a structure


  STDLAB = c("DONE", "QUIT","zoom out", "zoom in", "Left", "Right", "restore", "Pinfo","WINFO",
                           "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT", "SCALE", "Postscript")

sel=GH$COMPS=="V" 
p = swig(GH, sel=sel, STDLAB=STDLAB)
print(p)

 p = swig(GH, sel=sel, WIN=c(4,14) , STDLAB=c("DONE", "LAME", "DAME"))
print(p)


############    example with filter
data(KH)

thefilts =
          list(flo=
               c(0.02, 0.02, 0.02, 0.02, 0.02,   0.02,
                 0.02, 0.02, 0.02,  0.02, 0.02,  0.02,
                 0.02,
                 1/2, 1/50,1/100, 1/100,
1/100,1/100,1/100,1,1,
                 0.2, 15, 5, 2,1,
                 100),
               fhi=
               c(1/10, 1/6, 1/5, 1/4, 1/3, 1/2,
                 0.2,  0.5, 1.0,  2.0, 3.0,  4.0,
                 7.0,
                 8, 1/2.0,1/5.0,1/10.0,
1/20, 1/30,1/40,10,5,
                 7.0, 100, 100, 100,10,
                 100),
             type =
               c("LP","LP", "LP", "LP", "LP", "LP",
                 "LP","LP", "LP", "LP", "LP", "LP",
                 "LP",
                 "BP", "BP","BP","BP","BP","BP",
"BP","BP","BP",
                 "HP", "HP","HP", "HP","HP",
                 "None"))



swig(KH, filters=thefilts)





}

\keyword{iplot}
