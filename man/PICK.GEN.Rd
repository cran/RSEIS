\name{PICK.GEN}
\alias{PICK.GEN}

\title{Seismic Analysis }
\description{
Main Interactive Program for plotting and analyzing seismic waveform data.
}
\usage{
PICK.GEN(GH, sel = 1:length(GH$dt), ORD = NULL, WIN = NULL, APIX = NULL,
PHASE = NULL,
STDLAB = NULL, PADDLAB = NULL, TEMPBUT=NULL,
SHOWONLY = FALSE, CHOP = FALSE, TIT = "",
pts = FALSE, forcepix = FALSE, pcex=0.7, SCALE = 1,
velfile = "", stafile = "", LOC = NULL,
FILT=list(fl=.2, fh=15,  type="HP", proto="BU"), filters=NULL)
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
  \item{velfile}{velocity structure or file name }
  \item{stafile}{station structure or file name }
  \item{LOC}{source location structure (lat, lon, depth) }
  \item{FILT}{default filter definition list(fl=.2, fh=15,  type="HP",
    proto="BU")}
  \item{filters}{ a list of filters for choosfilt }

  
}
\details{
  This is the main program that drives the other analysis in RSEIS.
  GH is a list consisting of header (meta-data) and time series
  information.  See documentation on GH to get complete description.

  A set of filters can be defined by the user, see choosfilt
  
  Default Buttons, can be created by:
  STDLAB = c("DONE", "QUIT","zoom out", "zoom in", "Left", "Right", "restore", "Pinfo","WINFO",
                           "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT", "SCALE", "Postscript")


  
}
\value{
  Various strucutres are returned
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

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.DOC, GH, RPGM, choosfilt}
\examples{
data("GH")
###  This loads a structure


  STDLAB = c("DONE", "QUIT","zoom out", "zoom in", "Left", "Right", "restore", "Pinfo","WINFO",
                           "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT", "SCALE", "Postscript")

sel=GH$COMPS=="V" 
p = PICK.GEN(GH, sel=sel, STDLAB=STDLAB)
print(p)

 p = PICK.GEN(GH, sel=sel, WIN=c(4,14) , STDLAB=c("DONE", "LAME", "DAME"))
print(p)


}

\keyword{iplot}
