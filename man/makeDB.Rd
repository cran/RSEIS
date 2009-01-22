\name{makeDB}
\Rdversion{1.1}
\alias{makeDB}
\title{Create a seismic Waveform Database }
\description{Create a seismic Waveform Database}

\usage{
makeDB(path, pattern, kind = 1)
}
\arguments{
  \item{path}{Path to directory where files and directories exist}
  \item{pattern}{pattern for listing of files}
  \item{kind}{kind of data: segy=1; sac=2}
}
\details{
  The files are typically located in a directory structure
  created by programs like ref2segy, a PASSCAL program for
  downloading data in the field.  Each file contains one seismogram,
  with a header.  makeDB reads in all the headers and
  creates a list of meta-data for later use in RSEIS.
}
\value{
  list:
   \item{fn}{file name}
      \item{yr}{year}
      \item{jd}{julian day}
      \item{hr}{hour}
      \item{mi}{minute}
      \item{sec}{second}
      \item{dur}{duration, seconds}
      \item{t1}{time 1 in Epoch days}
     \item{ t2}{time 2 in Epoch days}
      \item{sta}{station name}
      \item{comp}{component name}
      \item{origyr}{origin year for epoch times}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Epoch times are used to accomodate problems where
  julian days cross year end boundaries, so that
  day 366 comes before day 1 of the next year.
}

\seealso{setupDB, Mine.seis , getseis24, plotseis24, EPOCHday, PICK.GEN}
\examples{
\dontrun{

#############  set directory
path = '/home/lees/Site/Santiaguito/SG09'
pattern = "R0*"

###   get DB information
XDB  =  makeDB(path, pattern, kind =1)

##### select a station
usta = "CAL"
acomp = "V"

#####   extract 24 hours worht of data
JJ = getseis24(DB, 2009, 2, usta, acomp, kind = 1)


#####  plot 24 hours worth of data

pjj = plotseis24(JJ, dy=1/18, FIX=24, SCALE=0, FILT=list(ON=FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"), RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )


##########   window a small portion on 24 hour display
w = winseis24(pjj)

###  open a new window
X11()
VNE = c("V", "N", "E")
###  IJK = c("I", "J", "K")

gsta=c("CAL", "KAM", "DOM",  "LAV")

ucomp=VNE

###  or:   ucomp=IJK

###   set epoch day and get the times
 eday = EPOCHday(w$yr, jd = w$jd, origyr = DB$origyr)

########  using epoch day, set times for beginning and end of window
        at1 = eday$jday + (w$hr[1])/24
        at2 = eday$jday + (w$hr[2])/24
########  extract data from the data base:
        GH = Mine.seis(at1, at2, DB, gsta, ucomp)

###  show data:
        PICK.GEN(GH)

}


}
\keyword{misc}
