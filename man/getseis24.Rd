\name{getseis24}
\Rdversion{1.1}
\alias{getseis24}
\title{Get 24 Hours of Seismic Data
}
\description{Get 24 Hours of Seismic Data
}
\usage{
getseis24(DB, iyear = 2009, iday = 1, usta = "",
 acomp = "", kind = 1,  Iendian=1, BIGLONG=FALSE)
}
\arguments{
  \item{DB}{
    Data base of meta-data about the seismic trace files
}
  \item{iyear}{Year for extraction
}
  \item{iday}{Julian day for extraction
}
  \item{usta}{station to show
}
  \item{acomp}{component to show
}
  \item{kind}{kind of data, default=1, SEGY data
  }
 \item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
  
  
}
\details{
  The DB file consists of a list of information on
  where to find the data and what times are covered.
   DB is \describe{
      \item{fn}{full path to file}
      \item{yr}{year}
      \item{jd}{julian day}
      \item{hr}{hour}
      \item{mi}{minute}
      \item{sec}{second}
      \item{dur}{duration, seconds}
      \item{origyr}{origin time for epoch calculations}
      }


}
\value{

\item{yr}{start year}
\item{jd}{start julian day}
\item{t1}{start t1 (with epoch) }
\item{t2}{start t2 (with epoch day) }
\item{ed}{ epoch day}
\item{hr}{start  hour}
\item{mi}{start  minute}
\item{sec}{start  seconds}
\item{gamp}{Amplitude of each trace}
\item{gdt}{delta-t, sample interval, in seconds}
\item{gnam}{station name}
\item{gfile}{file information}
\item{sigs}{List of time series}
\item{zna}{List of NA values in each time series}

  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{
  setupDB, plotseis24
}
\examples{

\dontrun{
JJ <- getseis24(DB, 2008, 366, usta, acomp, kind = 1)
 
pjj <- plotseis24(JJ, dy=1/18, FIX=24, SCALE=1,
FILT=list(ON<-FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"),
RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )

}



}
\keyword{misc}
