\name{JGET.seis}
\alias{JGET.seis}
\alias{getseisinfo}

\title{Reads various seismic file formats}
\description{
  This fuction calls c-routines to read in \sQuote{segy}, \sQuote{sac}.
}
\usage{
JGET.seis(fnames, kind = 1, PLOT = FALSE)
getseisinfo(fnames, kind = 1)
}
\arguments{
  \item{fnames}{list of file names.}
  \item{kind}{an integer {1, 2, 3}; 1 = segy, 2 = sac, 3 = AH.}
  \item{PLOT}{logical, for if the traces should be plotted.}
}
\value{
  
  a list containing the seismic data and header information.
  Each trace consists of a list with:
\item{fn}{original file name}
\item{sta}{station name}
\item{comp}{compnent}
\item{dt}{delta t in seconds}
\item{DATTIM}{time list}
   \item{yr}{year}
   \item{jd}{julian day}
   \item{mo}{month}
   \item{dom}{day of month}
   \item{hr}{hour}
   \item{mi}{minute}
   \item{sec}{sec}
   \item{msec}{milliseconds}
   \item{dt}{delta t in seconds}
   \item{t1}{time start of trace}
   \item{t2}{time end of trace}
   \item{off}{off-set}
   \item{N}{number of points in trace}
   \item{units}{units}
   \item{amp}{vector of trace values}
   
 }
 
\seealso{Package:Rsac}
\note{

  getseisinfo gets only the header information from the trace.
  

}



\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{  
\dontrun{

Lname = list.files(path='/data/wadati/bourbon/LaurelCanNC/R005.01' , pattern="08.005.01.41.23.9024", full.names=TRUE)

S1  = JGET.seis(Lname, kind = 1, PLOT = FALSE)


}


}
\keyword{misc}
