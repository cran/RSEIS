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
  \item{kind}{an integer 1, 2, 3; 1 = segy, 2 = sac, 3 = AH.}
  \item{PLOT}{logical, for if the traces should be plotted.}
}
\value{
  List containing the seismic data and header information.
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
 \details{
   JGET.seis  extracts digital seismic data from binary files
   stored in the file system.  The program uses C-code for I/O
   and passes data back to R.  Currently SAC, SEGY formats
   are installed but it is easy to extend.  AH format
   is available for LINUX systems, but there were problems compiling in
   WINDOWS and MACOS so this feature was removed.
   A filter for mseed format is currently being developed.
   
   }
\seealso{Mine.seis, Package:Rsac}
\note{

  getseisinfo gets only the header information from the trace.
  Information in the file names is ignored, so be sure to modify headers
  prior to using this method of extracting meta-data. (Or modify
  the meta data from the file names after reading in the data.)
  
   For SEGY files, in LINUX-UNIX, use:
    rename,    segymod (PASSCAL) to modify the headers
}



\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{  
\dontrun{

Lname = list.files(path='/data/wadati/bourbon/LaurelCanNC/R005.01' , pattern="08.005.01.41.23.9024", full.names=TRUE)

S1  = JGET.seis(Lname, kind = 1, PLOT = FALSE)


}


}
\keyword{misc}
