\name{convert2Rseis}
\alias{convert2Rseis}
\title{Convert Seismic data to RSEIS
}
\description{Convert Seismic in SAC or SEGY format
  to RSEIS native format.
}
\usage{
convert2Rseis(FLS, NEWDIR = ".", kind = 1, Iendian = "little", BIGLONG =
FALSE, NEWsta = "", NEWcomp = "")
}
\arguments{
  \item{FLS}{ array of File names
}
  \item{NEWDIR}{Destination directory path
}
  \item{kind}{an integer 1, 2, 3; 0=R(DAT) , 1 = segy, 2 = sac, 3 = AH.
}
  \item{Iendian}{Endian-ness of the data: 1,2,3: "little", "big", "swap".
 Default = 1 (little)

}
  \item{BIGLONG}{logical, TRUE=long=8 bytes

  }
 \item{NEWsta}{character vector, stations associated with the vector of files

 }
 
 \item{NEWcomp}{character vector, component name associated with the vector of files

 }
 
  
}
\details{
  Converts the data to R format so it can be loaded with the load
  command.
  After this conversion, files should be loaded in subsequent
  calls by using kind=0.
  
}
\value{
  Side effects - creates new files on local system
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{

JGET.seis  extracts digital seismic data from binary files
   stored in the file system.  The program uses readBin for I/O
   and passes data back to R.  Currently SAC, SEGY formats
   are installed but it is easy to extend.  AH format
   is available for LINUX systems, but there were problems compiling in
   WINDOWS and MACOS so this feature was removed.
   A filter for mseed format is currently being developed.



}


\seealso{JGET.seis, JSAC.seis , Mine.seis
}
\examples{
Iendian = .Platform$endian
data(GH)

##########  create some SAC files:
apath = tempdir()
J =  rseis2sac(GH, sel = 1:5, path = apath, BIGLONG =FALSE )
####  get SAC file file names:
Lname  <-  list.files(path=J , pattern='SAC', full.names=TRUE)

#####   convert each file to a saved RSEIS file, saved in apath
####  reading in SAC files, kind=2
convert2Rseis(Lname, NEWDIR = apath, kind = 2, Iendian = Iendian, BIGLONG =
FALSE )
####  check if files are there
list.files(path=apath)

}
\keyword{misc}
