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

\dontrun{

############   example of different scenarios
###  make list of the files
fls <- list.files(path="/home/lees/SAC", pat="sac")

###  convert them to R

####   was created on a little endian machine
###          and is being read on a little endian
convert2Rseis(fls, NEWDIR = "/home/lees/MY_RDATA", kind = 2,
 Iendian = "little", BIGLONG = FALSE)


####   was created on a little endian machine
###     and is being read on a big endian
convert2Rseis(fls, NEWDIR = "/home/lees/MY_RDATA",
kind = 2, Iendian = "swap", BIGLONG = FALSE)

####   was created on a little endian machine and is being read on a big
####                       endian, with  the 64 bit machine used LONG=8
convert2Rseis(fls, NEWDIR = "/home/lees/MY_RDATA",
kind = 2, Iendian = "swap", BIGLONG = TRUE)


F90dir <- "/mnt/flash/FUEGO/2009/F900"
kfl <- list.files(path=F90dir, full.names=TRUE)

bname <- basename(kfl)

###  now we want to convert the station names

###  extract the STA name from the file names:
STA <- unlist( lapply(  strsplit(split="\\.", bname), getmem , 3) )
###   extract comp name - in this case needs further extraction
COMP1 <- unlist( lapply(  strsplit(split="\\.", bname), getmem , 4) )
COMP <- substr(COMP1, 3, 3)

convert2Rseis(kfl, NEWDIR="/home/lees/Site/Fuego/2009",
kind <- 2, Iendian="swap" , BIGLONG=FALSE, NEWsta=STA, NEWcomp=COMP )






}


}
\keyword{misc}
