\name{setupDB}
\alias{setupDB}
\title{Set up a seismic data base
}
\description{
  Set up a data base storing the location
  and times for a set of seismic data.
}
\usage{
setupDB(DB, token = TRUE, split = "\\\\.")
}
\arguments{
  \item{DB}{
    \describe{
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
    \item{token}{logical, use tokens in the file names of the fn's to
      extract station and component names for selection. default=TRUE
    }
    \item{split}{ character string to split if using token,
      default is a period.
    }
}
\details{
  If token is FALSE, then the station name and component
  are selected using substr, i.e. by column number.
}
\value{
  DB with epoch time and station information appended,
  \item{t1}{epoch start time  }
  \item{t2}{expoch end time  = t1+nsamps*sample rate n seconds}
  \item{sta}{station}
  \item{comp}{component}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Program attaches station identification used for grepping. 
}


\seealso{EPOCHday, T12.pix, Mine.seis}

\examples{


##########  to illustrate, we make a set of individual seismograms
data(GH)
L1 = length(GH$JSTR)
DD = data.frame(GH$info)

GIVE = vector(mode='list')

for(i in 1:L1)
{
AA = DD[i,]
GIVE[[i]] = list(fn = AA$fn, sta =GH$STNS[i] , comp = GH$COMP[i], 
            dt = AA$dt, DATTIM = AA, N = AA$n1, units = NA, 
            coords = NA, amp = GH$JSTR[[i]]  )
}

###########  save the seismic data in a temporary directory
####   each trace in a separate file
tdir = tempdir()
for(i in 1:length(GIVE) )
{
    sig = GIVE[[i]]
  d1 =  dateStamp(sig$DATTIM)
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }

########################  Now  read files and make the DataBase:
LF  = list.files(path=tdir,  pattern='.RDS', full.names=TRUE)
DB = FmakeDB(LF, kind=-1)
## IDB = infoDB(DB)

plotDB(DB)



}


\keyword{misc}
