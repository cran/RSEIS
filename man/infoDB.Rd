\name{infoDB}
\Rdversion{1.1}
\alias{infoDB}
\title{Print information about the seismic database
}
\description{Print information about the seismic database
}
\usage{
infoDB(DB, verbose=TRUE)
}
\arguments{
  \item{DB}{Database list
}
  \item{verbose}{logical, print information to screen, default=TRUE
}

}

\value{
  list(
  \item{usta}{Unique station names}
  \item{ucomp}{Unique component names}
  \item{start}{starting date}
  \item{end}{ending date}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{makeDB
}
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
  d1 =  dateStamp(sig$DATTIM,  sep='_')
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }

########################  Now  read files and make the DataBase:
LF  = list.files(path=tdir, pattern='.RDS', full.names=TRUE)
DB = FmakeDB(LF, kind=-1)
IDB = infoDB(DB)





}
\keyword{misc}
