\name{editDB}
\alias{editDB}
\alias{pathDB}

\title{Edit Data Base
}
\description{Edit, or remove items from
  an RSEIS data base after it has been read in.
}
\usage{
editDB(DB, w)
pathDB(DB,  path1="", path2="")

}
\arguments{
  \item{DB}{RSEIS data base
}
  \item{w}{vector of index items to remove
  }


  \item{path1}{character for old path 
  }

  \item{path2}{character for new path to replace old path 
  }



  
}
\details{
  The DB is a list.  The program cycles through the elements of the list
  and removes all lnes that correspond to the
  idecies given in w.
  
}
\value{Returns a DB list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{A problem arises if the makeDB program reads in, or tries to read
  in
  files that have not data base header information.
  This program can eliminate these from the data base.
}


\seealso{makeDB, infoDB 
}
\examples{

##########   create a data set and a DB
tdir = tempdir()
data(GH)

DD = data.frame(GH$info)
WV = which(GH$COMPS=='V')

L1 = length(WV)

######
GIVE = vector(mode='list')

for(j in 1:L1)
{
 i = WV[j]   
AA = DD[i,]

GIVE[[j]] = list(fn = AA$fn, sta =GH$STNS[i] , comp = GH$COMP[i], 
            dt = AA$dt, DATTIM = AA, N = AA$n1, units = NA, 
            coords = NA, amp = GH$JSTR[[i]]  )

}

#######  save files in the tempdir
for(i in 1:length(GIVE) )
{

    sig = GIVE[[i]]
  d1 =  dateStamp(sig$DATTIM)
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }
LF  = list.files(path=tdir,pattern='.RDS', full.names=TRUE)

#######   make the database
cosoDB = FmakeDB(LF, kind=-1)

######  change the DB path:
path1<-tdir
path2<-"."

#######  change the path name of the trace files
newDB <- pathDB(cosoDB, path1, path2 )



}
\keyword{misc}
