\name{editDB}
\alias{editDB}
\title{Edit Data Base
}
\description{Edit, or remove items from
  an RSEIS data base after it has been read in.
}
\usage{
editDB(DB, w)
}
\arguments{
  \item{DB}{RSEIS data base
}
  \item{w}{vector of index items to remove
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

\dontrun{

DB = makeDB('/media/disk-1/TUNG_2010', "2010", kind=2,Iendian=1, BIGLONG=FALSE )

sw = which(DB$yr>2011)

##########   remove the bad lines from the DB
DDB = editDB(DB, -sw)
infoDB(DB)


}


}
\keyword{misc}
