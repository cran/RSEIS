\name{setupDB}
\Rdversion{1.1}
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

\dontrun{

###  location of data base file
datadir1  <-  '/home/lees/Site/Santiaguito/SG09'
d1  <-  'segyDB'
#### scan in the data base file
DB  <-  scan(file=paste(sep="/", datadir1, d1), list(fn="", yr=0, jd=0,
hr=0, mi=0, sec=0, dur=0))
#### interactively fix the DB list
DB  <-  setupDB(DB)


}




}


\keyword{misc}
