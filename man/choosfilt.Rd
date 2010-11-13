\name{choosfilt}
\alias{choosfilt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{INteractive CHoice of Filter}
\description{
 Choose Butterworth filter from a selection
}
\usage{
choosfilt(thefilts = thefilts, ncol = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thefilts}{list of filter parameters}
  \item{ncol}{number of columns}
}
\details{
  Used for interactive choices in swig.
  See example below.

}
\value{
  filter parameter list:
  \item{ON}{logical, TRUE=filter is on}
  \item{fl}{low frequency cut-off }
  \item{fh}{high frequency cut-off}
  \item{type}{type of filter, one of c("LP",  "HP","BP" ,"BR" )  }
  \item{proto}{prototype, c("BU", "BE" , "C1" ,"C2")  }

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{butfilt, RPMG}
\examples{

 thefilts =
          list(flo=
               c(0.02, 0.02, 0.02, 0.02, 0.02,   0.02,
                 0.02, 0.02, 0.02,  0.02, 0.02,  0.02,
                 0.02,
                 1/2, 1/50,1/100, 1/100,1,1,
                 0.2, 15, 5, 2,1,
                 100),
               fhi=
               c(1/10, 1/6, 1/5, 1/4, 1/3, 1/2,
                 0.2,  0.5, 1.0,  2.0, 3.0,  4.0,
                 7.0,
                 8, 1/2.0,1/5.0,1/10.0,10,5,
                 7.0, 100, 100, 100,10,
                 100),
             type =
               c("LP","LP", "LP", "LP", "LP", "LP",
                 "LP","LP", "LP", "LP", "LP", "LP",
                 "LP",
                 "BP", "BP","BP","BP","BP","BP",
                 "HP", "HP","HP", "HP","HP",
                 "None"))

choosfilt(thefilts = thefilts, ncol = 5)


}

\keyword{iplot}
