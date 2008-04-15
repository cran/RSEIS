\name{hodogram}
\alias{hodogram}

\title{HodoGram Plot}
\description{
  HodoGram Plot
}
\usage{
hodogram(nbaz, dt = dt, labs = c("Vertical", "North", "East"), COL =rainbow(140)[1:100] , STAMP = "")
}

\arguments{
  \item{nbaz}{n by 3 matrix }
  \item{dt}{time sample rate }
  \item{labs}{labels for the components}
  \item{COL}{color palette}
  \item{STAMP}{character stamp for identification }
}

\value{
  sx = list
  graphical side effect

}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

data("GH")

temp = cbind(GH$JSTR[[1]][1168:1500], GH$JSTR[[2]][1168:1500],
GH$JSTR[[3]][1168:1500])

pmolabs = c("Vertical", "North", "East")


sx = hodogram(temp, dt=GH$dt[1]  ,labs=pmolabs,  STAMP="Example",  COL=rainbow(100) )


}

\keyword{misc}
\keyword{hplot}
