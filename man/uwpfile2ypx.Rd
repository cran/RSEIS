\name{uwpfile2ypx}
\alias{uwpfile2ypx}

\title{UW pickfile to pphase pick data.frame}
\description{
 Read in ASCII version of pickfile.  This is the
 output list used to plot picks on swig, often called WPX or YPX
 in other functions.
}
\usage{
uwpfile2ypx(P)
}

\arguments{
  \item{P}{pickfile}
}

\value{
  list:
  \item{STAS}{input structure}
  \item{yr}{year}
  \item{mo}{month}
  \item{dom}{day of month}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
  \item{col}{color}
  \item{onoff}{logical, TRUE plot trace}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{
data("GH")
WW = RSEIS::uwpfile2ypx(GH$pickfile)
vertord <-  getvertsorder(GH$pickfile, GH)
R1 = rangedatetime(WW)
R2 = rangedatetime(GH$info)
S1 = secdifL(R2$min, R1$min)



swig(GH, sel=vertord$sel, APIX=WW, WIN=c(S1-1, 15) , SHOWONLY=0)

}
\keyword{misc}
