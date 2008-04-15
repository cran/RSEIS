\name{BKpfile2ypx}
\alias{BKpfile2ypx}
\title{Read output of ttimes}
\description{
  Convert a teleseismic pickfile to structure usefule for PICK.GEN.
   Takes as input the output file of program ttimes, by Brian Kennett,
  which uses the TauP method.
}
\usage{
BKpfile2ypx(P)
}
\arguments{
  \item{P}{output of TTteles:

\item{num}{phase arrival index}
\item{phase}{phase name}
\item{sec}{seconds}
\item{tmin}{minutes}
\item{tsec}{fractional seconds after minutes}
\item{dtdd}{derivative of time w.r.t distance dt/dd }
\item{dtdh}{derivative of time w.r.t depth dt/dh }
\item{dddp}{derivative of distance w.r.t ray parameter dd/dp }



  }
}

\value{
  List of PICKS:
 \item{tag}{tag for unique identification of trace}
\item{name}{station name}
\item{comp}{component name}
\item{c3}{three element id}
\item{phase}{phase name}
\item{err}{uncertainity of arrival time}
\item{pol}{polarity}
\item{flg}{flag for location program}
\item{res}{residual fom predicted model}
\item{yr}{year}
\item{mo}{month}
\item{dom}{day of month}
\item{jd}{Julian day}
\item{hr}{hour}
\item{mi}{minute}
\item{sec}{second}
\item{col}{color for plotting}
\item{onoff}{logical, flag whether to plot or not}
\item{cat}{category (usually equal to phase)}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  This routine assumes you have a program called ttimes,
  or some equivalent.  The output of that routine
  is dumped to a file by ttBK a routine I wrote,
  and R then reads this in.
}
\seealso{TTteles, gwpix2ypx, uwpfile2ypx, PICK.GEN}
\examples{

\dontrun{

 cmdd = paste(sep=' ', "ttBK 0 ", i , " > ttBK.tempoutput")
system(cmdd )

 A = scan(file="ttBK.tempoutput", list(num=0, phase="", sec=0, tmin=0, tsec=0, dtdd=0, dtdh=0, dddp=0), skip=1)
      attr(A, 'name') = "NPL"
       attr(A, 'input') = list(delta=i , depth=0)
     
}

}


\keyword{misc}
