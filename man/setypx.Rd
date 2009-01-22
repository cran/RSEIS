\name{setypx}
\alias{setypx}
\title{Create an empty window pick list}
\description{
  Create an empty window pick list.  This is used primarily internally.
}
\usage{
setypx()
}

\value{
  List:
\item{tag}{tag for identification of station and component}
\item{name}{station name}
\item{comp}{component name}
\item{c3}{compnent name with secondary tags}
\item{phase}{phase}
\item{err}{error}
\item{pol}{polarity}
\item{flg}{flag}
\item{res}{residual}
\item{dur}{duration}
\item{yr}{year}
\item{mo}{month}
\item{dom}{day of month}
\item{jd}{julian day}
\item{hr}{hour}
\item{mi}{minute}
\item{sec}{second}
\item{col}{color}
\item{onoff}{logical, ON or OFF for plotting}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{setwpix}
\examples{

a = setypx()

print(a)

}
\keyword{misc}
