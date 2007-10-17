\name{jpostscript}
\alias{jpostscript}

\title{Postscript Output}
\description{
  Get file name and recreate plot
  on a postsctipt device.  This program
  makes an attempt to keep the same size
  plot as viewed in the screen.
}
\usage{
jpostscript(name, P = NULL)
}

\arguments{
  \item{name}{Postscript file name}
  \item{P}{vector to fix the size, c(width, height) }
}
\details{
  If P=c(10,12) is missing, program will
  attempt to use current plotting region via par
  to duplicated the size
  of the postscript device. Must close this device with
  dev.off() to finish.
  }
  
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{par, postscript, device }

\examples{

jjj = local.file('hi', 'eps')
x= rnorm(10)
y= rnorm(10)

plot(x,y)

print('resize the current plot')

jpostscript(jjj)
plot(x,y)
dev.off()




}

\keyword{misc}
