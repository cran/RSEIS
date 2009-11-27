\name{textrect}
\alias{textrect}
\title{Text labels with border
}
\description{Plot Text labels with border and background color
}
\usage{
textrect(x, y, lab, textcol = "black", col = "white", border = "black",
off = 0.06, brd = 0.06, pos = 1, log="" , add=TRUE, ...)
}
\arguments{
  \item{x}{x-location, user coordinates
}
  \item{y}{y-location, user coordinates
}
  \item{lab}{character for label
}
  \item{textcol}{color for labels
}
  \item{col}{color for background
}
  \item{border}{color for border, NA=do not plot
}
  \item{off}{Offset from point, inches, default=0.06
}
  \item{brd}{Border around text, inches, default=0.06
}
\item{pos}{numeric, 
  position=one of (0.0, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5),
  as in the normal text call with pos=1,2,3,4,
  however, here I allow half way between points.
  0 indicates no offset and label is
  placed centered on the point.

}

\item{log}{character, as in plot }

\item{add}{add to existing plot (FALSE returns plotting rectangles)
}


  \item{\dots}{additional parameters from par, used for font, cex, etc...
}
}
\details{
  textrect plots a label on an existing plot at the location designated.
  The text is surrounded by a rectangular box with color inside and a
  border.
  The box can be placed around the designated point at 9
  positions.  Positions 1,2,3,4 are the same as text parameter pos.
  Position 0 is centered, i.e. no offset.
  Positions, 1.5, 2.5, 3.5, 4.5 are at an angle 45 degrees clockwise
  from the integer values.
  

  
}
\value{graphical side effects.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


thepos = c(0, seq(from=1, to=4.5, by=.5))
lab="the string"

x = 1:9
y = 1:9
plot(x,y, asp=1)
for(i in 1:length(thepos))
{
textrect(x[i], y[i], lab, col=i , border='green' , textcol="gold",  off=.06,  brd=.06 , pos=thepos[i], font=1, cex=.8 )
}


x = runif(10)
y = runif(10)
lab = floor( 1000*runif(10) )
i=sample(thepos, 10, replace = TRUE)
col = sample(rainbow(100) , 10, replace = TRUE)

plot(x,y, asp=1)
textrect(x, y, lab, pos=i , textcol="black", col=col)



}
\keyword{misc}
