\name{Mine.Teles}
\alias{Mine.Teles}
\title{Mine seismic data base for teleseismic/regional events
}
\description{Mine seismic data base for teleseismic/regional events
}
\usage{
Mine.Teles(DB, ev, sta, ista = 1, usta = "DOM", acomp = "V", velmodel = NULL, IASPpath = "", regdist = 110)
}
\arguments{
  \item{DB}{Data base of segy/sac files (makeDB) 
}
  \item{ev}{ list of events (lat, lon, date/time, mag)
}
  \item{sta}{ list of stations (name, lat, lon z)
}
  \item{ista}{ which stations to predict arrivals for
}
  \item{usta}{  which stations to show in plot
}
  \item{acomp}{  which components to show in plot
}
  \item{velmodel}{ regional/local 1D velocity model
}
  \item{IASPpath}{ path to IASP91 programs and binary vel models for Taup-P
}
  \item{regdist}{radius for regional vs global model calculations
}
}
\details{program must run external codes from Brian Kennets software.
}
\value{List of predicted arrival times per each event, plotting of
  events and
  picks on those events
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{TTteles
}
\examples{

\dontrun{

load("/home/Site/lees/SANTIAGUITO/SGRobjs/GUAT09.sta.RData")

sta$name = sta$nam

stinfo  = targetLL(sta, rdist=100)

proj = stinfo$proj



IASPpath = '/home/lees/Progs/TTIMES'
eq1 = scan(file="sg06_eqs_neic.csv", skip=1, sep=",", what=list(yr=0, mo=0, dom=0, t1="", lat=0, lon=0, mag=0, depth=0))

######  parse out the time:
eq1$hr = as.numeric( substr(eq1$t1, 1,2))
eq1$mi = as.numeric( substr(eq1$t1, 3,4))
eq1$sec = as.numeric( substr(eq1$t1, 5,9))

eq1$z = eq1$depth
eq1$jd = getjul(eq1$yr, eq1$mo,  eq1$dom)

w1 = which(eqdists$dist<110)

##########   restrict the global data to only very large events?
w2 = which(eq1$mag>5.5)


##    create a an earthquake structure list

W = c(w1, w2)
##  W = c(w1 )


mylist = list()
for(i in 1:length(W))
{
j = W[i] 
mylist[[i]] = list(yr=eq1$yr[j], jd=eq1$jd[j], mo=eq1$mo[j], dom=eq1$dom[j], hr=eq1$hr[j], 
mi=eq1$mi[j], sec=eq1$sec[j], lat=eq1$lat[j], lon=eq1$lon[j], z=eq1$z[j], mag=eq1$mag[j])


}
NEW.EV = Mine.Teles(DB, mylist, sta, ista=1, usta=sta$name, acomp="V", IASPpath=IASPpath, regdist=110 )


}


}
\keyword{misc}
