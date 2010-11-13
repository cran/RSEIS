\name{view.seis}
\alias{view.seis}
\title{Veiw seismic data window}
\description{Veiw seismic data (segy) window on an hourly basis.
}
\usage{
view.seis(aday, ihour, inkhour, SAVEFILE, days, DB, usta, acomp,  STDLAB =c("QUIT",  "NEXT", "PREV", "HALF"), TZ=NULL)
}
\arguments{
  \item{aday}{index of which day to use in vector days }
  \item{ihour}{hour to start }
  \item{inkhour}{increment in hours for viewing panel }
  \item{SAVEFILE}{file to save window picks in }
  \item{days}{vector of days to select from }
  \item{DB}{data base list of file names and start-times and durations }
  \item{usta}{stations to select }
  \item{acomp}{compnents to select }
  \item{STDLAB}{vector of buttons,  DEFAULT = c("QUIT",  "NEXT", "PREV", "HALF",  "WPIX", "zoom out",
         "refresh", "restore",  "SPEC", "SGRAM" ,"WLET", "FILT",
         "Pinfo", "WINFO") }
  \item{TZ}{Number of hours to add to GMT to get local time}
}
\details{
  The program view.seis assumes the data is stored in files accessable
  by the user and that the DB list has been scanned
  in and parsed.
  
}
\value{
 Graphical side effects and save.wpix stores appended picks.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  On LINUX systems I wrote these (non-R) programs
  to set up the data base for segy data:FLS.prl, segydatabase.
  To get these contact me directly.  TZ is (-6) for Guatemala.
}
\seealso{swig, save.wpix}
\examples{

\dontrun{


##############  method one:
#######  read in data base from files:
DB  = scan(file="/home/lees/Site/Santiaguito/SG09/segyDB", list(fn="",
yr=0, jd=0, hr=0, mi=0, sec=0, dur=0))

##  this file has first line:
#/home/lees/Site/Santiaguito/SG09/R001.02/09.001.11.57.09.SMI.J 2009 1 11 57 9.245000 3600.000000
#c...|....1....|....2....|....3....|....4....|....5....|....6....|....7.$

##  note: the file name includes the station SMI and the component J

##########  extract names of stations and components
das1  = substr(DB$fn, 58, 60)
comp1 = substr(DB$fn, 62,62))

###########  set up times (start and end) in julian days for selection
 DB$t1 = DB$jd + DB$hr/24 + DB$mi/(24*60) + DB$sec/(24*3600)
DB$t2 = DB$t1 + DB$dur/(24*3600)


#######   method 2:

DB = makeDB("/Users/lees/Site/Santiaguito/SG09", pattern = "R", kind = 1)
udays = unique(paste(DB$yr, DB$jd))
sdays =  as.numeric( unlist( strsplit(udays, split=" ") ) )


ye = sdays[seq(from=1, to=length(sdays), by=2)]
d  = sdays[seq(from=2, to=length(sdays), by=2)]
o = order(ye+d/366)

days = list(yr = ye[o], jd=d[o])



 das1  = DB$sta
  comp1 =  DB$comp

#######   get unique stations from list:
usta = unique(das1)

###########   choose the desired components 
acomp = c('I','V')

####   aday refers to one of the days listed in the days structure


###  days is a list of days (and associated years) that are in teh DB
days = list(jd=c(365,366, 1,2,3,4), yr=c(2008, 2008, rep(2009, times=4)))


#### extract days from the DB list:
udays = unique(paste(DB$yr, DB$jd))
sdays =  as.numeric( unlist( strsplit(udays, split=" ") ) )
ye = sdays[seq(from=1, to=length(sdays), by=2)]
d  = sdays[seq(from=2, to=length(sdays), by=2)]
o = order(ye+d/366)

days = list(yr = ye[o], jd=d[o])

####

aday = 3
SAVEFILE = 'wpix.out'
ihour = 15
inkhour = .5

####   aday refers to one of the days listed in the days structure


view.seis(aday, ihour, inkhour, SAVEFILE, days, DB, usta, acomp, TZ=(-6))



}



}
\keyword{misc}
