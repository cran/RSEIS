`prep1wig` <-
function(wig=vector(), dt=1, sta="STA", comp="CMP", units="UNITS" )
{
  if(missing(dt)) {dt=1}
  if(missing(sta)) {sta="STA"}
  if(missing(comp)) {comp="CMP"}
  if(missing(units)) {units="UNITS"}

  
  KLIST = list()
  KLIST[[1]] = list(
         amp=wig,
         dt=dt,
         nzyear=2000,
         nzjday=1,
         nzhour=1,
         nzmin=1,
         nzsec=0,
         nzmsec=0,
         b=0,
         e=0,
         o=0,
         fn="inputFILEname",
         sta=sta,
         comp=comp,
         DATTIM=
         list(yr=0,
              jd=1,
              mo=1,
              dom=1,
              hr=1,
              mi=1,
              sec=0,
              msec=0,
              dt=dt,
              t1=0,
              t2=0,
              off=0),
         N=length(wig),
         units=units)
  

  return(KLIST)
  

  
}

