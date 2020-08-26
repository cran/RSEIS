`Jtim` <-
function(jj, hr=hr, mi=mi, sec=sec, yr=NULL, origyr=NULL)
  {
    #######  do recdate but return a decimal julian day
    if(missing(hr)) { hr=0 }
    if(missing(mi)) { mi=0 }
    if(missing(sec)) { sec=0 }
    if(missing(yr)) yr = NULL

    if(is.list(jj))
      {
        jday = jj$jd
         hr  = jj$hr
         mi  = jj$mi
         sec  = jj$sec
         yr   = jj$yr
      }
    else
      {
        jday = jj
      }

    ##########  this section does not work
    if(!is.null(origyr))
      {
        eday = EPOCHday(yr, jd=jday, origyr=origyr)
        jday = eday$jday
      }
    
  d = jday+ hr/24+mi/1440+ sec/86400
  return(d)
  }

