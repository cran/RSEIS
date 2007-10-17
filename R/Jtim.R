`Jtim` <-
function(jj, hr=hr, mi=mi, sec=sec)
  {
    #######  do recdate but return a decimal julian day
    if(missing(hr)) { hr=0 }
    if(missing(mi)) { mi=0 }
    if(missing(sec)) { sec=0 }

    if(is.list(jj))
      {
        jday = jj$jd
         hr  = jj$hr
         mi  = jj$mi
         sec  = jj$sec
      }
    else
      {
        jday = jj
      }
    
  d = jday+ hr/24+mi/(24*60)+ sec/(24*3600)
  return(d)
  }

