`yeardate` <-
function(yr, jd, hr, mi, sec)
  {
    d  = yr+jd/365+hr/(365*24)+mi/(365*24*60)+sec/(365*24*3600)
    return(d)
  }

