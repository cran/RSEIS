`getHcard` <-
function(hcard)
{
  h = unlist(strsplit(split=" ", hcard))
  H = list(yr=as.numeric(h[2]),
    mo=as.numeric(h[3]),
    dom=as.numeric(h[4]),
    hr=as.numeric(h[5]),
    mi=as.numeric(h[6]),
    sec=as.numeric(h[7]),
    lat=as.numeric(h[8]),
    lon=as.numeric(h[9]),
    z=as.numeric(h[10]),
    mag=as.numeric(h[11]))

  return(H)
}

