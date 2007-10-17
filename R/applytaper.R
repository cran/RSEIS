`applytaper` <-
function(f, p=0.05)
  {

    if(missing(p)) { p = 0.05 }
    n = length(f)
    
    l=round((n-2)*p);

    s = seq(1,n)

    vwin=rep(1,n)

    bend = seq(1,round(l) )
    
    vwin[s<=round(l)] = 0.5*(1.0-cos(bend*pi/(l+1)));

    bend = seq(n-l-2, n )
    
    vwin[s>= n-l-2] = 0.5*(1.0-cos(bend*pi/(l+1)));

    newf = vwin*f
    return(newf)
}

