`itoxyz` <-
  function(i, nx, ny, nlay)
  {
    ##X## given a number index in  a vector, get the 3D pixel location   
    ##X## ###  itoxyz(24, 6, 6, 1)
    ##X## ###  itoxyz(24, 6, 6, 1)
    
    
    lentop = nx * ny
    side = (nx)
    nrem = i%%lentop


    iz = floor(i/lentop) + 1
    
    iz[nrem==0] = i/lentop	
    nrem[nrem==0] = lentop

    
    iy = floor((nrem - 1)/side) + 1
    ix = nrem - (iy - 1) * nx
    
    ix[ix==0] = nx
    
    return(list(ix = ix, iy = iy, iz = iz))
    
  }

