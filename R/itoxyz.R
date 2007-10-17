`itoxyz` <-
function(i, nx, ny, nlay)
  {
  ##X## given a number index in  a vector, get the 3D pixel location   
 ##X## ###  itoxyz(24, 6, 6, 1)
 ##X## ###  itoxyz(24, 6, 6, 1)
    
    

    lentop =nx*ny;
    side = (nx);

    nrem = i %% lentop;

    if(nrem == 0)
      {
        nrem = lentop;
        iz = i/lentop;
      }
    else
      {
        iz= floor(i/lentop) + 1 ;
      }

    iy=floor((nrem-1)/side)+1;
    ix=nrem-(iy-1)*nx;
    if(ix==0)
      { ix=nx;}
    return(list(ix=ix,iy=iy,iz=iz));
    

  }

