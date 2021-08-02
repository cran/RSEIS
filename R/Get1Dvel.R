`Get1Dvel` <-
function(infile, PLOT=TRUE)
  {
    ###  read in a UW format velocity file
    if(missing(PLOT)) { PLOT=TRUE }

    if(!is.list(infile) & is.character(infile) )
      {   
        
        
        av = scan(file=infile, nlines = 2, what="" , sep="\n", quiet =TRUE)
        
        v =  scan(file=infile, skip=2, list(zp=0, vp=0, ep=0, zs=0, vs=0, es=0), quiet =TRUE)
        v$name = infile
        
        v$descriptor = av
      }
    else
      {
        ###########  velocity file  is already in memory.....
        v = infile
      }
    
    
                                        #
    if(PLOT)
    {
        Plot1Dvel(v, tit =  v$name[1] )
       
      }
   
    return(v)
  }

