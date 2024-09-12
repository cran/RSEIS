pickseis24<-function(w, DB, usta, ucomp, kind=-1,  Iendian=1,
     BIGLONG=FALSE )
{

  
origyr=attr(DB, "origyr")
  
   
    L = length(w$hr)
    if( (L%%2)>0 ){ L = L-1  }
    
   ws = seq(from=1, to=L, by=2)
    
  for(i in 1:length(ws) )
    {
        eday = EPOCHday(w$yr, jd =w$jd, origyr = origyr)
        
      at1 = eday$jday + (w$hr[ ws[i] ] ) /24 
      at2 = eday$jday + (w$hr[ ws[i]+1  ] )/24
      GH = Mine.seis(at1, at2, DB, usta, ucomp, kind=kind)
        b = swig(GH)
        if(b$but=='QUIT') break
    }

}

