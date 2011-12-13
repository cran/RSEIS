`selWPX` <-
function(APX, ista=NULL , icomp=c("V", "N", "E") )
  {
    ###############   select from WPX
    if(missing(icomp)) { icomp=NULL  }

 
    MPX = data.frame(APX, stringsAsFactors = FALSE)

    #####aname = APX$name
    #####acomp = APX$comp

    if(is.null(icomp))
      {
        atag = APX$name
        itag = ista
        w1 = which(!is.na(match(atag, itag)))
        m = w1
      }
    else
      {
        
        M = meshgrid(1:length(ista), 1:length(icomp) )
        itag =  paste(sep=".",ista[M$x], icomp[M$y])
        atag = paste(sep=".", APX$name, APX$comp)
        m =  which(!is.na(match(atag, itag)))
        
      }
    
    
  
    
    SWP =  MPX[  m ,  ]

    SWP = as.list(SWP)


    
    return(SWP)
  }

