repairWPX<-function(wpx)
  {
###    returns 0 for failure, 1 for success
####  return an empty WPX
    zpx = cleanWPX()
    nam1 = names(zpx)
    nam2 = names(wpx)

    nn = as.vector( unlist( lapply(wpx, "length") ) )

    k = which(nam2=="sec")

    Npix  =nn[k]
    
###  check to make sure all the elements of a basic wpx list are there	
    m1 = match(nam1, nam2)

    ww = which(is.na(m1))
    
    for(i in 1:length(m1))	
      {
        if(!is.na(m1[i]))
          {
            zpx[[i]] = wpx[[m1[i] ]]
          }
        else
          {
            zpx[[i]] = rep(NA, times=Npix)
          }
      }

    #######  fix dates?
    #######   fix names?


    
    ##   WPX = data.frame(WPX, stringsAsFactors = FALSE)
    invisible(zpx)
  }
