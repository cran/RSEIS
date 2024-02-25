`trapz` <-
function(y, dt, rm.mean=TRUE )
  {
    # integrate a signal to get displacement plot
    #  using trapezoidal rule
    #  remove mean
      if(rm.mean)
          {
              z = y - mean(y, na.rm =TRUE )
          }
      else
          {
              z = y
          }
     n = length(z)

     if(any(is.na(z)))
       {
         warning("error in trapz, NA's exist")
         return(NULL) 

       }
     
     h = cumsum(  dt * 0.5*(z[1:(n-1)]+z[2:n]))
     
     h = c(0,h)
     return(h) 

  }

