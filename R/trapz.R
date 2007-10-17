`trapz` <-
function(y, dt)
  {
    # integrate a signal to get displacement plot
    #  using trapezoidal rule
    #  remove mean
     z = y - mean(y)
     n = length(z)
     
     h = cumsum(  dt * 0.5*(z[1:(n-1)]+z[2:n]))
     
     h = c(0,h)
     return(h) 

  }

