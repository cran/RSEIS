`xcor2` <-
function(a1, a2, DT, PLOT=FALSE, LAG=100)
{
  if(missing(PLOT)) { PLOT=FALSE }

  n1 = length(a1)
  n2 = length(a2)
  n = max(c(n1,n2))

  ts1 = c(a1-mean(a1), rep(0,n-n1))
  ts2 = c(a2-mean(a2), rep(0,n-n2))
  
  b1 = ts(ts1, deltat=DT)
  b2 = ts(ts2, deltat=DT)
  
  if(PLOT==TRUE)
    {
      opar <- par(no.readonly = TRUE)

      par(mfrow = c(3,1))  

    }
  if(missing(LAG)) { LAG=round(n/4) }

  xc =ccf(b1, b2, lag.max = LAG , type ="correlation",  plot = FALSE )
  mlag = xc$lag[which.max(xc$acf)]
  if(is.na(mlag) || is.null(mlag)) { xc$lag[1]  }
  if(PLOT==TRUE)
    {
      EX = DT*(1:n)

      k = which.max(ts2)
      
      gex = k+mlag/DT
      plot(DT*(1:n), ts1, xlab="time, s", type = 'l')
      abline(v=EX[gex], col=rgb(1,0,0), lty=2)
      
      plot(DT*(1:n), ts2, xlab="time, s", type = 'l')
      
      if(gex>0 & gex<=n)
        {
          segments(EX[k], ts2[k], EX[gex], ts2[k], col=rgb(1,0,0))
          abline(v=EX[gex], col=rgb(1,0,0), lty=2)
        }
      else
        {
          segments(EX[n/2], ts2[k], EX[(n/2)+mlag/DT], ts2[k], col=rgb(1,0,0))
        }
      plot(xc)
      points(xc$lag[which.max(xc$acf)], max(xc$acf), col=2)
      par(opar)
    }

  xc$mlag = mlag
  return(xc)
}

