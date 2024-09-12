winseis24<-function(pjj, pch=3, col='red' )
{
  if(missing(col)) col = 'red'
  if(missing(pch))  pch=3 

##   pch=3; col='red'
###########  after plotting plotseis24, interact with plot by clicking
  ###  pjj y=tick marks in the Y direction y axis
###      x=x-axis

  oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))

  par(pjj$mypar)
  
  LL = locator(pch=pch, type="p", col=col, cex=2)

  tix = pjj$y

   tlocs = abs(tix[!is.na(tix)])

  labs2 =round(24*(tlocs  - floor(tlocs)))


  tee = rep(NA, length(LL$y))

  for(i in 1:length(tee))
    {
      w1 = which.min(  abs(LL$y[i]-tix)  )
      tee[i] = labs2[w1]+LL$x[i]/(3600)
    }

  v = list(hr=tee, yr = pjj$yr, jd=pjj$jd)
  
  return(v) 
}


