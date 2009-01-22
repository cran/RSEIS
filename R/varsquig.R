varsquig<-function(x,y, L=locator(2) , FLIP=FALSE, col="blue", var=0)
{
  if(missing(FLIP) ) { FLIP=FALSE  }
  if(missing(L) ) { L=locator(2)  }
  if(missing(col) ) { col="red"  }
  if(missing(var) ) {  var=TRUE }
  ##  plot(x, y , type='n')

  if(is.numeric(var))
    {
      zee = var*(max(y, na.rm=TRUE)-mean(y, na.rm=TRUE))
    }
  else
    {
      zee = 0
    }

  if(FLIP==TRUE)
    {

      rx = RESCALE(y, min(L$x), max(L$x), min(y), max(y))
      ry = RESCALE(x, max(L$y), min(L$y), min(x), max(x))
      zer =RESCALE(zee, min(L$x), max(L$x), min(y), max(y))

      yup = rx>zer
      g = rx
      g[!yup] = zer 
      g[1] = zer
      g[length(rx)] = zer
      if(var) polygon(g, ry, col=col, border=NA)

    }
  else
    {
      rx = RESCALE(x, L$x[1], L$x[2], min(x), max(x))
      ry = RESCALE(y, L$y[1], L$y[2], min(y), max(y))
      zer =RESCALE(zee, L$y[1], L$y[2], min(y), max(y))

      yup = ry>zer
      g = ry
      g[!yup] = zer 
      g[1] = zer
      g[length(ry)] = zer
      if(var)  polygon(rx, g , col=col, border=NA, xpd=TRUE)

    }


  lines(rx, ry, col=col, xpd=TRUE)

### lines(g, ry, col=col)


}

