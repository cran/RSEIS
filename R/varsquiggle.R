varsquiggle<-function(GH, sel=c(1,2), WIN=c(0,1)  )
  {
#####  plot a seismic section as in exploration seismology
####  with varian - squiggle display

    ##  source("varsquig.R")

    if(missing(sel)) { sel= 1:length(GH$JSTR) }

     dt1 = GH$dt[sel[1] ]

    if(missing(WIN)) {

      WIN = c(0,  length(GH$JSTR[[  sel[1] ]]   )*dt1    )


      } 
    
    N = length(sel)

    XMAT  = NULL

    x = seq(from=0, length=length(GH$JSTR[[  sel[1] ]]   )   , by=dt1)

    
    j = 1
    for(i in 1:N)
      {
        sig = GH$JSTR[[  sel[i] ]]
        len = length(sig)
        x = seq(from=0, length=len , by=dt1)
      

        
        sig = sig[x>=WIN[1] & x<=WIN[2]]
        
        XMAT = cbind(XMAT, sig  )

      }


    d = dim(XMAT)
    x = seq(from=0, length=d[1], by=dt1)
    
    

    y1 = seq(from=0, by=1, length=d[2])

    fatness = (y1[2]-y1[1])/2
    Ry  = range(y1)
    plot(range(x),range(y1) , type='n', axes=FALSE, xlab="", ylab=""  )

    for(i in 1:length(y1))
      {
        L = list(x=range(x)   , y =c(y1[i]-fatness, y1[i]+fatness  ) )

        varsquig(x, XMAT[,i], L=L , FLIP=FALSE, col="blue", var=TRUE)
      }


  }
