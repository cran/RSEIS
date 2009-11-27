`GETARAIC` <-
function(z4, DT=0.008, Mar=8, O1=2, O2=0.2, WW= 2, T1=1 , PLOT=FALSE   )
  {
    if(missing(DT)) {  DT=0.008 }
    if(missing(Mar)) {  Mar = 8 }
    if(missing(O1)) { O1=2; }
    if(missing(O2)) { O2=0.2; }
    if(missing(WW)) { WW= 2 }
    if(missing(PLOT)) { PLOT=FALSE }
    

    
    Nz4 = length(z4)
    if(missing(T1)) { T1 = floor(Nz4/2) }
  
    aout = rep(0, Nz4)
      
             
              ary = .C("CALL_ARAIC",  PACKAGE = "RSEIS",
                as.double(z4), as.integer(Nz4),as.double(DT), as.integer(Mar),
                as.integer(T1), as.double(O1), as.double(O2), as.double(WW), as.double(aout)) 

              kaic = ary[[9]]
              kaic[kaic==0]=NA
              Taic =TFIN=  which.min(kaic)


    if(PLOT==TRUE)
      {
        par(mfrow=c(2,1))
        plot.ts(z4)
        if(all(!is.na(kaic)))
          {
            xkaic = 1:length(kaic)
            plot(xkaic,kaic, type='l')
            lm1 = lm(kaic ~ cbind(xkaic, xkaic^2, xkaic^3, xkaic^4))
            lines(xkaic[!is.na(kaic)], lm1$fitted.values, col=2)
            vline(Taic, COL=rgb(.4,.8,1) )
          }
        else
          {
            print("ERROR (GETARAIC) - had some NA in kaic.")
          }
        
      }
    
    return(Taic)
  }

