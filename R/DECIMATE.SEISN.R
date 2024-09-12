`DECIMATE.SEISN` <-
function(TH, sel=1:length(TH$JSTR), dec=5 , type="LP", proto="BU" , fl=2,  fh=10, RM=FALSE, zp=TRUE  )
{
    #####  it type= FALSE, no filter 
    if(missing(sel)) { sel = 1:length(TH$JSTR) }
    if(missing(fh)) { fh=10 }
    if(missing(dec)) { dec=5 }
    
    for(i in 1:length(sel))
      {
        ii = sel[i]

        xamp = TH$JSTR[[ii]]
        mn = mean(xamp, na.rm=TRUE)
        nacop = is.na(xamp)
        xamp[nacop] = mn
        
        dt = TH$dt[ii]

        if(!is.logical(type) )
        {
#####  type must be 'LP', 'HP' or 'BP', 'BR'
            m =  match(type, c('LP', 'HP', 'BP', 'BR' ) )
            if(is.na(m) )
            {
                stop('WRONG type designation for filter')
                
                }
            ynew = butfilt(xamp, fl=fl, fh=fh, deltat=dt, type=type, proto=proto, RM=RM, zp=zp  )
        }
        else
          {
              ynew =xamp

              }
        
        ynew[nacop] = NA
        TH$JSTR[[ii]] = ynew[seq(from=1, to=length(ynew), by=dec)]
        
        TH$dt[ii] = TH$dt[ii]*dec
        
        TH$info$dt[ii]  =TH$dt[ii]
        TH$info$n[ii] =TH$info$n1[ii] = TH$info$n2[ii] = length(TH$JSTR[[ii]])
        TH$info$t2[ii] = TH$info$dt[ii] * TH$info$n[ii]
      }

    proc = paste(sep=" ", "DECIMATE.SEISN", 'fl=', fl, 'fh=' , fh, 'type=', type, 'proto=', proto) 
   TH$process =  c(TH$process, proc)
    


    
    return(TH)
  }

