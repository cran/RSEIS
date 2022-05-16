SEIS2list<-function(GH)
{
### convert a seismic section to list format
    
    N <- length(GH$STNS)

    GG <- vector(mode='list')


    INFO <- data.frame( GH$info )
    ### cycle through the signals and pu;; out relevant info, create a list
    for(i in 1:N)
    {
        GG[[i]] <- list(fn=GH$info$fn[i],
                       sta=GH$STNS[i],
                       comp=GH$COMPS[i] ,
                       dt=GH$dt[i],
                       DATTIM=INFO[i, ],
                       N=GH$info$n1[i],
                       units=GH$units[i] ,
                       coords=list(lat=NA, lon=NA) ,
                       amp=GH$JSTR[[i]],
                       HEAD=list(gain=INFO$gain[i],  scalefac=INFO$scalefac[i]),
                       IO=list(kind=-1, Iendian=1,  BIGLONG=TRUE) )
        }
    

    invisible(GG)

    }

#######  if you have two swig structures GH and DH combine them by concatination:
###### yy = SEIS2list(DH); xx=SEIS2list(GH); GG = c(yy, xx); ZH = prepSEIS(GG)





