
####  make a seismic cross section

DISTxsec<-function(GH, dist, TIM.WIN=c(0, 3600) , sel,trace.width = 10 ,
                     col='black' ,
                     text.col='blue',
                     text.font = 2,
                     text.size = 0.8,
                     add=FALSE, plot=TRUE )
{
#######     GH is a normal RSEIS list used in swig

### for each trace in GH, a distance is provided, vector dist
#### the dist can be disance from a point, or distance projected along a line

###  a 2-vector of time is provided to window a time-region of interest

##### sel is a section of traces from GH
### trace.width = 10 is the thickness of each trace in the window

### if add = TRUE, do not start a new plot, but just add traces in current plot.

    expandbound<-function (g, pct = 0.1) 
    {
        r = range(g)
        dg = pct * diff(r)
        return(c(r[1] - dg, r[2] + dg))
    }
    
    dist.1 = dist[sel]

    Rdist = expandbound(range(dist.1), pct = 0.05)
    
    if(add == FALSE)
    {
        plot( Rdist, TIM.WIN, type='n', xlab='Distance, km', ylab='Time, s' )
        abline(v=dist.1 , col=grey(0.9) )
    }
    
    if(plot==FALSE) { return(Rdist) }

    Ntraces = length(sel)

    kol = col
    if(length(kol)==1) {  kol = rep(col, length=Ntraces )  }
    
    for(j in 1:Ntraces )
    {
        i = sel[j]
        dt = GH$dt[i]
        
        wig =  GH$JSTR[[i]]
        
        cut.i =   TIM.WIN/dt
        
        
        wig.cut = wig[cut.i[1]:cut.i[2]]
        TEE = seq(from=TIM.WIN[1], by=dt, length=length(wig.cut) )
        
        dee = dist[i]
        
        h = RESCALE(wig.cut,
                          dee-trace.width,
                          dee+trace.width,
                          min(wig.cut),
                          max(wig.cut) )
        
        lines(h, TEE, col=kol[j]  )
        
    
    
    }

    labs = GH$STNS[sel]
    Nsta = length(labs)
    lab.top = seq(from=1, to=Nsta, by=2)
     lab.bot = seq(from=2, to=Nsta, by=2)
   
    text(dist.1[lab.top] , rep(TIM.WIN[2],length=length(lab.top) ),
         labels=labs[lab.top ] , cex=text.size, srt=60, xpd=TRUE,
         adj=c(0, 0.5), col=text.col , font=text.font )
    
      text(dist.1[lab.bot] , rep(TIM.WIN[1],length=length(lab.bot) ),
           labels=labs[lab.bot ] , cex=text.size, srt=60, xpd=TRUE,
           adj=c(1, 0.5) , col=text.col , font=text.font  )
    


    return(Rdist)
    
}
