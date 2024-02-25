plotGH<-function(h)
    {
######  quick time series plot of a seismic trace in RSEIS
        
        plot(stats::ts(h$amp, deltat=h$dt) , ylab='Amplitude', xlab='Time, s')

        stachan = paste(h$sta, h$comp)
        dstmp = RSEIS::dateStamp(h$DATTIM)
        
        mtext(stachan, side = 3, line = 1 )
        
        mtext(dstmp, side = 3, line = 0, at =0, adj=c(0,0)   )

        
    }
