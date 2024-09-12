
FAKEDATA<-function(amp, OLDdt=0.01, newdt = 0.1, yr = 2000,
                   JD = 5, mi = 0, sec = 0,  Ntraces = 48,
                   seed=200, noise.est=c(1, 100), verbose=FALSE  )
{
########  generate fake data for examples
    if(newdt>OLDdt)
    {
        v =  downsample(amp , dt=OLDdt, newdt=newdt  )
    }
    else
    {
        v = amp
    }


    
    v.len =length(v)
    m.v = mean(v[ noise.est[1]:noise.est[2] ] )
    std.v = stats::sd(v[ noise.est[1]:noise.est[2]]   )

    dt = newdt 
    ex= seq(from=0, length=length(v), by=dt)
   
    ## plot(ex, v, type='l')
  
    FEX = seq(from=0, to=3600-dt, by=dt)
    yc = 1:(length(FEX)-length(v) )
    samples.hour = length(FEX)
    
    GIVE = vector(mode='list', length=Ntraces)
    
    set.seed(seed)
    Ksamp = sample(yc, Ntraces, replace = FALSE  )
    AMPLITUDES = runif(Ntraces, 0.5, 1.5 )
    jd = JD-1
    for(i in 1:Ntraces)
    {
        sample(yc, 1)
        y = stats::rnorm(samples.hour, m=m.v, sd=std.v)
        y.sig = v    ###    *AMPLITUDES[i]
        
#### y[Ksamp[i]:(Ksamp[i]+v.len-1)] = y[Ksamp[i]:(Ksamp[i]+v.len-1)] + y.sig

        y[Ksamp[i]:(Ksamp[i]+v.len-1)] =  y.sig
        fn = paste('FAKE_', formatC(i, width=3, flag=0), '.XXX', sep='')
        thesta = 'SYN'
        thecomp='V'
        N = length(y)
        t1 = 0 
        t2 = dt * (N - 1)
        HEAD = NULL
        IO = list(kind=-1, Iendian = 0)
        BIGLONG = TRUE
        #   if(i<24) { jd = JD; } else { jd = JD+1;  }

        jhr = (i-1) %% 24

        if(jhr == 0) { jd = jd+1  }
        md = getmoday(jd, yr)
        tstart = list(yr = yr, jd = jd, mo = md$mo, dom = md$dom, 
                      hr = jhr, mi = mi, sec = sec, msec = 0, dt = dt, t1 = t1, 
                      t2 = t2, off = 0)
        
        if(verbose) message(paste(i, yr, jd, md$mo, md$dom, jhr,  sep=' ') )

        aunits = "unknown"
        coords = NULL
        
        GIVE[[i]] = list(fn = fn, sta = thesta, comp = thecomp, 
                         dt = dt, DATTIM = tstart, N = N, units = aunits, 
                         coords = coords, amp = y, HEAD = HEAD,
                         IO = list(kind = -1,  Iendian = 0, BIGLONG = BIGLONG))


    }

    return(GIVE)

}

