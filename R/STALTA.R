STALTA<-function(y, fwlen =  125,  bwlen  = 125)
{
    ####  auto picking
#####  R version of STA-LTA
    ##### or short term/long term ratio
    lx = length(y)
    r = range(y)
    
    rat = rep(0, length(y))
    
    logflg = 0

##########  use the amplitude squared
    
    yB = y^2 / bwlen
    yF = y^2 / fwlen
    
    B1 = sum(yB[1:bwlen] )
    F1 = sum(yF[(1+bwlen):(bwlen+fwlen)])
              
    OFF = bwlen+fwlen
    sq = seq(from=OFF+1, to=lx-OFF-1, by=1)
    
    
    
    for(k in  sq)
    {
        B1 = B1 - yB[k-OFF+1] +  yB[k-fwlen]
        F1 = F1 - yF[k-fwlen] +  yF[k]
        rat[k] = F1 /B1
    }

    return(rat)
    
    }

