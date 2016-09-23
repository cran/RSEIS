`fixNA` <- function(y)
    {
        ny = length(y)
        if(ny<2) return(y)
        lala = is.na(y)
        w = which(lala)
        if(length(w)==0)
            {
                return(y)
            }
        
        LAW = vector(mode='numeric', length=length(y) )
        LAW[lala] = 1
       
        RLAW = rle(lala)
        
        wlaw = which(RLAW$values)

        LENS = RLAW$lengths[wlaw]

        
        CLEN  = cumsum(c(1, RLAW$lengths) )
        CLEN  = CLEN[1:(length(CLEN)-1)]
        cbind(RLAW$values,RLAW$lengths, CLEN)

        wget = which(RLAW$values==1)
      #######  wget
    
    #######   cbind(CLEN[wget],  wget,  RLAW$values[wget], 
    #######    RLAW$lengths[wget] )

#######  CLEN[wget]
        

       #######  cbind(CLEN[wget],  wget,  RLAW$values[wget], 
       #######  RLAW$lengths[wget] , start, end, meanys )

        for(i in 1:length(wget))
            {
                start = CLEN[wget[i]] 
                end = CLEN[wget[i]] + RLAW$lengths[wget[i]] - 1
              #######  print(c(start, end))
                if(start<=1) {
                    start =1
                    meanys = y[end+1]
                    y[start:end] = meanys
                    next
                    
                }
                if(end>=ny) {
                    end=ny
                    meanys = y[start-1]
                    y[start:end] = meanys
                    next
                }
                meanys = mean(c(y[start-1], y[end+1] ))
               y[start:end] = meanys
            }

    return(y)
}
