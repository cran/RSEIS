`writeUW.OSTAScard` <-
function(OSTAS)
  {
    
    v = vector()
    k = 0
    
    for(i in 0:(length(OSTAS)-1) )
      {
        #####print(paste(sep=' ', i, i %% 8))
        if( i %% 8 == 0 )
          {
            k = k + 1
            v[k] = paste(sep=" ", "O", OSTAS[i+1])
          }
        else
          {
            v[k] = paste(sep=" ", v[k], OSTAS[i+1])

          }

        
      }

        
    return(v)

  }

