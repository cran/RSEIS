`doMYBUTTS` <-
function(butt="",clicks=NULL, x=NULL)
  {

    message(paste(sep=' ', "Hi there you dummy", butt))
    message(clicks)
    
    if(identical("MED", butt) )
          {
            message(median(x))
          }
    if(identical("AVE", butt) )
          {
            message(mean(x))
          }
    if(identical("MIN", butt) )
          {
            message(min(x))
          }

    
  }

