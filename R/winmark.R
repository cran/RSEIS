`winmark` <-
function(a1, a2, UD=1, col=col)
{
  if(missing(col)) { col=4 }
  if(missing(UD)) { UD=1 }

  if(is.null(a1)==TRUE )
    {
      print("missing a1 in winmark")
      return(0)
    }

  if(is.null(a2)==TRUE )
    {
      print("missing a2 in winmark")
      return(0)
    }

  u = par("usr")

  if(UD==1)
    {
      bot = u[3]+0.8*(u[4]-u[3])
      top = u[3]+0.95*(u[4]-u[3])
      segments(a1, bot, a1, top, col=col)
      segments(a1, top, a2, top, col=col)
      segments(a2, bot, a2, top, col=col)
    }
  else
    {
      
      bot = u[3]+0.05*(u[4]-u[3])
      top = u[3]+0.2*(u[4]-u[3])
      segments(a1, bot, a1, top, col=col)
      segments(a1, bot, a2, bot, col=col)
      segments(a2, bot, a2, top, col=col)
    }
  
  
  
  
}

