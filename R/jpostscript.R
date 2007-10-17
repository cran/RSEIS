`jpostscript` <-
function(name, P=NULL)
{

  if(missing(name)) { name="JPLOT" }
  if(missing(P)) { P = NULL }



  if(is.null(P))
    {
      P = par('pin')
      P = round(P, digits=2)
      
    }
  
  
  psname = local.file(name, "eps")


  ## P = round(par('pin'))
  
  postscript(file=psname , width=P[1], height=P[2], paper = "special", horizontal=FALSE, onefile=TRUE,print.it=FALSE)

  return(psname)
  
}

