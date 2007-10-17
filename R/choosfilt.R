`choosfilt` <-
function(thefilts=thefilts, ncol=5)
  {
    ###  choosfilt()
    
    if(missing(thefilts))
      {
        thefilts =
          list(flo=
               c(0.02, 0.02, 0.02, 0.02, 0.02,   0.02,
                 0.02, 0.02, 0.02,  0.02, 0.02,  0.02,
                 0.02,
                 1/2, 1/50,1/100, 1/100,1,1,
                 0.2, 15, 5, 2,1,
                 100),
               fhi=
               c(1/10, 1/6, 1/5, 1/4, 1/3, 1/2,
                 0.2,  0.5, 1.0,  2.0, 3.0,  4.0,
                 7.0,
                 8, 1/2.0,1/5.0,1/10.0,10,5,
                 7.0, 100, 100, 100,10,
                 100),
             type =
               c("LP","LP", "LP", "LP", "LP", "LP",
                 "LP","LP", "LP", "LP", "LP", "LP",
                 "LP",
                 "BP", "BP","BP","BP","BP","BP",
                 "HP", "HP","HP", "HP","HP",
                 "None"))
      }
    
    if(missing(ncol)) {  ncol=5  }
    


if( length(thefilts$type) != length(thefilts$flo ) &   length(thefilts$type)  != length(thefilts$fhi ) )
  {
    print(paste(sep=' ',"problem with filter definition", length(thefilts$type),length(thefilts$flo ), length(thefilts$fhi )   ))

  }
    
###  print(data.frame(cbind(thefilts$flo, thefilts$fhi,   1/thefilts$flo, 1/thefilts$fhi,  thefilts$type)))
    
###    namcols = c("springgreen2",   "plum2" ,         "cyan3"  ,        "darkgoldenrod2")
###    match(namcols, colors())
   ###  print(Z)
    colpals = matrix(c(0,238,118,238,174,238,0,205,205,238,173,14)/255, nrow=3)

    colrgb = rgb(colpals[1,],colpals[2,], colpals[3,])
    
    N = length( thefilts$flo)

    utyp  = unique(thefilts$type)
    thecols = rep(colrgb[1], N)
    for(j in 1:length(utyp))
      {

        thecols[which(utyp[j]==thefilts$type) ] = colrgb[j]
      }
    get(getOption("device"))()  
   ### X11()
    

    cols = topo.colors(1.5*N)
   
    nrow = round((N/ncol)+.5)
    
    dx = 1/ncol
    dy =  1/nrow
 plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab='', ylab='')
    
    for(i in 1:N)
      {
        B =  itoxyz(i, ncol, nrow, 1)
        x = (B$ix-1)*dx
        y = (B$iy-1)*dy
        rect(x , y , x+dx, y+dy, lty=1, col=thecols[i] )

        if(thefilts$flo[i]<1)
          {
            lolab = paste(sep=' ', 1/thefilts$flo[i], "s")

          }
        else
          {
            lolab = paste(sep=' ', thefilts$flo[i], "Hz")
          }

        if(thefilts$fhi[i]<1)
          {
            hilab = paste(sep=' ', 1/thefilts$fhi[i], "s")

          }
        else
          {
            hilab = paste(sep=' ', thefilts$fhi[i], "Hz")
          }

        if(thefilts$type[i]=="LP")
          {
            lab = paste(sep='\n',thefilts$type[i],  hilab )
          }
        if(thefilts$type[i]=="HP")
          {
            lab = paste(sep='\n',thefilts$type[i],  lolab )
          }

          if(thefilts$type[i]=="BP")
          {
            lab = paste(sep='\n',thefilts$type[i],  lolab,  hilab )
          }
          if(thefilts$type[i]=="None")
          {
            lab = paste(sep='\n',thefilts$type[i] )
          }
       
        
        text(x+dx/2, y+dy/2, lab)
        
      }

    z = locator(type='p')

    if(length(z$x)<1)
      {
       
        dev.off(dev.cur())
        return(NULL)
        
      }


    
    ii = 1+floor(z$x/dx)
    jj = 1+floor(z$y/dy)
    B =  ii+(jj-1)*(ncol)

    i = B[length(B)]

    GIVE = list(type=thefilts$type[i],  fl=thefilts$flo[i],fh=thefilts$fhi[i],ON=FALSE,  proto="BU" )

    
    print(paste(sep=" ", "DATA is FILTERED", paste(collapse=" ", unlist(GIVE))))

    dev.off(dev.cur())
    
    return(GIVE)

    
########## data.frame(cbind( formatC(thefilts$flo, digits =5, width=6, flag=" "),
##########     formatC(thefilts$fhi, digits =5, width=6, flag=" ") ,
##########     formatC(1/thefilts$flo, digits =5, width=6, flag=" "),
##########     formatC(1/thefilts$fhi, digits =5, width=6, flag=" ") ))


  #####   ans=readline("which filter do you want? ")

   #####  ians = match(ans, 1:length(thefilts$flo))
    
   #####  return(list(ON=FALSE, fl=thefilts$flo[ians], fh=thefilts$fhi[ians], type=thefilts$type[ians], proto="BU"))

  }

