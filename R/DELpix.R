################################### source("~/Progs/R_stuff/DELpix.R")

DELpix<-function(nh, g)
{
#####  BUTTONDOC:JustF:'Show only Infrasound'

    kix = legitpix(g$sel, g$zloc, g$zenclick)
    ypick =  kix$ypick
    ppick = kix$ppick

##### half second tolerance
    TOLERANCE = 0.5

    zpix = data.frame( g$WPX )
   # message('#######################')
  #  message(zpix )

    
    
    if(length(ypick)>0)
    {

        TRICK = vector(mode='numeric', length=length(ypick) )
        
        ipick = g$sel[ypick]
        
        for(iz in 1:length(ypick))
        {
####  g$NPX = g$NPX+1
####  Nn = names(g$WPX)
####  g$WPX =rbind(g$WPX, rep(NA, length(Nn)))
            
            i1 = ipick[iz]
            i2 = ypick[iz]
            
            asec = nh$info$sec[i1] + nh$info$msec[i1]/1000 + nh$info$t1[i1] -
                nh$info$off[i1] + ppick[iz]
            pic1 = recdate(nh$info$jd[i1], nh$info$hr[i1], nh$info$mi[i1],
                           asec, yr=nh$info$yr[i1])
            

            ista = nh$STNS[i1]
            icomp = nh$COMPS[i1]
            
          #  message(paste(ista, icomp, i1, i2  ) )

            stacomp = which(zpix$name == ista  & zpix$comp == icomp )

          #  message(paste('stacomp=  ', stacomp ) , sep='\n')

            Tdif = secdifL(pic1, zpix[stacomp, ])

          #  message(paste('Tdif=  ', Tdif ) , sep='\n')
            
            Kstract = abs(Tdif) < TOLERANCE

            
          #  message(Tdif, sep='\n')

            
            
            if( any(Kstract)  )
            {
                Kw = which(Kstract)
                Ireject = stacomp[Kw[1]]
                message(paste(ista, icomp, i1, i2,Kw, Ireject ) )
                TRICK[iz] = Ireject
                
            }
            else
            {
                warning('no picks near this cursor location\n')
                
            }
            

            

        }

    }

    TRICK  = TRICK[TRICK>0]
    

    if(length(TRICK) > 0 ) { zpix =   deleteWPX(zpix, TRICK) }
  #  message('#######################  POST DEL')
####  message(data.frame(zpix) )
####      DF = data.frame(zpix)
####      Atemp = apply(DF , 1, 'paste', collapse=' ')			
####      message(paste(collapse=' ', names(DF) ))			
####      message(paste(collapse='\n', Atemp))
  #  message('#######################')
    g$WPX = zpix
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
}

