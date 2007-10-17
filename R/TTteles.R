`TTteles` <-
  function(EQ, staLLZ, ista=c(1), IASPHOME="" )
{
##########   get the hypocenters from the neic web site - 
#########               these are in pde format
##########               http://neic.usgs.gov/neis/epic/epic_rect.html

##########    staLLZ = list(name=c("9024", "9025", "9026"), 
####     	lat=c(LLZ24$lat, LLZ25$lat, LLZ26$lat), 
#### 	lon=c(LLZ24$lon, LLZ25$lon, LLZ26$lon), z=c(LLZ24$z, LLZ25$z, LLZ26$z) )

####   MUST have these set before running:
####  system("ln -s /home/lees/Progs/TTIMES/iasp91.hed")
####  system("ln -s /home/lees/Progs/TTIMES/iasp91.tbl")

  if(missing(ista)) { ista=1:length(staLLZ$lat)  }
  if(missing(IASPHOME)) { IASPHOME="/home/lees/Progs/TTIMES" }


  if(!file.exists('iasp91.hed'))
    {
      cmd1 = paste(sep="", "ln -s ", IASPHOME, "/iasp91.hed")
      system(cmd1)

    }
  
  if(!file.exists('iasp91.tbl'))
    {
      cmd2 = paste(sep="", "ln -s ", IASPHOME, "/iasp91.tbl")
      system(cmd2)
    }
  
  
  TELES = list()
  nam = vector()

  for(j  in 1:length(ista))
    {

      GD1 = GreatDist( EQ$lon, EQ$lat , staLLZ$lon[ista[j]], staLLZ$lat[ista[j]])


      cmdd = paste(sep=' ', "ttBK ", EQ$z, GD1$ddeg , "> ttBK.tempoutput")

      
      B =  system(cmdd )

      A = scan(file="ttBK.tempoutput", list(num=0, phase="", sec=0, tmin=0, tsec=0, dtdd=0, dtdh=0, dddp=0), skip=1, quiet=TRUE)
      attr(A, 'name') = staLLZ$name[ista[j]]
      attr(A, 'input') = list(delta=GD1$ddeg , depth=EQ$z)
      attr(A, 'LOC') = EQ
      TELES[[j]] = A 
      nam[j] = staLLZ$name[ista[j]]
    }

  names(TELES)<-nam
  
  return(TELES)
}

