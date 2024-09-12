`ReadSet.Instr` <-
function(file)
{
    #####  modification made to get rid of the upper lower case
    ##  cmd = paste(sep=" ","cat ", file)
    ##    INSTF = system(cmd, intern=TRUE)

    if(length(file)==1 & file.exists(file[1]) )
      {
        ACON = file(description=file, open = "r")
        INSTF = readLines(con = ACON, n = -1, ok = TRUE, warn = TRUE)
        close(ACON)
      }
    else
        {
            #####  here the input is a vector of character strings
            INSTF =file
            if(length(INSTF)<2)
                {
                    warning('ERROR: ReadSet.Instr, no poles and zeros in character string')
                    return(NULL)
                }
        }

    COMMENTS = NA
    First.Char = substr(INSTF, 1, 1)
    W.char = which(First.Char=='#')
    if(length(W.char) >0)
        {
            COMMENTS = INSTF[W.char]
            INSTF = INSTF[-W.char]
            
        }


    i.zeros = 1

      INSTF = tolower(INSTF)
      
      i.zeros = grep('zeros', INSTF)
      
    i.poles = grep('poles', INSTF)

    poles = NULL
    zeros = NULL
    Knorm = NULL
    Sense = NULL
    nz=0
    np=0
   


####  read in the zeros
     
    a = unlist(strsplit(INSTF[i.zeros],split=' '))
    nz = as.numeric(a[2])
    if(nz>0)
      {
        zeros = vector(length=nz, mode="complex")
        for( i in 1:nz)
          {
            a = unlist(strsplit(INSTF[i.zeros+i],split='\\ '))
            a = a[a!=""]
            zeros[i] = complex(real=as.numeric(a[1]), imaginary=as.numeric(a[2]))
          }
      }
  #  ip= 1+nz+1

  
#####   read in the poles
    
    ip= i.poles
    a = unlist(strsplit(INSTF[ip],split=' '))
    np = as.numeric(a[2])
    if(np>0)
      {
        poles = vector(length=np, mode="complex")
        for( i in 1:np)
          {
            a = unlist(strsplit(INSTF[ip+i],split='\\ '))
            a = a[a!=""]
            poles[i] = complex(real=as.numeric(a[1]), imaginary=as.numeric(a[2]))
          }
      }
    ip= np+nz+2+1

   #######  several synonyms for the constant
    i.K =  which( grepl('constant', INSTF) |
                  grepl('norm', INSTF)|
                  grepl('knorm', INSTF) ) 

    ####  take the first instance 
    a = unlist(strsplit(INSTF[i.K[1] ],split=' '))
    a = a[a!='']
    Knorm =  as.numeric(a[2])



#######  several synonyms for the sensitivity
    i.SENSE1 = grepl('sense', INSTF)
    i.SENSE2 = grepl('sensitivity', INSTF)

    i.SENSE =which( i.SENSE1  | i.SENSE2)
    
  
    a = unlist(strsplit(INSTF[ i.SENSE[1] ],split=' '))
    a = a[a!='']
    Sense =  as.numeric(a[2])
    return(list(np=np, poles=poles, nz=nz, zeros=zeros, Knorm=Knorm, Sense=Sense, Comments=COMMENTS))
    
  }

