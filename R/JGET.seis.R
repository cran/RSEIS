`JGET.seis` <-
function(fnames, kind=1, Iendian=1, BIGLONG=FALSE, HEADONLY=FALSE , PLOT=-1, RAW=FALSE)
{
  ###  get a bunch of AH files from a directory and store in structure
####  kind 1=segy, 2=sac, 3=AH, kind=0->R format,   kind= (-1) readRDS


    
  
  if(missing(PLOT)) { PLOT=-1 }
  if(missing(kind)) { kind=1 }
  if(missing(HEADONLY)) {HEADONLY=FALSE }
  if(missing(Iendian)) { Iendian=1 }
  if(missing(BIGLONG)) { BIGLONG=FALSE}
  if(missing(RAW)) { RAW=FALSE }

    Akind = c('RDS', 'RDATA', 'SEGY', 'SAC', 'AH' )
    Ikind = c(-1, 0, 1, 2, 3)

    
    if(is.character(kind) )
    {
        kind = toupper(kind)
        w.kind = which(kind==Akind)
        kind  = Ikind[w.kind]

        }


    
  tmpGIVE = as.list(1:length(fnames))

  ii = 1

  DATIM =  rep(0,length=4)
  n=1
  dt=0.025000
  sec = 0
  thesta="XXXXX"
  thecomp="XXXXX"


  for(i in 1:length(fnames))
    {

      fn = fnames[i]
      infile = fn
    ####   message(fn);
      ###  if this file does not exist, exit!
      if(file.exists(infile)==FALSE)
        {
         warning(paste(sep=' ', "file does not exist", fn) ); 
          next;
        }
      else
        {
          ###  message(paste(sep=' ', "file exists", fn) );

        }
########
########  there is a different way to store a single R object readRDS and saveRDS....
      
##################################  if the file is already in R format
      ###############  just load it it and skip to next file
      ##  this code assumes that the list is called DAT

      if(kind== -1)
        {
          DAT = readRDS(fn)
          if(is.null(names(DAT)) & length(DAT)>=1)
            {
              DAT = DAT[[1]]

            }
          
          if(HEADONLY) DAT$amp = NULL
          DAT$oldname = DAT$fn
          DAT$fn = fn
          tmpGIVE[[i]] = DAT

          
          next
        }



      
      if(kind==0)
        {
          DAT = list()
          GED  = load(fn)
          assign("DAT", get(GED))
          if(is.null(names(DAT)) & length(DAT)>=1)
            {
              DAT = DAT[[1]]

            }
          
          if(HEADONLY) DAT$amp = NULL
          DAT$oldname = DAT$fn
          DAT$fn = fn
          tmpGIVE[[i]] = DAT

          
          next
        }

      if(kind==1)
        {
        #####   message(paste("RAW=", RAW))
          
          DAT  = JSEGY.seis(fn, Iendian=Iendian, HEADONLY=HEADONLY , BIGLONG=BIGLONG, PLOT=PLOT, RAW=RAW)
          tmpGIVE[[i]] = DAT[[1]]
          next
        }

      if(kind==2)
        {
          DAT  = JSAC.seis(fn, Iendian=Iendian, HEADONLY=HEADONLY , BIGLONG=BIGLONG, PLOT=PLOT, RAW=RAW)
          tmpGIVE[[i]] = DAT[[1]]
          next
        }

      if(kind==3)
        {
          warning("AH format currently not available")
          tmpGIVE[[i]] = NA
          next
        }

      #############  if no kind matches, better return NA
      tmpGIVE[[i]] = NA

      
    }
  invisible(tmpGIVE)
}

