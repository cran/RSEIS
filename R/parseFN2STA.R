`parseFN2STA` <-
function(fn)
  {
    ###  get the station name and the component name from the file name
    ### function assumes that the station name and the component name
      ###  are the last  items on the finle name seperated by a period
    ## first get the file name from the full path name

    ##   could make this more general by adding options 

    f1 = unlist(strsplit(fn, "/"))
    fn1 = f1[length(f1)]
    f2 = unlist(strsplit(fn1, "\\."))
    sta = f2[length(f2)-1]
    comp = f2[length(f2)]
    return(list(sta=sta, comp=comp) )
  }

