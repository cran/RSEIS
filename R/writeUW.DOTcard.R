`writeUW.DOTcard` <-
function(STAS)
  {

 
    STAS$pol[is.na(STAS$pol)] = "_"
    STAS$res[is.na(STAS$res)] = 0
    STAS$flg[is.na(STAS$flg)] = 0
    STAS$err[is.na(STAS$err)] = 0
    STAS$phase[is.na(STAS$phase)] = "P"
   

    
    
    tag = paste(sep='', ".", paste(sep=".", STAS$name, STAS$c3))

    pflag = STAS$phase=="P"

    
  
    pdots = paste(sep=" ", tag , rep(" (P",length=length(STAS$name)),     
          STAS$phase ,
          STAS$pol  ,
          STAS$sec ,
          STAS$flg ,
          STAS$err ,
          STAS$res   , rep(")",length=length(STAS$name)))

    wflag = !is.na(STAS$name) &   !is.na(STAS$c3)  & !STAS$name=="" & !STAS$c3==""
     
    alldots = c(pdots[wflag])
    
    
    return(alldots)
  }

