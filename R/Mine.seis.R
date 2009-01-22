`Mine.seis` <-
function(at1, at2, DB, grepsta, grepcomp, kind=1)
{
  ##  find all the file that overlap the times times
  #########   at1 and at2 should be single times in julian days
  #######      if they are vectors, the first element will be used

 ##### grepsta, grepcomp should match what is on the file name, not
  ###  what is in the header of the seismic data

  
  
  if(missing(kind)) { kind = 1 }

  if(is.null(DB$t1))
    {
      DB = T12.pix(DB)
    }

  
##if(at1[1]>365)
##  {
##    at1 
##
##  }
  
  w1 = which( at1[1]>=DB$t1 & at1[1]<DB$t2 )
  w2 = which( at2[1]>=DB$t1 & at2[1]<DB$t2 )
  w3 = which( DB$t1>=at1[1] & DB$t1<at2[1] )
  w4 = which( DB$t2>=at1[1] & DB$t2<at2[1] )

    if(length(c(w1, w2, w3, w4) )<1)
      {
        print("No Match in DataBase")
        return()
      } 

  wi = unique(c(w1, w2, w3, w4))

  if(length(wi)<1) { return()} 
  

  fn1 = DB$fn[wi]

  nsta = length(grepsta)

  if(is.null(grepcomp)) grepcomp= "*"
  
  ncomp = length(grepcomp)
  
  gi = vector()
  
  for(i in 1:nsta)
    {
      for(j in 1:ncomp)
        {
          gi = c(gi, grep(paste(sep=".", grepsta[i], grepcomp[j]), fn1))
        }
    }
  
  if(length(gi)<1 ) { return() }
  
  fn2 = fn1[gi]
  
  
  KG4 = JGET.seis(fn2, kind = kind, PLOT = FALSE)

  
  
  RR = GLUE.GET.seis(KG4)
  
  GH=prepSEIS(RR)
  
  ##  the window is seconds from the begining of the traces
  ##   at1 and at2 are in julian days



 eday = EPOCHday(GH$info$yr, jd=GH$info$jd, origyr = DB$origyr)

  
  
  ss1 = secdif(eday$jday, GH$info$hr, GH$info$mi, GH$info$sec-GH$info$off,
    at1,0, 0, 0)
  
  ss2 = secdif(eday$jday, GH$info$hr, GH$info$mi, GH$info$sec-GH$info$off,
    at2,0, 0, 0)
  
  
  win = c(min(ss1), max(ss2))

  

 ##  print(win)
 ##  print("GH")
  ##  for(i in 1:length(GH$JSTR)) { print(paste(i, length(GH$JSTR[[i]]))) }

   HH = CHOP.SEISN(GH, WIN=win)

 ## print("HH")
 ##  for(i in 1:length(HH$JSTR)) { print(paste(i, length(HH$JSTR[[i]]))) }


  return(HH)
  
}

