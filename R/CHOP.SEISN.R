`CHOP.SEISN` <-
function(GH, sel=1:4, WIN=NULL)
{

  if(missing(sel)) { sel = 1:length(GH$dt)}


  
  if(is.logical(sel)) { sel = which(sel) }

  NEWH = GH


  if(missing(WIN))
    {
      WIN = NULL
      zloc  = ZOOM.SEISN(GH, sel, WIN=WIN)
    }
  else
    {
  

      zloc = WIN
      
    }


  if(is.list(zloc)==FALSE)
    {
      zloc = list(x=zloc)
    }

  for(i in 1:length(GH$dt))
    {
      ii = i
      tim = GH$dt[ii]*seq(from=0,to=length(GH$JSTR[[ii]])-1)

    ##   print(paste(sep=" ", min(tim), max(tim), zloc$x[1], zloc$x[2]))
      
      tflag = tim>=zloc$x[1]&tim<=zloc$x[2]

      
      amp = GH$JSTR[[ii]][tflag]
      n1 = length(amp)
      NEWH$JSTR[[ii]] = amp
      NEWH$dt[ii] =   GH$dt[ii]
      
      RDATE = recdate(NEWH$info$jd[ii],
        NEWH$info$hr[ii],
        NEWH$info$mi[ii],
        NEWH$info$sec[ii]+NEWH$info$t1[ii]+NEWH$info$msec[ii]/1000+ zloc$x[1]- NEWH$info$off[ii],
        NEWH$info$yr[ii])

      
      GDOM = getmoday(RDATE$jd, RDATE$yr)


      
      NEWH$info$yr[ii] = RDATE$yr
      NEWH$info$jd[ii] =RDATE$jd
      NEWH$info$mo[ii] = GDOM$mo
      NEWH$info$dom[ii] =GDOM$dom
      NEWH$info$hr[ii] =RDATE$hr
      NEWH$info$mi[ii] =RDATE$mi
      NEWH$info$sec[ii] =RDATE$sec

      ## print(paste(sep=" ", ii, NEWH$info$jd[ii], NEWH$info$hr[ii], NEWH$info$mi[ii], NEWH$info$sec[ii]))
      
      NEWH$info$msec[ii] = 0
      NEWH$info$t1[ii] = 0
    
      NEWH$info$off[ii] = 0

      NEWH$info$t2[ii] =  NEWH$info$t1[ii]+n1*GH$dt[ii]
	NEWH$info$n1[ii] = n1
	NEWH$info$n2[ii] = n1
	NEWH$info$n3[ii] = n1
	NEWH$info$n[ii] = n1




    }
  NEWH$ex = NEWH$dt[1]*seq(from=0,to=length(NEWH$JSTR[[1]])-1)

  invisible(NEWH)
  
}

