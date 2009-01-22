SEISNtime<-function(GH)
{
  rdate = recdatel(GH$info)
  
  yd = yeardate(rdate$yr,rdate$jd, rdate$hr, rdate$mi,rdate$sec      )
  
  w1 = which.min(yd)
  
  return(list( yr=rdate$yr[w1] , jd =rdate$jd[w1], hr =rdate$hr[w1], mi =rdate$mi[w1], sec = rdate$sec[w1], w1=w1 )   )
}

