`T12.pix` <-
function(A)
  {
    if(is.null(A$dur)) A$dur=A$res
    A$t1 = A$jd + A$hr/24 + A$mi/(1440) + A$sec/(86400) 
    A$t2 = A$t1 + A$dur/86400
    invisible(A)
  }

