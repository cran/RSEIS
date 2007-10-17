`getFcard` <-
function(FCARD)
  {

    F  = list(
      azim1= as.numeric(substr(FCARD, 3, 3+3)),
      plunge1= as.numeric(substr(FCARD, 7, 7+2)),
      val1= as.numeric(substr(FCARD, 10, 10+5)),
      
      azim2= as.numeric(substr(FCARD, 16, 16+3)),
      plunge2= as.numeric(substr(FCARD, 20, 20+2)),
      val2= as.numeric(substr(FCARD, 23, 23+5)),
      
      
      azim3= as.numeric(substr(FCARD, 29, 29+3)),
      plunge3= as.numeric(substr(FCARD, 33, 33+3)),
      val3= as.numeric(substr(FCARD, 36, 36+5)),
      herr= as.numeric(substr(FCARD, 41, 41+7)),
      verr  = as.numeric(substr(FCARD, 48, 48+7)))
    return(F)

  }

