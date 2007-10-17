`writeUW.Fcard` <-
function(F)
  {
     fcard = sprintf("F %3.0f %2.0f %5.2f %3.0f %2.0f %5.2f %3.0f %2.0f %5.2f%7.2f%7.2f",
       F$azim1,  F$plunge1,  F$val1, F$azim2, F$plunge2, F$val2, F$azim3, F$plunge3, F$val3, F$herr, F$verr)
     
 return(fcard)
  }

