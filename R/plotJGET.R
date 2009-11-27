plotJGET<-function(J, SHOWONLY=FALSE)
  {
    if(missing(SHOWONLY)) SHOWONLY=FALSE
    ##  Program for plotting the output of JGETseis
##    J  = JGET.seis(fn,kind=2,BIGLONG=FALSE,HEADONLY=FALSE,Iendian=3,PLOT=TRUE)

    GH=prepSEIS(J)
    PICK.GEN(GH, SHOWONLY=SHOWONLY)

    invisible(GH)
  }



