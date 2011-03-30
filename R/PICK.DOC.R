`PICK.DOC` <-
  function(w)
  {
    if(missing(w)) { w = NA }

    
    ALLLABS = c( "DONE","QUIT","NEXT","PREV","HALF","MARK","DOC","restore","refresh","zoom out","zoom in","Left","Right","SCALE","ZERO","SHOWALL","SHOWSEL","saveFN","FLIP","TR_INFO","Postscript","PNG","AUTOP","AUTOPALL","DETECT","MAP","XTR","SIG","SPEC.old","ASCII","AMPL","TRNAMPL","SPEC","SGRAM","WLET","FILT","UNFILT","BRUNE","DETAIL","PTS","MMARKS","PMOT","STERNET","GTAZI","ENVLP","WINFO","Pinfo","Plines","XCOR","PHLAG","3COMP","Predict1D","SavePF","SavePIX","LQUAKE","PickWin","Ppic","Spic","Apic","Pup","Pdown","Pnil","YPIX","WPIX","RMS","EDIX","NOPIX","REPIX","ADDBUTTS","NA")

    

    
    N = length(ALLLABS)
    DOC = vector("list", N)
    
    names(DOC) = ALLLABS



DOC[[1]] = "Close Window and Finish"
DOC[[2]] = "Quit application"
DOC[[3]] = "Sends a message Back to calling program for next trace"
DOC[[4]] = "Sends a message Back to calling program for previous trace"
DOC[[5]] = "Scroll by half a window length"
DOC[[6]] = "Mark a Specific Trace"
DOC[[7]] = "print Documentation"
DOC[[8]] = "restore the window traces to full screen"
DOC[[9]] = "refresh the screen"
DOC[[10]] = "Zoom out by a percent"
DOC[[11]] = "Zoom in  by a percent"
DOC[[12]] = "Shift Left by a percent"
DOC[[13]] = "Shift Right by a percent"
DOC[[14]] = "SCALE by trace or scale by Window"
DOC[[15]] = "Does nothing (maybe for zero padding?)"
DOC[[16]] = "Show all picks in memory (turns onoff to on)"
DOC[[17]] = "Show selected picks in memory (turns onoff to on)"
DOC[[18]] = "Save in file name created by current date/time"
DOC[[19]] = "Flip polarity of traces"
DOC[[20]] = "trace information"
DOC[[21]] = "create a Postscript file"
DOC[[22]] = "Plot to PNG file (alternative to postscript)"
DOC[[23]] = "Automatic picking on one trace"
DOC[[24]] = "Automatic picking on all traces in window"
DOC[[25]] = "signal detection using automated picking"
DOC[[26]] = "Make a map of stations (needs station info in list)"
DOC[[27]] = "extract a single trace and return at end of session"
DOC[[28]] = "pulse analysis (chugging)"
DOC[[29]] = "OLD SPEC.old"
DOC[[30]] = "Dump data out into file with ascii format"
DOC[[31]] = "Amplitude analysis"
DOC[[32]] = "ternary filtered AMPLITUDE ANALYSIS"
DOC[[33]] = "MTM spectrum in separate window with options"
DOC[[34]] = "spectrogram window"
DOC[[35]] = "wavelet analysis"
DOC[[36]] = "filter a trace with a set of choices"
DOC[[37]] = "Undo filtering"
DOC[[38]] = "do brune model analysis for short period data, save meta-data"
DOC[[39]] = "pick details of first arrivals and save meta data"
DOC[[40]] = "toggle points option, plot each point in time series"
DOC[[41]] = "Unused"
DOC[[42]] = "particle motion hodogram, must have three components in the window"
DOC[[43]] = "plot an equal area steronet with the points of the 3-comp seismogram"
DOC[[44]] = "SVD particle motion in windows (needs 3Comp seis)"
DOC[[45]] = "get envelope of a windowed time series"
DOC[[46]] = "Window Information (dumped to screen)"
DOC[[47]] = "pick information"
DOC[[48]] = "draws segments on traces"
DOC[[49]] = "cross correlation of traces"
DOC[[50]] = "calculate the phase lag between traces based on cross spectrum"
DOC[[51]] = "find all componenets of this station and plot in separate window"
DOC[[52]] = "Predict arrivals with 1D velocity model (model must be in memory)"
DOC[[53]] = "Save Pick File (UW format)"
DOC[[54]] = "Save picks in table (ascii)"
DOC[[55]] = "run Lquake on pickfile in memory"
DOC[[56]] = "Select a pick win"
DOC[[57]] = "P-arrival pick estimate"
DOC[[58]] = "S-arrival Pick estimate"
DOC[[59]] = "Acoustic arrival pick estimate"
DOC[[60]] = "P ick arrival up (vertical)"
DOC[[61]] = "Pick arrival down"
DOC[[62]] = "Pick arrival nil (nither up nor down)"
DOC[[63]] = "Y picking"
DOC[[64]] = "Window picks"
DOC[[65]] = "Root Mean Square of trace between picks"
DOC[[66]] = "edit window picks"
DOC[[67]] = "erase all previous picks"
DOC[[68]] = "restore previously erased picks"
DOC[[69]] = "Add buttons"
DOC[[70]] = "nothing"

   

J = 1:N
    
    if(is.character(w))
      {

        if(identical(w, "all") )
          {
            J = 1:N
          }
        else
          {
            J = match(w, ALLLABS)
          }


      }
    else
      {

        cp =  chooser(ALLLABS, nsel=NA)
        
        ma = match(cp, ALLLABS)
        J = ma
        
      }
    
    if(length(J)<1) { print(DOC) }

    
    else
      {
        for(i in 1:length(J)){   cat(paste(ALLLABS[i], "=", DOC[[J[i]]])); cat("\n")  }
      }
#####    grep 'K\[Nclick\]\=\=match' PICK.R > ho.doc
#####  ho.doc = scan("/home/lees/Progs/R_stuff/ho.doc", what="", sep="\n")
#####   hot.doc = strsplit( ho.doc, split="\\\"")
#####   possiblebutts = unlist(lapply(hot.doc, getmem, 2))
    
#####      kli   = system("grep NOLAB /home/lees/R_PAX/RSEIS/R/swig.R | grep match", intern=TRUE)

#####     cp =  chooser(blibs, nsel=NA)
#####     cp =  chooser(blibs, nsel=0)

  }   


