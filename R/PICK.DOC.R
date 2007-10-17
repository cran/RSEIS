`PICK.DOC` <-
function(w)
  {
    if(missing(w)) { w = "all" }

    ALLLABS = c( "DONE", "MARK" , "restore", "zoom out", "zoom in","Left",
      "Right", "SCALE", "Postscript", "AUTOP", "AUTOPALL", "DETECT", "XTR","SIG","SPEC.old","SPEC","SGRAM",
      "WLET", "FILT", "BRUNE",  "DETAIL", "PTS",
      "MMARKS", "PMOT", "STERNET", "GTAZI", "ENVLP", "Pinfo",  "XCOR" , "PHLAG",
      "YPIX", "WPIX", "EDIX", "NOPIX", "REPIX", "FLIP", "3COMP", "DOC")

    N = length(ALLLABS)
    DOC = vector("list", N)
    
    names(DOC) = ALLLABS

    DOC[[1]] = "DONE: Close Window and Finish"
    DOC[[2]] = "MARK Mark a Specific Trace"
    DOC[[3]] = "restore: restore the window traces to full screen"
    DOC[[4]] = "zoom out: Zoom out by a percent"
    DOC[[5]] = "zoom in: Zoom in  by a percent"
    DOC[[6]] = "Left: Shift Left by a percent"
    DOC[[7]] = "Right: Shift Right by a percent"
    DOC[[8]] = "SCALE: Toggle: SCALE by trace or scale by Window"
    DOC[[9]] = "Postscript: create a Postscript file"
    DOC[[10]] = "AUTOP: Automatic picking on one trace"
    DOC[[11]] = "AUTOPALL: Automatic picking on all traces in window"
    DOC[[12]] = "DETECT: signal detection using automated picking"
    DOC[[13]] = "XTR: extract a single trace and return at end of session"
    DOC[[14]] = "SIG: pulse analysis (chugging)"
    DOC[[15]] = "SPEC.old: "
    DOC[[16]] = "SPEC: MTM spectrum in separate window with options"
    DOC[[17]] = "SGRAM: spectrogram window"
    DOC[[18]] = "WLET: wavelet analysis"
    DOC[[19]] = "FILT: filter a trace with a set of choices"
    DOC[[20]] = "BRUNE: do brune model analysis for short period data, save meta-data"
    DOC[[21]] = "DETAIL: pick details of first arrivals and save meta data"
    DOC[[22]] = "PTS: toggle points option, plot each point in time series"
    DOC[[23]] = "MMARKS: Unused"
    DOC[[24]] = "PMOT: particle motion hodogram, must have three components in the window"
    DOC[[25]] = "STERNET: plot an equal area steronet with the points of the 3-comp seismogram"
    DOC[[26]] = "GTAZI: SVD particle motion in windows (needs 3Comp seis)"
    DOC[[27]] = "ENVLP: get envelope of a windowed time series"
    DOC[[28]] = "Pinfo: pick information"
    DOC[[29]] = "XCOR: cross correlation of traces"
    DOC[[30]] = "PHLAG: calculate the phase lag between traces based on cross spectrum"
    DOC[[31]] = "YPIX: Y picking"
    DOC[[32]] = "WPIX: Window picks"
    DOC[[33]] = "EDIX: edit window picks"
    DOC[[34]] = "NOPIX: erase all previous picks"
    DOC[[35]] = "REPIX: restore previously erased picks"
    DOC[[36]] = "FLIP: Flip polarity of traces"
     DOC[[37]] = "3COMP: find all componenets of this station and plot in separate window"
     DOC[[38]] = "DOC: print Documentation"
     
    J = NULL
    if(is.character(w) & !identical(w, "all") ) { J = match(w, ALLLABS) }
    if(length(J)<1) { print(DOC) }
    else
      {
        for(i in 1:length(J)){   print(DOC[[J[i]]]) }
      }
#####    grep 'K\[Nclick\]\=\=match' PICK.R > ho.doc
#####  ho.doc = scan("/home/lees/Progs/R_stuff/ho.doc", what="", sep="\n")
#####   hot.doc = strsplit( ho.doc, split="\\\"")
#####   possiblebutts = unlist(lapply(hot.doc, getmem, 2))
    
    
  }

