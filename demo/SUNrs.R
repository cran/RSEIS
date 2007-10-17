#######  read in data sunspot  and plot
######  using PICK.GEN

data(sunspots)

###  prepare the time series for input to RSEIS

####  step 1:  This puts it into the format used by Rsac
ES = prep1wig(wig=sunspots, dt=1/12, sta="STA", comp="CMP", units="UNITS"    )

####  this puts it into the format used by RSEIS:
####  step 2:
EH=prepSEIS(ES)


########  pop up the signals using PICK.GEN
###
STDLAB = c("DONE",  "zoom in", "zoom out", "refresh", "restore",
 "XTR", "SPEC", "SGRAM" ,"WLET", "FILT",  "Pinfo")

xx =  PICK.GEN( EH, STDLAB = STDLAB)

