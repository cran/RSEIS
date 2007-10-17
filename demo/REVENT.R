#######  read in data  from Reventador Volcano, Ecuador, and plot
######  using PICK.GEN

data(KH)
STDLAB = c("DONE",  "zoom in", "zoom out", "refresh", "restore",
 "XTR", "SPEC", "SGRAM" ,"WLET", "FILT",  "Pinfo")


xx =  PICK.GEN( KH, sel=which(KH$COMPS == "V"), STDLAB = STDLAB)
