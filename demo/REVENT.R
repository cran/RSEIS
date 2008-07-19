#######  read in data  from Reventador Volcano, Ecuador, and plot
######  using PICK.GEN

data(KH)
STDLAB = c("DONE",  "zoom in", "zoom out", "refresh", "restore",
 "XTR", "SPEC", "SGRAM" ,"WLET", "FILT",  "Pinfo")

######## PICK.GEN is a generic Picking program
######## Use left Mouse click to select traces and windows.
######## Use the buttons to operate on the selected traces/windows
########  To zoom, click twice with the left mouse on the traces, then right mouse zooms
########  Right Mouse with no left mouse clicks is equivalent to "Done"

xx =  PICK.GEN( KH, sel=which(KH$COMPS == "V"), STDLAB = STDLAB)
