
#######  read in data from the coso geothermal field and plot
######  using PICK.GEN

data("GH")

#########  the following shows all the data,
####  this might be a little too much!
PICK.GEN(GH)

######### 
#########     select only a few buttons,

STDLAB = c("DONE",  "zoom in", "zoom out", "refresh", "restore",
 "XTR", "SPEC", "SGRAM" ,"WLET", "FILT",  "Pinfo")

PICK.GEN(GH, STDLAB =STDLAB)
