
#######  read in data from the coso geothermal field and plot
######  using PICK.GEN

data("GH")

######### 
####  
STDLAB = c("DONE",  "zoom in", "zoom out", "refresh", "restore",
 "XTR", "SPEC", "SGRAM" ,"3COMP", "FILT",  "Pinfo")

###sel = which(GH$COMPS=="V")

gsel = getvertsorder(GH$pickfile, GH)


###PICK.GEN(GH, sel=sel,    STDLAB=STDLAB)


upix = uwpfile2ypx(GH$pickfile)


#########     Repeat, this time sort the traces, plot the archive picks with errors
#########      and
#########     select only a few buttons,
######### 

pickgeninfo()

#########  
PICK.GEN(GH, sel=gsel$sel, APIX =upix,    STDLAB =STDLAB, WIN =c(4,13) )
