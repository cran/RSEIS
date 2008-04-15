

ZGET.sac<-function(infile,  PLOT=FALSE)
{
  ####  read in a SAC file and set up memory
  ###   this routine reads in the full header
  ####  
  if(missing(PLOT))  PLOT=FALSE
  
  n=1
  dt=0.025000
  sec = 0
  thesta="XXXXX"
  thecomp="XXXXX"
  
  n=1
  dt=0.025000
  sec = 0
  thesta="XXXXX"
  thecomp="XXXXX"
  DATIM =  rep(0,length=4)
  SACINFO =  rep(0,length=11)


  

  barfa = .C("CALL_SETSAC",PACKAGE = "RSEIS", infile,
    as.integer(n),
    as.double(dt), 
    as.integer(DATIM),  
    as.double(SACINFO),
    as.double(sec),
    thesta , thecomp
    )
  
  
  N = barfa[[2]]

  x = rep(0,length=N)
  
  mints=rep(0, 40);
  dubs = rep(0,70);
  mchars = as.character(rep("123456789", 22))
  echars= as.character( rep("12345678901234567", 2))


  barfa = .C("CALL_NEWSAC2R",PACKAGE = "RSEIS", infile,
    as.double(x),
    as.double(dubs), 
    as.integer(mints),  
    mchars,
    echars
    )
  x = barfa[[2]]
  dubs=barfa[[3]]
  mints =barfa[[4]]
  mchars =barfa[[5]]
  echars= barfa[[6]]

  ##  as.integer(mints)


  dubnames = c(
    "delta",     "depmin",    "depmax",    "scale",     "odelta",
    "b",         "e",         "o",         "a",         "internal1",
    "t0",        "t1",        "t2",        "t3",       "t4",
    "t5",        "t6",        "t7",        "t8",        "t9",
    "f",         "resp0",     "resp1",     "resp2",     "resp3",
    "resp4",     "resp5",     "resp6",     "resp7",     "resp8",
    "resp9",     "stla",      "stlo",      "stel",      "stdp",
    "evla",      "evlo",      "evel",      "evdp",      "unused1",
    "user0",     "user1",     "user2",     "user3",     "user4",
    "user5",     "user6",     "user7",     "user8",     "user9",
    "dist",      "az",        "baz",       "gcarc",     "internal2",
    "internal3", "depmen",    "cmpaz",     "cmpinc",    "unused2",
    "unused3",   "unused4",   "unused5",   "unused6",   "unused7",
    "unused8",   "unused9",   "unused10",  "unused11",  "unused12")

  mintnames=c(
    "nzyear",    "nzjday",    "nzhour",    "nzmin",     "nzsec",
    "nzmsec",    "internal4", "internal5", "internal6", "npts",
    "internal7", "internal8", "unused13",  "unused14",  "unused15",
    "iftype",    "idep",      "iztype",    "unused16",  "iinst",
    "istreg",    "ievreg",    "ievtyp",    "iqual",     "isynth",
    "unused17",  "unused18",  "unused19",  "unused20",  "unused21",
    "unused22",  "unused23",  "unused24",  "unused25",  "unused26",
    "leven",     "lpspol",    "lovrok",    "lcalda",     "unused27")

charnames = c(
    "kstnm",  
    "khole",  "ko",     "ka",
    "kt0",    "kt1",    "kt2",
    "kt3",    "kt4",    "kt5",
    "kt6",    "kt7",    "kt8",
    "kt9",    "kf",     "kuser0",
    "kuser1", "kuser2", "kcmpnm",
    "knetwk", "kdatrd", "kinst")

  
  return(list(x=x, mints=mints, dubs=dubs, mchars=mchars, echars=echars, dubnames=dubnames, mintnames=mintnames, charnames=charnames))

}
