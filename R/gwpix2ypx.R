`gwpix2ypx` <-
function(wpix, LOC, sta=sta, comp=comp, PH=c(1,2,3,5, 9,13, 16, 41, 44, 25))
  {
     ##   em = c(1,2,3,5, 9,13, 16, 41, 44, 25)
    ##  em = 1:length(aphase$phs)

    ## em = match(c("P", "S"), aphase$phs)
    
    uphase =  c("P","pP","sP","PcP","PP","ScP","PcS","PKiKP",
      "S","SPn","pPKiKP","sPKiKP","sS","ScS","SKiKP","SS","PKKPdf","SKKPdf","PKKSdf","SKKSdf","P'P'df",
      "P'P'bc","P'P'ab","S'S'df","pS","PnS","SKSac","SKKSac","pSKSac","sSKSac","PKKPbc","S'S'ac",
      "SKKPbc","PKKSbc","SnSn","SP","PS","PnPn","SPg","sSn","Pn","pPn","sPn","Sn","PgS","Pdiff",
      "pPdiff","sPdiff","Sdiff","pSdiff","sSdiff","SKSdf","pSKSdf","sSKSdf","PKKPab","PKPdf",
      "pPKPdf","sPKPdf","SKPdf","PKSdf")

    AA = c("P",   1,
      "pP",  1,
      "sP",   1,
      "PcP",   1,
      "PP ",  1,
      "ScP",  2,
      "PcS",   1,
      "PKiKP", 3,
      "S",  2,
      "SPn",  2,
      "pPKiKP", 3,
      "sPKiKP",  5,
      "sS",  2,
      "ScS",  2,
      "SKiKP",  2,
      "SS",  2,
      "PKKPdf", 3,
      "SKKPdf",  5,
      "PKKSdf",  5,
      "SKKSdf",  5,
      "P'P'df", 3,
      "P'P'bc", 3,
      "P'P'ab", 3,
      "S'S'df", 5,
      "pS",  6,
      "PnS", 4,
      "SKSac",  5,
      "SKKSac",  5,
      "pSKSac",  5,
      "sSKSac",  5,
      "PKKPbc",  3,
      "S'S'ac", 5,
      "SKKPbc", 5,
      "PKKSbc",  5,
      "SnSn",   7,
      "SP",   8,
      "PS",  8,
      "PnPn", 4,
      "SPg", 6,
      "sSn", 6,
      "Pn", 4,
      "pPn", 4,
      "sPn", 6,
      "Sn", 6,
      "PgS", 4,
      "Pdiff", 3,
      "pPdiff", 3,
      "sPdiff",  8,
      "Sdiff",  8,
      "pSdiff",   8,
      "sSdiff",   8,
      "SKSdf",  5,
      "pSKSdf", 5,
      "sSKSdf", 5,
      "PKKPab", 3,
      "PKPdf", 3,
      "pPKPdf", 3,
      "sPKPdf", 5,
      "SKPdf", 5,
      "PKSdf", 5)


    aphase = list(phs=AA[seq(1, to=length(AA), by=2)], col=as.numeric(AA[seq(2, to=length(AA), by=2)]))

   
    if(missing(PH)) { em = 1:length(aphase$phs) } else {  em=match(PH, aphase$phs) } 
    cphase=  list(phs=aphase$phs[em] , col=aphase$col[em])
    
###   cphase = scan(file='', list(phs='', col=0))

    p1=matrix(c(
      1.000 ,  0.750,   0.500  ,
      1.000,   0.500,   0.000 , 
      1.000,   1.000,   0.600  ,
      1.000 ,  1.000,   0.200  ,
      0.700 ,  1.000,   0.550,
      0.200 ,  1.000,   0.000 ,
      0.650,   0.930,   1.000 ,
      0.100,   0.700 ,  1.000 ,
      0.800 ,  0.750,   1.000 ,
      0.400 ,  0.300,   1.000 ,
      1.000 ,  0.600,   0.750 ,
      0.900 ,  0.100,   0.200), byrow=TRUE, ncol=3
      )
    pal1 = rev(rgb(p1[,1], p1[,2], p1[,3]))

    YPX = list()
    k = 1


    STAS=list()
    
    STAS$tag = wpix$tag
    N = length(wpix$tag)
 
           STAS$name=wpix$name
           STAS$comp=wpix$comp
           STAS$c3=wpix$c3
           STAS$phase=wpix$phase
           STAS$sec=secdifL(LOC,  wpix)

    
           STAS$err=rep(0, N)
           STAS$pol=rep('', N)
           STAS$flg=rep(0, N)
           STAS$res=rep(0, N)
           STAS$lat=rep(NA, N)
           STAS$lon=rep(NA, N)
           STAS$z=rep(NA, N)
           STAS$pdel=rep(0, N)
           STAS$sdel=rep(0, N)
     

  YPX=STAS
  n = length(STAS$name)
  YPX$yr = rep(LOC$yr, length=n)
  YPX$mo = rep(LOC$mo, length=n)
  YPX$dom = rep(LOC$dom, length=n)
  YPX$jd = rep(LOC$jd, length=n)
  YPX$hr = rep(LOC$hr, length=n)
  YPX$mi = rep(LOC$mi, length=n)
  YPX$sec = STAS$sec

  pcol=rep("springgreen4", length(YPX$sec))
  phas = YPX$phase
  pcol[phas=="P"] = "violetred2"
  pcol[phas=="S"] = "deepskyblue4"

    wco = match( as.vector(wpix$phase), cphase$phs)

     pcol=pal1[cphase$col[wco]]
    
  YPX$col = pcol
  YPX$onoff = rep(0, length(YPX$sec))


    
   
    return(YPX)
  }

