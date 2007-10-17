require(RSEIS)


data(KH)


##### informaiton on the data structure:
names(KH)

#####  seismic wiggles are in JSTR


dt = KH$dt[1]

y =  KH$JSTR[[1]]

x =  seq(from=0, by=dt, length=length(y))

##########  plot the data:
plot(x,y, type='l', xlab='s', ylab='amp')

readline("To Continue Hit Enter Key\n")

y = y[x>=1000 & x<=2200]
x = x[x>=1000 & x<=2200]
plot(x,y, type='l', xlab='s', ylab='amp')


readline("To Continue Hit Enter Key\n")

###############
len  = length(y)
len2 = 2*next2(len)

Mspec =   mtapspec(y,dt, klen=len2,  MTP=list(kind=1,nwin=5, npi=3,inorm=0)  )

f1 = 0.01
f2 = 10
f = Mspec$freq
    amp = Mspec$spec[1:length(f)]
    
    flag = f >= f1 & f <= f2
  displ = amp
###############  plot data
plot((f[flag]), (displ[flag]), type = "l",
            log = "", axes = TRUE, xlab = "Hz", ylab = "Spec")


readline("To Continue Hit Enter Key\n")

###############  plot data with LOG=LOG
plot((f[flag]), (displ[flag]), type = "l",
            log = "xy", axes = TRUE, xlab = "Hz", ylab = "Spec")


readline("To Continue Hit Enter Key\n")

###############  plot data with DB scale

db1  = 20*log10(displ[flag]/max(displ[flag]) ) 
plot((f[flag]),  db1   , type = "l",
            log = "", axes = TRUE, xlab = "Hz", ylab = "Db")


readline("To Continue Hit Enter Key\n")

############  compare different kinds of power spectra

   data(OH)
    
y=OH$JSTR[[1]]
dt = OH$dt
    
fn = 1/(2*dt)    
t = seq(0, by=3000, length=length(y))
plot(t, y, type='l')

y = y-mean(y)
    
z = applytaper(y , p=.05)

plot(t,z, type='l')


    Z = fft(z)
    Zyy = Z * Conj(Z)
    n = length(Zyy)/2
    Syy = Mod(Zyy[1:n])/length(z)

Syy = 20*log10(Syy/max(Syy))
 fyy = (0:(length(Syy)-1))*fn/length(Syy)


Mspec =   mtapspec(z,dt, klen=len2,  MTP=list(kind=1,nwin=5, npi=3,inorm=0)  )

    f1 = 0.01
f2 = 10
f = Mspec$freq
    amp = Mspec$spec[1:length(f)]
    
    flag = f >= f1 & f <= f2
  
###############  plot data

 dbamp = 20*log10(amp/max(amp))


prange = range(c( dbamp[flag], Syy[fyy>= f1& fyy<= f2] ) )
frange = range( f[flag] )


plot(frange, prange, type = "n",
            log = "", axes = TRUE, xlab = "Hz", ylab = "Db")

lines(f[flag],dbamp[flag] , col='black')

lines(fyy, Syy, col='red')

title(main="Delta-O18 data with simple periodogram vs MTM method")

readline("To Continue Hit Enter Key\n")

  ###



####    use welch's method to determine power spectrum
  
    rwelch =    setwelch(y, win=min(80,floor(length(y)/10)),
      inc=min(24, floor(length(y)/30)), coef=256, wintaper=0.2 )
    
    KK = apply(rwelch$values, 2, FUN="mean")
    
    wf=seq(from=0, to=0.5, length=256)*1/dt
    DbKK = 20*log10(KK/max(KK))
    

    lines(wf, DbKK, col='blue')


readline("To Continue Hit Enter Key\n")

######################################################
######  Interactive display of spectrum
######################################################

a = list(y=y, dt=dt)

######################################################
#########   click in figure for interaction:
######### when finished click "Done"
######################################################

drive.MTM(a, f1=.01, f2=10, len2=1024, COL=2, PLOT=TRUE, PADDLAB=NULL, GUI=TRUE)


readline("To Continue Hit Enter Key\n")


#####  repeat the above analysis with a different time series:

A = as.vector(sunspots)


dt = 1/12

X =  seq(from=0, by=dt, length=length(A))
plot(X,A, type='l', xlab='Yr', ylab='amp', main="Sunspots")

readline("To Continue Hit Enter Key\n")


#######  remove mean
A = A-mean(A)

len  = length(A)
len2 = 2*next2(len)


###  get MTM spectrum
Mspec =   mtapspec(A,dt, klen=len2,  MTP=list(kind=1,nwin=5, npi=3,inorm=0)  )

f1 = 0.01
f2 = 3
f = Mspec$freq
    amp = Mspec$spec[1:length(f)]
    
    flag = f >= f1 & f <= f2
  displ = amp
db1  = 20*log10(displ[flag]/max(displ[flag]) ) 
plot((f[flag]),  db1   , type = "l",
            log = "", axes = TRUE, xlab = "1/yr", ylab = "Db", main="Sunspot Spectrum")

abline(v=1/11, col='blue', lty=2)

mtext("11 year", side=3, at=1/11)

readline("To Continue Hit Enter Key\n")

print("End of SPECT demo")
#####################################
#####################################
#####################################
