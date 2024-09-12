downsample <-
function(sig, dt=0.001, newdt=0.01, PLOT=FALSE )
{
    x = seq(from=0, length=length(sig), by=dt )
    y = sig
    xout = seq(from=0, to=max(x) , by=newdt )
    k = stats::approx(x, y , xout, method = "linear" )
    
    if(PLOT)
    {
        def.par = par(no.readonly = TRUE) 
        
        par(mfrow=c(2,1)) 
        plot(x, y, type='l')
        plot(k$x, k$y, type='l')
        par(def.par)
    }
    
    return(k$y)
}
