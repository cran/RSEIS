Plot1Dvel<-function(v, tit=NULL, col=c('blue', 'brown'),   ... )
{
    
    plot(c(v$vp, v$vs), c(-v$zp, -v$zs), type = "n", xlab = "Velocity, km/s", 
         ylab = "Depth, km")
    lines(v$vp, -v$zp, type = "s",col=col[1],  ...)
    lines(v$vs, -v$zs, type = "s",col=col[2], ...)
    if(is.null(tit)){
    title(v$name)
    }else{
        title(tit)
        }
    u = par("usr")
    LEG = legend("bottomleft", c("Vp", "Vs"),
                 lwd = 2, col = col, plot = TRUE)

}
