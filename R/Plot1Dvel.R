Plot1Dvel<-function(v, tit=NULL )
{
    plot(c(v$vp, v$vs), c(-v$zp, -v$zs), type = "n", xlab = "Velocity, km/s", 
         ylab = "Depth, km")
    lines(v$vp, -v$zp, type = "s", col = 4)
    lines(v$vs, -v$zs, type = "s", col = 3)
    if(is.null(tit)){
    title(v$name)
    }else{
        title(tit)
        }
    u = par("usr")
    LEG = legend("bottomleft", c("Vp", "Vs"), lwd = 2, col = c(4, 
                                                               3), plot = TRUE)

}
