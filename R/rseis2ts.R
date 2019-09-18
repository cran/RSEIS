rseis2ts<-function(GH, sel=1, notes='')
    {
### this function converts one trace from a n RSEIS
### list to a ts object in R
###   useful for giving a single trace to someone
        
        if(length(sel)>1) sel = sel[1]

        if(!is.numeric(sel) ) {
            cat('sel must be an index to GH\n')
            return(NULL)
        }
### A is a time series; attribute info is a list
        
        A = ts(GH$JSTR[[sel]], deltat = GH$dt[sel])
        info = as.list( data.frame( GH$info )[sel, ] )
        info$stn = GH$STNS[sel]
        info$comp = GH$COMPS[sel]
        info$notes = notes
        
        attr(A, 'info') <- info

       return(A)
    }


