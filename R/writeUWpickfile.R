`writeUWpickfile` <-
function(A, output="")
  {
    if(missing(output)) output=""

    
    cat(file=output  , writeUW.Acard(A$LOC), sep="\n")
    
    if(!is.null( A$E ))
      {
        if(!all(is.na( A$E )))
          {
            
            if(!is.na( A$E$sterrx ))
              {
                cat(file=output  , writeUW.Ecard(A$E), sep="\n", append=TRUE)
              }
          }
        
        
      }
    
    
    if(!is.null( A$F ))
      {
        
        if(!all(is.na( A$F )))
          {
            
            cat(file=output  , writeUW.Fcard(A$F), sep="\n", append=TRUE)
          }
      }

    if(!is.null( A$STAS ))
      {
        
        if(TRUE)
          {
            
            cat(file=output  , writeUW.DOTcard(A$STAS), sep="\n", append=TRUE)
          }
      }

    
    if(!is.null( A$N ))
      {
        if(!all(is.na( A$N )))
          {
            
            cat(file=output  ,   writeUW.Ncard(A$N) , sep="\n", append=TRUE)
          }
      }

    if(!is.null( A$H ))
      {
        if(!all(is.na( A$H )))
          {
            
            cat(file=output  ,   writeUW.Hcard(A$H) , sep="\n", append=TRUE)
          }
      }


    if(!is.null( A$OSTAS ))
      {
        if(!all(is.na( A$OSTAS )))
          {
            cat(file=output  ,   writeUW.OSTAScard(A$OSTAS) , sep="\n", append=TRUE)
          }
      }

    if(!is.null( A$comments ))
      {
        if(!all(is.na( A$comments )))
          {
            cat(file=output  ,   writeUW.Commentcard(A$comments) , sep="\n", append=TRUE)
          }
      }


    
  }

