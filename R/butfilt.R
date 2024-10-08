`butfilt` <-
function(a, fl=0, fh=0.5, deltat=1, type="BP", proto="BU", npoles=5, chebstop=30.0, trbndw=0.3, RM=FALSE, zp=TRUE )
{
#####  comments from the C code in /home/lees/Progs/Rc/LLNfilt.c
   #####  int iord:      number of poles for filter (< 10; preferably < 5)
   #####     char *type:    "LP" for low pass (SAC default = LP)
   #####                   "HP" for high pass
   #####                   "BP" for band pass
   #####                   "BR" for band reject
   #####    char *proto:   "BU" for Butterworth (SAC default = BU)
   #####                   "BE" for Bessel
   #####                   "C1" for Chebyshev type 1
   #####                   "C2" for Chebyshev type 2
   #####   float a:       Chebyshev stop band attenuation (ignored for others)
   #####                 (SAC uses 30.0 for the default)
   #####    float trbndw:  Chebyshev transition bandwidth (ignored for others)
   #####                  (SAC uses 0.3 for the default)
   #####    float fl:      high pass corner freq. (ignored if type LP)
   #####                 (SAC default = 0.5)
   #####  float fh:      low pass corner freq. (ignored if type HP)
   #####                 (SAC default = 5.0)
   #####  float ts:      time sample interval in secs (e.g.,0.01 = 100 samp/sec)
   #####                (SAC default = 0.01)
   ###    checks to avoid problems
    available.types =  c('LP', 'HP', 'BP', 'BR' )

    
  if(missing(npoles)) { npoles=5 }
  if(missing(proto)) { proto="BU" }
    if(missing(type)) { type="BP" }
  
    m =  match(toupper(type) ,  available.types  )
    if(is.na(m))
    {
        warning("Incorrect type provided: setting to DEFAULT= BP")
        type = "BP"
    }
    else
    {
     type =   toupper(type)
        }
    if(missing(deltat)) { deltat = 1 }

#####  fh can not be higher than nyquist frequency
    if(missing(fh)) { fh = 1/(2*deltat) }
    if(fh>  1/(2*deltat) ) fh = 1/(2*deltat)
    
  if(missing(fl)) { fl = 0.01 }
  if(missing(trbndw)) { trbndw=0.3 }
    if(missing(chebstop)) { chebstop=30.0 }
    if(missing(RM)) { RM = FALSE }
    if(missing(zp)) { zp = TRUE }

  if(RM) { removemean=1 } else { removemean=0 }
  if(zp) { zerophase=1 } else { zerophase=0 }

  if(any(is.na(a))){ warning("ERROR in BUTFILT: NA in data"); return(NULL)      }
  if(any(is.null(a))){ warning("ERROR in BUTFILT: NULL in data"); return(NULL)      }
  if(length(a)<1){ warning("ERROR in BUTFILT: length in data"); return(NULL)      }
  
  
  if(is.null(fl) ){ warning("ERROR in BUTFILT: filter definition"); return(NULL)      }
  if(is.null(fh) ){ warning("ERROR in BUTFILT: filter definition"); return(NULL)      }
  if(is.null(deltat) ){ warning("ERROR in BUTFILT: deltat null"); return(NULL)      }


  
  
.C("CALL_JFILT", PACKAGE = "RSEIS",
as.double(a),
as.integer(length(a)),
as.integer(npoles),
as.character(type) ,
as.character(proto) ,
as.double(chebstop) ,
as.double(trbndw) ,
as.double(fl) ,
as.double(fh) ,
as.double(deltat) , as.integer(removemean),as.integer(zerophase),
output  = double(length(a)))$output
}

