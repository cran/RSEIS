`butfilt` <-
function(a, fl, fh, deltat, type, proto)
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

  if(any(is.na(a))){ print("ERROR in BUTFILT: NA in data"); return(NULL)      }
  if(any(is.null(a))){ print("ERROR in BUTFILT: NULL in data"); return(NULL)      }
  if(length(a)<1){ print("ERROR in BUTFILT: length in data"); return(NULL)      }
  
  
  if(is.null(fl) ){ print("ERROR in BUTFILT: filter definition"); return(NULL)      }
  if(is.null(fh) ){ print("ERROR in BUTFILT: filter definition"); return(NULL)      }
  if(is.null(deltat) ){ print("ERROR in BUTFILT: deltat null"); return(NULL)      }
  
  
.C("CALL_JFILT", PACKAGE = "RSEIS",
as.single(a),
as.integer(length(a)),
as.integer(8),
as.character(type) ,
as.character(proto) ,
as.double(30.0) ,
as.double(0.3) ,
as.double(fl) ,
as.double(fh) ,
as.double(deltat) ,
output  = single(length(a)))$output
}

