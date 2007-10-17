`INSTFREQS` <-
function(b,a,w)
  {
    s = complex(real=0, imag=1)*w;
    h = jpolyval(b,s) / jpolyval(a,s)
    return(h)
  }

