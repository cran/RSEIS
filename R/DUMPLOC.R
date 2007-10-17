`DUMPLOC` <-
function(zloc)
  {
    nam = deparse(substitute(zloc))
    cat(file="",paste(sep="", nam,"=list()"), fill=TRUE)
    cat(file="",paste(sep="", nam, "$x=c(", paste(zloc$x, collapse=","), ")") , fill=TRUE)
    cat(file="",paste(sep="", nam,"$y=c(", paste(zloc$y, collapse=","), ")") , fill=TRUE)

  }

