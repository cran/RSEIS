`getNcard` <-
function(ncard)
{
  h = unlist(strsplit(split=" ", ncard))
  N = list(name=h[2])
  return(N)
}

