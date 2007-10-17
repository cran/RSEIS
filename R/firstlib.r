.First.lib <- function(lib, pkg)
{
 ##  library.dynam("Rwave", pkg, lib)
  library.dynam("RSEIS", pkg, lib)
  
  cat("RSEIS is loaded\n")
}
