sysinfo<-function()
  {


A = .Machine

B = .Platform


message(paste(sep=" ", "Platform=", B$OS.type))
message(paste(sep=" ", "Endian=", B$endian))
message(paste(sep=" ", "Size of LONG=", A$sizeof.long))

invisible(list(A=A, B=B))
  }


