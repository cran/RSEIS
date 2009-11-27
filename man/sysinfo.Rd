\name{sysinfo}
\Rdversion{1.1}
\alias{sysinfo}
\title{System Information
}
\description{Extract OS system information
}
\usage{
sysinfo()
}
\details{Returns parts of the output of variables .Machine and
  .Platform.

  Endian Problem
  
  these should be used for reading binary data when crossing platforms.
  If binary files are created on a little-endian platform, but are being
  read on a big-endian platform, then one should use "swap".

  SizeOf Problem
  
  Many older machines use 4 bytes for LONG.  Newer 64 bit machines use 8
  bytes for LONG = so this is a big problem.

  
}
\value{A=.Machine, B=.Platform
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{.Machine, .Platform
}
\examples{

sysinfo()


}
\keyword{misc}
