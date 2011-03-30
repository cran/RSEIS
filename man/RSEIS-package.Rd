\name{RSEIS-package}
\alias{RSEIS-package}
\alias{RSEIS}
\docType{package}
\title{
Seismic Analysis and Display
}
\description{
  Multiple interactive codes to analize seismic data, via, spectrum
  analysis, wavelet transforms, particle motion, hodograms.
}
\details{
\tabular{ll}{
Package: \tab RSEIS\cr
Type: \tab Package\cr
Version: \tab 2.5-6\cr
Date: \tab 2010-11-13\cr
License: \tab GPL\cr
}

swig is general all pupose code that uses interaction for seismic analysis.
}
\author{
Jonathan M. Lees<jonathan.lees.edu>
Maintainer:Jonathan M. Lees<jonathan.lees.edu>
}

\note{
  \describe{ 
    \item{Seismic Sections}{
      JGET.seis
      view.seis
      swig
      Mine.seis
      VELOCITY.SEISN  
      DISPLACE.SEISN
      ZOOM.SEISN 
      wlet.drive 
      SENSORsensitivity
      PLOT.MATN
      PLOT.SEISN
      PLOT.TTCURVE
      PLOT.ALLPX
      plotevol
      MTMdisp
      MTMplot
      NEW.getUWSTAS
      NEWPLOT.WPX
      INSTFREQS
      INSTresponse
      GLUE.GET.seis
      GLUEseisMAT
      FILT.SEISN
      FILT.spread
      CHOP.SEISN
      get.corner
      grotseis
    }
    
    \item{Velocity-Travel Time:}{
      Put1Dvel
      Ray.time1D
      setLQUAKE
      selAPX
      Get1Dvel
      Comp1Dvel
      Comp1Dvels
      travel.time1D
    }

    \item{Particle Motion:}{
      hodogram
      PMOT.drive
      complex.hodo
      addpoints.hodo
      idpoints.hodo
      DO.PMOT.ARR
      partmotnet
      prep1wig
      prepSEIS
      EmptySEIS
      GAZI
    }

    \item{Time series:}{
      xcor2  
      wlet.drive 
      wlet.do  
      wiggle.env 
      plotwlet
      STLTcurve
      SPECT.drive
      rsspec.taper
      evolfft
      GETARAIC
      PSTLTcurve
      getphaselag2
      envelope
      hilbert
      LocalUnwrap
      lagplot
      applytaper
      autoreg
      butfilt
      choosfilt
      MTM.drive
    }

    \item{Date-Time Functions:}{
      yeardate YRsecdif  Zdate 
      recdatel
      recdate
      tojul
      getjul
      getmoday
      secdifL
      secdif
      secdifv
      JtimL
      Jtim
      fromjul
    }

    \item{Graphics:}{
      plocator
      ilocator
      meshgrid
      ymarginfo
      zlocator winmark
      vline
      screens
      RESCALE
      pwlet2freqs
      addtix
      circle
      circ
      letter.it
      jpostscript
      JBLACK
      JGRAY
      HOZscale
      gaddtix
      Gcols
      jlegend
      tomo.colors
    }
    \item{Misc:}{
      BKpfile2ypx
      brune.doom
      brune.func
      brune.search
      comp.env
      contwlet
      deconinst
      detail.pick
      distaz
      DUMPLOC
      EmptyPickfile
      ETECTG
      finteg
      fixcompname
      fixcomps
      fixUWstasLL
      fmod
      FRWDft
      getb1b2
      getNcard
      getpfile
      getseisinfo
      getvertsorder
      gpoly
      GreatDist
      gwpix2ypx
      hilow
      hypot
      integ1
      INVRft
      itoxyz
      jadjust.length
      jpolyval
      jstats
      local.file
      logspace
      makefreq
      mirror.matrix
      Mmorlet
      mtapspec
      peaks
      PICK.DOC
      pickit
      plt.MTM0
      PLTpicks
      PPIX
      PreSet.Instr
      ReadSet.Instr
      readUW.OSTAS
      scal2freqs
      SEARCHPIX
      setstas
      setwelch
      shade.col
      SNET.drive
      T12.pix
      Thresh.J
      TOCART
      trapz
      tung.pulse
      unpackAcard
      uwpfile2ypx

    }
  }
}
\keyword{ package }
\seealso{
RPGM, RFOC
}
\examples{
data("GH")
swig(GH)

}

