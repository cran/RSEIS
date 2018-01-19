Spectrum<-function(x, dt, one_sided = TRUE, type = 1, method = 1){
  ## Wrapper for fft and RSEIS::mtapspec. Returns spectrum with correct frequency axis and scaling to physical units (obeying Parseval's relation).
  ## type: 1 = power, 2 = energy, 3 = amplitude, 4 = phase
  ## method: 1 = fft, 2 = multitaper

  N = length(x)
  nyq = 0.5/dt
  
  ## first, calculate spectrum according to method, and scale to power spectrum
  if(method == 1){ # fft
    df = 1/(N*dt)
    f = (1:N - 1) * df
    ## note: for s = fft(x) and df = 1/(length(x)*dt), sum(x^2)*dt == sum((s*dt)^2) * df
    S = fft(x)
    spectrum = abs(S)^2 * dt/N # [V^2/Hz, integrates to V^2 (power)]
    phase = Arg(S)
    if(one_sided){ # if one-sided, need to throw out negative frequencies
      w = (f <= nyq)
      f = f[w]
      spectrum = spectrum[w]
      spectrum[f > 0 & f < nyq] = 2 * spectrum[f > 0 & f < nyq] # double remaining spectrum to conserve energy; f = 0 and f = nyq do not have equivalents in the negative frequencies, so don't double them.
      phase = phase[w]
    }else{} ## do nothing if two-sided, everything is already correct for two-sided spectrum
  }else if(method == 2){ ## multitaper
    S = mtapspec(x, dt) # calculate multi-taper spectrum
    df = S$df
    f = S$freq
    if(one_sided){
      f = S$freq
      spectrum = S$spec[1:length(f)]
      spectrum[f > 0 & f < nyq] = 2 * spectrum[f > 0 & f < nyq] # double remaining spectrum to conserve energy; f = 0 and f = nyq do not have equivalents in the negative frequencies, so don't double them.
    }else{
      ## output of mtapspec of length M is always f=0, M/2-1 values, f=nyq, M/2-1 values; M is always even
      f = df * 0:(round(1/(df*dt))-1)
      spectrum = rep(0, length(f))
      M = length(f)
      spectrum[1:(M/2+1)] = S$spec[1:(M/2+1)]
      spectrum[(M/2+2):M] = S$spec[(M/2):2]
    }
    spectrum = spectrum * (N*dt) # needed to make mtapspec output scale to power spectrum correctly: units are now V^2/Hz (integrates to V^2)
  }else{
    stop('Invalid method: must be 1 (fft) or 2 (multitaper)')
  }
  ## done calculating power spectrum. Now, turn into desired spectrum.
  if(type == 1){ # power spectrum
    ## This is already the power spectrum, so don't change it [Pa^2/Hz, integrates to Pa^2]
    ## Parseval's relation: sum(s)*df == sum(x^2)*dt/(N*dt) == mean(x^2)
    output = list(f = f, df = df, spectrum = spectrum, type = 'Power', units = 'V^2 / Hz') 
  }else if(type == 2){ # energy spectrum
    ## energy spectrum is power * duration
    ## Parseval's relation: sum(s)*df == sum(x^2)*dt
    output = list(f = f, df = df, spectrum = spectrum * (N*dt), type = 'Energy', units = 'V^2 s / Hz')
  }else if(type == 3){ # amplitude spectrum
    ## amp. spec. is sqrt of power
    ## Parseval's relation: sum(s^2)*df == sum(x^2)*dt/(N*dt) == mean(x^2)
    output = list(f = f, df = df, spectrum = sqrt(spectrum), type = 'Amplitude', units = 'V / Hz^0.5')
  }else if(type == 4){ # phase spectrum
    if(method == 1){
      output = list(f = f, df = df, spectrum = phase, type = 'Phase', units = 'radians')
    }else{
      stop('Phase spectrum is not yet implemented for methods other than fft')
    }
  }
  invisible(output)
}
