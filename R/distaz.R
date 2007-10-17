`distaz` <-
function( olat, olon, tlat, tlon)
{
# *   This subroutine will compute the distance, azimuth, and back
## * azimuth (in degrees), given the latitude and longitude (in degrees)
# * of an origin point and a target point.  (E+,W-; N+,S-)
  twopi = 2*pi;
  R.MAPK=6378.2064
    DEG2RAD=pi/180
    RAD2DEG=180/pi
 
  ERR=1;
  OK=0;
  L = list(err=0, del=0, az=0, baz=0)
  L$err = 0;

  olat[is.na(olat)] = -100
  tlat[is.na(tlat)] = -100

  olat[olat < -90. || olat > 90.] = NA
  
  tlat[tlat < -90. || tlat > 90.] = NA

  

  olon = fmod(olon,360)
  tlon = fmod(tlon,360)
  
  L$err=1;
  clat = 90. - olat;
  clon = olon;
  ####    if(clon < 0.) { clon =clon+ 360.; }
  clon = fmod(clon,360)
  clar = DEG2RAD*clat;
  clor = DEG2RAD*clon ;
  stho = sin(clar);
  ctho = cos(clar);
  ctlat = 90. - tlat;
  ctlon = tlon;
  #### if(clon < 0.) ctlon =ctlon+ 360.;
ctlon = fmod(ctlon,360)
  
  ctlar = DEG2RAD*ctlat ;
  ctlor = DEG2RAD*ctlon;
  sth = sin(ctlar);
  cth = cos(ctlar);
  dph = ctlor - clor;
  sdph = sin(dph);
  cdph = cos(dph);
  delr = acos(stho * sth * cdph + ctho * cth);
  del = RAD2DEG* delr ;

#### /* compute forward azimuth */

####	if(sth == 0.) { azr = 0.;}
####	else { azr = atan2(sdph,stho*cth/sth-ctho*cdph);}

  azr = rep(0, length(sdph))
  azr[sth!=0.0] = atan2(sdph,stho*cth/sth-ctho*cdph);
  az = RAD2DEG*azr;
  azr = fmod(azr,360)

###/* compute back azimuth */
  bazr = rep(0, length(sdph))

  
  bazr[stho!=0.0] = bazr = atan2(-sdph,sth*ctho/stho-cth*cdph);

  baz = RAD2DEG*bazr;
  bazr = fmod(bazr,360)

  
  L$del = del
  L$az =az
  L$baz = baz
  L$dist = L$del*2*pi*R.MAPK/360
  
	return(L);
}

