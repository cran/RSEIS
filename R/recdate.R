`recdate` <-
function(jd, hr, mi, sec)
{
#  recdate:  take a structure with time and rectify it
  secs = jd*(86400)+hr*(3600)+mi*(60)+sec;
  days = floor( secs / (86400));
  left =  secs - days*(86400);
  hrs = floor( left / (3600));
  left =  left - hrs*(3600);
  mins = floor(left/60.0);
   sec = left - mins*60;
  
  list( jd=days, hr=hrs, mi=mins, sec=sec)
}

