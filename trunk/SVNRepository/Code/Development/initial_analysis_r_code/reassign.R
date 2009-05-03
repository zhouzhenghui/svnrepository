library(R.utils)

library(timeSeries); 
naomit <- stats::na.omit; 
mynaomit <- timeSeries::na.omit;
reassignInPackage("na.omit", pkgName="stats", mynaomit); 
