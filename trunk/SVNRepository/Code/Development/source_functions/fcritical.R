# Estimates of Critical 1#,5#, 10# values of Dolado et al Fractional Dickey Fuller Test based on 
# Sephton, "Critical Values of the Augmented Fractional Dickey-Fuller Test", June 2007
# 
# Response surfaces estimated using MARS (www.salford-systems.com)
# 
# All MARS models based on 10 fold cross validation, maximum basis functions = 50
# 
# define T as the sample size and d as the fractional differencing parameter
#
# MARS Models without Adjustment for Serial Correlation
#
# 			predictors include 1/T and d 
#
# MARS models with Adjustment for Serial Correlation
#
# 	 		predictors include 1/T, d, and the # lags (K) / T
#  			
#  

"fdfcritical" = function(sim,lags,nobs,diffparm,test){


# Input: sim lags nobs diffparm test
#
#
# 	sim is 0 if based on FDF test without adjustment for serial correlation, 
# 	sim is 1 if based on FDF test with adjustment for serial correlation 
#  
#	k 		Number of lags in AFDF test (maximum value = 10)
#	diffparm 	estimate of fractional differencing parameter under the alternative
#			
# 	nobs 		sample size
# 	test 		0 if no constant test, 1 if constant test, 2 if constant and trend test
#
#
#
#
# Output: MARS estimate of critical value
#
#
#
#
# Written by Peter S. Sephton, June 7, 2007
# Code does NOT check for proper passing of inputs
#
#

D=diffparm;
K=lags;
TINV=1/nobs;
KTINV=K*TINV;



############################### 
#####
#####          K=0, ie no lags
#####    MARS models using D and tinverse as predictors
################################

 BF1 = max(0,D-0.590);
 BF2 = max(0,0.590-D);
 BF3 = max(0,TINV-0.003);
 BF4 = max(0,0.003-TINV);
 BF5 = max(0,TINV-0.001)*BF1;
 BF6 = max(0,D-0.380)*BF4;
 BF7 = max(0,0.380-D)*BF4;
 BF8 = max(0,D-0.810);
 BF11 = max(0,0.450-D);
 BF13 = max(0,0.013-TINV)*BF11;
 BF14 = max(0,D-0.590)*BF3;
 BF16 = max(0,D-0.710);
 BF17 = max(0,0.710-D);
 BF19 = max(0,0.003-TINV)*BF16;
 BF22 = max(0,TINV-0.008)*BF11;
 BF24 = max(0,TINV-0.010)*BF17;
 BF26 = max(0,D-0.460)*BF4;
 BF30 = max(0,D-0.220);
 BF31 = max(0,0.220-D);
 BF34 = max(0,D-0.300)*BF3;
 BF36 = max(0,D-0.320)*BF4;
 BF39 = max(0,0.003-TINV)*BF31;
 BF42 = max(0,D-0.170);

 onenc0 = -2.387-0.276*BF2-4.851*BF3+78.722*BF5-175.169*BF6+45.031*BF7-0.060*BF8-6.399*BF13-77.455*BF14-0.048*BF16+43.321*BF19-6.864*BF22+3.455*BF24+63.760*BF26-0.124*BF30+3.513*BF34+142.719*BF36-56.341*BF39+0.122*BF42;

            
            
 BF1 = max(0,D-0.570);
 BF2 = max(0,0.570-D);
 BF3 = max(0,TINV-0.003);
 BF4 = max(0,0.003-TINV);
 BF5 = max(0,TINV-0.001)*BF1;
 BF6 = max(0,D-0.790);
 BF7 = max(0,0.790-D);
 BF8 = max(0,D-0.450);
 BF9 = max(0,0.450-D);
 BF10 = max(0,D-0.120);
 BF11 = max(0,0.120-D);
 BF12 = max(0,D-0.660);
 BF14 = max(0,D-0.510)*BF4;
 BF15 = max(0,0.510-D)*BF4;
 BF16 = max(0,TINV-0.011)*BF9;
 BF17 = max(0,0.011-TINV)*BF9;
 BF18 = max(0,TINV-0.004)*BF11;
 BF19 = max(0,0.004-TINV)*BF11;
 BF20 = max(0,TINV-0.011)*BF7;
 BF21 = max(0,0.011-TINV)*BF7;
 BF22 = max(0,D-0.370);
 BF24 = max(0,D-0.510);
 BF26 = max(0,D-0.180);
 BF27 = max(0,0.180-D);
 BF28 = max(0,TINV-0.011)*BF27;
 BF29 = max(0,0.011-TINV)*BF27;
 BF30 = max(0,D-0.900);
 BF32 = max(0,D-0.720);
 BF34 = max(0,D-0.610);
 BF36 = max(0,TINV-0.011)*BF2;
 BF37 = max(0,0.011-TINV)*BF2;
 BF38 = max(0,D-0.070);
 BF39 = max(0,0.070-D);
 BF40 = max(0,TINV-0.004)*BF10;
 BF42 = max(0,D-0.780)*BF3;
 BF44 = max(0,TINV-0.004)*BF1;
 BF46 = max(0,TINV-0.008)*BF39;
 BF47 = max(0,0.008-TINV)*BF39;
 BF48 = max(0,D-0.260)*BF4;
 BF50 = max(0,TINV-0.001)*BF30;

 onec0 = -2.558+1.619*BF1-1.615*BF2-4.794*BF3+7.063*BF4+0.093*BF5-0.382*BF6-0.213*BF8+0.092*BF10-0.182*BF12-58.886*BF14-3.886*BF15+4.324*BF16-12.372*BF17-8.386*BF18-19.391*BF19-23.040*BF20+31.181*BF21-0.156*BF22-0.273*BF24+0.084*BF26-8.786*BF28+9.344*BF29-0.063*BF30-0.189*BF32-0.197*BF34+21.705*BF36-33.687*BF37+0.009*BF38+5.760*BF40+ 25.850*BF42-17.404*BF44+11.631*BF46+ 39.294*BF47+32.120*BF48-3.604*BF50;
            
 BF1 = max(0,D-0.630);
 BF2 = max(0,0.630-D);
 BF3 = max(0,TINV-0.004);
 BF4 = max(0,0.004-TINV);
 BF5 = max(0,D-0.810)*BF4;
 BF6 = max(0,0.810-D)*BF4;
 BF7 = max(0,D-0.450);
 BF9 = max(0,D-0.810);
 BF10 = max(0,0.810-D);
 BF11 = max(0,D-0.660)*BF3;
 BF12 = max(0,0.660-D)*BF3;
 BF13 = max(0,D-0.130);
 BF14 = max(0,0.130-D);
 BF15 = max(0,TINV-0.010)*BF13;
 BF16 = max(0,0.010-TINV)*BF13;
 BF17 = max(0,TINV-0.011)*BF2;
 BF18 = max(0,0.011-TINV)*BF2;
 BF19 = max(0,TINV-0.002)*BF13;
 BF21 = max(0,TINV-0.010)*BF7;
 BF22 = max(0,0.010-TINV)*BF7;
 BF23 = max(0,TINV-0.002)*BF1;
 BF25 = max(0,D-0.720);
 BF27 = max(0,D-0.540);
 BF29 = max(0,D-0.900);
 BF31 = max(0,D-0.320)*BF3;
 BF33 = max(0,TINV-0.010)*BF25;
 BF34 = max(0,0.010-TINV)*BF25;
 BF35 = max(0,D-0.210)*BF4;
 BF37 = max(0,TINV-0.007)*BF13;
 BF39 = max(0,TINV-0.007)*BF27;
 BF40 = max(0,0.007-TINV)*BF27;
 BF41 = max(0,TINV-0.003)*BF13;
 BF43 = max(0,TINV-0.017)*BF10;
 BF44 = max(0,0.017-TINV)*BF10;
 BF45 = max(0,D-0.590)*BF4;
 BF47 = max(0,D-0.390);
 BF49 = max(0,TINV-0.004)*BF14;

 onect0 = -2.905+1.845*BF1-1.187*BF2-27.059*BF3-6.954*BF4-134.269*BF5+117.470*BF6-0.139*BF7-0.192*BF9+81.519*BF11-84.207*BF12-0.067*BF13-29.111*BF15+35.556*BF16+99.431*BF17-106.424*BF18+40.251*BF19+0.248*BF21-2.584*BF22-92.870*BF23-0.220*BF25-0.202*BF27-0.215*BF29-9.064*BF31+3.603*BF33-12.915*BF34+68.522*BF35+9.498*BF37-0.142*BF39-13.042*BF40+10.360*BF41+6.572*BF43-8.585*BF44-36.913*BF45-0.053*BF47-43.010*BF49;


            
 BF1 = max(0,D-0.590);
 BF2 = max(0,0.590-D);
 BF3 = max(0,TINV-0.004);
 BF4 = max(0,0.004-TINV);
 BF5 = max(0,D-0.580)*BF3;
 BF6 = max(0,0.580-D)*BF3;
 BF7 = max(0,D-0.410)*BF4;
 BF8 = max(0,0.410-D)*BF4;
 BF9 = max(0,D-0.790);
 BF11 = max(0,D-0.440);
 BF12 = max(0,0.440-D);
 BF13 = max(0,D-0.680);
 BF15 = max(0,D-0.510)*BF4;
 BF17 = max(0,TINV-0.013)*BF12;
 BF18 = max(0,0.013-TINV)*BF12;
 BF19 = max(0,D-0.120);
 BF20 = max(0,0.120-D);
 BF21 = max(0,TINV-0.011);
 BF23 = max(0,TINV-0.003)*BF13;
 BF24 = max(0,0.003-TINV)*BF13;
 BF25 = max(0,D-0.370);
 BF27 = max(0,D-0.900);
 BF29 = max(0,TINV-0.006)*BF20;
 BF30 = max(0,0.006-TINV)*BF20;
 BF31 = max(0,D-0.530);
 BF33 = max(0,D-0.610);
 BF37 = max(0,D-0.260)*BF3;
 BF39 = max(0,TINV-0.002)*BF13;
 BF41 = max(0,TINV-0.002)*BF19;
 BF43 = max(0,D-0.590)*BF4;
 BF46 = max(0,0.014-TINV)*BF27;
 BF47 = max(0,D-0.880)*BF3;
 BF49 = max(0,D-0.930);

 fivenc0 = -1.696+0.421*BF1-0.400*BF2-3.862*BF3+5.142*BF4-2.072*BF5+3.147*BF6+13.199*BF7-10.736*BF8-0.058*BF9-0.018*BF11-0.054*BF13-11.805*BF15+0.441*BF17-3.857*BF18+0.421*BF21+21.574*BF23-1.716*BF24-0.035*BF25-0.155*BF27-1.281*BF29+12.873*BF30-0.060*BF31-0.076*BF33+1.238*BF37-20.103*BF39+3.491*BF41-15.519*BF43+8.425*BF46+7.750*BF47+0.052*BF49;

            
            
 BF1 = max(0,D-0.570);
 BF2 = max(0,0.570-D);
 BF3 = max(0,TINV-0.004);
 BF4 = max(0,0.004-TINV);
 BF5 = max(0,D-0.570)*BF3;
 BF6 = max(0,0.570-D)*BF3;
 BF7 = max(0,D-0.790);
 BF9 = max(0,D-0.450);
 BF11 = max(0,D-0.510)*BF4;
 BF12 = max(0,0.510-D)*BF4;
 BF13 = max(0,D-0.130);
 BF15 = max(0,D-0.670);
 BF17 = max(0,TINV-0.011)*BF13;
 BF18 = max(0,0.011-TINV)*BF13;
 BF19 = max(0,TINV-0.011)*BF9;
 BF20 = max(0,0.011-TINV)*BF9;
 BF21 = max(0,D-0.390);
 BF22 = max(0,0.390-D);
 BF23 = max(0,D-0.240)*BF4;
 BF25 = max(0,D-0.510);
 BF27 = max(0,TINV-0.002);
 BF29 = max(0,D-0.900)*BF27;
 BF30 = max(0,0.900-D)*BF27;
 BF32 = max(0,0.002-TINV)*BF22;
 BF33 = max(0,D-0.610);
 BF34 = max(0,0.610-D);
 BF35 = max(0,D-0.710);
 BF37 = max(0,D-0.900)*BF3;
 BF39 = max(0,TINV-0.014)*BF34;
 BF40 = max(0,0.014-TINV)*BF34;
 BF41 = max(0,D-0.200);
 BF43 = max(0,D-0.080);
 BF45 = max(0,TINV-0.003)*BF41;
 BF46 = max(0,0.003-TINV)*BF41;
 BF47 = max(0,D-0.510)*BF3;
 BF49 = max(0,TINV-0.008)*BF41;

 fivec0 = -1.941+1.398*BF1-1.626*BF2-7.763*BF3+9.998*BF4+30.606*BF5-30.444*BF6-0.185*BF7-0.163*BF9-96.553*BF11+29.619*BF12+0.066*BF13-0.189*BF15-3.133*BF17+11.079*BF18-4.194*BF19-9.459*BF20-0.198*BF21+26.077*BF23-0.337*BF25-66.452*BF29+18.193*BF30-53.270*BF32-0.190*BF33-0.171*BF35+45.811*BF37+6.178*BF39-8.306*BF40+0.116*BF41+0.152*BF43-7.667*BF45+12.448*BF46+13.909*BF47+1.639*BF49;

            
            
 BF1 = max(0,D-0.630);
 BF2 = max(0,0.630-D);
 BF3 = max(0,TINV-0.004);
 BF4 = max(0,0.004-TINV);
 BF5 = max(0,TINV-0.004)*BF2;
 BF6 = max(0,0.004-TINV)*BF2;
 BF7 = max(0,D-0.850);
 BF8 = max(0,0.850-D);
 BF9 = max(0,D-0.490);
 BF11 = max(0,D-0.150)*BF4;
 BF12 = max(0,0.150-D)*BF4;
 BF13 = max(0,D-0.740);
 BF15 = max(0,TINV-0.011)*BF13;
 BF16 = max(0,0.011-TINV)*BF13;
 BF17 = max(0,TINV-0.011)*BF9;
 BF18 = max(0,0.011-TINV)*BF9;
 BF19 = max(0,TINV-0.002);
 BF20 = max(0,0.002-TINV);
 BF21 = max(0,D-0.100);
 BF23 = max(0,TINV-0.011)*BF21;
 BF24 = max(0,0.011-TINV)*BF21;
 BF25 = max(0,D-0.370)*BF19;
 BF26 = max(0,0.370-D)*BF19;
 BF27 = max(0,TINV-0.013)*BF8;
 BF28 = max(0,0.013-TINV)*BF8;
 BF29 = max(0,D-0.560)*BF20;
 BF30 = max(0,0.560-D)*BF20;
 BF31 = max(0,D-0.560);
 BF33 = max(0,D-0.920);
 BF35 = max(0,D-0.180)*BF4;
 BF37 = max(0,TINV-0.008)*BF21;
 BF39 = max(0,TINV-0.003)*BF21;
 BF41 = max(0,TINV-0.017)*BF21;
 BF43 = max(0,D-0.440);
 BF45 = max(0,D-0.150);
 BF47 = max(0,TINV-0.007)*BF45;
 BF48 = max(0,0.007-TINV)*BF45;
 BF49 = max(0,D-0.680);

 fivect0 = -2.343+1.786*BF1-1.972*BF2-30.359*BF3+63.049*BF4+7.499*BF5-72.509*BF6-0.220*BF7-0.060*BF9-151.352*BF11+55.119*BF12-0.256*BF13+2.911*BF15-18.042*BF16+4.629*BF17-15.647*BF18+17.865*BF19-14.696*BF23+24.418*BF24+18.616*BF25-27.414*BF26+9.554*BF27-11.950*BF28-56.359*BF29-23.586*BF30-0.226*BF31-0.167*BF33+112.128*BF35+2.606*BF37+12.249*BF39+2.671*BF41 -0.126*BF43+0.094*BF45-14.553*BF47+18.810*BF48-0.113*BF49;
  
 
 BF1 = max(0,D-0.580);
 BF2 = max(0,0.580-D);
 BF3 = max(0,TINV-0.004);
 BF4 = max(0,0.004-TINV);
 BF5 = max(0,D-0.570)*BF3;
 BF6 = max(0,0.570-D)*BF3;
 BF7 = max(0,D-0.800);
 BF9 = max(0,D-0.440);
 BF12 = max(0,0.510-D)*BF4;
 BF13 = max(0,D-0.690);
 BF14 = max(0,0.690-D);
 BF15 = max(0,D-0.120);
 BF17 = max(0,TINV-0.014)*BF15;
 BF18 = max(0,0.014-TINV)*BF15;
 BF19 = max(0,TINV-0.004)*BF9;
 BF20 = max(0,0.004-TINV)*BF9;
 BF22 = max(0,0.014-TINV)*BF1;
 BF23 = max(0,D-0.370);
 BF25 = max(0,TINV-0.008)*BF15;
 BF27 = max(0,TINV-0.013)*BF14;
 BF29 = max(0,D-0.900);
 BF31 = max(0,TINV-0.014)*BF7;
 BF33 = max(0,D-0.510);
 BF35 = max(0,D-0.610);
 BF37 = max(0,TINV-0.008)*BF35;
 BF38 = max(0,0.008-TINV)*BF35;
 BF39 = max(0,D-0.740);
 BF41 = max(0,D-0.170)*BF3;
 BF43 = max(0,TINV-0.002);
 BF44 = max(0,0.002-TINV);
 BF46 = max(0,0.590-D)*BF43;
 BF47 = max(0,D-0.190)*BF44;
 BF48 = max(0,0.190-D)*BF44;
 BF50 = max(0,0.008-TINV)*BF39;

 tennc0 = -1.367+0.473*BF1-0.414*BF2-8.770*BF3+8.584*BF4-15.297*BF5+22.437*BF6-0.041*BF7-0.051*BF9-25.406*BF12-0.051*BF13-3.211*BF17+4.756*BF18+2.993*BF19+6.520*BF20-5.333*BF22-0.039*BF23+2.380*BF25-0.736*BF27-0.037*BF29+8.846*BF31-0.065*BF33+10.528*BF37-17.871*BF38-0.055*BF39+3.149*BF41+7.260*BF43-19.184*BF46-10.882*BF47+31.652*BF48+3.523*BF50;
 
 
 
            
            
            
 BF1 = max(0,D-0.570);
 BF2 = max(0,0.570-D);
 BF3 = max(0,TINV-0.005);
 BF4 = max(0,0.005-TINV);
 BF5 = max(0,D-0.710)*BF4;
 BF6 = max(0,0.710-D)*BF4;
 BF7 = max(0,D-0.560)*BF3;
 BF8 = max(0,0.560-D)*BF3;
 BF9 = max(0,D-0.800);
 BF11 = max(0,D-0.460);
 BF13 = max(0,D-0.130);
 BF15 = max(0,D-0.670);
 BF17 = max(0,D-0.540)*BF4;
 BF19 = max(0,TINV-0.013)*BF13;
 BF20 = max(0,0.013-TINV)*BF13;
 BF21 = max(0,D-0.400);
 BF24 = max(0,0.013-TINV)*BF11;
 BF25 = max(0,D-0.230)*BF4;
 BF27 = max(0,TINV-0.003);
 BF29 = max(0,D-0.530)*BF27;
 BF30 = max(0,0.530-D)*BF27;
 BF31 = max(0,D-0.740);
 BF33 = max(0,D-0.890);
 BF35 = max(0,D-0.610);
 BF36 = max(0,0.610-D);
 BF37 = max(0,D-0.510);
 BF39 = max(0,D-0.200);
 BF41 = max(0,D-0.070);
 BF43 = max(0,TINV-0.008)*BF39;
 BF44 = max(0,0.008-TINV)*BF39;
 BF45 = max(0,TINV-0.008)*BF37;
 BF46 = max(0,0.008-TINV)*BF37;
 BF47 = max(0,TINV-0.005)*BF9;
 BF48 = max(0,0.005-TINV)*BF9;
 BF49 = max(0,TINV-0.014)*BF36;
 BF50 = max(0,0.014-TINV)*BF36;

 tenc0 = -1.616+1.392*BF1-1.601*BF2-10.658*BF3+13.470*BF4+8.398*BF5-12.216*BF6+9.411*BF7+9.854*BF8-0.145*BF9-0.175*BF11+0.096*BF13-0.254*BF15-53.567*BF17-3.336*BF19+8.625*BF20-0.238*BF21-8.516*BF24+14.532*BF25+8.799*BF27-18.278*BF29-19.672*BF30-0.193*BF31-0.108*BF33-0.227*BF35-0.101*BF37+0.077*BF39+0.163*BF41-6.654*BF43+13.669*BF44+31.694*BF45-43.675*BF46-0.927*BF47+25.726*BF48+6.977*BF49-7.824*BF50;

 BF1 = max(0,D-0.630);
 BF2 = max(0,0.630-D);
 BF3 = max(0,TINV-0.005);
 BF4 = max(0,0.005-TINV);
 BF5 = max(0,TINV-0.005)*BF2;
 BF6 = max(0,0.005-TINV)*BF2;
 BF7 = max(0,D-0.830)*BF4;
 BF8 = max(0,0.830-D)*BF4;
 BF9 = max(0,D-0.470);
 BF11 = max(0,D-0.800);
 BF12 = max(0,0.800-D);
 BF13 = max(0,D-0.150)*BF4;
 BF15 = max(0,TINV-0.013)*BF9;
 BF16 = max(0,0.013-TINV)*BF9;
 BF17 = max(0,TINV-0.002);
 BF18 = max(0,0.002-TINV);
 BF19 = max(0,D-0.460)*BF3;
 BF20 = max(0,0.460-D)*BF3;
 BF21 = max(0,D-0.100);
 BF22 = max(0,0.100-D);
 BF23 = max(0,TINV-0.011)*BF21;
 BF24 = max(0,0.011-TINV)*BF21;
 BF25 = max(0,TINV-0.002)*BF12;
 BF26 = max(0,0.002-TINV)*BF12;
 BF27 = max(0,D-0.910);
 BF29 = max(0,D-0.730);
 BF31 = max(0,D-0.560);
 BF33 = max(0,D-0.400)*BF17;
 BF35 = max(0,D-0.200)*BF4;
 BF37 = max(0,TINV-0.011)*BF29;
 BF38 = max(0,0.011-TINV)*BF29;
 BF39 = max(0,D-0.660)*BF18;
 BF40 = max(0,0.660-D)*BF18;
 BF41 = max(0,TINV-0.008)*BF21;
 BF43 = max(0,TINV-0.004)*BF21;
 BF45 = max(0,TINV-0.002)*BF21;
 BF47 = max(0,TINV-0.006)*BF22;
 BF49 = max(0,D-0.850);

 tenct0 = -2.036+1.925*BF1-2.214*BF2-51.431*BF3+22.927*BF4+11.063*BF5-49.310*BF6-53.742*BF7+51.643*BF8+0.046*BF9-0.253*BF11+21.893*BF13+39.955*BF15-39.132*BF16+16.957*BF17-62.413*BF19+37.654*BF20+0.352*BF21+20.118*BF23-13.404*BF24+15.075*BF25-17.048*BF26-0.211*BF27-0.247*BF29-0.232*BF31-13.750*BF33+63.540*BF35-8.388*BF37-14.702*BF38-50.169*BF39-28.395*BF40+5.167*BF41+10.700*BF43+18.194*BF45-79.607*BF47-0.149*BF49;

 
 
 ###################
 ####
 ####  end K=0, ie no lags
 ####   
 ###################
 
 
 
 
 #############################################
 # now allow for lags from 0 to 10
 # predictors include d, 1/T, K/T 
 # MARS response surfaces based on 10 fold cross validation, 3 variable interaction
 # and maximum number of basis functions set at 50
 #############################################

 
 

 
 BF1=max(0,D-0.520);
 BF2=max(0,0.520-D);
 BF3=max(0,KTINV-0.009)*BF1;
 BF4=max(0,0.009-KTINV)*BF1;
 BF5=max(0,KTINV-0.011);
 BF6=max(0,0.011-KTINV);
 BF7=max(0,TINV-0.013);
 BF8=max(0,0.013-TINV);
 BF9=max(0,KTINV-0.013)*BF2;
 BF10=max(0,0.013-KTINV)*BF2;
 BF11=max(0,KTINV-0.003);
 BF12=max(0,0.003-KTINV);
 BF13=max(0,D-0.780)*BF12;
 BF14=max(0,0.780-D)*BF12;
 BF15=max(0,TINV-0.014)*BF9;
 BF16=max(0,0.014-TINV)*BF9;
 BF17=max(0,KTINV-0.004)*BF8;
 BF18=max(0,0.004-KTINV)*BF8;
 BF19=max(0,TINV-0.006)*BF11;
 BF20=max(0,0.006-TINV)*BF11;
 BF21=max(0,KTINV-0.033)*BF7;
 BF22=max(0,0.033-KTINV)*BF7;
 BF23=max(0,TINV-0.013)*BF6;
 BF24=max(0,0.013-TINV)*BF6;
 BF25=max(0,KTINV-0.043)*BF7;
 BF27=max(0,KTINV-0.038);
 BF28=max(0,0.038-KTINV);
 BF29=max(0,TINV-0.003)*BF28;
 BF30=max(0,0.003-TINV)*BF28;
 BF31=max(0,KTINV-0.080)*BF7;
 BF33=max(0,TINV-0.013)*BF28;
 BF35=max(0,KTINV-0.083)*BF7;
 BF37=max(0,KTINV-0.120)*BF7;
 BF39=max(0,KTINV-0.140);
 BF40=max(0,0.140-KTINV);
 BF41=max(0,KTINV-0.117)*BF7;
 BF43=max(0,D-0.230)*BF28;
 BF44=max(0,0.230-D)*BF28;
 BF45=max(0,TINV-0.005)*BF4;
 BF46=max(0,0.005-TINV)*BF4;
 BF47=max(0,TINV-0.017)*BF40;
 BF49=max(0,TINV-0.017)*BF39;
 BF50=max(0,0.017-TINV)*BF39;

 onenck=-2.882+0.085*BF1-0.146*BF2-0.159*BF3-0.264*BF4-45.713*BF5+43.219*BF6-11.495*BF7+4.752*BF8+0.506*BF9-5.666*BF10+37.502*BF11-12.958*BF13-31.573*BF14-49.999*BF15+66.886*BF16+1243.054*BF17-1985.850*BF18+1234.706*BF19-1279.169*BF20-5152.249*BF21+6163.894*BF22+1550.326*BF23+521.613*BF24+4174.299*BF25+0.606*BF27-46.829*BF29-98.071*BF30-1850.811*BF31-5645.889*BF33+1577.459*BF35-1544.826*BF37+1.390*BF39+1317.751*BF41+0.531*BF43-1.419*BF44+559.987*BF45+5.421*BF46-33.844*BF47+122.484*BF49+1812.171*BF50;

 
 
 
 
 BF1=max(0,D-0.600);
 BF2=max(0,0.600-D);
 BF3=max(0,TINV-0.003);
 BF4=max(0,0.003-TINV);
 BF5=max(0,KTINV-0.004);
 BF6=max(0,0.004-KTINV);
 BF7=max(0,D-0.430)*BF6;
 BF8=max(0,0.430-D)*BF6;
 BF9=max(0,D-0.590)*BF3;
 BF10=max(0,0.590-D)*BF3;
 BF11=max(0,D-0.810);
 BF12=max(0,0.810-D);
 BF13=max(0,D-0.510);
 BF14=max(0,0.510-D);
 BF15=max(0,TINV-0.011)*BF5;
 BF16=max(0,0.011-TINV)*BF5;
 BF17=max(0,D-0.150);
 BF19=max(0,KTINV-0.120)*BF3;
 BF20=max(0,0.120-KTINV)*BF3;
 BF21=max(0,KTINV-0.053)*BF14;
 BF22=max(0,0.053-KTINV)*BF14;
 BF23=max(0,D-0.690);
 BF25=max(0,D-0.460)*BF4;
 BF26=max(0,0.460-D)*BF4;
 BF27=max(0,KTINV-0.017)*BF17;
 BF28=max(0,0.017-KTINV)*BF17;
 BF29=max(0,KTINV-0.001)*BF4;
 BF30=max(0,0.001-KTINV)*BF4;
 BF31=max(0,KTINV-0.025)*BF3;
 BF33=max(0,KTINV-0.043);
 BF34=max(0,0.043-KTINV);
 BF35=max(0,TINV-0.011)*BF12;
 BF36=max(0,0.011-TINV)*BF12;
 BF37=max(0,D-0.400)*BF34;
 BF38=max(0,0.400-D)*BF34;
 BF39=max(0,KTINV-0.018)*BF36;
 BF40=max(0,0.018-KTINV)*BF36;
 BF41=max(0,TINV-0.008)*BF34;
 BF42=max(0,0.008-TINV)*BF34;
 BF43=max(0,KTINV-0.003)*BF36;
 BF45=max(0,KTINV-0.117);
 BF47=max(0,D-0.570)*BF30;
 BF48=max(0,0.570-D)*BF30;
 BF49=max(0,KTINV-0.129)*BF3;

 oneck=-2.614+0.978*BF1-1.371*BF2-215.632*BF3+8.362*BF4+16.316*BF5+12.702*BF6+3.050*BF7-52.443*BF8+9.023*BF9+6.862*BF10-0.261*BF11-0.445*BF13+2045.185*BF15-2042.501*BF16+0.214*BF17-1992.081*BF19+1756.943*BF20+0.383*BF21+0.313*BF22-0.320*BF23-28.202*BF25-32.307*BF26+0.219*BF27+2.008*BF28+2094.999*BF29+25014.357*BF30-308.096*BF31+1.288*BF33-8.113*BF35-25.178*BF36-0.592*BF37-2.418*BF38-1824.531*BF39+2016.123*BF40-44.499*BF41-27.661*BF42+1913.549*BF43+1.846*BF45-27969.162*BF47-52106.465*BF48+117.434*BF49;

 

 
 
 
            
            
 BF1=max(0,D-0.550);
 BF2=max(0,0.550-D);
 BF3=max(0,KTINV-0.013);
 BF4=max(0,0.013-KTINV);
 BF5=max(0,KTINV-0.012)*BF2;
 BF6=max(0,0.012-KTINV)*BF2;
 BF7=max(0,TINV-0.002);
 BF8=max(0,0.002-TINV);
 BF9=max(0,KTINV-0.003)*BF1;
 BF10=max(0,0.003-KTINV)*BF1;
 BF11=max(0,KTINV-0.003);
 BF12=max(0,0.003-KTINV);
 BF13=max(0,TINV-0.013)*BF9;
 BF14=max(0,0.013-TINV)*BF9;
 BF15=max(0,D-0.750)*BF12;
 BF16=max(0,0.750-D)*BF12;
 BF17=max(0,KTINV-0.031);
 BF18=max(0,0.031-KTINV);
 BF19=max(0,D-0.420)*BF18;
 BF20=max(0,0.420-D)*BF18;
 BF21=max(0,KTINV-0.005)*BF7;
 BF22=max(0,0.005-KTINV)*BF7;
 BF23=max(0,KTINV-0.056);
 BF24=max(0,0.056-KTINV);
 BF25=max(0,D-0.790)*BF24;
 BF26=max(0,0.790-D)*BF24;
 BF27=max(0,D-0.260)*BF7;
 BF28=max(0,0.260-D)*BF7;
 BF29=max(0,D-0.750)*BF22;
 BF30=max(0,0.750-D)*BF22;
 BF31=max(0,TINV-0.013)*BF26;
 BF32=max(0,0.013-TINV)*BF26;
 BF33=max(0,KTINV-0.001)*BF8;
 BF34=max(0,0.001-KTINV)*BF8;
 BF35=max(0,D-0.330);
 BF37=max(0,KTINV-0.007)*BF35;
 BF38=max(0,0.007-KTINV)*BF35;
 BF39=max(0,TINV-0.004)*BF18;
 BF40=max(0,0.004-TINV)*BF18;
 BF41=max(0,TINV-0.014)*BF23;
 BF42=max(0,0.014-TINV)*BF23;
 BF43=max(0,TINV-0.008)*BF12;
 BF44=max(0,0.008-TINV)*BF12;
 BF45=max(0,TINV-0.008)*BF19;
 BF46=max(0,0.008-TINV)*BF19;
 BF47=max(0,D-0.560)*BF12;
 BF49=max(0,D-0.120)*BF44;
 BF50=max(0,0.120-D)*BF44;

 onectk=-4.227+1.043*BF1-1.194*BF2-97.078*BF3+106.864*BF4+3.117*BF5-23.997*BF6-2.946*BF7-16.900*BF8-2.738*BF9+428.328*BF10+92.061*BF11+69.416*BF13-66.062*BF14-75.771*BF15-93.754*BF16+3.445*BF17+6.937*BF19-7.472*BF20-236.883*BF21-2725.395*BF22+4.051*BF23-3.113*BF25-4.157*BF26+5.292*BF27-7.890*BF28+6625.083*BF29+2179.720*BF30- 425.226*BF31+101.615*BF32+2432.287*BF33+61726.043*BF34-0.188*BF35+1.101*BF37+26.211*BF38-397.433*BF39+236.781*BF40+130.433*BF41-222.567*BF42+6156.761*BF43 -4639.641*BF44-689.641*BF45-638.275*BF46-411.940*BF47+9518.793*BF49+8077.858*BF50;

 

 
 
 
 
 
 
 
 
 
 BF1=max(0,D-0.510);
 BF2=max(0,0.510-D);
 BF3=max(0,KTINV-0.009);
 BF4=max(0,0.009-KTINV);
 BF5=max(0,KTINV-0.009)*BF2;
 BF6=max(0,0.009-KTINV)*BF2;
 BF7=max(0,TINV-0.002);
 BF8=max(0,0.002-TINV);
 BF9=max(0,KTINV-0.003)*BF1;
 BF10=max(0,0.003-KTINV)*BF1;
 BF11=max(0,KTINV-0.038);
 BF12=max(0,0.038-KTINV);
 BF13=max(0,KTINV-0.004);
 BF14=max(0,0.004-KTINV);
 BF15=max(0,D-0.740)*BF12;
 BF16=max(0,0.740-D)*BF12;
 BF17=max(0,D-0.530)*BF14;
 BF18=max(0,0.530-D)*BF14;
 BF19=max(0,KTINV-0.014);
 BF20=max(0,0.014-KTINV);
 BF21=max(0,TINV-0.014)*BF5;
 BF22=max(0,0.014-TINV)*BF5;
 BF23=max(0,D-0.290);
 BF25=max(0,TINV-0.001)*BF10;
 BF26=max(0,KTINV-0.032)*BF7;
 BF27=max(0,0.032-KTINV)*BF7;
 BF28=max(0,TINV-0.004)*BF3;
 BF29=max(0,0.004-TINV)*BF3;
 BF30=max(0,KTINV-0.051)*BF7;
 BF32=max(0,TINV-0.010)*BF12;
 BF33=max(0,0.010-TINV)*BF12;
 BF34=max(0,TINV-0.010)*BF20;
 BF35=max(0,0.010-TINV)*BF20;
 BF36=max(0,KTINV-0.040);
 BF37=max(0,0.040-KTINV);
 BF38=max(0,TINV-0.010)*BF37;
 BF40=max(0,TINV-0.002)*BF37;
 BF41=max(0,0.002-TINV)*BF37;
 BF43=max(0,0.900-D)*BF35;
 BF44=max(0,KTINV-0.031);
 BF45=max(0,0.031-KTINV);
 BF46=max(0,TINV-0.010)*BF45;
 BF48=max(0,KTINV-0.067)*BF7;
 BF50=max(0,D-0.010)*BF46;

 fivenck=-1.591+0.155*BF1-0.212*BF2-18.361*BF3+19.301*BF4+0.696*BF5-3.450*BF6-29.303*BF7+14.844*BF8-0.260*BF9+82.322*BF10+8.148*BF11+16.551*BF13-0.584*BF15-1.440*BF16-55.000*BF17-32.033*BF18-6.975*BF19-46.415*BF21+39.221*BF22-0.037*BF23+3484.200*BF25-1461.656*BF26+1374.891*BF27+1309.057*BF28-1231.456*BF29+179.114*BF30+2324.152*BF32-1435.780*BF33+717.743*BF34+1417.312*BF35+3.157*BF36+169.174*BF38-1308.601*BF40+501.044*BF41-271.725*BF43+1.306*BF44-2158.687*BF46-33.894*BF48-103.800*BF50;

 

 
 
 
 
 
 
 
 
 
 
 
 
  BF1=max(0,D-0.610);
 BF2=max(0,0.610-D);
 BF3=max(0,KTINV-0.013);
 BF4=max(0,0.013-KTINV);
 BF6=max(0,0.450-D)*BF4;
 BF7=max(0,TINV-0.002);
 BF8=max(0,0.002-TINV);
 BF9=max(0,KTINV-0.003);
 BF10=max(0,0.003-KTINV);
 BF11=max(0,D-0.600)*BF7;
 BF12=max(0,0.600-D)*BF7;
 BF13=max(0,D-0.810);
 BF15=max(0,D-0.520);
 BF16=max(0,0.520-D);
 BF17=max(0,KTINV-0.004)*BF16;
 BF18=max(0,0.004-KTINV)*BF16;
 BF19=max(0,D-0.150);
 BF21=max(0,KTINV-0.043)*BF19;
 BF22=max(0,0.043-KTINV)*BF19;
 BF23=max(0,D-0.700);
 BF26=max(0,0.380-D)*BF10;
 BF27=max(0,TINV-0.013)*BF3;
 BF28=max(0,0.013-TINV)*BF3;
 BF29=max(0,KTINV-0.025)*BF7;
 BF30=max(0,0.025-KTINV)*BF7;
 BF31=max(0,KTINV-0.031);
 BF32=max(0,0.031-KTINV);
 BF34=max(0,0.410-D)*BF32;
 BF35=max(0,D-0.510)*BF8;
 BF36=max(0,0.510-D)*BF8;
 BF37=max(0,TINV-0.005)*BF10;
 BF38=max(0,0.005-TINV)*BF10;
 BF39=max(0,TINV-0.007)*BF32;
 BF40=max(0,0.007-TINV)*BF32;
 BF41=max(0,D-0.460)*BF40;
 BF42=max(0,0.460-D)*BF40;
 BF43=max(0,KTINV-0.050)*BF7;
 BF45=max(0,TINV-0.007)*BF19;
 BF46=max(0,0.007-TINV)*BF19;
 BF47=max(0,TINV-0.007)*BF4;
 BF49=max(0,TINV-0.007)*BF15;
 BF50=max(0,0.007-TINV)*BF15;

 fiveck=-2.303+1.058*BF1-1.427*BF2-42.814*BF3+37.738*BF4-4.722*BF6+9.579*BF7+8.525*BF8+33.263*BF9+9.472*BF11-1.739*BF12-0.258*BF13-0.505*BF15+0.795*BF17-10.142*BF18+0.215*BF19+0.165*BF21+0.182*BF22-0.326*BF23-70.092*BF26-792.292*BF27+799.122*BF28+694.419*BF29-832.241*BF30+1.748*BF31-2.419*BF34-22.304*BF35-12.200*BF36+3086.104*BF37-1755.595*BF38-369.024*BF39-46.847*BF40-249.665*BF41-124.322*BF42+80.502*BF43-5.654*BF45+19.010*BF46+976.574*BF47+6.348*BF49-24.187*BF50;

 
 

 
 
 
 
 
 
 
  BF1=max(0,D-0.550);
 BF2=max(0,0.550-D);
 BF3=max(0,KTINV-0.013);
 BF4=max(0,0.013-KTINV);
 BF5=max(0,KTINV-0.012)*BF2;
 BF6=max(0,0.012-KTINV)*BF2;
 BF7=max(0,KTINV-0.003)*BF1;
 BF8=max(0,0.003-KTINV)*BF1;
 BF9=max(0,TINV-0.002);
 BF10=max(0,0.002-TINV);
 BF11=max(0,KTINV-0.048);
 BF12=max(0,0.048-KTINV);
 BF14=max(0,0.003-KTINV);
 BF15=max(0,D-0.790)*BF12;
 BF16=max(0,0.790-D)*BF12;
 BF17=max(0,D-0.720)*BF14;
 BF18=max(0,0.720-D)*BF14;
 BF19=max(0,TINV-0.008)*BF14;
 BF20=max(0,0.008-TINV)*BF14;
 BF21=max(0,TINV-0.006)*BF7;
 BF22=max(0,0.006-TINV)*BF7;
 BF23=max(0,D-0.330);
 BF25=max(0,D-0.630)*BF20;
 BF26=max(0,0.630-D)*BF20;
 BF27=max(0,TINV-0.002)*BF2;
 BF28=max(0,0.002-TINV)*BF2;
 BF30=max(0,0.001-KTINV)*BF10;
 BF31=max(0,KTINV-0.023)*BF23;
 BF32=max(0,0.023-KTINV)*BF23;
 BF33=max(0,TINV-0.002)*BF14;
 BF34=max(0,0.002-TINV)*BF14;
 BF35=max(0,KTINV-0.044)*BF27;
 BF36=max(0,0.044-KTINV)*BF27;
 BF37=max(0,TINV-0.008)*BF32;
 BF38=max(0,0.008-TINV)*BF32;
 BF39=max(0,TINV-0.007)*BF23;
 BF40=max(0,0.007-TINV)*BF23;
 BF41=max(0,KTINV-0.090)*BF1;
 BF43=max(0,KTINV-0.002)*BF40;
 BF44=max(0,0.002-KTINV)*BF40;
 BF45=max(0,KTINV-0.007)*BF1;
 BF47=max(0,D-0.430)*BF4;
 BF48=max(0,0.430-D)*BF4;
 BF49=max(0,KTINV-0.001)*BF28;
 BF50=max(0,0.001-KTINV)*BF28;

 fivectk=-2.697+1.361*BF1-1.418*BF2-4.839*BF3+17.459*BF4+5.116*BF5-28.265*BF6-48.240*BF7+52.906*BF8-8.194*BF9+2.120*BF10+4.117*BF11-4.070*BF15-4.221*BF16-8.227*BF17-82.271*BF18-10583.233*BF19+13951.679*BF20-148.446*BF21+654.915*BF22-0.200*BF23-24411.715*BF25-7015.310*BF26+3.148*BF27+80.424*BF28+119132.344*BF30+0.214*BF31+8.100*BF32+10882.229*BF33-37439.336*BF34-127.386*BF35-146.755*BF36-938.235*BF37+868.515*BF38+11.378*BF39-34.176*BF40+3.920*BF41+411.251*BF43+24892.764*BF44+45.891*BF45-10.931*BF47-6.875*BF48-10629.545*BF49-153361.813*BF50;

 

 
 
 
 
 
 
          
 BF1=max(0,D-0.510);
 BF2=max(0,0.510-D);
 BF3=max(0,KTINV-0.009)*BF1;
 BF4=max(0,0.009-KTINV)*BF1;
 BF5=max(0,KTINV-0.013);
 BF6=max(0,0.013-KTINV);
 BF7=max(0,KTINV-0.013)*BF2;
 BF8=max(0,0.013-KTINV)*BF2;
 BF9=max(0,KTINV-0.003);
 BF10=max(0,0.003-KTINV);
 BF11=max(0,KTINV-0.050);
 BF12=max(0,0.050-KTINV);
 BF13=max(0,TINV-0.013);
 BF14=max(0,0.013-TINV);
 BF15=max(0,D-0.760)*BF10;
 BF16=max(0,0.760-D)*BF10;
 BF17=max(0,D-0.290)*BF12;
 BF18=max(0,0.290-D)*BF12;
 BF19=max(0,KTINV-0.030)*BF14;
 BF20=max(0,0.030-KTINV)*BF14;
 BF21=max(0,TINV-0.002)*BF10;
 BF22=max(0,0.002-TINV)*BF10;
 BF23=max(0,KTINV-0.033)*BF13;
 BF24=max(0,0.033-KTINV)*BF13;
 BF25=max(0,TINV-0.013)*BF6;
 BF26=max(0,0.013-TINV)*BF6;
 BF27=max(0,TINV-0.013)*BF11;
 BF28=max(0,0.013-TINV)*BF11;
 BF29=max(0,D-0.730)*BF14;
 BF30=max(0,0.730-D)*BF14;
 BF31=max(0,TINV-0.003)*BF12;
 BF33=max(0,KTINV-0.067)*BF13;
 BF35=max(0,KTINV-0.100);
 BF36=max(0,0.100-KTINV);
 BF37=max(0,TINV-0.006)*BF7;
 BF38=max(0,0.006-TINV)*BF7;
 BF39=max(0,D-0.280)*BF21;
 BF40=max(0,0.280-D)*BF21;
 BF41=max(0,D-0.290)*BF36;
 BF42=max(0,0.290-D)*BF36;
 BF43=max(0,KTINV-0.180);
 BF45=max(0,KTINV-0.167);
 BF47=max(0,KTINV-0.150)*BF13;
 BF49=max(0,KTINV-0.133);

 tennck=-1.733+0.125*BF1-0.180*BF2-0.246*BF3-1.053*BF4-30.263*BF5+28.588*BF6+0.652*BF7-8.051*BF8+29.063*BF9+1.470*BF11+7.002*BF13-2.155*BF14-23.462*BF15-42.180*BF16+0.873*BF17-0.616*BF18+161.669*BF19-41.613*BF20+2308.166*BF21+373.235*BF22-732.522*BF23-585.985*BF24+935.131*BF25+447.832*BF26+1082.659*BF27-134.489*BF28-4.216*BF29-1.292*BF30-122.623*BF31-392.093*BF33+0.520*BF35-27.588*BF37+389.849*BF38+907.880*BF39+1935.811*BF40+0.102*BF41-0.839*BF42+4.399*BF43-6.002*BF45+486.269*BF47-0.988*BF49;
  
          
 
 

          
          
          
 BF1=max(0,D-0.610);
 BF2=max(0,0.610-D);
 BF3=max(0,KTINV-0.013);
 BF4=max(0,0.013-KTINV);
 BF5=max(0,KTINV-0.012)*BF2;
 BF6=max(0,0.012-KTINV)*BF2;
 BF7=max(0,TINV-0.008);
 BF8=max(0,0.008-TINV);
 BF9=max(0,D-0.490)*BF8;
 BF10=max(0,0.490-D)*BF8;
 BF11=max(0,KTINV-0.003);
 BF12=max(0,0.003-KTINV);
 BF13=max(0,D-0.790);
 BF15=max(0,D-0.430)*BF12;
 BF16=max(0,0.430-D)*BF12;
 BF17=max(0,KTINV-0.038);
 BF18=max(0,0.038-KTINV);
 BF19=max(0,D-0.150)*BF8;
 BF21=max(0,D-0.510);
 BF23=max(0,D-0.190)*BF11;
 BF24=max(0,0.190-D)*BF11;
 BF25=max(0,D-0.190)*BF17;
 BF26=max(0,0.190-D)*BF17;
 BF27=max(0,D-0.580)*BF7;
 BF28=max(0,0.580-D)*BF7;
 BF29=max(0,D-0.690);
 BF31=max(0,TINV-0.004)*BF18;
 BF32=max(0,0.004-TINV)*BF18;
 BF33=max(0,TINV-0.007)*BF12;
 BF34=max(0,0.007-TINV)*BF12;
 BF35=max(0,KTINV-0.025)*BF7;
 BF36=max(0,0.025-KTINV)*BF7;
 BF37=max(0,TINV-0.008)*BF4;
 BF38=max(0,0.008-TINV)*BF4;
 BF39=max(0,D-0.430)*BF38;
 BF40=max(0,0.430-D)*BF38;
 BF41=max(0,KTINV-0.060)*BF7;
 BF43=max(0,KTINV-0.020);
 BF44=max(0,0.020-KTINV);
 BF45=max(0,TINV-0.008)*BF44;
 BF46=max(0,0.008-TINV)*BF44;
 BF47=max(0,KTINV-0.067)*BF7;
 BF49=max(0,KTINV-0.100)*BF21;
 BF50=max(0,0.100-KTINV)*BF21;

 tenck=-1.941+1.323*BF1-1.733*BF2-41.735*BF3+40.213*BF4+0.087*BF5-4.742*BF6-1.298*BF7-8.290*BF8-65.771*BF9+26.067*BF10+40.468*BF11-0.315*BF13-3.904*BF15-84.903*BF16+0.818*BF17+50.493*BF19-0.366*BF21-1.269*BF23+6.109*BF24+0.421*BF25-5.128*BF26+8.740*BF27+4.655*BF28-0.282*BF29+73.284*BF31-95.068*BF32+3560.022*BF33-2023.778*BF34-113.339*BF35-1232.850*BF36+116.992*BF37+975.335*BF38-743.527*BF39 -989.265*BF40+338.615*BF41+0.946*BF43+735.925*BF45-14.071*BF46-237.002*BF47+1.445*BF49-0.647*BF50;

            
           
 
 
 
 
 
 


            
 BF1=max(0,D-0.550);
 BF2=max(0,0.550-D);
 BF3=max(0,KTINV-0.013);
 BF4=max(0,0.013-KTINV);
 BF5=max(0,KTINV-0.012)*BF2;
 BF6=max(0,0.012-KTINV)*BF2;
 BF7=max(0,KTINV-0.053)*BF1;
 BF8=max(0,0.053-KTINV)*BF1;
 BF9=max(0,KTINV-0.003);
 BF10=max(0,0.003-KTINV);
 BF11=max(0,TINV-0.008);
 BF12=max(0,0.008-TINV);
 BF13=max(0,D-0.800)*BF10;
 BF14=max(0,0.800-D)*BF10;
 BF15=max(0,TINV-0.010)*BF9;
 BF16=max(0,0.010-TINV)*BF9;
 BF17=max(0,TINV-0.010)*BF5;
 BF18=max(0,0.010-TINV)*BF5;
 BF19=max(0,KTINV-0.035);
 BF20=max(0,0.035-KTINV);
 BF21=max(0,KTINV-0.001)*BF12;
 BF22=max(0,0.001-KTINV)*BF12;
 BF23=max(0,D-0.760)*BF20;
 BF24=max(0,0.760-D)*BF20;
 BF25=max(0,D-0.750)*BF22;
 BF26=max(0,0.750-D)*BF22;
 BF27=max(0,D-0.330)*BF12;
 BF28=max(0,0.330-D)*BF12;
 BF29=max(0,KTINV-0.008)*BF27;
 BF30=max(0,0.008-KTINV)*BF27;
 BF31=max(0,KTINV-0.071);
 BF32=max(0,0.071-KTINV);
 BF33=max(0,D-0.370)*BF32;
 BF34=max(0,0.370-D)*BF32;
 BF35=max(0,KTINV-0.033)*BF11;
 BF36=max(0,0.033-KTINV)*BF11;
 BF37=max(0,KTINV-0.050)*BF11;
 BF39=max(0,KTINV-0.023)*BF1;
 BF41=max(0,D-0.240)*BF9;
 BF43=max(0,KTINV-0.007)*BF12;
 BF44=max(0,0.007-KTINV)*BF12;
 BF45=max(0,D-0.600)*BF44;
 BF47=max(0,KTINV-0.018);
 BF49=max(0,D-0.240)*BF47;

 tenctk=-3.216+0.878*BF1-1.324*BF2-89.140*BF3+96.801*BF4+3.257*BF5-24.182*BF6-2.719*BF7+5.163*BF8+87.004*BF9-78.728*BF11+4.201*BF12+27.680*BF13-92.560*BF14+2579.853*BF15-2611.244*BF16-62.574*BF17+82.515*BF18+5.143*BF19+1863.680*BF21+24603.283*BF22-6.265*BF23-6.184*BF24-29139.049*BF25-40264.082*BF26-17.467*BF27+2.635*BF28+336.553*BF29 +4516.521*BF30+0.811*BF31+1.593*BF33-3.960*BF34-2862.335*BF35+2135.521*BF36+294.209*BF37+2.012*BF39-6.018*BF41 +777.310*BF43-6136.241*BF45+0.755*BF47+5.801*BF49;
     
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 if (sim==0) {
     
 	if (test==0){             
		print('Test Without Constant');
		print('1,5,10# Critical Values, No Lags');	
		critval=cbind(onenc0,fivenc0,tennc0);

	}else  {
		if (test==1){

			print('Test With Constant');
			print('1,5,10# Critical Values, No Lags');
			critval=cbind(onec0,fivec0,tenc0);

			}else {
				if (test==2){			
					print('Test with Constant and Trend');
					print('1,5,10# Critical Values, No Lags');
					critval=cbind(onect0,fivect0,tenct0);
					}
				}
		}
 	}  else { 
			if (sim==1){
			

			     if (test==0) {
		
					print('Test without Constant')
					print('1,5,10# Critical Values with Lags'); 
					critval=cbind(onenck,fivenck,tennck);
				} else {

					if (test==1){

						print('Test with Constant')
						print('1,5,10# Critical Values with Lags') 					
						critval=cbind(oneck,fiveck,tenck);

					} else {
						if (test==2) {

							print('Test with Constant and Trend')
							print('1,5,10# Critical Values with Lags'); 	
							critval=cbind(onectk,fivectk,tenctk);
  							}
						}
					}
				}
			}
     structure(list(onepercent=critval[1],fivepercent=critval[2],tenpercent=critval[3]))
 }