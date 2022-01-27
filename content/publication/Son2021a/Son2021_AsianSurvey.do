* Table 2 (the primary GARCH estimates) and Figure 5
* load the data

set more off
ssc install plotbeta, replace

tset d

* Model 1
arch dlnkrw l.after ashock dshock ipol, ///
	arch(1/1) garch(1/1)  /// 
	 het(d.yen ipol election /// 
	 f.election sewol) gtolerance(999) ///
	dfp distribution(normal)  vce(robust) ar(1) 
	
	eststo nointer 
	
* Model 2	
arch dlnkrw l.after ashock dshock ai ipol, ///
	arch(1/1) garch(1/1)  /// 
	 het(d.yen ipol election /// 
	 f.election sewol) gtolerance(999) ///
	dfp distribution(normal)  vce(robust) ar(1) 
	
	eststo ainter 
	
* model 3
arch dlnkrw l.after ashock dshock di ipol, ///
	arch(1/1) garch(1/1)  /// 
	 het(d.yen ipol election /// 
	 f.election sewol) gtolerance(999) ///
	dfp distribution(normal)  vce(robust) ar(1) 
	
	eststo dinter 
	
plotbeta ipol | ipol + di , ///
			vertical yline(0, lp(dash)) /// 
			xlabel(0 " " 1 "No" 2 "Yes" 3 " ", grid labc(black)) /// 
			xlabel(none, axis(2)) xtitle("Negative Approval Rating Shock", c(black)) ///
			scheme(s1color) name(marginal2a, replace) ylabel(, grid) level(90)
	



* model 4
arch dlnkrw l.after ashock dshock ai di ipol, ///
	arch(1/1) garch(1/1)  /// 
	 het(d.yen ipol election /// 
	 f.election sewol) gtolerance(999) ///
	dfp distribution(normal)  vce(robust) ar(1) 	
	
	eststo bothinter 

plotbeta ipol | ipol + ai , ///
			vertical yline(0, lp(dash)) /// 
			xlabel(0 " " 1 "No" 2 "Yes" 3 " ", grid labc(black)) /// 
			xlabel(none, axis(2)) xtitle("Positive Approval Rating Shock", c(black)) ///
			scheme(s1color) name(marginal2b, replace) ylabel(, grid) level(90)
		
		gr combine marginal2b marginal2a , ycommon // Figure 5
	

esttab 	nointer ainter  dinter bothinter, b(3) se(3) star(* 0.1 ** 0.05)
   
   		
	
