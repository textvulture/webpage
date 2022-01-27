use healthcost.dta, clear // load the data


xtset ccode year	

xtpcse 	lnhealth  auto  ///
	i.ccode i.year c.year ///
	, c(psar1) pairwise
	
	eststo r0
	
xtpcse 	lnhealth  auto  ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc  ///
	i.ccode c.year i.year ///
	, c(psar1) pairwise
	
	eststo r1
	
xtpcse lninf auto  ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc ///
	i.ccode i.year c.year ///
	, c(psar1) pairwise
	
	eststo r2
	
xtpcse ihme_lifexp_allf	auto  ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc ///
	i.ccode i.year c.year ///
	, c(psar1) pairwise

	eststo r3
	
xtpcse 	lnhealth vauto2   ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc ///
	i.ccode i.year c.year ///
	, c(psar1) pairwise

	eststo v
	
* horizontal accountability
xtpcse 	lnhealth hauto2   ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc ///
	i.ccode i.year c.year ///
	, c(psar1) pairwise	

	eststo h


		
esttab r0 r1  h v r2 r3 , ///
	b(3) se(3) star(* 0.10 ** 0.05) r2 ///
	keep(auto  vauto2 hauto2  ///
	leftist gender lnedisum ///
	growth lnGDP lnGDPpc) replace
	
