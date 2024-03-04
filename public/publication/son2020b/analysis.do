/*

Project title: Interpersonal Trust and Confidence in Labor Union in South Korea
Date: JEAS 2020 (https://ben-son.netlify.app/publication/son2020b/)
Take: Analysis (do-file)
	- performs all the primary analyses
	- generates figures and tables

*/

global data "[local path to the directory where the data file is located]"



clear

cd "$data"
use data


capture log close
log using trust_labor, replace
* descriptive data
** summary stats

reg labor ideo govt trust income  male age edu econsat demage
estat sum

cibar labor if kor==1 & S020>1988, over1(S020) /// 
	graphopt(scheme(s1color) ///
	xtitle("Survey Year") xlabel(1 "1990" 2 "1996" 3 "2001" 4 "2005" 5 "2010") ///
	ylabel(0(1)3, grid) saving(confidence, replace) /// 
	graphr(ic(white) fc(white) lc(white)) /// 
			plotr(ic(white) fc(white) lc(white)) /// 
			legend(off) ytitle("Confidence in Labor Unions")) 
	 
	gr export confidence.png, replace


* structural equation modeling
capture log off
log using sem1, replace
local control "ideo male age edu income econsat life"
local wave " 2 4 5 6" /* wave3 not available for _life_ */
foreach X of local wave {
gsem (labor <- trust `control', ologit) ///
	(trust <- life `control', logit) if S002==`X', ///
	vce(robust)
	
	eststo life`X'
	
	nlcom _b[labor:trust]*_b[trust:life]
	}
	*
	
local control "ideo male age edu income econsat"
local wave " 2 3 4 5 6"
foreach X of local wave {
gsem (labor <- trust `control', ologit) ///
	(trust <-  `control', logit) if S002==`X', ///
	vce(robust)
	
	eststo sat`X'
	
	nlcom _b[labor:trust]*_b[trust:econsat]
	}
	*

log close





********************************************************************
* causal mediation: alternative to structural equation model

** for medef tables

local wave "2 3 4 5 6"
foreach X of local wave {
logit trust econsat income  male edu age ideo if S002==`X', r

eststo up`X'
}
*

local wave " 2 3 4 5 6"
foreach X of local wave {
medeff (logit trust econsat income  male edu age ideo) /// 
	(logit labor2 trust econsat ideo income  male edu /// 
	age) if S002==`X', ///
	treat(econsat) mediate(trust) sims(2000) seed(2) vce(robust)
	
	eststo med`X'
	}
	*

	* sensitivity
	
	medsens (logit trust econsat income  male edu age ideo) /// 
			(regress labor trust econsat ideo income  male edu /// 
			age) if S020==2001, level(90) ///
			treat(econsat) mediate(trust) sims(1000)
	
	
	medsens (logit trust econsat income  male edu age ideo) /// 
			(regress labor trust econsat ideo income  male edu /// 
			age) if S020==2005, level(90) ///
			treat(econsat) mediate(trust) sims(1000)
*************************************************************************
	
*** Benchmark		
* trust-government ideology Model	


meologit labor ideo govt trust income  male age edu econsat demage /// 
	|| S002:  , vce(robust) cov(unstructured)

	eststo bench1	
	
	tab labor, gen(labor_2)
	gr bar labor_21 labor_22 labor_23 labor_24 if S020 !=1982, over(S020) ///
		scheme(s1color) graphr(ic(white) fc(white) lc(white)) /// 
				plotr(ic(white) fc(white) lc(white)) ///
				 ylabel(0 0.2 0.4 0.6 0.8 1.0, grid) saving(union, replace) /// 
				 legend(order(1 "none" 2 "not much" 3 "quite" 4 "great"))
				 
				 gr export union.png, replace
		
meologit labor ideo i.govt##i.trust income  male age edu econsat demage /// 
	|| S002:  , vce(robust) cov(unstructured)
	
	eststo bench2
		
		margins, dydx(trust) at(govt=(0) )
			marginsplot, saving(temp1, replace) ///
				recast(scatter) scheme(s1color) ///
				ytitle("") title("A. Conservative Government") xtitle("") ///
				xlabel(0 "none" 1 "not much" 2 "quite" 3 "great") /// 
				yline(0, lp(dash) lc(red)) /// 
				ylabel(-0.05 -0.03 -0.01 0.01 0.03 0.05, grid) ///
				graphr(ic(white) fc(white) lc(white)) /// 
				plotr(ic(white) fc(white) lc(white)) 
	

		margins, dydx(trust) at(govt=(1) )
			marginsplot, saving(temp2, replace) ///
			recast(scatter) scheme(s1color) ///
				ytitle("") title("B. Liberal Government") xtitle("") ///
				xlabel(0 "none" 1 "not much" 2 "quite" 3 "great") /// 
				yline(0, lp(dash) lc(red)) /// 
				ylabel(-0.05 -0.03 -0.01 0.01 0.03 0.05, grid) ///
				graphr(ic(white) fc(white) lc(white)) /// 
				plotr(ic(white) fc(white) lc(white)) 

		gr combine temp1.gph temp2.gph, ycommon scheme(s1color) /// 
			l1title("Marginal Effect of Trust") /// 
			b1title("Confidence in Labor Union") saving(marginal)
			
			gr export marginal.png, replace
		
		cibar trust if e(sample), over1(econsat2) over2(S020)  level(90) ///
			graphopt(scheme(s1mono) ///
			legend(order(1 "dissaatisfied" 2 "satisfied")) ylabel(, grid) ///
			saving(sat_trust, replace) /// 
			graphr(ic(white) fc(white) lc(white)) /// 
			plotr(ic(white) fc(white) lc(white)) ///
			ytitle("") title(""))
			
				gr export bar_trust.png, replace
				
				
		cibar labor if e(sample), over1(trust) over2(S020) level(90) /// 
			graphopt(scheme(s1color) ///
			legend(order(1 "no trust" 2 "trust")) ylabel(, grid) ///
			saving(bar_trust, replace) /// 
			graphr(ic(white) fc(white) lc(white)) /// 
			plotr(ic(white) fc(white) lc(white)) ///
			ytitle("") title("")) 
				gr export sat_trust.png, replace
			
		gr combine sat_trust.gph bar_trust.gph , c(2) scheme(s1color)
		gr export trust_combined.png, replace
		
		
local control "ideo male age edu income econsat"
local year "1990 2001 2005 2010"

foreach X of local year {
gsem (labor <- trust `control', ologit) ///
	 (labor <- life `control', ologit) ///
	 (trust <- life `control', ologit) if S020==`X', vce(robus)
	 
	 eststo sem`X'
}
*	
* level-2 disaggregatge

local wave "2 3 4 5 6"
foreach X of local wave {
ologit labor trust ideo income econsat  male age edu /// 
	if S002 == `X' , r	
	eststo disag`X'
		}
*

local wave "2 3 4 5 6"
foreach X of local wave {
ologit labor trust ideo income econsat  male age edu /// 
	if S002 == `X' , r	
	est store eq`X'
		}

*
coefplot (eq2, label(1990) lp(dash))(eq3, label(1996)lp(dash))(eq4, label(2001)) ///
	(eq5, label(2005))(eq6, label(2010) lp(dash)), drop(_cons) ///
	xline(0, lp(dash)) xlabel(, grid) scheme(s1color) ///
	keep(trust) legend(c(5)) /// 
	saving(coefplot, replace) /* grid(between /// 
	glpattern(dash) glwidth(*2) glcolor(gray)) */

	gr export coefplot.png, replace
	
** TABLES

*1. benchmark	
esttab bench1 bench2 using t1, tex brackets b(3) se(3) sca(ll df_m) /// 
	replace label wide aic bic transform(ln*: exp(@)^2 exp(@)^2) /// 
	eqlabels("" "Level 1 Variance" "Level 2 Variance")

	
*2. Causal Mediation
esttab life2 life4 life5 life6 using life, /// 
	tex brackets b(3) se(3) sca(ll) /// 
	replace label aic bic
esttab sat2 sat3 sat4 sat5 sat6 using sat, /// 
	tex brackets b(3) se(3) sca(ll) /// 
	replace label aic bic	



*** Appendix TABLES
* A1. disagregate level
esttab disag2 disag3 disag4 disag5 disag6 using A1, /// 
	tex brackets b(3) se(3) sca(ll df_m) /// 
	replace label aic bic	
	
* A2. GSEM	
esttab gsem using A2, tex brackets b(3) se(3) sca(ll df_m) /// 
	replace label aic bic transform(ln*: exp(@)^2 exp(@)^2) /// 
	eqlabels("" "Level 1 Variance" "Level 2 Variance")	
	
	
	
log close	
	
	
	
	
	
/* 

trust-personal ideology Model

: works with multi-level models, but doesn't when disaggregated.
*/
meologit labor c.ideo##i.trust income  male age edu econsat demage /// 
	|| S002: trust , vce(robust) cov(unstructured)
	
	margins, dydx(trust) at(ideo=(1(1)10))	
	marginsplot, recast(line)  recastci(rarea) /// 
		scheme(s1color) saving(trust, replace) /// 
		title("")   ///
		plot1opt(lw(thick)lc(red) lp(dash)) ci1opts(color(red*0.1)) /// 
		plot2opt(lw(thick)lc(red)) ci2opts(color(red*0.3)) ///
		plot3opt(lw(thick)lc(blue) ) ci3opts(color(blue*0.1)) /// 
		plot4opt(lw(thick)lc(blue) lp(dash)) ci4opts(color(blue*0.3)) ///
		xtitle("Ideology: right-left") ///
		ytitle("marginal effect of Trust") /// 
		ylabel(, grid) xlabel(, grid) ///
		legend(order(5 "no confidence" 6 "little confidence" /// 
		7 "much confidence" 8 "great confidence"))
	
		gr export trust.png, replace
	
	
gr combine trust2.gph trust3.gph /// 
	trust4.gph trust5.gph trust6.gph ///
	, c(5) ycommon scheme(s1color) saving(trust_combine, replace) ///
	l1title("marginal effect of trust") /// 
	b1title("Ideology: right - left")
	
	gr export trust_combine.png, replace
	

*******************
twoway (scatter mlabor year if S003 !=410 & S003 !=32, ///
	msymbol(Oh) msize(large)) ///
	(scatter mlabor year if S003==410, ///
	msymbol(O) msize(large) mcolor(blue)) /// 
	(scatter mlabor year if S003==32, ///
	msymbol(O) msize(large) mcolor(red)) /// 
	if mlabor>0 & year>1978 & year<2016, ///
	ytitle("Confidence in Labor") /// 
	legend(order(1 "Rest of the World" 2 "South Korea" 3 "Argentina") ///
	pos(7) ring(0)) ///
	yline(1.0262) text(0.74 1982 "Global Mean")  ///
	xlabel(1980 1990 2000 2010 2015)
		
	gr export compare.eps, replace
	
