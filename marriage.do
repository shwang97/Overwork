/* 
Replication Package:  Labor Laws as Family Policy: The Effects of 'Overwork' on Lowest-Low Marriage and Fertility
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-08-04
Note: The file contains the analysis of marriage timing
*/


clear
set more off
macro define DTA "WorkHours"
macro define graph "WorkHours"


***** 1. Load and prepare data *****
use $DTA/koweps_hp01_19_long_250331, replace

/// Coded as 1  if married, separted, divorced, widowed, and 0 otherwise. 
gen married = 1 if h_g10>=1 & h_g10 <=4 
replace married = 0 if h_g10 ==5|h_g10==0
keep if married~=. 

/// Creating the variable indicating eligibility for five-day workweek
gen week =. 
replace week =1 if year>=2005 &h_eco10==10 & h_eco4 >=1 & h_eco4<=2
replace week =1 if year>=2006 &h_eco10>=8&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace week =1 if year>=2007 &h_eco10>=7&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace week =1 if year>=2008 &h_eco10>=5&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace week =1 if year>=2009 &h_eco10>=3&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace week =1 if year>=2012 &h_eco10>=2&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace week =0 if week ==. 

/// Creating the variable indicating eligibility for 52 hours workweek
gen law=.
replace law = 1 if year>=2019 &h_eco10>=8&h_eco10<=10& h_eco4 >=1 & h_eco4<=2
replace law = 1 if year>=2021 &h_eco10>=5&h_eco10<=10 & h_eco4 >=1 & h_eco4<=2
replace law = 1 if year>=2022 &h_eco10>=2&h_eco10<=10& h_eco4 >=1 & h_eco4<=2
replace law = 0 if law ==.

/// Creating the weekly-working-hours variable: 
/// Weekly working hours for those without regular schedule are calculated as (daily working hours × number of days worked per month) / 4.34. 
/// The information on working hours not surveyed in 2006. 
drop if year==2006
gen Wh =. 
replace Wh= p02_8 if p02_8~=999
replace Wh = p02_9 * p02_7/4.34 if Wh ==. & p02_9~=99 & p02_7~=99
replace Wh = 0 if h_eco4>=7& h_eco4<=9 
keep if Wh~=.

// Drop missings in companysize; unemployed coded as zero. 
keep if h_eco10~=11 & h_eco10~=99
replace h_eco10=0 if h_eco10==. 

/// Covariates: sex, age, father and mother's education
gen sex = h_g3
gen age = year - h_g4
gen status = married

sort h_pid wv
bysort h_pid: carryforward np06_39, gen(f_educ)
bysort h_pid: carryforward np06_40, gen(m_educ)

replace f_educ =0 if f_educ==. 
replace m_educ =0 if m_educ ==. 

/// Sample selection for marriage analysis 
/// include respondents who were unmarried at thier survey participation
cap drop tag
sort h_pid wv
bysort h_pid: gen tag = married == 0 if _n == 1
bysort h_pid: carryforward tag, replace
keep if tag==1 

//// code duration variable for survival analysis
sort h_pid wv 
by h_pid: gen marriage_age =age if married ==1 
by h_pid: replace marriage_age = marriage_age[_n-1] if married[_n-1]==1 
replace marriage_age=age if marriage_age==.

/// drop observations after marriage occurs
forval x = 1/20 {
by h_pid: drop if married[_n-1]==1 & _n>1
}

/// age restriction
keep if marriage_age>=15 & marriage_age<=50
sort h_pid year

/// the onset of risk at age 15 
replace marriage_age = marriage_age-15
replace marriage_age = 0.5 if marriage_age ==0

save $DTA/marriage, replace
***** 2. Main analysis  *****

use $DTA/marriage, replace
/// first stage relevance (F-statistic)
reg Wh week law i.h_eco10 i.h_eco4 i.year , cluster(h_pid) 
test (week=0) (law=0)
/// Calculating KP-LM statistic
ivreg2 marriage_age (Wh=week law) i.h_eco10 i.h_eco4 i.year, cluster(h_pid)

/// check model fit: AIC and BIC by distribution 
foreach x in exponential weibull lognormal loglogistic {
stset marriage_age, failure(married) id(h_pid)
streg , distribution(`x') time iter(50) cluster(h_pid)
estimates store `x'
}
estimates stats _all

/// Accelerated Failure Time (AFT) without insturment
stset marriage_age, failure(married) id(h_pid)
streg c.Wh i.h_eco10 i.h_eco4 i.year, distribution(lognormal) time iter(50) cluster(h_pid)

/// overidentification test
reg Wh week law i.h_eco10 i.h_eco4 i.year , cluster(h_pid)
cap drop pred
predict pred if e(sample),

stset marriage_age, failure(married) id(h_pid)
streg c.pred law i.h_eco10 i.h_eco4 i.year, distribution(lognormal) time iter(50) cluster(h_pid)

/// Two-Stage Least Square (2SLS)
reg Wh week law i.h_eco10 i.h_eco4 i.year, cluster(h_pid)
cap drop pred
predict pred if e(sample),
test (week=0) (law=0)

stset marriage_age, failure(married) id(h_pid)
streg c.pred i.h_eco10 i.h_eco4 i.year, distribution(lognormal) time iter(50) cluster(h_pid)

/// Two-Stage Least Square (2SLS) with controls 
streg c.pred i.h_eco10 i.h_eco4 i.year i.h_g3 i.h_g6 i.f_educ i.m_educ i.h_reg7, distribution(lognormal) time iter(50) cluster(h_pid)

/// Two-Stage Residual Inclusion (2SRI)
reg Wh week law i.h_eco10 i.h_eco4 i.year, cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

stset marriage_age, failure(married) id(h_pid) 
streg c.Wh i.h_eco10 i.h_eco4 i.year c.resid , distribution(lognormal) time iter(50) cluster(h_pid)

/// Correlated Random Coefficient (CRC) 
reg Wh week law i.h_eco10 i.h_eco4 i.year, cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

stset marriage_age, failure(married) id(h_pid)
streg c.Wh i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh , distribution(lognormal) time iter(50) 

///// Correlated Random Coefficient (CRC) with quadratic specification
reg Wh week law i.h_eco10 i.h_eco4 i.year, cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

stset marriage_age, failure(married) id(h_pid)
streg c.Wh##c.Wh i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh , distribution(lognormal) time iter(50) cluster(h_pid)

margins, at(Wh=(40(1)80)) expression(_b[c.Wh]+ 2*_b[c.Wh#c.Wh]*(Wh)) 
 
/// graph for quadratic specification
preserve 
forval x = 1/41 {
scalar coef_`x'= r(table)[1, `x']
gen coef_`x'=coef_`x'
scalar se_`x'= r(table)[2, `x']	
gen se_`x'=se_`x'
}

collapse (mean) coef_* se_* 
gen id = 1 

reshape long coef_ se_, i(id)
gen upper = coef_ + 1.96 * se_ 
gen lower = coef_ - 1.96 * se_ 

replace _j = _j+39
graph set window fontface "times new roman"
tw (rarea upper lower _j, color(gray*0.5))(line coef_ _j, lcolor(black) lwidth(0.3)), yline(0) xtitle("Working hours", size(vlarge)) ytitle("Effect estimates", height(2) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(40(10)80,nogrid notick labsize(vlarge)) ylabel(-0.15(0.05)0.15, nogrid labsize(large)) title("(a) Marriage Timing", size(vlarge)) plotregion(lcolor(black))
restore

// effect heterogeniety by gender 
replace sex = sex-1 
foreach x in Wh week law  {
cap drop `x'_int
gen `x'_int = `x' * sex	
}

*** 2SLS
reg Wh week law week_int law_int i.h_eco10 i.h_eco4 i.year i.sex

cap drop pred
predict pred if e(sample)

reg Wh_int week law week_int law_int i.h_eco10 i.h_eco4 i.year i.sex

cap drop pred_int
predict pred_int if e(sample)
stset marriage_age, failure(married) id(h_pid)
streg c.pred c.pred_int i.h_eco10 i.h_eco4 i.year i.sex, distribution(lognormal) time iter(50) cluster(h_pid)

foreach x in SLS {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 2]
scalar `x'_se_2 = r(table)[2, 2]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}

*** 2SRI
reg Wh week law i.h_eco10 i.h_eco4 i.year i.sex, cluster(h_pid)

cap drop resid
predict resid if e(sample), resid

stset marriage_age, failure(married) id(h_pid) 
streg c.Wh c.Wh#i.sex i.h_eco10 i.h_eco4 i.year c.resid c.resid#i.sex i.sex, distribution(lognormal) time iter(50) cluster(h_pid)

foreach x in SRI {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 3]
scalar `x'_se_2 = r(table)[2, 3]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}

*** CRC 
reg Wh week law i.h_eco10 i.h_eco4 i.year i.sex, 
cap drop resid
predict resid if e(sample), resid

stset marriage_age, failure(married) id(h_pid)
streg c.Wh c.Wh#i.sex i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh i.sex i.sex#c.resid, distribution(loglogistic) time iter(50) 


foreach x in CRC {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 3]
scalar `x'_se_2 = r(table)[2, 3]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}


/// graph combining 2SLS, 2SRI, CRC
clear 
set obs 1 
foreach x in SLS SRI CRC {
foreach v in coef se {
gen `x'_`v'_1 = `x'_`v'_1
gen `x'_`v'_2 = `x'_`v'_2
}		
}

gen id = 1
reshape long SLS_coef_ SLS_se_ SRI_coef_ SRI_se_ CRC_coef_ CRC_se_ , i(id)

foreach x in SLS SRI CRC {
gen `x'_upper = `x'_coef_ + 1.96*`x'_se_
gen `x'_lower = `x'_coef_ - 1.96*`x'_se_
}
graph set window fontface "Times new roman"

foreach x in SLS {
tw (rcap `x'_upper `x'_lower _j if _j==1, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==1, mcolor(black) msize(small) ) (rcap `x'_upper `x'_lower _j if _j==2, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==2, mcolor(black) msize(small) ), yline(0) xtitle("") ytitle("Effect estimates", height(1) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(0.5 " " 1 "Men" 2 "Women" 2.5 " ",nogrid notick labsize(vlarge)) ylabel(-0.1(0.05)0.1, nogrid labsize(large)) title("(b) 2SLS", size(vlarge)) plotregion(lcolor(black))
}

graph save $DTA/marriage_2SLS.gph, replace

foreach x in SRI {
tw (rcap `x'_upper `x'_lower _j if _j==1, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==1, mcolor(black) msize(small) ) (rcap `x'_upper `x'_lower _j if _j==2, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==2, mcolor(black) msize(small) ), yline(0) xtitle("") ytitle("Effect estimates", height(1) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(0.5 " " 1 "Men" 2 "Women" 2.5 " ",nogrid notick labsize(vlarge)) ylabel(-0.1(0.05)0.1, nogrid labsize(large)) title("(b) 2SRI", size(vlarge)) plotregion(lcolor(black))
}

graph save $DTA/marriage_2SRI.gph, replace

foreach x in CRC {
tw (rcap `x'_upper `x'_lower _j if _j==1, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==1, mcolor(black) msize(small) ) (rcap `x'_upper `x'_lower _j if _j==2, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==2, mcolor(black) msize(small) ), yline(0) xtitle("") ytitle("Effect estimates", height(1) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(0.5 " " 1 "Men" 2 "Women" 2.5 " ",nogrid notick labsize(vlarge)) ylabel(-0.1(0.05)0.1, nogrid labsize(large)) title("(c) CRC", size(vlarge)) plotregion(lcolor(black))
}

graph save $DTA/marriage_CRC.gph, replace
