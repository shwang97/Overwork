/* 
Replication Package:  Labor Laws as Family Policy: The Effects of 'Overwork' on Lowest-Low Marriage and Fertility
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-08-04
Note: The file contains the analysis of number of childern and robustness checks
*/

clear
set more off
macro define DTA "WorkHours"
macro define graph "WorkHours"

***** 1. calculating the number of children from the household data *****
use $DTA/koweps_h01_19_long_250331, replace
forval x= 1/9 {
local v = (`x'-1)*12 + 3 
gen mem`x' = 1 if h01_`v'>= 11 & h01_`v' <=19 
replace mem`x'= 0 if mem`x'==.
}

forval x= 1/9 {
local v = (`x'-1)*12 + 3 
gen identify_`x'= h01_`v' if mem`x'==1
}
egen max = rowmax(identify_1 identify_2 identify_3 identify_4 identify_5 identify_6 identify_7 identify_8 identify_9)

gen child_num = max -10 
replace child_num =0 if child_num ==. 

forval j = 11/19 {
forval x= 1/9 {
local v = (`x'-1)*12 + 3 
gen mem`j'_`x' = 1 if h01_`v'>= 10*`j'+`x' & h01_`v' <=10*`j'+9 
replace mem`j'_`x'= 0 if mem`j'_`x'==.
}
}

forval j = 11/19 {
forval x= 1/9 {
local v = (`x'-1)*12 + 3 
gen identify_`j'_`x'= h01_`v' if mem`j'_`x'==1
}
}

forval j = 11/19 {
egen max_`j' = rowmax(identify_`j'_1 identify_`j'_2 identify_`j'_3 identify_`j'_4 identify_`j'_5 identify_`j'_6 identify_`j'_7 identify_`j'_8 identify_`j'_9)
}

forval j = 11/19 {
gen child_num_`j'= max_`j'-(`j'*10)
}

collapse (mean) child_num child_num_*, by(h_merkey year) 
save $DTA/child_num_new, replace

**** 2. Merging main data set with number of children 
use $DTA/koweps_hp01_19_long_250331, replace
keep if h_g2>=10
sort h_pid wv
bysort h_pid: carryforward np06_39, gen(f_educ)
bysort h_pid: carryforward np06_40, gen(m_educ)

merge m:1 h_merkey year using $DTA/child_num_new

gen child_num_fi = child_num if h_g2==10|h_g2==20

forval j = 11/19 {
replace child_num_fi = child_num_`j' if h_g2 == `j'	
}

forval j = 21/29 {
local v = `j'-10
replace child_num_fi = child_num_`v' if h_g2 == `j'	
}

sort h_pid year 
bysort h_pid: carryforward child_num_fi, replace

bysort h_pid: replace child_num_fi = child_num_fi[_n-1] if (child_num_fi<child_num_fi[_n-1]&_n>1&child_num_fi[_n-1]~=.) 

replace child_num_fi =0 if child_num_fi==. & h_g2>=10

***** 3. data cleaning *****
/// Creating the weekly-working-hours variable: 
/// Weekly working hours for those without regular schedule are calculated as (daily working hours × number of days worked per month) / 4.34. 
/// The information on working hours not surveyed in 2006. 
keep if year~=2006
gen Wh =. 
replace Wh= p02_8 if p02_8~=999
replace Wh = p02_9 * p02_7/4.34 if Wh ==. & p02_9~=99 & p02_7~=99
replace Wh = 0 if h_eco4>=7& h_eco4<=9
keep if Wh~=. 

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

// Drop missings in companysize; unemployed coded as zero. 
keep if h_eco10~=11 & h_eco10~=99
replace h_eco10=0 if h_eco10==. 

/// Covariates: sex, age, father and mother's education
gen age = year - h_g4
gen sex= h_g3 

replace f_educ =0 if f_educ==. 
replace m_educ =0 if m_educ ==. 

/// creating lagged variable
sort h_pid year 
bysort h_pid: gen child_num_fi_1 = child_num_fi[_n+1]

/// creating variable for effect hetergeniety analysis
replace sex = sex-1 
foreach x in Wh week law  {
cap drop `x'_int
gen `x'_int = `x' * sex	
}

/// age restriction
keep if age>=15 & age<=50

save $DTA/fertility, replace

***** 4. Main analysis  *****
use $DTA/fertility, replace

/// first stage equation 
reg Wh week law i.h_eco10 i.h_eco4 i.year  if child_num_fi_1~=., cluster(h_pid)
test (week=0)(law=0)

/// Ordinary Least Square (OLS) 
reg child_num_fi_1 Wh i.h_eco10 i.h_eco4 i.year, first cluster(h_pid)

/// Twoway Fixed Effects (TWFE)
xtset h_pid year
xtreg child_num_fi_1 Wh i.h_eco10 i.h_eco4 i.year, fe cluster(h_pid)

// Two-Stage Least Square (2SLS)
ivreg2 child_num_fi_1 (Wh=week law) i.h_eco10 i.h_eco4 i.year,  first cluster(h_pid)

/// Two-Stage Least Square (2SLS) with controls 
ivreg2 child_num_fi_1 (Wh=week law) i.h_eco10 i.h_eco4 i.year i.h_g3 i.h_g6 i.f_educ i.m_educ i.h_reg7, cluster(h_pid)

/// Two-Stage Residual Inclusion (2SRI)
reg Wh week law  i.h_eco10 i.h_eco4 i.year if child_num_fi_1~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

reg child_num_fi_1  c.Wh i.h_eco10 i.h_eco4 i.year c.resid, cluster(h_pid)
cap drop resid_2
predict resid_2 if e(sample), resid

reg resid_2 week law i.h_eco10 i.h_eco4 i.year
scalar Jstat = e(r2)*e(N)
display Jstat 

/// Correlated Random Coefficient (CRC)
reg Wh week law i.h_eco10 i.h_eco4 i.year if child_num_fi_1~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

reg child_num_fi_1 c.Wh i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh, cluster(h_pid)

/// Correlated Random Coefficient (CRC) with quadratic specification

reg Wh week law i.h_eco10 i.h_eco4 i.year if child_num_fi_1~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

reg child_num_fi_1 c.Wh##c.Wh i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh, cluster(h_pid)

margins, dydx(Wh) at(Wh=(40(1)80))

/// graph 
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

tw (rarea upper lower _j, color(gray*0.5))(line coef_ _j, lcolor(black) lwidth(0.3)), yline(0) xtitle("Working hours", size(vlarge)) ytitle("Effect estimates", height(2) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(40(10)80,nogrid notick labsize(vlarge)) ylabel(-0.06(0.02)0.06, nogrid labsize(large)) title("(b) Number of Children", size(vlarge)) plotregion(lcolor(black))

graph save $DTA/fertility_quadratic.gph, replace
restore 

// effect heterogeniety by gender 
/// 2SLS

reg Wh week law week_int law_int i.h_eco10 i.h_eco4 i.year i.sex

cap drop pred
predict pred if e(sample)

reg Wh_int week law week_int law_int i.h_eco10 i.h_eco4 i.year i.sex

cap drop pred_int
predict pred_int if e(sample)

reg child_num_fi_1 pred pred_int i.h_eco10 i.h_eco4 i.year i.sex, cluster(h_pid)

foreach x in SLS {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 2]
scalar `x'_se_2 = r(table)[2, 2]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}

/// 2SRI
reg Wh week law i.h_eco10 i.h_eco4 i.year i.sex if child_num_fi_1~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

reg child_num_fi_1 c.Wh c.Wh#i.sex i.sex i.h_eco10 i.h_eco4 i.year c.resid c.resid#i.sex, cluster(h_pid)

foreach x in SRI {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 3]
scalar `x'_se_2 = r(table)[2, 3]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}

//// CRC 
reg Wh week law i.h_eco10 i.h_eco4 i.year if child_num_fi_1~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

reg child_num_fi_1 c.Wh c.Wh#i.sex i.sex i.h_eco10 i.h_eco4 i.year c.resid c.resid#i.sex, cluster(h_pid)

foreach x in CRC {
scalar `x'_coef_1 = r(table)[1 ,1]
scalar `x'_se_1 = r(table)[2, 1]

scalar `x'_coef_2 = r(table)[1, 3]
scalar `x'_se_2 = r(table)[2, 3]

scalar `x'_coef_2 = `x'_coef_1 + `x'_coef_2 
scalar `x'_se_2 = sqrt(`x'_se_1^2 + `x'_se_2^2)
}


/// graph
preserve
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

graph save $DTA/fertility_2SLS.gph, replace

foreach x in SRI {
tw (rcap `x'_upper `x'_lower _j if _j==1, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==1, mcolor(black) msize(small) ) (rcap `x'_upper `x'_lower _j if _j==2, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==2, mcolor(black) msize(small) ), yline(0) xtitle("") ytitle("Effect estimates", height(1) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(0.5 " " 1 "Men" 2 "Women" 2.5 " ",nogrid notick labsize(vlarge)) ylabel(-0.1(0.05)0.1, nogrid labsize(large)) title("(b) 2SRI", size(vlarge)) plotregion(lcolor(black))
}

graph save $DTA/fertility_2SRI.gph, replace

foreach x in CRC {
tw (rcap `x'_upper `x'_lower _j if _j==1, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==1, mcolor(black) msize(small) ) (rcap `x'_upper `x'_lower _j if _j==2, lcolor(black) lcolor(black)) (scatter `x'_coef_ _j if _j==2, mcolor(black) msize(small) ), yline(0) xtitle("") ytitle("Effect estimates", height(1) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(0.5 " " 1 "Men" 2 "Women" 2.5 " ",nogrid notick labsize(vlarge)) ylabel(-0.1(0.05)0.1, nogrid labsize(large)) title("(c) CRC", size(vlarge)) plotregion(lcolor(black))
}

graph save $DTA/fertility_CRC.gph, replace
restore 

***** 4. robustness checks *****
// creating variables for robustness checks
gen married = 1 if h_g10>=1 & h_g10 <=4 
replace married = 0 if h_g10 ==5|h_g10==0

// the effect of working hour on marriage probabiltiy
/// 2SLS 
ivreg2 married (Wh=week law) i.h_eco10 i.h_eco4 i.year ,  first cluster(h_pid)

/// probit control function 
reg Wh week law i.h_eco10 i.h_eco4 i.year if married ~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

glm married c.Wh i.h_eco10 i.h_eco4 i.year c.resid , family(binomial) link(probit) cluster(h_pid)
margins, dydx(Wh)

cap drop resid_2
predict resid_2 if e(sample), res

reg resid_2 week law i.h_eco10 i.h_eco4  i.year
scalar Jstat = e(r2)*e(N)
display Jstat 

/// Probit correlated random coefficient 
reg Wh week law i.h_eco10 i.h_eco4 i.year if married ~=., cluster(h_pid)
cap drop resid
predict resid if e(sample), resid

glm married c.Wh i.h_eco10 i.h_eco4 i.year c.resid c.resid#c.Wh , family(binomial) link(probit) cluster(h_pid)

margins, dydx(Wh)

/// Negative control outcomes
tsset h_pid year
reg L.married week law i.h_eco10 i.h_eco4 i.year , cluster(h_pid)
test week law

reg L.child_num_fi week law i.h_eco10 i.h_eco4 i.year, cluster(h_pid)

//// IV-(Dynamic) DID
preserve 
keep if child_num_fi_1!=. 

/// creating group indicator
cap drop compsize
gen compsize = 1 if h_eco10>=2&h_eco10<3 & h_eco4 >=1 & h_eco4<=2
replace compsize = 2 if h_eco10>=3&h_eco10<5  & h_eco4 >=1 & h_eco4<=2
replace compsize = 3 if h_eco10>=5&h_eco10<7 & h_eco4 >=1 & h_eco4<=2
replace compsize = 4 if h_eco10>=7&h_eco10<8 & h_eco4 >=1 & h_eco4<=2
replace compsize = 5 if h_eco10>=8&h_eco10<10 & h_eco4 >=1 & h_eco4<=2
replace compsize = 6 if h_eco10==10 & h_eco4 >=1 & h_eco4<=2
replace compsize = 0 if compsize==.

/// creating variable indicating eligibiliy for either five-day workweek or 52 hours workweek regulation
gen inst = 1 if week==1 |law==1
replace inst=0 if inst==. 

foreach x in married child_num_fi_1 {
capture program drop did_ratio
program define did_ratio, rclass             
    version 18
    quietly {
        /* --- First DiD regression --- */
        didregress (Wh) (inst), ///
            group(compsize) time(year) ///
            vce(cluster h_pid)
        tempname b1
        scalar   `b1' = r(table)[1,1]       

        /* --- Second DiD regression --- */
        didregress (`x') (inst), ///
            group(compsize) time(year) ///
            vce(cluster h_pid)
        tempname b2
        scalar   `b2' = r(table)[1,1]       

        /* --- Ratio and return value --- */
        return scalar ratio = `b2' / `b1'
    }
end


set seed 12345                            
bootstrap r(ratio),                        ///
         reps(500)                        /// 
         cluster(h_pid)                    /// 
         saving("`x'_DID.dta", replace): ///
    did_ratio
}
restore


/// Falsification tests 
/// preparing permutations
/// Note: We iterated this process for the 52-hour work-week regulation and also for the marriage sample.
cap drop compsize
gen compsize = 1 if h_eco10>=2&h_eco10<3 & h_eco4 >=1 & h_eco4<=2
replace compsize = 2 if h_eco10>=3&h_eco10<5  & h_eco4 >=1 & h_eco4<=2
replace compsize = 3 if h_eco10>=5&h_eco10<7 & h_eco4 >=1 & h_eco4<=2
replace compsize = 4 if h_eco10>=7&h_eco10<8 & h_eco4 >=1 & h_eco4<=2
replace compsize = 5 if h_eco10>=8&h_eco10<10 & h_eco4 >=1 & h_eco4<=2
replace compsize = 6 if h_eco10==10 & h_eco4 >=1 & h_eco4<=2
replace compsize = 0 if compsize==.

local iterations = 5000

/// Create storage for coefficient draws
gen coef_week_reduced = . 
gen coef_law_reduced = . 


/// Loop over number of pseudo-iterations 
forval x = 1/`iterations' {
	
    tempname rweek rlaw
    preserve
        clear
        set obs 7
        gen cat = _n

        gen random_week = runiformint(2005,2012)
        gen random_law  = runiformint(2019,2022)
	
        replace random_week=0 if _n==7
	replace random_law=0 if _n==7

        forvalues i = 1/7 {
	local v = `i'-1
            scalar rweek_`v' = random_week[`i']
            scalar rlaw_`v'  = random_law[`i']
        }
    restore
    
    cap drop week_re
    cap drop law_re
    
    gen week_re = .
    forvalues i = 0/6 {
        replace week_re = 1 if compsize==`i' & year >= rweek_`i'
    }
    replace week_re = 0 if week_re==.
    
    gen law_re = .
    forvalues i = 0/6 {
        replace law_re = 1 if compsize==`i' & year >= rlaw_`i'
    }
    replace law_re = 0 if law_re==.
   
    // Regress using these newly assigned indicators
    quietly reg Wh week_re law i.h_eco10 i.h_eco4 i.year
    
    // Save coefficients from this iteration
    scalar coef_week_reduced_`x' = _b[week_re]
    replace coef_week_reduced = coef_week_reduced_`x' if _n == `x'
}

keep  coef_week_reduced
save $DTA/fertility_placebo_new_1, replace 


