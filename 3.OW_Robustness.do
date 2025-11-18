/* 
Replication Package: Labor laws as family policy
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-11-18
Note: The file contains the code for the robustness checks in the appendix figures/tables
*/

clear
set more off
macro define DTA "WorkHours"
macro define graph "WorkHours"

use $DTA/, replace

*--------------------------------------------------------------*
* Appendix 3. effect heterogeniety analysis by company size
* creating bianry company size 
* comp_ca = 1 if compsize >= 50
* comp_ca = 0 compsize < 50
*--------------------------------------------------------------*

gen comp_ca = compsize 
recode comp_ca (0/4=0)(5/7=1)

gen Wh_comp=Wh*comp_ca 
gen week_comp =week* comp_ca
gen law_comp =law * comp_ca

set more off
local cross "†"   

* Instruments and controls (match your specs)
local Zc   "week law week_comp law_comp"
local BASE "i.compsize i.h_eco4 i.year"
local COV  "i.compsize i.h_eco4 i.year i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ"
*================ PANEL A: 2SLS  MARRIAGE =================

eststo clear

* ---------- Model 1 (BASE) ----------
quietly ivreg2 married (Wh Wh_comp = `Zc') `BASE', cluster(h_pid) first
local KPF1 : display %9.2f e(rkf)
local KPL1 : display %9.2f e(idstat)
local  J1  : display %9.2f e(j)
local JP1  : display %9.3f e(jp)
eststo M1: ivreg2 married (Wh Wh_comp = `Zc') `BASE', cluster(h_pid)
estimates restore M1
estadd scalar KP_F  = `KPF1'
estadd scalar KP_LM = `KPL1'
estadd scalar HJ    = `J1'
estadd scalar HJ_p  = `JP1'

* ---------- Model 2 (+ covariates) ----------
quietly ivreg2 married (Wh Wh_comp = `Zc') `COV', cluster(h_pid) first
local KPF2 : display %9.2f e(rkf)
local KPL2 : display %9.2f e(idstat)
local  J2  : display %9.2f e(j)
local JP2  : display %9.3f e(jp)
eststo M2: ivreg2 married (Wh Wh_comp = `Zc') `COV', cluster(h_pid)
estimates restore M2
estadd scalar KP_F  = `KPF2'
estadd scalar KP_LM = `KPL2'
estadd scalar HJ    = `J2'
estadd scalar HJ_p  = `JP2'

* ---------- Model 3 (as written; same as BASE) ----------
quietly ivreg2 married (Wh Wh_comp = `Zc') `COV' [pw=weight], cluster(h_pid) first
local KPF3 : display %9.2f e(rkf)
local KPL3 : display %9.2f e(idstat)
local  J3  : display %9.2f e(j)
local JP3  : display %9.3f e(jp)
eststo M3: ivreg2 married (Wh Wh_comp = `Zc') `COV' [pw=weight], cluster(h_pid)
estimates restore M3
estadd scalar KP_F  = `KPF3'
estadd scalar KP_LM = `KPL3'
estadd scalar HJ    = `J3'
estadd scalar HJ_p  = `JP3'

* ---- Export Panel A ----
esttab M1 M2 M3 using $DTA/.rtf, replace ///
    title("Panel A. 2SLS  Marriage (endogenous: Wh, Wh×compsize; instruments: week, law, week×compsize, law×compsize)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh Wh_comp) order(Wh Wh_comp) ///
    coeflabels(Wh "Main effect: Wh" Wh_comp "Interaction: Wh × compsize") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

*========== PANEL B: 2SLS  NUMBER OF CHILDREN ==========
eststo clear

* ---------- Model 1 (BASE) ----------
quietly ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `BASE', cluster(h_pid) first
local KPF4 : display %9.2f e(rkf)
local KPL4 : display %9.2f e(idstat)
local  J4  : display %9.2f e(j)
local JP4  : display %9.3f e(jp)
eststo C1: ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `BASE', cluster(h_pid)
estimates restore C1
estadd scalar KP_F  = `KPF4'
estadd scalar KP_LM = `KPL4'
estadd scalar HJ    = `J4'
estadd scalar HJ_p  = `JP4'
estadd local  Covariates "No"
estadd local  SurveyW    "No"

* ---------- Model 2 (+ covariates) ----------
quietly ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `COV', cluster(h_pid) first
local KPF5 : display %9.2f e(rkf)
local KPL5 : display %9.2f e(idstat)
local  J5  : display %9.2f e(j)
local JP5  : display %9.3f e(jp)
eststo C2: ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `COV', cluster(h_pid)
estimates restore C2
estadd scalar KP_F  = `KPF5'
estadd scalar KP_LM = `KPL5'
estadd scalar HJ    = `J5'
estadd scalar HJ_p  = `JP5'
estadd local  Covariates "Yes"
estadd local  SurveyW    "No"

* ---------- Model 3 (weighted) ----------
quietly ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `COV' [pweight=weight], cluster(h_pid) first
local KPF6 : display %9.2f e(rkf)
local KPL6 : display %9.2f e(idstat)
local  J6  : display %9.2f e(j)
local JP6  : display %9.3f e(jp)
eststo C3: ivreg2 child_num_fi_1 (Wh Wh_comp = `Zc') `COV' [pweight=weight], cluster(h_pid)
estimates restore C3
estadd scalar KP_F  = `KPF6'
estadd scalar KP_LM = `KPL6'
estadd scalar HJ    = `J6'
estadd scalar HJ_p  = `JP6'
estadd local  Covariates "No"
estadd local  SurveyW    "No"

* ---- Export Panel B ----
esttab C1 C2 C3 using $DTA/, append ///
    title("Panel B. 2SLS  Number of children (endogenous: Wh, Wh×compsize; instruments: week, law, week×compsize, law×compsize)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh Wh_comp) order(Wh Wh_comp) ///
    coeflabels(Wh "Main effect: Wh" Wh_comp "Interaction: Wh × compsize") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(Covariates SurveyW KP_F KP_LM HJ HJ_p N, ///
          labels("Covariates (sex, educ, age^2, region, parents educ)" ///
                 "Survey weight" "KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9s %9s %9.2f %9.2f %9.2f %9.3f %9.0f))

*--------------------------------------------------------------*
* Appendix 5. AFT model for marriage timing
* 1. select sample who are unmarried at first survey participation
* 2. drop sample after marriage occurs
* 3. create duration variable for AFT 
* 4. assess model fit using different distribution
* 5. conducting analyses
*--------------------------------------------------------------*

use $DTA/, replace

cap drop tag
sort h_pid wv
bysort h_pid: gen tag = married == 0 if _n == 1
bysort h_pid: carryforward tag, replace
keep if tag==1 

//// code duration variable for survival analysis
sort h_pid wv 
by h_pid: gen marriage_age =age

/// drop observations after marriage occurs
forval x = 1/20 {
by h_pid: drop if married[_n-1]==1 & _n>1
}

/// the onset of risk at age 18
replace marriage_age = marriage_age-18
replace marriage_age = 0.5 if marriage_age ==0

stset marriage_age , failure(married) id(h_pid)

/// Assess model fit
foreach x in exponential weibull lognormal loglogistic {
stset marriage_age, failure(married) id(h_pid)
streg , distribution(`x') time iter(50) cluster(h_pid)
estimates store `x'
}
estimates stats _all


****************************************************
* Marriage: First Stage (OLS), Reduced Form (AFT), Second Stage (AFT-CF)
****************************************************
set more off
local cross "†"   

eststo clear
* ---------- Model 1 ----------
eststo a1: reg Wh week law i.compsize i.h_eco4 i.year, vce(cluster h_pid)
quietly ivreg2 marriage_age (Wh=week law) i.compsize i.h_eco4 i.year, ///
    cluster(h_pid) first
scalar a1_kpf  = e(rkf)
scalar a1_kplm = e(idstat)
estimates restore a1
estadd scalar KP_F  = a1_kpf
estadd scalar KP_LM = a1_kplm
cap eststo drop a1
eststo a1

* ---------- Model 2 (+ covariates) ----------
eststo a2: reg Wh week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ, vce(cluster h_pid)
quietly ivreg2 marriage_age (Wh=week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ, cluster(h_pid) first
scalar a2_kpf  = e(rkf)
scalar a2_kplm = e(idstat)
estimates restore a2
estadd scalar KP_F  = a2_kpf
estadd scalar KP_LM = a2_kplm
cap eststo drop a2
eststo a2

* ---------- Model 3 (+ covariates + weights) ----------
eststo a3: reg Wh week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ [pw=weight], vce(cluster h_pid)
quietly ivreg2 marriage_age (Wh=week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ [pw=weight], cluster(h_pid) first
scalar a3_kpf  = e(rkf)
scalar a3_kplm = e(idstat)
estimates restore a3
estadd scalar KP_F  = a3_kpf
estadd scalar KP_LM = a3_kplm
cap eststo drop a3
eststo a3

* --- Export Panel A ---
esttab a1 a2 a3 using $DTA/, replace ///
    title("Panel A. First stage  Marriage (DV: Wh)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(week law) order(week law) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM N, labels("KP rk F" "KP under-ID LM" "N") fmt(%9.2f %9.2f %9.0f))

/***************************************************
 * PANEL B: REDUCED FORM  AFT (lognormal)
 ***************************************************/
eststo clear

* Unweighted survival setup
stset, clear
stset marriage_age, failure(married) id(h_pid)
eststo b1: streg week law i.compsize i.h_eco4 i.year, ///
    dist(lognormal) vce(cluster h_pid)

* With covariates
eststo b2: streg week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ, ///
    dist(lognormal) vce(cluster h_pid)

* Weighted survival setup
stset, clear
stset marriage_age [pw=weight], failure(married) id(h_pid)
eststo b3: streg week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ, ///
    dist(lognormal) vce(cluster h_pid)

esttab b1 b2 b3 using $DTA/, append ///
    title("Panel B. Reduced form  Time to marriage (lognormal AFT)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(week law) order(week law) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

/***************************************************
 * PANEL C: SECOND STAGE  AFT (control-function)
 ***************************************************/
eststo clear

* ---------- Model 1 ----------
cap drop resid
reg Wh week law i.compsize i.h_eco4 i.year, vce(cluster h_pid)
predict double resid, resid

stset, clear
stset marriage_age, failure(married) id(h_pid)
eststo s1: streg Wh i.compsize i.h_eco4 i.year c.resid, ///
    dist(lognormal) vce(cluster h_pid)

/// Bollen overid test
quietly streg Wh week i.compsize i.h_eco4 i.year c.resid, ///
    dist(lognormal) vce(cluster h_pid)
quietly test week
scalar s1_overid_p = r(p)

estimates restore s1
estadd scalar OverID_p = s1_overid_p
cap eststo drop s1
eststo s1

* ---------- Model 2 (+ covariates) ----------
cap drop resid
reg Wh week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ, vce(cluster h_pid)
predict double resid, resid

stset, clear
stset marriage_age, failure(married) id(h_pid)
eststo s2: streg Wh i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ c.resid, ///
    dist(lognormal) vce(cluster h_pid)

/// Bollen overid test
quietly streg Wh week i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ c.resid, ///
    dist(lognormal) vce(cluster h_pid)
quietly test week
scalar s2_overid_p = r(p)

estimates restore s2
estadd scalar OverID_p = s2_overid_p
cap eststo drop s2
eststo s2

* ---------- Model 3 (+ covariates + weights) ----------
cap drop resid
reg Wh week law i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ [pw=weight], vce(cluster h_pid)
predict double resid, resid

stset, clear
stset marriage_age [pw=weight], failure(married) id(h_pid)
eststo s3: streg Wh i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ c.resid, ///
    dist(lognormal) vce(cluster h_pid)

/// Bollen overid test
quietly streg Wh week i.compsize i.h_eco4 i.year ///
    i.sex i.educ i.h_reg7 i.f_educ i.m_educ c.resid, ///
    dist(lognormal) vce(cluster h_pid)
quietly test week
scalar s3_overid_p = r(p)

estimates restore s3
estadd scalar OverID_p = s3_overid_p
cap eststo drop s3
eststo s3

* --- Export Panel C ---
esttab s1 s2 s3 using $DTA/, append ///
    title("Panel C. Second stage  Time to marriage (lognormal AFT, control-function)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(OverID_p N, labels("Over-ID p-value (week excluded)" "N") fmt(%9.3f %9.0f))

*--------------------------------------------------------------*
* Appendix 6. Calculating APE based on CRC model
*--------------------------------------------------------------*

use $DTA/fertility_final, replace

****************************************************
* CRC second-stage tables (Marriage & Fertility)
****************************************************
set more off
local cross "†"   // 10% significance symbol

* Common controls (to keep code tidy)
local BASE "i.compsize i.h_eco4 i.year"
local COV  "i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ"

eststo clear

* ----- Model 1 -----
cap drop resid
reg Wh week law `BASE', vce(cluster h_pid)
predict double resid, resid
eststo m1: reg married Wh c.Wh#c.resid `BASE' c.resid, vce(cluster h_pid)

* ----- Model 2 -----
cap drop resid
reg Wh week law `BASE' `COV', vce(cluster h_pid)
predict double resid, resid
eststo m2: reg married Wh c.Wh#c.resid `BASE' `COV' c.resid, vce(cluster h_pid)

* ----- Model 3 (weighted) -----
cap drop resid
reg Wh week law `BASE' `COV' [pw=weight], vce(cluster h_pid)
predict double resid, resid
eststo m3: reg married Wh c.Wh#c.resid `BASE' `COV' c.resid [pw=weight], vce(cluster h_pid)

* --- Export Panel A ---
esttab m1 m2 m3 using $DTA/, replace ///
    title("Panel A. CRC second stage  Marriage (DV: married)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh c.Wh#c.resid) order(Wh c.Wh#c.resid) ///
    coeflabels(Wh "Wh" c.Wh#c.resid "Wh × residual (CRC)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

eststo clear
* ----- Model 1 -----
cap drop resid
reg Wh week law `BASE', vce(cluster h_pid)
predict double resid, resid
eststo f1: reg child_num_fi_1 Wh c.Wh#c.resid `BASE' c.resid, vce(cluster h_pid)

* ----- Model 2 -----
cap drop resid
reg Wh week law `BASE' `COV', vce(cluster h_pid)
predict double resid, resid
eststo f2: reg child_num_fi_1 Wh c.Wh#c.resid `BASE' `COV' c.resid, vce(cluster h_pid)

* ----- Model 3 (weighted) -----
cap drop resid
reg Wh week law `BASE' `COV' [pw=weight], vce(cluster h_pid)
predict double resid, resid
eststo f3: reg child_num_fi_1 Wh c.Wh#c.resid `BASE' `COV' c.resid [pw=weight], vce(cluster h_pid)

* --- Export Panel B (APPEND below Panel A) ---
esttab f1 f2 f3 using $DTA/, append ///
    title("Panel B. CRC second stage  Number of children (DV: child_num_fi_1)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh c.Wh#c.resid) order(Wh c.Wh#c.resid) ///
    coeflabels(Wh "Wh" c.Wh#c.resid "Wh × residual (CRC)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))
    
*--------------------------------------------------------------*
* Appendix 7. Non-linearity 
*--------------------------------------------------------------*

use $DTA/, replace
//// estimate non-linearity based on CRC model
reg Wh week law i.compsize i.h_eco4 i.year, cluster(h_pid)
cap drop
predict resid, resid 

reg married Wh c.Wh#c.Wh i.compsize i.h_eco4 i.year c.resid c.resid#c.Wh c.resid#c.Wh#c.Wh, cluster(h_pid)

margins, dydx(Wh) at(Wh=(40(1)80))

/// creating graph for marriage
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

graph set window fontface "Times new roman"
tw (rarea upper lower _j, color(gray*0.5))(line coef_ _j, lcolor(black) lwidth(0.5)) (function y=0, range(40 80) lcolor(cranberry) lpattern(dash)),  xtitle("Working hours", size(large)) ytitle("Effect estimates", height(2) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(40(10)80,nogrid notick labsize(large)) ylabel(-0.02(0.01)0.02, nogrid labsize(large)) title("(a) Marriage Probability", size(large)) plotregion(lcolor(black))

graph export $DTA/.png, as(png) replace width(1200) height(1200)
restore

/// creating graph for fertility
reg child_num_fi_1 Wh c.Wh#c.Wh i.compsize i.h_eco4 i.year c.resid c.resid#c.Wh c.resid#c.Wh#c.Wh, cluster(h_pid)

margins, dydx(Wh) at(Wh=(40(1)80))

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

graph set window fontface "Times new roman"
tw (rarea upper lower _j, color(gray*0.5))(line coef_ _j, lcolor(black) lwidth(0.5)) (function y=0, range(40 80) lcolor(cranberry) lpattern(dash)),  xtitle("Working hours", size(large)) ytitle("Effect estimates", height(2) size(vlarge)) legend(off) xsize(12) ysize(12) xlabel(40(10)80,nogrid notick labsize(large)) ylabel(-0.06(0.02)0.06, nogrid labsize(large)) title("(b) Number of children", size(large)) plotregion(lcolor(black))

graph export $DTA/.png, as(png) replace width(1200) height(1200)

*--------------------------------------------------------------*
* Appendix 8. Effect estimates under alternative definitions of instrument assignment
* 1. redefine instruments solely based on company size 
* 2. conduct analyses 
*--------------------------------------------------------------*
use $DTA/, replace

*--------------------------------------------------------------*
* Five-day workweek eligibility
*   week = 1 if covered by policy in given year
*   week = 0 otherwise
*--------------------------------------------------------------*
cap drop week 
gen byte week = 0

replace week = 1 if ( ///
    (year >= 2005 & h_eco10 == 10)              | ///
    (year >= 2006 & inrange(h_eco10, 8, 10))    | ///
    (year >= 2007 & inrange(h_eco10, 7, 10))    | ///
    (year >= 2008 & inrange(h_eco10, 5, 10))    | ///
    (year >= 2009 & inrange(h_eco10, 3, 10))    | ///
    (year >= 2012 & inrange(h_eco10, 2, 10))    )

label var week "Eligible for 5-day workweek"

*--------------------------------------------------------------*
* 52-hour workweek eligibility
*   law = 1 if covered by 52-hour regulation in given year
*   law = 0 otherwise
*--------------------------------------------------------------*
cap drop law
gen byte law = 0

replace law = 1 if  ( ///
    (year >= 2019 & inrange(h_eco10, 8, 10)) | ///
    (year >= 2021 & inrange(h_eco10, 5, 10)) | ///
    (year >= 2022 & inrange(h_eco10, 2, 10)) )

label var law "Eligible for 52-hour workweek"


set more off
local cross "†"   

/***************************************************
 * PANEL A: 2SLS  Marriage 
 ***************************************************/
eststo clear
* -------- Model 1 --------
eststo M1: ivreg2 married (Wh = week law) i.compsize i.year, cluster(h_pid)
scalar M1_KPF  = e(rkf)
scalar M1_KPLM = e(idstat)
scalar M1_J    = e(j)
scalar M1_Jp   = e(jp)
estimates restore M1
estadd scalar KP_F  = M1_KPF
estadd scalar KP_LM = M1_KPLM
estadd scalar HJ    = M1_J
estadd scalar HJ_p  = M1_Jp
cap eststo drop M1
eststo M1

* -------- Model 2 --------
eststo M2: ivreg2 married (Wh = week law) i.compsize i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar M2_KPF  = e(rkf)
scalar M2_KPLM = e(idstat)
scalar M2_J    = e(j)
scalar M2_Jp   = e(jp)
estimates restore M2
estadd scalar KP_F  = M2_KPF
estadd scalar KP_LM = M2_KPLM
estadd scalar HJ    = M2_J
estadd scalar HJ_p  = M2_Jp
cap eststo drop M2
eststo M2

* -------- Model 3 (weighted) --------
eststo M3: ivreg2 married (Wh = week law) i.compsize i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid)
scalar M3_KPF  = e(rkf)
scalar M3_KPLM = e(idstat)
scalar M3_J    = e(j)
scalar M3_Jp   = e(jp)
estimates restore M3
estadd scalar KP_F  = M3_KPF
estadd scalar KP_LM = M3_KPLM
estadd scalar HJ    = M3_J
estadd scalar HJ_p  = M3_Jp
cap eststo drop M3
eststo M3

* ---- Export Panel A ----
esttab M1 M2 M3 using $DTA/.rtf, replace ///
    title("Panel A. 2SLS  Marriage (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

/***************************************************
 * PANEL B: 2SLS  Number of children 
 ***************************************************/
eststo clear

* -------- Model 1 --------
eststo C1: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.year, cluster(h_pid)
scalar C1_KPF  = e(rkf)
scalar C1_KPLM = e(idstat)
scalar C1_J    = e(j)
scalar C1_Jp   = e(jp)
estimates restore C1
estadd scalar KP_F  = C1_KPF
estadd scalar KP_LM = C1_KPLM
estadd scalar HJ    = C1_J
estadd scalar HJ_p  = C1_Jp
cap eststo drop C1
eststo C1

* -------- Model 2 --------
eststo C2: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar C2_KPF  = e(rkf)
scalar C2_KPLM = e(idstat)
scalar C2_J    = e(j)
scalar C2_Jp   = e(jp)
estimates restore C2
estadd scalar KP_F  = C2_KPF
estadd scalar KP_LM = C2_KPLM
estadd scalar HJ    = C2_J
estadd scalar HJ_p  = C2_Jp
cap eststo drop C2
eststo C2

* -------- Model 3 (weighted) --------
eststo C3: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid)
scalar C3_KPF  = e(rkf)
scalar C3_KPLM = e(idstat)
scalar C3_J    = e(j)
scalar C3_Jp   = e(jp)
estimates restore C3
estadd scalar KP_F  = C3_KPF
estadd scalar KP_LM = C3_KPLM
estadd scalar HJ    = C3_J
estadd scalar HJ_p  = C3_Jp
cap eststo drop C3
eststo C3

* ---- Export Panel B ----
esttab C1 C2 C3 using $DTA/, append ///
    title("Panel B. 2SLS  Number of children (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

*--------------------------------------------------------------*
* Appendix 9. Effects Estimates Excluding Cases with Missing Working Hours
* 1. exclude missingness in working hours
* 2. conduct analyses 
*--------------------------------------------------------------*

use $DTA/, replace

cap drop Wh
sort h_pid year
gen Wh =. 
replace Wh= p02_8 if p02_8~=999 & year !=2006
replace Wh = p02_9 * p02_7/4.34 if Wh ==. & p02_9~=99 & p02_7~=99 & year !=2006
replace Wh = 0 if h_eco4>=8& h_eco4<=9 & year !=2006
keep if Wh!=. 

set more off
local cross "†"   

/***************************************************
 * PANEL A: 2SLS  Marriage 
 ***************************************************/
eststo clear

* -------- Model 1 --------
eststo M1: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year, cluster(h_pid)
scalar M1_KPF  = e(rkf)
scalar M1_KPLM = e(idstat)
scalar M1_J    = e(j)
scalar M1_Jp   = e(jp)
estimates restore M1
estadd scalar KP_F  = M1_KPF
estadd scalar KP_LM = M1_KPLM
estadd scalar HJ    = M1_J
estadd scalar HJ_p  = M1_Jp
cap eststo drop M1
eststo M1

* -------- Model 2 --------
eststo M2: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar M2_KPF  = e(rkf)
scalar M2_KPLM = e(idstat)
scalar M2_J    = e(j)
scalar M2_Jp   = e(jp)
estimates restore M2
estadd scalar KP_F  = M2_KPF
estadd scalar KP_LM = M2_KPLM
estadd scalar HJ    = M2_J
estadd scalar HJ_p  = M2_Jp
cap eststo drop M2
eststo M2

* -------- Model 3 (weighted) --------
eststo M3: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid)
scalar M3_KPF  = e(rkf)
scalar M3_KPLM = e(idstat)
scalar M3_J    = e(j)
scalar M3_Jp   = e(jp)
estimates restore M3
estadd scalar KP_F  = M3_KPF
estadd scalar KP_LM = M3_KPLM
estadd scalar HJ    = M3_J
estadd scalar HJ_p  = M3_Jp
cap eststo drop M3
eststo M3

* ---- Export Panel A ----
esttab M1 M2 M3 using $DTA/, replace ///
    title("Panel A. 2SLS  Marriage (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

/***************************************************
 * PANEL B: 2SLS  Number of children 
 ***************************************************/
eststo clear

* -------- Model 1 --------
eststo C1: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year, cluster(h_pid)
scalar C1_KPF  = e(rkf)
scalar C1_KPLM = e(idstat)
scalar C1_J    = e(j)
scalar C1_Jp   = e(jp)
estimates restore C1
estadd scalar KP_F  = C1_KPF
estadd scalar KP_LM = C1_KPLM
estadd scalar HJ    = C1_J
estadd scalar HJ_p  = C1_Jp
cap eststo drop C1
eststo C1

* -------- Model 2 --------
eststo C2: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar C2_KPF  = e(rkf)
scalar C2_KPLM = e(idstat)
scalar C2_J    = e(j)
scalar C2_Jp   = e(jp)
estimates restore C2
estadd scalar KP_F  = C2_KPF
estadd scalar KP_LM = C2_KPLM
estadd scalar HJ    = C2_J
estadd scalar HJ_p  = C2_Jp
cap eststo drop C2
eststo C2

* -------- Model 3 (weighted) --------
eststo C3: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid)
scalar C3_KPF  = e(rkf)
scalar C3_KPLM = e(idstat)
scalar C3_J    = e(j)
scalar C3_Jp   = e(jp)
estimates restore C3
estadd scalar KP_F  = C3_KPF
estadd scalar KP_LM = C3_KPLM
estadd scalar HJ    = C3_J
estadd scalar HJ_p  = C3_Jp
cap eststo drop C3
eststo C3

* ---- Export Panel B ----
esttab C1 C2 C3 using $DTA/, append ///
    title("Panel B. 2SLS  Number of children (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

*--------------------------------------------------------------*
* Appendix 10. Effect Estimates Using Different Age Ranges for Sample Selection
* Note: this analysis must use file without selecting sample based on age
*--------------------------------------------------------------*
use $DTA/, replace

preserve 
forval x = 0/10 {
ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year  if age_first>=15+`x' & age_first<=50-`x' , cluster(h_pid)
scalar coef_`x' = _b[Wh]
scalar se_`x'= _se[Wh]
}

clear 
set obs 1 
forval x = 0/10 {
gen coef_`x'=coef_`x'
gen se_`x'= se_`x'
}

gen id =1 
reshape long coef_ se_, i(id)
gen upper = coef_+1.96*se_
gen lower = coef_-1.96*se_ 

tw (rarea upper lower _j, fcolor(navy*0.4) lcolor(white)) (line coef_ _j, lcolor(navy) lwidth(0.5)) (function y =0, range(0 10) lcolor(black) lpattern(dash)), ylabel(-0.03(0.01)0.03, nogrid labsize(large)) xlabel(0 "15-50" 1 "16-49" 2 "17-48" 3 "18-47" 4 "19-46" 5 "20-45" 6 "21-44" 7 "22-43" 8 "23-42" 9 "24-41" 10 "25-40", labsize(large) angle(45) nogrid) xtitle("Age ranges", size(large)) ytitle("Effect esitmates", size(large)) xsize(12) ysize(12) legend(off) title("(a) Marriage probabiltiy", size(large))  plotregion(lcolor(black))

graph export $DTA/.png, as(png) replace width(1200) height(1200)
restore

forval x = 0/10 {
ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year  if age_first>=15+`x' & age_first<=50-`x' , cluster(h_pid)
scalar coef_`x' = _b[Wh]
scalar se_`x'= _se[Wh]
}

clear 
set obs 1 
forval x = 0/10 {
gen coef_`x'=coef_`x'
gen se_`x'= se_`x'
}

gen id =1 
reshape long coef_ se_, i(id)
gen upper = coef_+1.96*se_
gen lower = coef_-1.96*se_ 

tw (rarea upper lower _j, fcolor(navy*0.4) lcolor(white)) (line coef_ _j, lcolor(navy) lwidth(0.5)) (function y =0, range(0 10) lcolor(black) lpattern(dash)), ylabel(-0.08(0.02)0.08, nogrid labsize(large)) xlabel(0 "15-50" 1 "16-49" 2 "17-48" 3 "18-47" 4 "19-46" 5 "20-45" 6 "21-44" 7 "22-43" 8 "23-42" 9 "24-41" 10 "25-40", labsize(large) angle(45) nogrid) xtitle("Age ranges", size(large)) ytitle("Effect esitmates", size(large)) xsize(12) ysize(12) legend(off) title("(b) Number of children", size(large))  plotregion(lcolor(black))

graph export $DTA/.png, as(png) replace width(1200) height(1200)

*--------------------------------------------------------------*
* Appendix 11. LIML
*--------------------------------------------------------------*

use $DTA/, replace

set more off
local cross "†"   // 10% significance marker for coefficients

/***************************************************
 * PANEL A: LIML  Marriage 
 ***************************************************/
eststo clear
* -------- Model 1 --------
eststo M1: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year, ///
    cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* -------- Model 2 --------
eststo M2: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, ///
    cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* -------- Model 3 (weighted) --------
eststo M3: ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* ---- Export Panel A ----
esttab M1 M2 M3 using $DTA/, replace ///
    title("Panel A. LIML  Marriage (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

/***************************************************
 * PANEL B: LIML  Number of children 
 ***************************************************/
eststo clear

* -------- Model 1 --------
eststo C1: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year, ///
    cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* -------- Model 2 --------
eststo C2: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, ///
    cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* -------- Model 3 (weighted) --------
eststo C3: ivreg2 child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year ///
    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
    [pw=weight], cluster(h_pid) liml
estadd scalar KP_F  = e(rkf)
estadd scalar KP_LM = e(idstat)
estadd scalar HJ    = e(j)
estadd scalar HJ_p  = e(jp)

* ---- Export Panel B ----
esttab C1 C2 C3 using $DTA/, append ///
    title("Panel B. LIML  Number of children (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    coeflabels(Wh "Wh (treatment)") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))



*--------------------------------------------------------------*
* Appendix 12. IV-probit and IV-poisson
*--------------------------------------------------------------*
use $DTA/, replace

ivprobit married (Wh = week law) i.compsize i.h_eco4 i.year, ///
    vce(cluster h_pid) 
margins, dydx(Wh) predict(pr)

ivpoisson gmm child_num_fi_1 (Wh = week law) i.compsize i.h_eco4 i.year, ///
    vce(cluster h_pid) 
estat overid
margins, dydx(Wh) predict(pr)




