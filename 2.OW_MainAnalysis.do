/* 
Replication Package: Labor laws as family policy
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-11-18
Note: The file contains the code for the main analysis
*/

clear
set more off
macro define DTA ""
macro define graph ""
use $DTA/, replace

******************** Table 1 (descriptive statistics) *********************

gen child_num_ca = child_num_fi 
recode child_num_ca (3/9=3)

dtable i.married i.child_num_ca Wh age_first i.educ i.compsize i.h_eco4 , nformat(%16.2fc mean sd) by(sex) factor(, statistic(fvfrequency)) continuous(, statistic(mean)) export($DTA/.docx, replace)

dtable i.married i.child_num_ca Wh age_first i.educ i.compsize i.h_eco4 , nformat(%16.2fc mean sd) by(sex) factor(, statistic(fvpercent)) continuous(, statistic(sd)) export($DTA/.docx, replace)

******************** Table 2 (first stage and reduced form equations) *********************
use $DTA/, replace
local cross "†"   

*==================== PANEL A: FIRST STAGE ====================

eststo clear

* ---------- Model 1 ----------
eststo a1: reg Wh week law i.compsize i.h_eco4 i.year, cluster(h_pid)
quietly ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year, cluster(h_pid) first
scalar a1_F  = e(rkf)
scalar a1_LM = e(idstat)
estimates restore a1
estadd scalar KP_F  = a1_F
estadd scalar KP_LM = a1_LM
cap eststo drop a1
eststo a1

* ---------- Model 2 ----------
eststo a2: reg Wh week law i.compsize i.h_eco4 i.year ///
                      i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
quietly ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
                      i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid) first
scalar a2_F  = e(rkf)
scalar a2_LM = e(idstat)
estimates restore a2
estadd scalar KP_F  = a2_F
estadd scalar KP_LM = a2_LM
cap eststo drop a2
eststo a2

* ---------- Model 3 (weighted) ----------
eststo a3: reg Wh week law i.compsize i.h_eco4 i.year ///
                      i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                      [pw=weight], cluster(h_pid)
quietly ivreg2 married (Wh = week law) i.compsize i.h_eco4 i.year ///
                      i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                      [pw=weight], cluster(h_pid) first
scalar a3_F  = e(rkf)
scalar a3_LM = e(idstat)
estimates restore a3
estadd scalar KP_F  = a3_F
estadd scalar KP_LM = a3_LM
cap eststo drop a3
eststo a3

* ---- Export Panel A ----
esttab a1 a2 a3 using $DTA/, replace ///
    title("Panel A. First stage (DV: Wh)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(week law) order(week law) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM N, labels("KP rk F" "KP under-ID LM" "N") fmt(%9.2f %9.2f %9.0f))

*============== PANEL B: REDUCED FORM  MARRIAGE ==============

eststo clear
eststo b1: reg married week law i.compsize i.h_eco4 i.year, cluster(h_pid)
eststo b2: reg married week law i.compsize i.h_eco4 i.year ///
                     i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
eststo b3: reg married week law i.compsize i.h_eco4 i.year ///
                     i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                     [pw=weight], cluster(h_pid)

esttab b1 b2 b3 using $DTA/, append ///
    title("Panel B. Reduced form  Marriage (DV: married)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(week law) order(week law) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

*============ PANEL C: REDUCED FORM  FERTILITY =============

eststo clear
* ---------- Model 1 ----------
eststo c1: reg child_num_fi_1 week law i.compsize i.h_eco4 i.year, cluster(h_pid)
estimates restore c1
estadd local Covariates "No"
estadd local SurveyW    "No"
cap eststo drop c1
eststo c1

* ---------- Model 2 ----------
eststo c2: reg child_num_fi_1 week law i.compsize i.h_eco4 i.year ///
                     i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
estimates restore c2
estadd local Covariates "Yes"
estadd local SurveyW    "No"
cap eststo drop c2
eststo c2
* ---------- Model 3 (weighted) ----------
eststo c3: reg child_num_fi_1 week law i.compsize i.h_eco4 i.year ///
                     i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                     [pw=weight], cluster(h_pid)
estimates restore c3
estadd local Covariates "Yes"
estadd local SurveyW    "Yes"
cap eststo drop c3
eststo c3

* ---- Export Panel C ----
esttab c1 c2 c3 using $DTA/, append ///
    title("Panel C. Reduced form  Fertility (DV: number of children)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(week law) order(week law) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(Covariates SurveyW N, ///
          labels("Covariates (sex, educ, age^2, region, parents educ)" ///
                 "Survey weight" "N") ///
          fmt(%9s %9s %9.0f))
	  

****************** Table 3 (secondstage equation; marraige)********************
set more off
local cross "†"   

*==================== PANEL A: OLS  MARRIAGE ====================

eststo clear
eststo A1: reg married Wh i.compsize i.h_eco4 i.year, cluster(h_pid)
eststo A2: reg married Wh i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
eststo A3: reg married Wh i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                    [pw=weight], cluster(h_pid)

esttab A1 A2 A3 using $DTA/, replace ///
    title("Panel A. OLS  Marriage (DV: married)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

*==================== PANEL B: TWFE  MARRIAGE ===================
xtset h_pid
eststo clear

eststo B1: xtreg married Wh i.compsize i.h_eco4 i.year, fe vce(cluster h_pid)

esttab B1 using $DTA/, append ///
    title("Panel B. TWFE  Marriage (DV: married)") ///
    mtitles("FE Model") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

*==================== PANEL C: 2SLS  MARRIAGE ===================

eststo clear
eststo C1: ivreg2 married (Wh=week law) i.compsize i.h_eco4 i.year, cluster(h_pid)
scalar C1_J  = e(j)
scalar C1_Jp = e(jp)
estimates restore C1
estadd scalar HJ   = C1_J
estadd scalar HJ_p = C1_Jp
cap eststo drop C1
eststo C1

eststo C2: ivreg2 married (Wh=week law) i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar C2_J  = e(j)
scalar C2_Jp = e(jp)
estimates restore C2
estadd scalar HJ   = C2_J
estadd scalar HJ_p = C2_Jp
cap eststo drop C2
eststo C2

eststo C3: ivreg2 married (Wh=week law) i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                    [pw=weight], cluster(h_pid)
scalar C3_J  = e(j)
scalar C3_Jp = e(jp)
estimates restore C3
estadd scalar HJ   = C3_J
estadd scalar HJ_p = C3_Jp
cap eststo drop C3
eststo C3

esttab C1 C2 C3 using $DTA/, append ///
    title("Panel C. 2SLS  Marriage (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(HJ HJ_p N, labels("Hansen J" "Hansen p-value" "N") fmt(%9.2f %9.3f %9.0f))

****************** Table 4 (secondstage equation; fertility)********************
eststo clear

eststo D1: reg child_num_fi_1 Wh i.compsize i.h_eco4 i.year, cluster(h_pid)
eststo D2: reg child_num_fi_1 Wh i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
eststo D3: reg child_num_fi_1 Wh i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                    [pw=weight], cluster(h_pid)

esttab D1 D2 D3 using $DTA/, append ///
    title("Panel D. OLS  Number of children (DV: child_num_fi_1)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

*==================== PANEL E: TWFE  FERTILITY =================
xtset h_pid
eststo clear

eststo E1: xtreg child_num_fi_1 Wh i.compsize i.h_eco4 i.year, fe vce(cluster h_pid)

esttab E1 using $DTA/, append ///
    title("Panel E. TWFE  Number of children (DV: child_num_fi_1)") ///
    mtitles("FE Model") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(N, labels("N") fmt(%9.0f))

*==================== PANEL F: 2SLS  FERTILITY ================

eststo clear

* Model 1
eststo F1: ivreg2 child_num_fi_1 (Wh=week law) i.compsize i.h_eco4 i.year, cluster(h_pid)
scalar F1_J  = e(j)
scalar F1_Jp = e(jp)
estimates restore F1
estadd scalar HJ   = F1_J
estadd scalar HJ_p = F1_Jp
estadd local Covariates "No"
estadd local SurveyW    "No"
cap eststo drop F1
eststo F1

* Model 2
eststo F2: ivreg2 child_num_fi_1 (Wh=week law) i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ, cluster(h_pid)
scalar F2_J  = e(j)
scalar F2_Jp = e(jp)
estimates restore F2
estadd scalar HJ   = F2_J
estadd scalar HJ_p = F2_Jp
estadd local Covariates "Yes"
estadd local SurveyW    "No"
cap eststo drop F2
eststo F2

* Model 3 (weighted)
eststo F3: ivreg2 child_num_fi_1 (Wh=week law) i.compsize i.h_eco4 i.year ///
                    i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ ///
                    [pw=weight], cluster(h_pid)
scalar F3_J  = e(j)
scalar F3_Jp = e(jp)
estimates restore F3
estadd scalar HJ   = F3_J
estadd scalar HJ_p = F3_Jp
estadd local Covariates "Yes"
estadd local SurveyW    "Yes"
cap eststo drop F3
eststo F3

esttab F1 F2 F3 using $DTA/, append ///
    title("Panel F. 2SLS  Number of children (endogenous: Wh; instruments: week, law)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh) order(Wh) ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(Covariates SurveyW HJ HJ_p N, ///
          labels("Covariates (sex, educ, age^2, region, parents educ)" ///
                 "Survey weight" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9s %9s %9.2f %9.3f %9.0f))


*********************** Table 5 (effect heterogeniety analysis by gender) ****************** 

set more off
local cross "†"   // 10% significance marker

* Instruments and controls
local Zs    "week law week_int law_int"
local BASE  "i.compsize i.h_eco4 i.year i.sex"
local COV   "i.compsize i.h_eco4 i.year i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ"

*=================== PANEL A: MARRIAGE ===================

eststo clear
* ----- Model 1: baseline controls -----
eststo M1: ivreg2 married (Wh Wh_int = `Zs') `BASE', cluster(h_pid) first
scalar M1_F   = e(rkf)
scalar M1_LM  = e(idstat)
scalar M1_J   = e(j)
scalar M1_Jp  = e(jp)
estimates restore M1
estadd scalar KP_F  = M1_F
estadd scalar KP_LM = M1_LM
estadd scalar HJ    = M1_J
estadd scalar HJ_p  = M1_Jp
cap eststo drop M1
eststo M1

* ----- Model 2: + covariates -----
eststo M2: ivreg2 married (Wh Wh_int = `Zs') `COV', cluster(h_pid) first
scalar M2_F   = e(rkf)
scalar M2_LM  = e(idstat)
scalar M2_J   = e(j)
scalar M2_Jp  = e(jp)
estimates restore M2
estadd scalar KP_F  = M2_F
estadd scalar KP_LM = M2_LM
estadd scalar HJ    = M2_J
estadd scalar HJ_p  = M2_Jp
cap eststo drop M2
eststo M2

* ----- Model 3: + covariates + survey weight -----
eststo M3: ivreg2 married (Wh Wh_int = `Zs') `COV' [pw=weight], cluster(h_pid) first
scalar M3_F   = e(rkf)
scalar M3_LM  = e(idstat)
scalar M3_J   = e(j)
scalar M3_Jp  = e(jp)
estimates restore M3
estadd scalar KP_F  = M3_F
estadd scalar KP_LM = M3_LM
estadd scalar HJ    = M3_J
estadd scalar HJ_p  = M3_Jp
cap eststo drop M3
eststo M3

* ---- Export Panel A ----
esttab M1 M2 M3 using $DTA/, replace ///
    title("Panel A. 2SLS  Marriage (endogenous: Wh, Wh×sex; instruments: week, law, week×sex, law×sex)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh Wh_int) order(Wh Wh_int) ///
    coeflabels(Wh "Main effect: Wh" Wh_int "Interaction: Wh × sex") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(KP_F KP_LM HJ HJ_p N, ///
          labels("KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9.2f %9.2f %9.2f %9.3f %9.0f))

*============= PANEL B: NUMBER OF CHILDREN =============
eststo clear

* ----- Model 1: baseline controls -----
eststo C1: ivreg2 child_num_fi_1 (Wh Wh_int = `Zs') `BASE', cluster(h_pid) first
scalar C1_F   = e(rkf)
scalar C1_LM  = e(idstat)
scalar C1_J   = e(j)
scalar C1_Jp  = e(jp)
estimates restore C1
estadd scalar KP_F  = C1_F
estadd scalar KP_LM = C1_LM
estadd scalar HJ    = C1_J
estadd scalar HJ_p  = C1_Jp
estadd local  Covariates "No"
estadd local  SurveyW    "No"
cap eststo drop C1
eststo C1

* ----- Model 2: + covariates -----
eststo C2: ivreg2 child_num_fi_1 (Wh Wh_int = `Zs') `COV', cluster(h_pid) first
scalar C2_F   = e(rkf)
scalar C2_LM  = e(idstat)
scalar C2_J   = e(j)
scalar C2_Jp  = e(jp)
estimates restore C2
estadd scalar KP_F  = C2_F
estadd scalar KP_LM = C2_LM
estadd scalar HJ    = C2_J
estadd scalar HJ_p  = C2_Jp
estadd local  Covariates "Yes"
estadd local  SurveyW    "No"
cap eststo drop C2
eststo C2

* ----- Model 3: + covariates + survey weight -----
eststo C3: ivreg2 child_num_fi_1 (Wh Wh_int = `Zs') `COV' [pw=weight], cluster(h_pid) first
scalar C3_F   = e(rkf)
scalar C3_LM  = e(idstat)
scalar C3_J   = e(j)
scalar C3_Jp  = e(jp)
estimates restore C3
estadd scalar KP_F  = C3_F
estadd scalar KP_LM = C3_LM
estadd scalar HJ    = C3_J
estadd scalar HJ_p  = C3_Jp
estadd local  Covariates "Yes"
estadd local  SurveyW    "Yes"
cap eststo drop C3
eststo C3

* ---- Export Panel B ----
esttab C1 C2 C3 using $DTA/, append ///
    title("Panel B. 2SLS  Number of children (endogenous: Wh, Wh×sex; instruments: week, law, week×sex, law×sex)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    keep(Wh Wh_int) order(Wh Wh_int) ///
    coeflabels(Wh "Main effect: Wh" Wh_int "Interaction: Wh × sex") ///
    b(%6.3f) ci(%6.3f) brackets ///
    star(`cross' 0.10 * 0.05 ** 0.01 *** 0.001) ///
    nonumbers noobs label ///
    stats(Covariates SurveyW KP_F KP_LM HJ HJ_p N, ///
          labels("Covariates (sex, educ, age^2, region, parents educ)" ///
                 "Survey weight" "KP rk F" "KP under-ID LM" "Hansen J" "Hansen p-value" "N") ///
          fmt(%9s %9s %9.2f %9.2f %9.2f %9.3f %9.0f))

	  
