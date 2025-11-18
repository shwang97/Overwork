/* 
Replication Package: Labor laws as family policy
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-11-18
Note: The file contains the code for the falsification tests
*/

clear
set more off
macro define DTA ""
macro define graph ""

use $DTA/, replace

********************************************
*** Appendix 14. Permutation test ***

* 1. Temporarily preserve the dataset 
* 2. Create 8 observations (one for each compsize category)
* 3. For each category, draw a random year for 'week_re' and one for 'law_re' within each implemtation period
* 4. Assign those random "implementation" years back to scalars
* 5. Restore original data
* 6. Create the indicators week_re and law_re 

* Note: This file only contains estimating reduced form for number of childern. The procedure should be iterated for working hours and marriage
********************************************
local iterations = 5000

* Create storage for coefficient draws
gen coef_week_reduced = . 
gen coef_law_reduced = . 

forval x = 1/`iterations' {

    tempname rweek rlaw
    preserve
        clear
        set obs 8
        gen cat = _n

        gen random_week = runiformint(2005,2012)
        gen random_law  = runiformint(2019,2022)
	
        replace random_week=0 if _n==8
	replace random_law=0 if _n==8
        
        forvalues i = 1/8 {
	local v = `i'-1
            scalar rweek_`v' = random_week[`i']
            scalar rlaw_`v'  = random_law[`i']
        }
    restore
    
    cap drop week_re
    cap drop law_re
    
    gen week_re = .

    forvalues i = 0/7 {
        replace week_re = 1 if compsize==`i' & year >= rweek_`i'
    }
    replace week_re = 0 if week_re==.
    
    gen law_re = .
    forvalues i = 0/7 {
        replace law_re = 1 if compsize==`i' & year >= rlaw_`i'
    }
    replace law_re = 0 if law_re==.
    
    quietly reg child_num_fi_1 week_re law i.compsize i.h_eco4 i.year i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ [pw=weight]

    scalar coef_week_reduced_`x' = _b[week_re]
    replace coef_week_reduced = coef_week_reduced_`x' if _n == `x'
    
    quietly reg child_num_fi_1 week law_re i.compsize i.h_eco4 i.year i.sex i.educ c.age##c.age i.h_reg7 i.f_educ i.m_educ [pw=weight]
    
    scalar coef_law_reduced_`x' = _b[law_re]
    replace coef_law_reduced = coef_law_reduced_`x' if _n == `x'
    */
}

keep  coef_week_reduced coef_law_reduced
save $DTA/, replace 
