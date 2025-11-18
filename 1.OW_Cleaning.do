/* 
Replication Package: Labor laws as family policy
Author: Sungsik Hwang and Jenna Nobles
Contact: shwang97@wisc.edu
Date: 2025-11-18
Note: The file contains the code for preparing data for the analysis
*/

clear
set more off
macro define DTA ""
macro define graph ""

*--------------------------------------------------------------*
* 1. calculating number of childern using household data
*--------------------------------------------------------------*
use $DTA/, replace
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
save $DTA/, replace

*--------------------------------------------------------------*
* 2. cleaning individual-level data 
*--------------------------------------------------------------*
use $DTA/, clear

/// Merging number of children from household data
merge m:1 h_merkey year using $DTA/
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

/// creating lagged variable
sort h_pid year 
bysort h_pid: gen child_num_fi_1 = child_num_fi[_n+1]

/// creating marital status
gen married = 1 if h_g10>=1 & h_g10 <=4 
replace married = 0 if h_g10 ==5|h_g10==0

*--------------------------------------------------------------*
* Weekly working hours (Wh)
*   - If regular schedule: use reported weekly hours (p02_8)
*   - If irregular schedule: (daily hours * days worked per month) / 4.34
*   - 2006: working-hours info not surveyed → Wh left missing
*   - Unemployed / out of labor force (h_eco4 8-9): Wh = 0
*--------------------------------------------------------------*
capture drop Wh
gen float Wh = .

* Regular weekly hours, except 2006 and missing code (999)
replace Wh = p02_8 if p02_8 != 999 & year != 2006

* Construct weekly hours for those without regular schedule
replace Wh = p02_9 * p02_7 / 4.34 if missing(Wh)      ///
    & p02_9 != 99 & p02_7 != 99                        ///
    & year != 2006

* Unemployed / non-working: set weekly hours to 0
replace Wh = 0 if inrange(h_eco4, 8, 9)
label var Wh "Weekly working hours"

*--------------------------------------------------------------*
* Five-day workweek eligibility
*   week = 1 if covered by policy in given year
*   week = 0 otherwise
*--------------------------------------------------------------*
gen byte week = 0

replace week = 1 if inrange(h_eco4, 1, 2) & ( ///
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
gen byte law = 0

replace law = 1 if inrange(h_eco4, 1, 2) & ( ///
    (year >= 2019 & inrange(h_eco10, 8, 10)) | ///
    (year >= 2021 & inrange(h_eco10, 5, 10)) | ///
    (year >= 2022 & inrange(h_eco10, 2, 10)) )

label var law "Eligible for 52-hour workweek"

*--------------------------------------------------------------*
* Company size variable
*   7 = 1000+ (h_eco10 == 10)
*   6 = 300-999 (h_eco10 8-9)
*   5 = 100-299  (h_eco10 7)
*   4 = 50-99  (h_eco10 5-6)
*   3 = 20-49  (h_eco10 3-4)
*   2 = 5-19   (h_eco10 2)
*   1 = < 5    (h_eco10 1)
*   0 = unemployed (missing h_eco10)
*--------------------------------------------------------------*
gen byte compsize = . 

replace compsize = 7 if h_eco10 == 10
replace compsize = 6 if inrange(h_eco10, 8, 9)
replace compsize = 5 if h_eco10 == 7
replace compsize = 4 if inrange(h_eco10, 5, 6)
replace compsize = 3 if inrange(h_eco10, 3, 4)
replace compsize = 2 if h_eco10 == 2
replace compsize = 1 if h_eco10 == 1
* unemployed 
replace compsize = 0 if missing(compsize)
/// missing (I don't know, non-response)
replace compsize =. if h_eco10==11 | h_eco10==99
label var compsize "Company size category (0=unemployed)"

*--------------------------------------------------------------*
* Working status variable 
*   9 = not in laborforce (h_eco4 == 9)
*   8 = unemployed 	(h_eco4 == 8)
*   7 = unpaid family work (h_eco4 == 7)
*   6 = self-employed 	(h_eco4 == 6)
*   5 = employer 	(h_eco4 == 5)
*   3 = daily worker 	(h_eco4 == 3-4)
*   2 = temporary worker (h_eco4 == 2)
*   1 = regular worker 	(h_eco4 == 1)

*--------------------------------------------------------------*
recode h_eco4 (4=3)

*--------------------------------------------------------------*
* Creating baseline covariates
*--------------------------------------------------------------*
/// carry forward baseline covariates
sort h_pid wv 
bysort h_pid: carryforward np06_39, gen(f_educ)
bysort h_pid: carryforward np06_40, gen(m_educ)
bysort h_pid: carryforward np06_41, gen(f_occu)
bysort h_pid: carryforward np06_42, gen(m_occu)

foreach x in f_educ m_educ {
	recode `x' (1/3=1)(4=2)(5=3)(6/8=4)(9=5)
	tab `x', g(`x')
}

gen age = year - h_g4
gen sex= h_g3 

/// creating variable for effect heterogeniety analysis
replace sex = sex-1 
foreach x in Wh week law  {
cap drop `x'_int
gen `x'_int = `x' * sex	
}

/// Baseline covariates
foreach x in h_g3 h_g6 h_reg7 {
bysort h_pid: replace `x'=`x'[1]
}

cap drop educ*
gen educ = h_g6 
recode educ (1/3=1)(4=2)(5=3)(6/9=4)
tab educ, g(educ)

cap drop region
gen region = h_reg7
tab region, g(region)

*--------------------------------------------------------------*
* Sample selection
* 1. Exclude individuals aged <18 or >45 at survey participation
* 2. Exclude individuals observed in a single survey wave only (child_num_fi_1)
* 3. Exclude observations with missing covariate or a survey weight of zero
*--------------------------------------------------------------*
bysort h_pid: gen age_first = age[1]
keep if age_first >=18 & age_first <=45

foreach x in child_num_fi_1 married compsize h_eco4 sex educ region f_educ m_educ {
keep if `x'!=.
}

gen weight = p_wsl
replace weight = p_wsl_all if p_wsl_all!=.
replace weight = p_wsl_n_all if p_wsl_n_all!=. 
keep if weight!=0

/// save file without imputation 
save $DTA/, replace

/// save file for imputing weekly working hours 
keep h_pid year Wh week law compsize h_eco4 married child_num_fi child_num_fi_1 age sex educ region f_educ m_educ
save $DTA/, replace

*--------------------------------------------------------------*
* Merging imputed values of working hours
* Replication code for imputation in seperate file
*--------------------------------------------------------------*
use $DTA/, replace
cap drop _merge
merge 1:1 h_pid year using $DTA/, 

drop Wh 
rename Wh_imp Wh

save $DTA/, replace

