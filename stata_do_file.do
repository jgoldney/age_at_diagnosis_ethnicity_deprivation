
use "...", clear

keep patid exp aged sex imd eth8 ethn						
mdesc

replace imd = 1 if imd == 1 | imd == 2
replace imd = 2 if imd == 3 | imd == 4
replace imd = 3 if imd == 5 | imd == 6
replace imd = 4 if imd == 7 | imd == 8
replace imd = 5 if imd == 9 | imd == 10

egen age_bands = cut(aged), at(0 18 25 30 35 40 45 50 55 60 65 70 75 80 85 120)
label define Ab 18 "18-24" 25 "25-29" 30 "30-34" 35 "35-39" 40 "40-44" 45 "45-49" 50 "50-54" 55 "55-59" 60 "60-64" 65 "65-69" 70 "70-74" 75 "75-79" 80 "80-84" 85 "≥85"
label values age_bands Ab

drop if missing(ethn)  | missing(imd)
drop if ethn == "Unknown"

gen 	eths = .
replace eths = . if missing(eth8)
replace eths = 1 if eth8 == "White"
replace eths = 2 if eth8 == "Black"
replace eths = 3 if eth8 == "South East Asians"
replace eths = 4 if missing(eths)
label define eths 1 "White" 2 "Black" 3 "South Asian" 4 "Other"
label values eths eths
tab eths, m
drop eth8		

label define diab 0 "No_diabetes" 1 "Diabetes"
label values exp diab




*******************************************************
////eths margins and relative risk calculation diab vs non_diab
*******************************************************

preserve	
	
keep if imd ==1 | imd==5
	
contract exp sex age_bands eths imd

gen sum = .
gen percent = .
bysort exp sex age_band imd: replace sum = sum(_freq) 
bysort exp sex age_band imd: replace sum = sum[_N] 
bysort exp sex age_band imd: replace percent = _freq / sum


gen plci =.
gen puci =.
gen pse =.
tab exp
forvalues i = 1/`r(N)' {
	cii proportions sum[`i'] _freq[`i'], wilson
	replace plci = r(lb) if _n==`i'
	replace puci = r(ub) if _n==`i'
	replace pse = r(se) if _n==`i'
}


///Reshape to get RRs

reshape wide _freq sum percent plci puci pse, i(sex age_bands eths imd) j(exp)
gen RR = percent1 / percent0

//To calculate RR ci, using delta method: see essential medical statistics

gen logRR = ln(RR)
gen se_logRR = sqrt((1/_freq1) - (1/sum1) + (1/_freq0) - (1/sum0))
gen log_RRlci = logRR - 1.96*se_logRR
gen log_RRuci = logRR + 1.96*se_logRR
gen RR_lci = exp(log_RRlci)
gen RR_uci = exp(log_RRuci)

keep sex age_bands eths imd RR RR_lci RR_uci

save "...RRs_eths.dta", replace

use "...RRs_eths.dta", clear
gen order = eths
replace order= 3.5 if order==1
label values age_bands Ab
label define imd 1 "Least deprived" 5 "Most deprived"
label values imd imd

label values age_bands Ab

sort age_bands
sort order, stable
sort imd, stable
sort sex, stable
drop order
format RR RR_lci RR_uci %9.2f
order sex imd eths age_bands 
export excel using "...RR_eth.xls", firstrow(variables) replace

restore


*******************************************************
////imd margins and relative risk calculation diab vs non_diab
*******************************************************
preserve	
	
	
contract exp sex age_bands eths imd

gen sum = .
gen percent = .
bysort exp sex age_band eths: replace sum = sum(_freq) 
bysort exp sex age_band eths: replace sum = sum[_N] 
bysort exp sex age_band eths: replace percent = _freq / sum

keep if imd ==1 | imd==5

gen plci =.
gen puci =.
gen pse =.
tab exp
forvalues i = 1/`r(N)' {
	cii proportions sum[`i'] _freq[`i'], wilson
	replace plci = r(lb) if _n==`i'
	replace puci = r(ub) if _n==`i'
	replace pse = r(se) if _n==`i'
}

///Reshape to get RRs

reshape wide _freq sum percent plci puci pse, i(sex age_bands eths imd) j(exp)
gen RR = percent1 / percent0

//To calculate RR ci, using delta method: see essential medical statistics

gen logRR = ln(RR)
gen se_logRR = sqrt((1/_freq1) - (1/sum1) + (1/_freq0) - (1/sum0))
gen log_RRlci = logRR - 1.96*se_logRR
gen log_RRuci = logRR + 1.96*se_logRR
gen RR_lci = exp(log_RRlci)
gen RR_uci = exp(log_RRuci)

keep sex age_bands eths imd RR RR_lci RR_uci

save "...RRs_imd.dta", replace

use "...RRs_imd.dta", clear
gen order = eths
replace order= 3.5 if order==1
label values age_bands Ab
label values imd imd
label values age_bands Ab

sort age_bands
sort imd, stable
sort order, stable
sort sex, stable
drop order
format RR RR_lci RR_uci %9.2f
order sex eths imd  age_bands 
export excel using "...RR_imd.xls", firstrow(variables) replace

restore


*********************************************************************************************
///Table 1: describe ethnicity composition in diabetes and diabetes free cohorts by age_bands
*********************************************************************************************

preserve 
keep if sex==1
dtable i.sex aged i.age_bands i.eths i.imd, by(exp, nototals) nformat(%9.0fc median q1 q3) sformat("[%s," q1) sformat("%s]" q3)  factor(sex age_bands eths imd, stat(fvfrequency fvpercent))  continuous(aged, statistics(median q1 q3)) export("...Tables.xlsx", modify sheet("Tab1_men"))  
restore


preserve 
keep if sex==2
dtable i.sex aged i.age_bands i.eths i.imd, by(exp, nototals) nformat(%9.0fc median q1 q3) sformat("[%s," q1) sformat("%s]" q3)  factor(sex age_bands eths imd, stat(fvfrequency fvpercent))  continuous(aged, statistics(median q1 q3)) export("...Tables.xlsx", modify sheet("Tab1_women"))  
restore




*********************************************************************************************
/// Supplementary Table S1: Breakdown of minority ethnicities by age at diagnosis and age
*********************************************************************************************


contract exp sex eths ethn

gen sum = .
gen percent = .
bysort exp sex: replace sum = sum(_freq) 
bysort exp sex: replace sum = sum[_N] 
bysort exp sex: replace percent = _freq / sum

gen prev = percent*100

format prev %9.2f

tostring _freq sum prev, replace u force

gen n_prev = _freq + " (" + prev + "%)"

drop _freq sum prev

reshape wide n_prev, i(ethn eths exp) j(sex)

rename n_prev1 men
rename n_prev2 women
order exp eths ethn women


sort ethn
sort eths, stable
sort exp, stable

export excel using "...Supp_tab1.xls", firstrow(variables) replace





******************
******************
////GRAPHS MARGINS********
******************
******************



use "...all_crude_margins_eths.dta", clear



**********************************************************************************************
// margin for each ethnicity by age band and imd
**********************************************************************************************

. //black women eth



rename imd _at3
rename age_bands _at1
rename exp _at2

replace _at1 = 	1	 if _at1 == 	18
replace _at1 = 	2	 if _at1 == 	25
replace _at1 = 	3	 if _at1 == 	30
replace _at1 = 	4	 if _at1 == 	35
replace _at1 = 	5	 if _at1 == 	40
replace _at1 = 	6	 if _at1 == 	45
replace _at1 = 	7	 if _at1 == 	50
replace _at1 = 	8	 if _at1 == 	55
replace _at1 = 	9	 if _at1 == 	60
replace _at1 = 	10	 if _at1 == 	65
replace _at1 = 	11	 if _at1 == 	70
replace _at1 = 	12	 if _at1 == 	75
replace _at1 = 	13	 if _at1 == 	80
replace _at1 = 	14	 if _at1 == 	85

replace _at1 = _at1 - 0.1 if _at2 ==0
replace _at1 = _at1 + 0.1 if _at2 ==1


forval i = 1(1)4 {

	replace proportion`i' = proportion`i'
	replace uci`i' = uci`i'
	replace lci`i' = lci`i'
}

//black women

graph drop _all
preserve
. keep if sex == 2
graph twoway /*
	*/ rcap lci2 uci2 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci2 uci2 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci2 uci2 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci2 uci2 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion2 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion2 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion2 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion2 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion2 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion2 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion2 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion2 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("A", replace) title("Black women") yscale(range(0 0.20)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.20 "0.20") 
restore

//black men

preserve
. keep if sex == 1
graph twoway /*
	*/ rcap lci2 uci2 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci2 uci2 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci2 uci2 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci2 uci2 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion2 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion2 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion2 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion2 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion2 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion2 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion2 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion2 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes"))  xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Proportion", size(*1.5)) name("B", replace) legend(off) title("Black men") yscale(range(0 0.20)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.20 "0.20")
restore

. //south asian women eth


preserve
. keep if sex == 2
graph twoway /*
	*/ rcap lci3 uci3 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci3 uci3 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci3 uci3 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci3 uci3 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion3 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion3 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion3 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion3 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion3 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion3 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion3 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion3 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("C", replace) title("South Asian women") yscale(range(0 0.20)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.20 "0.20")
restore

. //south asian men eth

preserve
. keep if sex == 1
graph twoway /*
	*/ rcap lci3 uci3 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci3 uci3 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci3 uci3 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci3 uci3 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion3 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion3 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion3 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion3 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion3 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion3 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion3 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion3 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("D", replace) title("South Asian men") yscale(range(0 0.20)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.20 "0.20")
restore

. //white women eth

preserve
. keep if sex == 2
graph twoway /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci1 uci1 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci1 uci1 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion1 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion1 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion1 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion1 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("E", replace) title("White women") yscale(range(0 1)) ylabel(0 0.1 "0.1" 0.2 "0.2" 0.3 "0.3" 0.4 "0.4" 0.5 "0.5" 0.6 "0.6" 0.7 "0.7" 0.8 "0.8" 0.9 "0.9" 1) 
restore


. //white men eth

preserve
. keep if sex == 1
graph twoway /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci1 uci1 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci1 uci1 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion1 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion1 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion1 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion1 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("F", replace) title("White men") yscale(range(0 1)) ylabel(0 0.1 "0.1" 0.2 "0.2" 0.3 "0.3" 0.4 "0.4" 0.5 "0.5" 0.6 "0.6" 0.7 "0.7" 0.8 "0.8" 0.9 "0.9" 1) 
restore

. //other women eth

preserve
. keep if sex == 2
graph twoway /*
	*/ rcap lci4 uci4 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci4 uci4 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci4 uci4 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci4 uci4 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion4 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion4 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion4 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion4 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion4 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion4 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion4 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion4 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(1 2 3 4) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)" 3 "hi" 4 "yes")) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ name("G", replace) title("Women of other ethnic groups") yscale(range(0 0.15)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15")
restore

. //other men eth

preserve
. keep if sex == 1
graph twoway /*
	*/ rcap lci4 uci4 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5) ||  /*
	*/ rcap lci4 uci4 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5) || /*
	*/ rcap lci4 uci4 _at1 if _at3==5 & _at2==1, color(green*1.5%50) msize(*2.5) ||	/*
	*/ rcap lci4 uci4 _at1 if _at3==5 & _at2==0, color(green*1.5%50) msize(*2.5) ||/*
	*/ scatter proportion4 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion4 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  ||	/*	
	*/ scatter proportion4 _at1 if _at3==5 & _at2==1, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion4 _at1 if _at3==5 & _at2==0, mcolor(green*1.5) msymbol(Th) msize(*2)  ||	/*
	*/ line proportion4 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)|| /*
	*/ line proportion4 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot) || /*
	*/ line proportion4 _at1 if _at3==5 & _at2==1, lcolor(green*1.5) lpattern(dot)|| /*
	*/ line proportion4 _at1 if _at3==5 & _at2==0, lcolor(green*1.5) lpattern(dot)	/*
	*/ legend(order(5 7 6 8) label(5 "Quintile 1 (least deprived) with T2D") label(6 "Quintile 1 (least deprived) without T2D") label(7 "Quintile 5 (most deprived) with T2D") label(8 "Quintile 5 (most deprived) without T2D") rows(2) position(6))  xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) /*
	*/ name("H", replace) title("Men of other ethnic groups") xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") yscale(range(0 0.15)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15")
restore

graph combine A B C D E F G H, row(4) col(2) ysize(12) xsize(12) scale(0.45) imargin(0 0 4 0) name("margineth_by_eth", replace)

graph export "...margineth_by_eth.svg", as(svg) name("margineth_by_eth") replace



*****************************
//// MARGIN table eths
*******************************


use "...all_crude_margins_eths.dta", clear

rename exp diabetes_status

label define Ab 18 "18-24" 25 "25-29" 30 "30-34" 35 "35-39" 40 "40-44" 45 "45-49" 50 "50-54" 55 "55-59" 60 "60-64" 65 "65-69" 70 "70-74" 75 "75-79" 80 "80-84" 85 "≥85"
label values age_bands Ab


forval i = 1(1)4 {
	gen p`i' =  string(proportion`i', "%9.2f")
	gen l`i' =  string(lci`i', "%9.2f")
	gen u`i' =  string(uci`i', "%9.2f")
	gen prev`i' = p`i' + " (" + l`i' + ", " + u`i' + ")" 
	drop proportion`i' lci`i' uci`i' p`i' l`i' u`i'
}

rename prev1 White
rename prev2 Black
rename prev3 South_Asian
rename prev4 Other

order sex diabetes_status imd age_bands Black South_Asian White Other

sort age_bands
sort imd, stable
sort diabetes_status, stable
sort sex, stable

label define imd 1 "Least deprived" 5 "Most deprived"
label values imd imd


export excel using "...margins_eths.xls", firstrow(variables) replace


******************
******************
////GRAPHS MARGINS IMD********
******************
******************


use "...all_crude_margins_imd.dta", clear

rename eths _at3
rename age_bands _at1
rename exp _at2

replace _at1 = 	1	 if _at1 == 	18
replace _at1 = 	2	 if _at1 == 	25
replace _at1 = 	3	 if _at1 == 	30
replace _at1 = 	4	 if _at1 == 	35
replace _at1 = 	5	 if _at1 == 	40
replace _at1 = 	6	 if _at1 == 	45
replace _at1 = 	7	 if _at1 == 	50
replace _at1 = 	8	 if _at1 == 	55
replace _at1 = 	9	 if _at1 == 	60
replace _at1 = 	10	 if _at1 == 	65
replace _at1 = 	11	 if _at1 == 	70
replace _at1 = 	12	 if _at1 == 	75
replace _at1 = 	13	 if _at1 == 	80
replace _at1 = 	14	 if _at1 == 	85

replace _at1 = _at1 - 0.1 if _at2 ==0
replace _at1 = _at1 + 0.1 if _at2 ==1


. //quintile 1 women


preserve
. keep if sex == 2
graph twoway /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==2 & _at2==1, mcolor(green) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==2 & _at2==0, mcolor(green) msymbol(Th) msize(*2)   ||	/*
	*/ scatter proportion1 _at1 if _at3==3 & _at2==1, mcolor(dkorange) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==3 & _at2==0, mcolor(dkorange) msymbol(Th) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==4 & _at2==1, mcolor(lavender) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==4 & _at2==0, mcolor(lavender) msymbol(Th) msize(*2) ||/*
	*/ line proportion1 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)  || /*
	*/ line proportion1 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot)  || /*
	*/ line proportion1 _at1 if _at3==2 & _at2==1, lcolor(green) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==2 & _at2==0, lcolor(green) lpattern(dot)  ||	/*
	*/ line proportion1 _at1 if _at3==3 & _at2==1, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==3 & _at2==0, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==4 & _at2==1, lcolor(lavender) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==4 & _at2==0, lcolor(lavender) lpattern(dot) ||/*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5)||  /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5)|| /*
	*/ rcap lci1 uci1 _at1 if _at3==2 & _at2==1, color(green%50) msize(*2.5)||	/*
	*/ rcap lci1 uci1 _at1 if _at3==2 & _at2==0, color(green%50) msize(*2.5)||/*
	*/ rcap lci1 uci1 _at1 if _at3==3 & _at2==1, color(dkorange%50) ||  /*
	*/ rcap lci1 uci1 _at1 if _at3==3 & _at2==0, color(dkorange%50) || /*
	*/ rcap lci1 uci1 _at1 if _at3==4 & _at2==1, color(lavender%50) ||	/*
	*/ rcap lci1 uci1 _at1 if _at3==4 & _at2==0, color(lavender%50) /*
	*/ xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ yscale(range(0 0.30)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.2 "0.20" 0.25 "0.25" 0.30 "0.30") name("A", replace) title("Women from quintile 1 (least deprived)")
restore

//quintile 1 men
preserve
. keep if sex == 1
graph twoway /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==2 & _at2==1, mcolor(green) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion1 _at1 if _at3==2 & _at2==0, mcolor(green) msymbol(Th) msize(*2)   ||	/*
	*/ scatter proportion1 _at1 if _at3==3 & _at2==1, mcolor(dkorange) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==3 & _at2==0, mcolor(dkorange) msymbol(Th) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==4 & _at2==1, mcolor(lavender) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion1 _at1 if _at3==4 & _at2==0, mcolor(lavender) msymbol(Th) msize(*2) ||/*
	*/ line proportion1 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)  || /*
	*/ line proportion1 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot)  || /*
	*/ line proportion1 _at1 if _at3==2 & _at2==1, lcolor(green) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==2 & _at2==0, lcolor(green) lpattern(dot)  ||	/*
	*/ line proportion1 _at1 if _at3==3 & _at2==1, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==3 & _at2==0, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==4 & _at2==1, lcolor(lavender) lpattern(dot) || /*
	*/ line proportion1 _at1 if _at3==4 & _at2==0, lcolor(lavender) lpattern(dot) ||/*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5)||  /*
	*/ rcap lci1 uci1 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5)|| /*
	*/ rcap lci1 uci1 _at1 if _at3==2 & _at2==1, color(green%50) msize(*2.5)||	/*
	*/ rcap lci1 uci1 _at1 if _at3==2 & _at2==0, color(green%50) msize(*2.5)||/*
	*/ rcap lci1 uci1 _at1 if _at3==3 & _at2==1, color(dkorange%50) ||  /*
	*/ rcap lci1 uci1 _at1 if _at3==3 & _at2==0, color(dkorange%50) || /*
	*/ rcap lci1 uci1 _at1 if _at3==4 & _at2==1, color(lavender%50) ||	/*
	*/ rcap lci1 uci1 _at1 if _at3==4 & _at2==0, color(lavender%50) /*
	*/ xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ yscale(range(0 0.30)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.2 "0.20" 0.25 "0.25" 0.30 "0.30") name("B", replace) title("Men from quintile 1 (least deprived)")
restore

. //quintile 5 women

preserve
. keep if sex == 2
graph twoway /*
	*/ scatter proportion5 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==2 & _at2==1, mcolor(green) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==2 & _at2==0, mcolor(green) msymbol(Th) msize(*2)   ||	/*
	*/ scatter proportion5 _at1 if _at3==3 & _at2==1, mcolor(dkorange) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==3 & _at2==0, mcolor(dkorange) msymbol(Th) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==4 & _at2==1, mcolor(lavender) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==4 & _at2==0, mcolor(lavender) msymbol(Th) msize(*2) ||/*
	*/ line proportion5 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)  || /*
	*/ line proportion5 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot)  || /*
	*/ line proportion5 _at1 if _at3==2 & _at2==1, lcolor(green) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==2 & _at2==0, lcolor(green) lpattern(dot)  ||	/*
	*/ line proportion5 _at1 if _at3==3 & _at2==1, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==3 & _at2==0, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==4 & _at2==1, lcolor(lavender) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==4 & _at2==0, lcolor(lavender) lpattern(dot) ||/*
	*/ rcap lci5 uci5 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5)||  /*
	*/ rcap lci5 uci5 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5)|| /*
	*/ rcap lci5 uci5 _at1 if _at3==2 & _at2==1, color(green%50) msize(*2.5)||	/*
	*/ rcap lci5 uci5 _at1 if _at3==2 & _at2==0, color(green%50) msize(*2.5)||/*
	*/ rcap lci5 uci5 _at1 if _at3==3 & _at2==1, color(dkorange%50) ||  /*
	*/ rcap lci5 uci5 _at1 if _at3==3 & _at2==0, color(dkorange%50) || /*
	*/ rcap lci5 uci5 _at1 if _at3==4 & _at2==1, color(lavender%50) ||	/*
	*/ rcap lci5 uci5 _at1 if _at3==4 & _at2==0, color(lavender%50) /*
	*/ xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5)) legend(off) /*
	*/ yscale(range(0 0.45)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.2 "0.20" 0.25 "0.25" 0.30 "0.30" 0.35 "0.35" 0.40 "0.40" 0.45 "0.45") name("C", replace) title("Women from quintile 5 (most deprived)")
restore



. //quintile 5 men

preserve
. keep if sex == 1
graph twoway /*
	*/ scatter proportion5 _at1 if _at3==1 & _at2==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==1 & _at2==0, mcolor(red) msymbol(Th) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==2 & _at2==1, mcolor(green) msymbol(Oh) msize(*2)  || /*
	*/ scatter proportion5 _at1 if _at3==2 & _at2==0, mcolor(green) msymbol(Th) msize(*2)   ||	/*
	*/ scatter proportion5 _at1 if _at3==3 & _at2==1, mcolor(dkorange) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==3 & _at2==0, mcolor(dkorange) msymbol(Th) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==4 & _at2==1, mcolor(lavender) msymbol(Oh) msize(*2) || /*
	*/ scatter proportion5 _at1 if _at3==4 & _at2==0, mcolor(lavender) msymbol(Th) msize(*2) ||/*
	*/ line proportion5 _at1 if _at3==1 & _at2==1, lcolor(red) lpattern(dot)  || /*
	*/ line proportion5 _at1 if _at3==1 & _at2==0, lcolor(red) lpattern(dot)  || /*
	*/ line proportion5 _at1 if _at3==2 & _at2==1, lcolor(green) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==2 & _at2==0, lcolor(green) lpattern(dot)  ||	/*
	*/ line proportion5 _at1 if _at3==3 & _at2==1, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==3 & _at2==0, lcolor(dkorange) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==4 & _at2==1, lcolor(lavender) lpattern(dot) || /*
	*/ line proportion5 _at1 if _at3==4 & _at2==0, lcolor(lavender) lpattern(dot) ||/*
	*/ rcap lci5 uci5 _at1 if _at3==1 & _at2==1, color(red%50) msize(*2.5)||  /*
	*/ rcap lci5 uci5 _at1 if _at3==1 & _at2==0, color(red%50) msize(*2.5)|| /*
	*/ rcap lci5 uci5 _at1 if _at3==2 & _at2==1, color(green%50) msize(*2.5)||	/*
	*/ rcap lci5 uci5 _at1 if _at3==2 & _at2==0, color(green%50) msize(*2.5)||/*
	*/ rcap lci5 uci5 _at1 if _at3==3 & _at2==1, color(dkorange%50) ||  /*
	*/ rcap lci5 uci5 _at1 if _at3==3 & _at2==0, color(dkorange%50) || /*
	*/ rcap lci5 uci5 _at1 if _at3==4 & _at2==1, color(lavender%50) ||	/*
	*/ rcap lci5 uci5 _at1 if _at3==4 & _at2==0, color(lavender%50) /*
	*/ xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85") xtitle("Age group (years)") ytitle("Proportion", size(*1.5))  /*
		*/ legend(off) yscale(range(0 0.45)) ylabel(0 0.05 "0.05" 0.10 "0.10" 0.15 "0.15" 0.2 "0.20" 0.25 "0.25" 0.30 "0.30" 0.35 "0.35" 0.40 "0.40" 0.45 "0.45") name("D", replace) title("Men from quintile 5 (most deprived)")
restore


graph combine A B C D, row(2) col(2) ysize(6) xsize(11) scale(0.7) imargin(0 0 4 0) name("marginimd_by_imd", replace)

graph export "...marginimd_by_imd.svg", as(svg) name("marginimd_by_imd") replace



*********************************************************
///GRAPHS RRS// - eths
**********************************************************


use "...RRs_eths.dta", clear

graph drop _all
gen RR2 = 1

replace age_bands = 	1	 if age_bands == 	18
replace age_bands = 	2	 if age_bands == 	25
replace age_bands = 	3	 if age_bands == 	30
replace age_bands = 	4	 if age_bands == 	35
replace age_bands = 	5	 if age_bands == 	40
replace age_bands = 	6	 if age_bands == 	45
replace age_bands = 	7	 if age_bands == 	50
replace age_bands = 	8	 if age_bands == 	55
replace age_bands = 	9	 if age_bands == 	60
replace age_bands = 	10	 if age_bands == 	65
replace age_bands = 	11	 if age_bands == 	70
replace age_bands = 	12	 if age_bands == 	75
replace age_bands = 	13	 if age_bands == 	80
replace age_bands = 	14	 if age_bands == 	85

replace age_bands = age_bands -0.1 if imd ==1
replace age_bands = age_bands +0.1 if imd ==5

/// Black women


preserve
. keep if eths==2 & sex==2
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1) sort legend(off)  yscale(log range(0.8 2.5)) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Black women") name("A",replace) 
restore


/// Black men

preserve
. keep if eths==2 & sex==1
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1) sort legend(off)   yscale(log range(0.8 2.5)) ylabel(1 1.5 2 2.5) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Black men") name("B",replace) 
restore

/// South Asian women

preserve
. keep if eths==3 & sex==2
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1)  sort legend(off)   yscale(log range(0.9 3)) ylabel(1 1.5 2 2.5 3) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("South Asian women") name("C",replace) 
restore


/// South Asian men

preserve
. keep if eths==3 & sex==1
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2)  || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1)  legend(off)   yscale(log range(0.9 3)) ylabel(1 1.5 2 2.5 3) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")   ytitle("Relative risk", size(*1.5)) title("South Asian men") name("D",replace) 
restore

/// White women

preserve
. keep if eths==1 & sex==2
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1) sort legend(off)   yscale(log range(0.84 1.005)) ylabel(0.85 "0.85" 0.9 "0.90" 0.95 "0.95" 1) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5))title("White women") name("E",replace) 
restore


 
/// White men

preserve
. keep if eths==1 & sex==1
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red)  msymbol(Oh) msize(*2) || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1) sort legend(off)  yscale(log range(0.84 1.005)) ylabel(0.85 "0.85" 0.9 "0.90" 0.95 "0.95" 1) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")   ytitle("Relative risk", size(*1.5)) title("White men") name("F",replace) 
restore


/// Other women

preserve
. keep if eths==4 & sex==2
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) yline(1)  sort legend(off)  yscale(log range(0.8 2)) ylabel(0.8 "0.8" 1 1.5 2) xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women of other ethnic groups") name("G",replace) 
restore



/// other men

preserve
. keep if eths==4 & sex==1
graph twoway /*
*/scatter RR age_bands if imd==1, mcolor(red) msymbol(Oh) msize(*2)  || /*
*/scatter RR age_bands if imd==5, mcolor(green*1.5) msymbol(Oh) msize(*2) || /*
*/rcap RR_lci RR_uci age_bands if imd==1, color(red%50) ||  /*
*/rcap RR_lci RR_uci age_bands if imd==5, color(green*1.5%50) || /*
*/line RR age_bands if imd==1, mcolor(red) lpattern(dot)  || /*
*/line RR age_bands if imd==5, lcolor(green*1.5) lpattern(dot) /*
*/ sort legend(off) yline(1)  xtitle("Age group (years)") xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  ytitle("Relative risk", size(*1.5)) title("Men of other ethnic groups") name("H",replace) 
restore

graph combine A B C D E F G H, row(4) col(2) ysize(12) xsize(12) scale(0.45) imargin(0 0 2 0) name("RReth_imd", replace)

graph export "...RReth_imd.svg", as(svg) name("RReth_imd") replace

///legend(order(1 2) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 5 (most deprived)") rows(1) position(6))  yscale(log range(0.8 2)) ylabel(0.8 "0.8" 1 1.5 2) 


*********************************************************
///GRAPHS RRS// imd
**********************************************************


use "...RRs_imd.dta", clear

gen RR2 = 1

replace age_bands = 	1	 if age_bands == 	18
replace age_bands = 	2	 if age_bands == 	25
replace age_bands = 	3	 if age_bands == 	30
replace age_bands = 	4	 if age_bands == 	35
replace age_bands = 	5	 if age_bands == 	40
replace age_bands = 	6	 if age_bands == 	45
replace age_bands = 	7	 if age_bands == 	50
replace age_bands = 	8	 if age_bands == 	55
replace age_bands = 	9	 if age_bands == 	60
replace age_bands = 	10	 if age_bands == 	65
replace age_bands = 	11	 if age_bands == 	70
replace age_bands = 	12	 if age_bands == 	75
replace age_bands = 	13	 if age_bands == 	80
replace age_bands = 	14	 if age_bands == 	85

***BLACK***


///women in Q1 ()

preserve
. keep if imd==1 & sex==2 & eths ==2
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 1 (least deprived)") name("A",replace) yscale(log range(0.4 1.4)) ylabel(0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore

///men in Q1 ()

preserve
. keep if imd==1 & sex==1 & eths ==2
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 1 (least deprived)") name("B",replace) yscale(log range(0.4 1.4)) ylabel(0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore


///women in Q5 ()

preserve
. keep if imd==5 & sex==2 & eths ==2
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 5 (most deprived)") name("C",replace) yscale(log range(0.9 1.25)) ylabel(0.9 "0.9" 1 1.1 1.2)
restore

///men in Q5 ()

preserve
. keep if imd==5 & sex==1 & eths ==2
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 5 (most deprived)") name("D",replace) yscale(log range(0.9 1.25)) ylabel(0.9 "0.9" 1 1.1 1.2)
restore

***SA***


///women in Q1 ()

preserve
. keep if imd==1 & sex==2 & eths ==3
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 1 (least deprived)") name("E",replace)  yscale(log range(0.58 1.4)) ylabel( 0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore

///men in Q1 ()

preserve
. keep if imd==1 & sex==1 & eths ==3
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 1 (least deprived)") name("F",replace) yscale(log range(0.58 1.4)) ylabel(0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore


///women in Q5 ()

preserve
. keep if imd==5 & sex==2 & eths ==3
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1)  sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 5 (most deprived)") name("G",replace) yscale(log range(0.8 1.47)) ylabel(0.8 "0.8" 1 1.2 1.4)
restore

///men in Q5 ()

preserve
. keep if imd==5 & sex==1 & eths ==3
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 5 (most deprived)") name("H",replace) yscale(log range(0.8 1.47)) ylabel(0.8 "0.8" 1 1.2 1.4)
restore

***White***


///women in Q1 ()

preserve
. keep if imd==1 & sex==2 & eths ==1
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 1 (least deprived)") name("I",replace)  yscale(log range(0.92 1.04)) ylabel(0.92 "0.92" 0.96 "0.96" 1 1.04)
restore

///men in Q1 ()

preserve
. keep if imd==1 & sex==1 & eths ==1
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 1 (least deprived)") name("J",replace) yscale(log range(0.92 1.04)) ylabel(0.92 "0.92" 0.96 "0.96" 1 1.04)
restore


///women in Q5 ()

preserve
. keep if imd==5 & sex==2 & eths ==1
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 5 (most deprived)") name("K",replace) yscale(log range(0.95 1.12)) ylabel(0.95 "0.95" 1 1.05 1.1)
restore

///men in Q5 ()

preserve
. keep if imd==5 & sex==1 & eths ==1
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 5 (most deprived)") name("L",replace) yscale(log range(0.95 1.12)) ylabel(0.95 "0.95" 1 1.05 1.1)
restore



***Other***


///women in Q1 ()

preserve
. keep if imd==1 & sex==2 & eths ==4
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 1 (least deprived)") name("M",replace) yscale(log range(0.6 1.4)) ylabel(0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore

///men in Q1 ()

preserve
. keep if imd==1 & sex==1 & eths ==4
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 1 (least deprived)") name("N",replace) yscale(log range(0.6 1.4)) ylabel(0.6 "0.6" 0.8 "0.8" 1 1.2 1.4)
restore


///women in Q5 ()

preserve
. keep if imd==5 & sex==2 & eths ==4
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort legend(off)   xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Women from quintile 5 (most deprived)") name("O",replace) yscale(log range(0.8 1.6)) ylabel(0.8 "0.8" 1 1.2 1.4 1.6)
restore

///men in Q5 ()

preserve
. keep if imd==5 & sex==1 & eths ==4
graph twoway /*
	*/ scatter RR age_bands if eths==1 , mcolor(red) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==2 , mcolor(green) msymbol(Oh) msize(*2.5)  || /*
	*/ scatter RR age_bands if eths==3 , mcolor(dkorange) msymbol(Oh) msize(*2.5) || /*
	*/ scatter RR age_bands if eths==4 , mcolor(lavender) msymbol(Oh) msize(*2.5) || /*
	*/ rcap RR_lci RR_uci age_bands if eths==1 , color(red%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==2 , color(green%50) ||	/*
	*/ rcap RR_lci RR_uci age_bands if eths==3 , color(dkorange%50) ||  /*
	*/ rcap RR_lci RR_uci age_bands if eths==4 , color(lavender%50)	||/*
	*/ line RR age_bands if eths==1, lcolor(red) lpattern(dot)  || /*
	*/ line RR age_bands if eths==2, lcolor(green) lpattern(dot)  || /*
	*/ line RR age_bands if eths==3, lcolor(dkorange) lpattern(dot) || /*
	*/ line RR age_bands if eths==4, lcolor(lavender) lpattern(dot) yline(1) sort  legend(off)  xlabel(1 "18-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" 8 "55-59" 9 "60-64" 10 "65-69" 11 "70-74" 12 "75-79" 13 "80-84" 14 "≥85")  xtitle("Age group (years)")  ytitle("Relative risk", size(*1.5)) title("Men from quintile 5 (most deprived)") name("P",replace) yscale(log range(0.8 1.6)) ylabel(0.8 "0.8" 1 1.2 1.4 1.6)
restore

graph combine A B E F  C D G H  I J M N  K L O P, col(4) ysize(12) xsize(18) altshrink imargin(6 0 4 0) name("RRimd_eth2", replace)

graph export "...RRimd_eth2.svg", as(svg) name("RRimd_eth2") replace



*****************************
//// MARGIN table imd
*******************************


use "...all_crude_margins_imd.dta", clear


label define Ab 18 "18-24" 25 "25-29" 30 "30-34" 35 "35-39" 40 "40-44" 45 "45-49" 50 "50-54" 55 "55-59" 60 "60-64" 65 "65-69" 70 "70-74" 75 "75-79" 80 "80-84" 85 "≥85"
label values age_bands Ab


forval i = 1(4)5 {
	gen p`i' =  string(proportion`i', "%9.2f")
	gen l`i' =  string(lci`i', "%9.2f")
	gen u`i' =  string(uci`i', "%9.2f")
	gen prev`i' = p`i' + " (" + l`i' + ", " + u`i' + ")" 
	drop proportion`i' lci`i' uci`i' p`i' l`i' u`i'
}

rename prev1 IMD1
rename prev5 IMD5

order sex exp eths age_bands IMD1 IMD5

sort age_bands
sort eths, stable
sort exp, stable
sort sex, stable

rename exp diabetes_status
export excel using "...margins_imd.xls", firstrow(variables) replace



