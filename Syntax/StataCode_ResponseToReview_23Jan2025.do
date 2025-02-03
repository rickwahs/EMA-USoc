***** This code has been created to respond to reviewers and initiated November 2024 
*** Written by Richard Shaw richard.shaw@glasgow.ac.uk
* Note I am dyslexic and Stata doesn't highlight spelling mistakes. 

***** Background
*** This is an adapation of the code used for the primary analsyes, and has been severely limitted  
*** Initial project proposal was pre-registered and is avaiable from  https://osf.io/u3avm
*** Main analysis code is availabe from https://github.com/rickwahs/EMA-USoc
*** Sections to derive any houshold variables, children in foster familes are in the orginal analysis file. This will be indicated  by the derived file path. New analsyes for the additional falsifaction test use the reviewr file path (see below). 



***** The  key contributions of this file are 
*** New Youth data files: Line 35
*** Calculating the weights for the youth sample: Line 120
*** Merging youth data for further analysis: Line ~340
*** Identifying those in the bottom 10% of incomes: Line 480
*** Merging youth data with main analysis fle data: Line 540  


***** File locations 
*** Original data is stored in seperate location. 
*** Derived file locatoins are for files derived for the main analysis. 
*** Reviewer files derived for addtional anlayses. 
global original "T:\projects\UnderstandingSocietyRShaw\Data\Original\"
global derived "T:\projects\UnderstandingSocietyRShaw\Data\AnalysisCheck\"
global reviewer "T:\projects\UnderstandingSocietyRShaw\Data\Reviewer\"


***** DATA VERSIONS
*** There are vary release versions Understanding Society. These have been applied to SN6931 
*** University of Essex, Institute for Social and Economic Research. (2023). Understanding Society: Waves 1-13, 2009-2022 and Harmonised BHPS: Waves 1-18, 1991-2009: Special Licence Access. [data collection]. 16th Edition. UK Data Service. SN: 6931, DOI: http://doi.org/10.5255/UKDA-SN-6931-15
*** Date of relase July 16th
*** Date of access of used version 03 Oct 2024



**** Youth data files 

*** Start with wave A 
foreach x in a {
use "$original\`x'_youth_protect.dta", clear

keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
`x'_ythscus_xw  /// Youth date weight variables 
`x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv /// Youth Interview data variables 
`x'_sex `x'_dvage  `x'_ethn_dv /// Youth demograhic variables and  happiness variabels on next line
`x'_yphsw `x'_yphap `x'_yphfm `x'_yphfr `x'_yphsc `x'_yphlf

*Recode to create happiness measure 
recode `x'_yphlf -9 = . 1 = 7 2 = 6 3 = 5 4 = 4 5 = 3 6 = 2 7 = 1, gen(`x'_happiness)

* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
gen `x'_interview_acad_year =  `x'_intdaty_dv  if  `x'_intdatm_dv >= 9 & `x'_intdatm_dv  !=. 
replace `x'_interview_acad_year =  `x'_intdaty_dv -1  if `x'_intdatm_dv <= 8 & `x'_intdatm_dv  > 0  
gen `x'_interview_cal_month =  `x'_intdatm_dv

*Rename the weight variable for consistency. It is not used in analysis. 
rename `x'_ythscus_xw `x'_youth_weight_xc 
save "$reviewer\`x'_youth_selected.dta", replace
}




*** Waves 2 to 5 (B to e)

foreach x in b  c d e  {
use "$original\`x'_youth_protect.dta", clear

keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
`x'_ythscub_xw  /// Youth date weight variables 
`x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv /// Youth Interview data variables 
`x'_sex `x'_dvage  `x'_ethn_dv /// Youth demograhic variables and  happiness on next line
`x'_yphsw `x'_yphap `x'_yphfm `x'_yphfr `x'_yphsc `x'_yphlf

*Recode to create happiness measure 
recode `x'_yphlf -9 = . 1 = 7 2 = 6 3 = 5 4 = 4 5 = 3 6 = 2 7 = 1, gen(`x'_happiness)

* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
gen `x'_interview_acad_year =  `x'_intdaty_dv  if  `x'_intdatm_dv >= 9 & `x'_intdatm_dv  !=. 
replace `x'_interview_acad_year =  `x'_intdaty_dv -1  if `x'_intdatm_dv <= 8 & `x'_intdatm_dv  > 0  
gen `x'_interview_cal_month =  `x'_intdatm_dv

*Rename the weight variable for consistency. It is not used in analysis. 
rename `x'_ythscub_xw `x'_youth_weight_xc 
save "$reviewer\`x'_youth_selected.dta", replace
}

*Waves 6 to 11 (f to k) Note as l starts in the pandemic this is ignored

foreach x in  f g h i j k  {
use "$original\`x'_youth_protect.dta", clear

keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
`x'_ythscui_xw  /// Youth date weight variables 
`x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv /// Youth Interview data variables 
`x'_sex `x'_dvage  `x'_ethn_dv /// Youth demograhic variables and  happiness variabels on next line
`x'_yphsw `x'_yphap `x'_yphfm `x'_yphfr `x'_yphsc `x'_yphlf

*Recode to create happiness measure 
recode `x'_yphlf -9 = . 1 = 7 2 = 6 3 = 5 4 = 4 5 = 3 6 = 2 7 = 1, gen(`x'_happiness)

* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
gen `x'_interview_acad_year =  `x'_intdaty_dv  if  `x'_intdatm_dv >= 9 & `x'_intdatm_dv  !=. 
replace `x'_interview_acad_year =  `x'_intdaty_dv -1  if `x'_intdatm_dv <= 8 & `x'_intdatm_dv  > 0  
gen `x'_interview_cal_month =  `x'_intdatm_dv

*Rename the weight variable for consistency. It is not used in analysis. 
rename `x'_ythscui_xw `x'_youth_weight_xc 
save "$reviewer\`x'_youth_selected.dta", replace
}




*** Youth Revised weights 
**This was created by editing the code used for the main analsyes with some minor edits.  

**** Calculating revised weights to correct for sample being allocated based on school year. 

* Key info for coding the weights. 
* Weblink to the relevant weight guidlines at understanding Society  
* https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/how-to-use-weights-analysis-guidance-for-weights-psu-strata






*Note that all the unmatched people in the youth survey appear to have a derived age of 16. Which makes sense. 

foreach x in a {
use "$original\`x'_child_protect.dta", clear
merge 1:1 pidp using  "$original\`x'_youth_protect.dta" , gen(merge_`x') 	
keep if merge_`x' == 3
keep pidp `x'_month `x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv `x'_sampst `x'_ythscus_xw 

save  "$reviewer\\`x'_youth_vars_for_acadweight.dta" ,replace  
}



foreach x in b c d e  {
use "$original\`x'_child_protect.dta", clear
merge 1:1 pidp using  "$original\`x'_youth_protect.dta" , gen(merge_`x') 	
keep if merge_`x' == 3
keep pidp `x'_month `x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv `x'_sampst `x'_ythscub_xw 
save  "$reviewer\\`x'_youth_vars_for_acadweight.dta" ,replace  
}


*Note for waves h and beyound there are small number of people, n 2 to 12, who are aged 16 when completing the youth questionaire and not matched with the child questionaire. These will be dropped not relevant at a later stage anyway. 
foreach x in f g h i j k  {
use "$original\`x'_child_protect.dta", clear
merge 1:1 pidp using  "$original\`x'_youth_protect.dta" , gen(merge_`x') 	
keep if merge_`x' == 3
keep pidp `x'_month `x'_intdatd_dv `x'_intdatm_dv `x'_intdaty_dv `x'_sampst `x'_ythscui_xw 
save  "$reviewer\\`x'_youth_vars_for_acadweight.dta" ,replace  
}


use "$reviewer\a_youth_vars_for_acadweight.dta", clear
foreach x in  b c d e f g h i j k  {
	merge 1:1 pidp using  "$reviewer\`x'_youth_vars_for_acadweight.dta" , gen(merge_`x') 	
}


* Create base multiplier for all individuals  
gen ind = 1 
*Note create numerator/denominators for weighted number of indiviudals in each period accounting for slightly different weights across waves. 
* A wave the only "us" weight
foreach x in a {
sum ind [aw = `x'_ythscus_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_ythscus_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_ythscus_xw]  if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}

*The "ub" waves b c d e 
foreach x in b c d e  {
sum ind [aw = `x'_ythscub_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_ythscub_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_ythscub_xw] if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}

*The "ui" waves f g h i j k  
foreach x in f g h i j k  {
sum ind [aw = `x'_ythscui_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_ythscui_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_ythscui_xw] if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}



*** 2009 this requires addtional weighting from wave A + wave b 
* Note the Yr1 months from a should probably be weighted as normal: 
* but the Yr2 months should probably increase weighed to compensate for the lack of Yr2 froma previous wave. 
*note the Yr1 in a are 9 to 12 

sum ind [aw = a_ythscus_xw ] if a_month >=9 & a_month <=12 
gen a_wtd_yr1_normal_add = r(sum_w)
sum ind [aw = b_ythscub_xw] if b_month >=9 & b_month <=12 
gen b_wtd_yr1_normal_add = r(sum_w)


sum ind [aw = a_ythscus_xw] if a_month >=13 & a_month <=20
gen a_wtd_yr2_extra_need = r(sum_w)
sum ind [aw = b_ythscub_xw]  if b_month >=13 & b_month <=24
gen b_wtd_yr2_extra_needed = r(sum_w)


*Set all as 0 
gen weight_2009 = 0 
*Yr1 people in 2009 academic year A wave weighted normally
replace weight_2009 = a_ythscus_xw *(b_wtd_yr1_normal_add/a_wtd_yr1_normal_add) if a_month >=9 & a_month < =12  
*Yrs 2 people in A wave academic year upweighted to account for no 0 wave 
replace weight_2009 = a_ythscus_xw *(b_wtd_yr2_extra_needed/a_wtd_yr2_extra_need) if a_month >=13 & a_month < =20  



*The people from wave b can be included as is already possible 
replace weight_2009 = b_ythscub_xw*(b_wtd_yr1_add/b_wtd_yr1_add) if b_month >=1 & b_month <= 8 


*** 2010 this requires some people from a, b and c. 
* Being done first as this is a more standard weighting approach. 
gen weight_2010 = 0 
replace weight_2010 = a_ythscus_xw*(b_wtd_yr2_add/a_wtd_yr2_add) if a_month >=21 & a_month <=24 
replace weight_2010 = b_ythscub_xw*(b_wtd_core/b_wtd_core) if b_month >=9 & b_month < =20  
replace weight_2010 = c_ythscub_xw*(b_wtd_yr1_add/c_wtd_yr1_add) if c_month >=1 & c_month <= 8 

*** 2011 
gen weight_2011 = 0 
replace weight_2011 = b_ythscub_xw*(b_wtd_yr2_add/b_wtd_yr2_add) if b_month >=21 & b_month <=24 
replace weight_2011 = c_ythscub_xw*(b_wtd_core/c_wtd_core) if c_month >=9 & c_month < =20 
replace weight_2011 = d_ythscub_xw*(b_wtd_yr1_add/d_wtd_yr1_add) if d_month >=1 & d_month <= 8 

*** 2012 
gen weight_2012 = 0 
replace weight_2012 = c_ythscub_xw*(b_wtd_yr2_add/c_wtd_yr2_add) if c_month >=21 & c_month <=24 
replace weight_2012 = d_ythscub_xw*(b_wtd_core/d_wtd_core) if d_month >=9 & d_month < =20 
replace weight_2012 = e_ythscub_xw*(b_wtd_yr1_add/e_wtd_yr1_add) if e_month >=1 & e_month <= 8 

*** 2013 
gen weight_2013 = 0 
replace weight_2013 = d_ythscub_xw*(b_wtd_yr2_add/d_wtd_yr2_add) if d_month >=21 & d_month <=24 
replace weight_2013 = e_ythscub_xw*(b_wtd_core/e_wtd_core) if e_month >=9 & e_month < =20 
replace weight_2013 = f_ythscui_xw*(b_wtd_yr1_add/f_wtd_yr1_add) if f_month >=1 & f_month <= 8 

*** 2014 
gen weight_2014 = 0 
replace weight_2014 = e_ythscub_xw*(b_wtd_yr2_add/e_wtd_yr2_add) if e_month >=21 & e_month <=24 
replace weight_2014 = f_ythscui_xw*(b_wtd_core/f_wtd_core) if f_month >=9 & f_month < =20 
replace weight_2014 = g_ythscui_xw*(b_wtd_yr1_add/g_wtd_yr1_add) if g_month >=1 & g_month <= 8 

*** 2015 
gen weight_2015 = 0 
replace weight_2015 = f_ythscui_xw*(b_wtd_yr2_add/f_wtd_yr2_add) if f_month >=21 & f_month <=24 
replace weight_2015 = g_ythscui_xw*(b_wtd_core/g_wtd_core) if g_month >=9 & g_month < =20
replace weight_2015 = h_ythscui_xw*(b_wtd_yr1_add/h_wtd_yr1_add) if h_month >=1 & h_month <= 8 

*** 2016 
gen weight_2016 = 0 
replace weight_2016 = g_ythscui_xw*(b_wtd_yr2_add/g_wtd_yr2_add) if g_month >=21 & g_month <=24 
replace weight_2016 = h_ythscui_xw*(b_wtd_core/h_wtd_core) if h_month >=9 & h_month < =20 
replace weight_2016 = i_ythscui_xw*(b_wtd_yr1_add/i_wtd_yr1_add) if i_month >=1 & i_month <= 8 

*** 2017 
gen weight_2017 = 0 
replace weight_2017 = h_ythscui_xw*(b_wtd_yr2_add/h_wtd_yr2_add) if h_month >=21 & h_month <=24 
replace weight_2017 = i_ythscui_xw*(b_wtd_core/i_wtd_core) if i_month >=9 & i_month < =20 
replace weight_2017 = j_ythscui_xw*(b_wtd_yr1_add/j_wtd_yr1_add) if j_month >=1 & j_month <= 8 

*** 2018 
gen weight_2018 = 0 
replace weight_2018 = i_ythscui_xw*(b_wtd_yr2_add/i_wtd_yr2_add) if i_month >=21 & i_month <=24 
replace weight_2018 = j_ythscui_xw*(b_wtd_core/j_wtd_core) if j_month >=9 & j_month < =20 
replace weight_2018 = k_ythscui_xw*(b_wtd_yr1_add/k_wtd_yr1_add) if k_month >=1 & k_month <= 8 



foreach year in 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 {
	label variable weight_`year' "Academic year starting x Sep to Aug"
}

gen a_acad_weight = .
replace a_acad_weight = weight_2009 if a_month >=9 & a_month <= 20  
replace a_acad_weight = weight_2010 if a_month >=21 & a_month <=24 


*This reiterates as above from b to i 
local number = 2009 
foreach letter in b c d e f g h i {
	display "`letter'" 
	gen `letter'_acad_weight = .
	display "weight_`number'"
	replace  `letter'_acad_weight = weight_`number' if `letter'_month >= 1 & `letter'_month < =8 
	local number = `number' + 1 
	display "weight_`number'"
	replace  `letter'_acad_weight = weight_`number' if `letter'_month >= 9 & `letter'_month < =20
	local number = `number' + 1
	display "weight_`number'"
	replace  `letter'_acad_weight = weight_`number' if `letter'_month >= 21 & `letter'_month < =24
	local number = `number' - 1 
}

*Different code for the waves which overlap with Covid
gen j_acad_weight = . 
replace j_acad_weight = weight_2017 if j_month >= 1 & j_month < =8 
replace j_acad_weight = weight_2018 if j_month >=9 & j_month < =20  
replace j_acad_weight =  . if j_month >=21 & j_month <=24 

gen k_acad_weight = . 
replace k_acad_weight = weight_2018 if k_month >= 1 & k_month < =8 
replace k_acad_weight = . if k_month >=9 & k_month < =20  
replace k_acad_weight =  . if k_month >=21 & k_month <=24 


* Saving the 
keep  pidp *_month weight_* *_acad_weight



save  "$reviewer\\Academic_year_sample_weights_youth.dta" ,replace  




*********************************************************************************
***** Merging files for further analysis  


use "$original\\xwavedat_protect.dta", clear



foreach x in a b c d e f g h i j k   {
merge 1:1 pidp using "$reviewer\`x'_youth_selected.dta", gen(`x'_merge)
merge m:1 `x'_hidp using "$derived\`x'_hhresp_core.dta", gen(`x'_hh_merge)
drop if `x'_hh_merge == 2
merge 1:1 pidp using "$derived\`x'_foster_child.dta", gen(`x'_hh_foster)
drop if `x'_hh_foster == 2 
}



*Cohort allocation 
gen academic_birth_cohort =.
forval i = 1900(1)2021 { 
replace academic_birth_cohort = `i' if (birthy == `i' & birthm >= 9 & birthm !=. ) 
replace academic_birth_cohort = `i' - 1 if (birthy == `i' & birthm <= 8 & birthm > 0) 
}


*This identifies when a person has a school year that relates to sixth form and has carried on in education. 
*** Note line below has been moved above 
foreach  x in a b c d e f g h i j k  {
gen `x'_pre_gsce_school_year = 1 if `x'_interview_acad_year == academic_birth_cohort + 14 & academic_birth_cohort !=. & academic_birth_cohort !=. 
replace `x'_pre_gsce_school_year = 2 if  `x'_interview_acad_year == academic_birth_cohort + 15 & academic_birth_cohort !=. & academic_birth_cohort !=. 
}



merge 1:1 pidp using "$reviewer\Academic_year_sample_weights_youth.dta", gen(acad_weights_youth_merge)

**Generate ever in target age group
gen ever_pre_gcse = .
foreach x in a b c d e f g h i j k   {
recode ever_pre_gcse . = 1 if `x'_pre_gsce_school_year ==1 | `x'_pre_gsce_school_year == 2
}


***********************************************
*Generating an anual income measure 
foreach x in a b c d e f g h i j k {
	gen `x'_an_income = `x'_fihhmngrs1_dv *12
}	



********************************************************************************
****


keep pidp strata psu /// fixed US design variables 
*_youth_weight_xc /// varing US variables 
*_month weight_* *_acad_weight  /// the variables from the weight file 
*_pre_gsce_school_year *_interview_acad_year *_interview_cal_month /// Varying study design variables 
*_hh_foster /// varying study selection variables 
*_country  /// Varying person var 
*_an_income  /// varying exposure variables 
academic_birth_cohort sex racel_dv bornuk_dv  ///   fixed person variabels Outcomes on the next line 
*_happiness *_yphsw *_yphap *_yphfm *_yphfr *_yphsc *_yphlf



rename a_* *_1
rename b_* *_2
rename c_* *_3
rename d_* *_4
rename e_* *_5
rename f_* *_6
rename g_* *_7
rename h_* *_8
rename i_* *_9
rename j_* *_10
rename k_* *_11


reshape long ///
youth_weight_xc_ /// varing US variables 
acad_weight_ month_ /// The additional variables brought through to correct the weights
pre_gsce_school_year_ interview_acad_year_ interview_cal_month_ /// Varying study design variables 
hh_foster_ /// varying study selection variables 
country_   /// varying person variabels 
an_income_  /// finance variables next row is health vars
happiness_ , i(pidp) j(wave)


*** Alternative for coding outcomt
clonevar happiness 

*** Exposure variables 1 country and period 
*Academic year and period allcoation 
gen acad_year = interview_acad_year
*Note adding sf12_physicla to this makes no difference to results.  
gen any_out = 1 if happiness_  != . 


*** Sample selection stage 1 pre_percentile 
gen in_analytic_period = 1 if interview_acad_year >= 2009 & interview_acad_year <= 2018 
gen has_country = 1 if country >0 & country <5 
gen correct_age = 1 if pre_gsce_school_year_ < 3
gen has_weights = 1 if acad_weight_ >0 &  acad_weight_ !=. 
tab hh_foster 
tab any_out 

*selecting sample for weights 
tab sex, miss
keep if in_analytic_period == 1
tab sex, miss
keep if has_country == 1
tab sex, miss
keep if correct_age == 1
tab sex, miss  
keep if has_weights ==1 
tab sex, miss
keep if hh_foster == 1
tab sex, miss
keep if any_out == 1
tab sex, miss 
drop if sex == -9 
tab sex, miss 


 


*The EMA periods for anlysis
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012 = 3 "Post EMA") (2013/2014 = 4 "Age 17") (2015/2018 = 5 "Age 18" ) (2019/2020 = . ) , gen(detailed )
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012/2018 = 3 "Post EMA" ) (2019/2020 = . ) , gen(three_cat )

 

*Survey setting the data 
svyset psu [pweight = acad_weight_ ], strata(strata)




*******************************************************************************
*** Bottom 10% of sample 

*** Sample seleciton for_weights 

*Running figures across multiple years 
	
	
*Uses return list to allocate observations to academic year. 
*Note previous version had to include  & analytic_sample_weights == 1
gen bottom_10 = . 
forval i = 2009/2018 {
_pctile an_income_  [pweight=acad_weight_] if   acad_year == `i',  p(10 11 12 14)
display `i'
display r(r1)
replace bottom_10 = 1 if an_income_ <= r(r1) & acad_year == `i' 
}


**** Country allocation 
clonevar country = country_ 
recode country -9 = . 
recode country (1 = 1 "England") (2/4 = 2 "Other") , gen(devolved) 
recode country (1 = 1 "England") (2/4 =  0 "RUK") , gen(devolved2)
recode country 1 = 0 2 = 1 3= 0 4= 0, gen(Wales)
recode country 1 = 0 2 = 0 3 = 0 4 = 1 , gen(NIreland)


*** Sample seleciton for_weights  
tab sex, miss 
*gen analytic_sample = 1   if  analytic_sample_weights == 1 & bottom_10 == 1  

keep if bottom_10 == 1 
tab sex, miss 

 *key adjustment factors 
rename interview_cal_month_ month
recode  pre_gsce_school_year_  1 = 13 2 = 14, gen(age_start)


save "$reviewer\\temp4.dta", replace

use  "$reviewer\\temp4.dta", clear




keep  pidp psu strata   acad_weight_ month_ /// Surey administration variables 
/// analytic_sample  /// sample selection variabels 
happiness_ /// outcome variables 
country devolved devolved2 three_cat detailed /// exposure variables 
sex age_start month /// main covariates + other covariates 
 Wales NIreland


rename month_ sample_month

save "$reviewer\\PreGcse_File_10Dec24.dta", replace


**** Merging youth data with main analysis fle data 

use "$reviewer\\PreGcse_File_10Dec24.dta" , clear 
keep pidp psu strata age_start sex country detailed three_cat happiness_ month acad_weight_  
rename happiness_ outcome
order pidp psu strata age_start sex country detailed three_cat outcome month acad_weight_  
gen EMA = 0 

save "$reviewer\\PRE_EMA.dta" , replace 

use "$derived\Trial_R_File_15Jan24.dta" , clear 
keep pidp psu strata age_start sex country detailed three_cat life_sat month acad_weight_
rename life_sat outcome 
order pidp psu strata age_start sex country detailed three_cat outcome month acad_weight_  
gen EMA = 1 

save "$reviewer\\EMA.dta" , replace 

append using "$reviewer\\PRE_EMA.dta" 

foreach var in  age_start age_start sex country detailed three_cat outcome month {
tab `var', miss
}
*Drop those who do not have the happiness outocme. 
drop if outcome == . 
keep if country == 1
label drop k_sclfsato 
rename outcome happiness

save "$reviewer\\ReviewerAnalysis.dta" , replace 


