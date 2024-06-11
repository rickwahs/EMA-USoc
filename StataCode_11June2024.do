***** This version of analysis code initiated on 13 May 2024. 
*** Written by Richard Shaw richard.shaw@glasgow.ac.uk
* Note I am dyslexic and Stata doesn't highlight spelling mistakes. 

***** Background
*** This code is created for a papaer investigating the impact of the Education Maintainence Allowance on Mental health using data from Understanding Society. 
*** Stata was principally used for cleaning and coding data with the analysis condcuted in R. 

*** Initial project proposal was pre-registered and is avaiable from  https://osf.io/u3avm

*** Available outputs
* A pre-print will be avaialble (Insert location here).
* Policy Brief to go here.  
* Final accepted journal draft availble from: (insert location here) 
* Abstract for work to be presented at the Society for Social Medicine Annual Scientific meeting available from (Insert location here). 

***** Structure of code 
*** Stata has been used to create data files which have subsequently been transfered to R. 
*** There is a main analysis data file. Then subsequent data files have been created to carry out The sensitivity analysis. 

**** There is a core set of code. Then  breaks to indicate where code has deviated for the seperate senstivity analysis. 
* 
* Note that the sensitivity analyses diverge from the main analysis as approximatley as follows (Subject to subsequent edits). 
* Line numbers below are approximate. 
* Line 650 This is where Sensitivity 3 stops
* Line 850 Sensitivity 1 & 2 diverge 
* Line 930 Sensitivity 1 alternative code starts
* Line 1005 Sensitivity 2 alternate code starts
* Line 1085 Sensitivity 3 alternate codes starts
* Line 1330 BHPS code starts


**** Additional things to note 
* code for weights starts approx line 365 


***** File locations 
*** Original data is stored in seperate location. Files that are derived from this are stored seperately. 
*** Finally, post analysis some of results will be stored in third location for additional manipulation, and analysis. 
*** Where possible the file locations will be indcated using macros, for easy of replication. 
*** Although there may be occaisions where the actual file location was used. 
global original "[Project_folder]\Data\Original\"
global derived "[Project_folder]\Data\AnalysisCheck\"
global original_bhps "[Project_folder]\DataBHPS\Original\"
global derived_bhps  "[Project_folder]\DataBHPS\AnalysisCheck\"


***** DATA VERSIONS
*** There are vary release versions Understanding Society. These have been applied to SN6931 
*** University of Essex, Institute for Social and Economic Research. (2023). Understanding Society: Waves 1-13, 2009-2022 and Harmonised BHPS: Waves 1-18, 1991-2009: Special Licence Access. [data collection]. 16th Edition. UK Data Service. SN: 6931, DOI: http://doi.org/10.5255/UKDA-SN-6931-15
*** Date of relase July 16th
*** Date of access of used version 03 Oct 2024

***** Data files available in orignal folder are: 



***** Deriving data files 
* This section is for selecting variables from the appropriate subfiles of Understanding Society. 

**** Household varaibles 
*** This selects covariates rom the the _hhresp files. 
* The variables needed to do this are. 
* a_hidp a household id  variable 
* a_nikds_dv - number of kids under 15 in the household 
* a_tenure_dv - a derived housing tenure measure
* a_fhighmngrs1_dv - Gross income at household level single most accurate measure
* a_hhsize - household siz

foreach w in a b c d e f g h i j k {
use "$original\\`w'_hhresp_protect.dta", clear
keep `w'_hidp `w'_nkids_dv `w'_tenure_dv `w'_fihhmngrs1_dv `w'_hhsize
save "$derived\\`w'_hhresp_core.dta", replace
}

**** Identify chilren who might be living with a foster parent and thus potentially eligble for bursary fund. 

foreach w in a b c d e f g h i j k {
use "$original\\`w'_egoalt_protect.dta", clear
keep if `w'_relationship_dv == 6
egen pickone =  tag(pidp)
gen `w'_isfostered = 1
keep if pickone == 1
keep pidp `w'_isfostered 
save  "$derived\\`w'_foster_child.dta" ,replace  
}


**** Individiual level data files 
*** At an early step and order to keep the number of variables down. Many of the variables have been coded, by wave before merging data. This is not necessariy alwasy the most efficient code. 
*** Some variables in particular weights have different names due to different sampling. These need to be corrected later. 



*** Start with wave A 
foreach x in a {
use "$original\`x'_indresp_protect.dta", clear

**** In education 
label define in_ed 0 "No" 1 "Yes"
gen `x'_in_ed  = . 
recode `x'_in_ed . = 1 if `x'_school == 3
recode `x'_in_ed . = 0 if `x'_school == 2
recode `x'_in_ed . = 1 if `x'_fenow  == 3 
recode `x'_in_ed . = 0  if `x'_fenow  == 2
recode `x'_in_ed . = 0  if `x'_fenow  == 1
label values `x'_in_ed in_ed

*** defining has a job 
label define paid_job 0 "No" 1 "Yes"
gen `x'_paid_job = . 
recode `x'_paid_job  . = 1  if `x'_jbhas == 1
recode `x'_paid_job . = 1 if `x'_jboff  == 1
recode `x'_paid_job . = 0 if `x'_jboff == 2 | `x'_jboff == 3
label values `x'_paid_job paid_job
 
 *** Neet 
label define neet  0 "In education" 1 "Emp & Train" 2 "NEET"
recode `x'_in_ed  1 = 0 0 = 2, gen(`x'_neet)
recode `x'_neet 2 = 1 if `x'_paid_job == 1
recode `x'_neet . = 0  2 = 0 if `x'_jbstat == 7 
recode `x'_neet . = 1  2 = 1 if `x'_jbstat == 1  | `x'_jbstat == 2  | `x'_jbstat == 9 | `x'_jbstat == 10  | `x'_jbstat == 11 | `x'_jbstat == 12
recode `x'_neet . = 2  if `x'_jbstat >= 3  & `x'_jbstat <= 6 
recode `x'_neet . = 2  if `x'_jbstat == 8  |  `x'_jbstat  == 13 |  `x'_jbstat  == 97 
label values `x'_neet neet


keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
a_indscus_xw  a_indinus_xw /// The weight variables 
`x'_istrtdatd `x'_istrtdatm `x'_istrtdaty /// Interview data variables 
`x'_sex `x'_dvage  `x'_racel ///
`x'_sf12pcs_dv `x'_sf12mcs_dv `x'_sf1 `x'_scghq1_dv `x'_scghq2_dv /// health variables
`x'_in_ed   `x'_school  `x'_fenow  `x'_paid_job  `x'_jbhas `x'_jboff `x'_neet  `x'_jbstat  /// education employment vars
`x'_sclfsat1 `x'_sclfsat2 `x'_sclfsat7 `x'_sclfsato /// Life Satisfaction variables economic variables on next line
`x'_finnow `x'_finfut  `x'_bensta2 

*** Interview dates
*Interview start dates will be used to indicate the time of the interivew
* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
* Particlular for scotland, but not the issue that I may not have a baseline for Scotland. 
gen `x'_interview_acad_year =  `x'_istrtdaty  if  `x'_istrtdatm >= 9 & `x'_istrtdatm  !=. 
replace `x'_interview_acad_year =  `x'_istrtdaty -1  if `x'_istrtdatm <= 8 & `x'_istrtdatm  > 0  
gen `x'_interview_cal_month =  `x'_istrtdatm
rename a_indscus_xw a_sc_weight_xc
rename a_indinus_xw a_main_weight_xc 
save "$derived\`x'_indresp_selected.dta", replace
}





*** Waves 2 to 5 (B to e)

foreach x in  b c d e  {
use "$original\`x'_indresp_protect.dta", clear


**** In education 
label define in_ed 0 "No" 1 "Yes"
gen `x'_in_ed  = . 
recode `x'_in_ed . = 1 if `x'_school == 3
recode `x'_in_ed . = 0 if `x'_school == 2
recode `x'_in_ed . = 1 if `x'_fenow  == 3 
recode `x'_in_ed . = 0  if `x'_fenow  == 2
recode `x'_in_ed . = 0  if `x'_fenow  == 1
label values `x'_in_ed in_ed

*** defining has a job 
label define paid_job 0 "No" 1 "Yes"
gen `x'_paid_job = . 
recode `x'_paid_job  . = 1  if `x'_jbhas == 1
recode `x'_paid_job . = 1 if `x'_jboff  == 1
recode `x'_paid_job . = 0 if `x'_jboff == 2 | `x'_jboff == 3
label values `x'_paid_job paid_job
 
 *** Neet 
label define neet  0 "In education" 1 "Emp & Train" 2 "NEET"
recode `x'_in_ed  1 = 0 0 = 2, gen(`x'_neet)
recode `x'_neet 2 = 1 if `x'_paid_job == 1
recode `x'_neet . = 0  2 = 0 if `x'_jbstat == 7 
recode `x'_neet . = 1  2 = 1 if `x'_jbstat == 1  | `x'_jbstat == 2  | `x'_jbstat == 9 | `x'_jbstat == 10  | `x'_jbstat == 11 | `x'_jbstat == 12
recode `x'_neet . = 2  if `x'_jbstat >= 3  & `x'_jbstat <= 6 
recode `x'_neet . = 2  if `x'_jbstat == 8  |  `x'_jbstat  == 13 |  `x'_jbstat  == 97 
label values `x'_neet neet




keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
`x'_indscub_xw  `x'_indinub_xw /// The weight variables 
`x'_istrtdatd `x'_istrtdatm `x'_istrtdaty /// Interview data variables 
 `x'_sex `x'_dvage  `x'_racel ///
`x'_sf12pcs_dv `x'_sf12mcs_dv `x'_sf1 `x'_scghq1_dv `x'_scghq2_dv /// health variables
`x'_in_ed   `x'_school  `x'_fenow  `x'_paid_job  `x'_jbhas `x'_jboff `x'_neet  `x'_jbstat  /// education employment vars
`x'_sclfsat1 `x'_sclfsat2 `x'_sclfsat7 `x'_sclfsato /// Life Satisfaction variables economic variables on next line
`x'_finnow `x'_finfut  `x'_bensta2 

*** Interview dates
*Interview start dates will be used to indicate the time of the interivew
* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
* Particlular for scotland, but not the issue that I may not have a baseline for Scotland. 
gen `x'_interview_acad_year =  `x'_istrtdaty  if  `x'_istrtdatm >= 9 & `x'_istrtdatm  !=. 
replace `x'_interview_acad_year =  `x'_istrtdaty -1  if `x'_istrtdatm <= 8 & `x'_istrtdatm  > 0  
gen `x'_interview_cal_month =  `x'_istrtdatm
rename `x'_indscub_xw `x'_sc_weight_xc
rename `x'_indinub_xw `x'_main_weight_xc 

save "$derived\`x'_indresp_selected.dta", replace
}



*Waves 6 to 11 (f to k) Note as l starts in the pandemic this is ignored

foreach x in  f g h i j k  {
use "$original\`x'_indresp_protect.dta", clear


**** In education 
label define in_ed 0 "No" 1 "Yes"
gen `x'_in_ed  = . 
recode `x'_in_ed . = 1 if `x'_school == 3
recode `x'_in_ed . = 0 if `x'_school == 2
recode `x'_in_ed . = 1 if `x'_fenow  == 3 
recode `x'_in_ed . = 0  if `x'_fenow  == 2
recode `x'_in_ed . = 0  if `x'_fenow  == 1
label values `x'_in_ed in_ed

*** defining has a job 
label define paid_job 0 "No" 1 "Yes"
gen `x'_paid_job = . 
recode `x'_paid_job  . = 1  if `x'_jbhas == 1
recode `x'_paid_job . = 1 if `x'_jboff  == 1
recode `x'_paid_job . = 0 if `x'_jboff == 2 | `x'_jboff == 3
label values `x'_paid_job paid_job
 
 *** Neet 
label define neet  0 "In education" 1 "Emp & Train" 2 "NEET"
recode `x'_in_ed  1 = 0 0 = 2, gen(`x'_neet)
recode `x'_neet 2 = 1 if `x'_paid_job == 1
recode `x'_neet . = 0  2 = 0 if `x'_jbstat == 7 
recode `x'_neet . = 1  2 = 1 if `x'_jbstat == 1  | `x'_jbstat == 2  | `x'_jbstat == 9 | `x'_jbstat == 10  | `x'_jbstat == 11 | `x'_jbstat == 12
recode `x'_neet . = 2  if `x'_jbstat >= 3  & `x'_jbstat <= 6 
recode `x'_neet . = 2  if `x'_jbstat == 8  |  `x'_jbstat  == 13 |  `x'_jbstat  == 97 
label values `x'_neet neet



keep ///
pidp  `x'_hidp  `x'_country `x'_gor_dv  /// Adiministrative variables  `x'_indpxus_xw 
`x'_indscui_xw  `x'_indinui_xw /// The weight variables 
`x'_istrtdatd `x'_istrtdatm `x'_istrtdaty /// Interview data variables 
 `x'_sex `x'_dvage  `x'_racel ///
`x'_sf12pcs_dv `x'_sf12mcs_dv `x'_sf1 `x'_scghq1_dv `x'_scghq2_dv /// health variables
`x'_in_ed   `x'_school  `x'_fenow  `x'_paid_job  `x'_jbhas `x'_jboff `x'_neet  `x'_jbstat  /// education employment vars
`x'_sclfsat1 `x'_sclfsat2 `x'_sclfsat7 `x'_sclfsato /// Life Satisfaction variables economic variables on next line
`x'_finnow `x'_finfut  `x'_bensta2 

*** Interview dates
*Interview start dates will be used to indicate the time of the interivew
* Will allocate to Year based on 1st of september. Note this will not necessarily be accurate for all countries 
* Particlular for scotland, but not the issue that I may not have a baseline for Scotland. 
gen `x'_interview_acad_year =  `x'_istrtdaty  if  `x'_istrtdatm >= 9 & `x'_istrtdatm  !=. 
replace `x'_interview_acad_year =  `x'_istrtdaty -1  if `x'_istrtdatm <= 8 & `x'_istrtdatm  > 0  
gen `x'_interview_cal_month =  `x'_istrtdatm
rename `x'_indscui_xw `x'_sc_weight_xc
rename `x'_indinui_xw `x'_main_weight_xc 

save "$derived\`x'_indresp_selected.dta", replace
}



****  Next is a multiple part version to derive variables based on parents. 

*** Parental characteristics. 
** Note creating different variables for mothers and father at this stage. Including different ID variables which match those in the individal file. 
** Although seperate variables will be used to indicate parents. 
foreach w in a b c d e f g h i j k {
use "$original\\`w'_indresp_protect.dta", clear

* NS-SEC
clonevar nssec5 = `w'_jbnssec5_dv 
replace  nssec5 = `w'_jlnssec5_dv if nssec5 < 1 | nssec5 == . 
clonevar `w'_mot_nssec = nssec5
clonevar `w'_fat_nssec = nssec5

* Education 
clonevar `w'_mot_educ = `w'_hiqual_dv
clonevar `w'_fat_educ = `w'_hiqual_dv

* Work status 
gen `w'_haswork = . 
recode `w'_haswork . = 1 if `w'_jbhas == 1 | `w'_jboff == 1
recode `w'_haswork . = 0 if `w'_jboff == 2 | `w'_jboff == 3 
recode `w'_haswork . = 0 if `w'_jbhas == 2
clonevar `w'_mot_work = `w'_haswork
clonevar `w'_fat_work = `w'_haswork


* drop and rename the link variables 
drop `w'_mnspid  `w'_fnspid
clonevar  `w'_mnspid = pidp
clonevar  `w'_fnspid = pidp

* Saving file for mothers file
preserve
keep `w'_mnspid `w'_mot_nssec `w'_mot_educ `w'_mot_work 


save  "$derived\\`w'_mot_vars.dta" , replace

* Saving file for fathers 
restore 
keep `w'_fnspid `w'_fat_nssec `w'_fat_educ `w'_fat_work 
save  "$derived\\`w'_fat_vars.dta" , replace
}

** Combining individual (child data) with those of parents. 

foreach w in a b c d e f g h i j k {
use "$original\\`w'_indresp_protect.dta", clear

keep pidp `w'_mnspid `w'_fnspid
merge m:1 `w'_mnspid using  "$derived\\`w'_mot_vars.dta" , gen(`w'_match_mum) 
drop if `w'_match_mum == 2 
merge m:1 `w'_fnspid using  "$derived\\`w'_fat_vars.dta" , gen(`w'_match_fat) 
drop if `w'_match_fat == 2 

* Parental social class 
replace `w'_mot_nssec = . if  `w'_mot_nssec < 0
replace `w'_fat_nssec = . if  `w'_fat_nssec < 0
clonevar `w'_par_nssec = `w'_mot_nssec
replace `w'_par_nssec =  `w'_fat_nssec if `w'_fat_nssec < `w'_mot_nssec 
label variable `w'_par_nssec "Highest parental NSSEC" 

* Highest qualification 
replace `w'_mot_educ = . if `w'_mot_educ <0
replace `w'_fat_educ = . if `w'_fat_educ <0
clonevar `w'_par_educ = `w'_mot_educ
replace `w'_par_educ = `w'_fat_educ if `w'_fat_educ < `w'_mot_educ 
label variable `w'_par_educ  "Highest parental Education" 

* Two parent household
gen `w'_two_par = 1 if  `w'_match_mum  == 3  & `w'_match_fat  == 3
recode `w'_two_par  . = 0 
label values `w'_two_par two_par
label define two_par 0 "No" 1 "Yes"

* No parent working 
gen `w'_no_workpar = 0  if `w'_mot_work == 1 |  `w'_fat_work == 1
recode `w'_no_workpar . = 1 
label define no_workpar 0 "Working parent" 1 "No working parent"
label values `w'_no_workpar no_workpar

save  "$derived\\`w'_par_vars.dta" , replace
}

**** Calculating revised weights to correct for sample being allocated based on school year. 

* Key info for coding the weights. 
* Weblink to the relevant weight guidlines at understanding Society  
* https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/how-to-use-weights-analysis-guidance-for-weights-psu-strata





/* Note that Understanding Society is collected by Calender year. However analyis by School year starting on the 1st September. 
The Table  below indicaes how people in the first 4 wavees of understanding society are allocated to academic year based on study month and year. Note year in column refers to academic year people should be allocated to. The letters refer to the US wave. 
					  A	   B 	C 	 D
           1 jan yr1 2008 2009 2010 2011
           2 feb yr1 2008 2009 2010 2011
           3 mar yr1 2008 2009 2010 2011
           4 apr yr1 2008 2009 2010 2011
           5 may yr1 2008 2009 2010 2011
           6 jun yr1 2008 2009 2010 2011
           7 jul yr1 2008 2009 2010 2011
           8 aug yr1 2008 2009 2010 2011
		 -----------------------------------------
           9 sep yr1 2009 2010 2011 2012
          10 oct yr1 2009 2010 2011 2012
          11 nov yr1 2009 2010 2011 2012
          12 dec yr1 2009 2010 2011 2012
          13 jan yr2 2009 2010 2011 2012
          14 feb yr2 2009 2010 2011 2012
          15 mar yr2 2009 2010 2011 2012
          16 apr yr2 2009 2010 2011 2012
          17 may yr2 2009 2010 2011 2012
          18 jun yr2 2009 2010 2011 2012
          19 jul yr2 2009 2010 2011 2012
          20 aug yr2 2009 2010 2011 2012
		  -------------------------------------
          21 sep yr2 2010 2011 2012 2013
          22 oct yr2 2010 2011 2012 2013
          23 nov yr2 2010 2011 2012 2013
          24 dec yr2 2010 2011 2012 2013

*/

*Note that 1 to 8 will for the first wave go to the 2008 year. This will have the same weighting as the 2009 academic year as same wave only. 
* Note second set of weights will be for 9 to 20. 

*I will use B as the base as that will be the central year for 2010 and have both a for and afterwards added. 
* Note use the same correction factor the 2009 year as the second year. 

*Note I will need to reweight yr2 results from 2009 from wave a to compensate for lack of Year 2 dates from a previous wave. 
* Think aweight x bpop/apop + aweight 

*Start with wave A not this is aimed at home code so will need lost of changes 


foreach x in a {
use "$original\`x'_indresp_protect.dta", clear
keep pidp `x'_month `x'_istrtdatd `x'_istrtdatm `x'_istrtdaty `x'_sampst `x'_indscus_xw 

save  "$derived\\`x'_vars_for_acadweight.dta" ,replace  
}


foreach x in b c d e  {
use "$original\`x'_indresp_protect.dta", clear
keep pidp `x'_month `x'_istrtdatd `x'_istrtdatm `x'_istrtdaty `x'_sampst `x'_indscub_xw 

save  "$derived\\`x'_vars_for_acadweight.dta" ,replace  
}


foreach x in f g h i j k  {
use "$original\`x'_indresp_protect.dta", clear
keep pidp `x'_month `x'_istrtdatd `x'_istrtdatm `x'_istrtdaty `x'_sampst `x'_indscui_xw 

save  "$derived\\`x'_vars_for_acadweight.dta" ,replace  
}



*Work computer file locations (needed for protected files) 


use "$derived\a_vars_for_acadweight.dta", clear
foreach x in  b c d e f g h i j k  {
	merge 1:1 pidp using  "$derived\`x'_vars_for_acadweight.dta" , gen(merge_`x') 	
}


* Create base multiplier for all individuals  
gen ind = 1 
*Note create numerator/denominators for weighted number of indiviudals in each period accounting for slightly different weights across waves. 
* A wave the only "us" weight
foreach x in a {
sum ind [aw = `x'_indscus_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_indscus_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_indscus_xw] if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}

*The "ub" waves b c d e 
foreach x in b c d e  {
sum ind [aw = `x'_indscub_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_indscub_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_indscub_xw] if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}

*The "ui" waves f g h i j k  
foreach x in f g h i j k  {
sum ind [aw = `x'_indscui_xw] if `x'_month >=1 & `x'_month <=8 
gen `x'_wtd_yr1_add = r(sum_w)
sum ind [aw = `x'_indscui_xw] if `x'_month >=9 & `x'_month <= 20 
gen `x'_wtd_core = r(sum_w)
sum ind [aw = `x'_indscui_xw] if `x'_month >=21 & `x'_month <=24 
gen `x'_wtd_yr2_add = r(sum_w)
}



*** 2009 this requires addtional weighting from wave A + wave b 
* Note the Yr1 months from a should probably be weighted as normal: 
* but the Yr2 months should probably increase weighed to compensate for the lack of Yr2 froma previous wave. 
*note the Yr1 in a are 9 to 12 

sum ind [aw = a_indscus_xw] if a_month >=9 & a_month <=12 
gen a_wtd_yr1_normal_add = r(sum_w)
sum ind [aw = b_indscub_xw] if b_month >=9 & b_month <=12 
gen b_wtd_yr1_normal_add = r(sum_w)


sum ind [aw = a_indscus_xw] if a_month >=13 & a_month <=20
gen a_wtd_yr2_extra_need = r(sum_w)
sum ind [aw = b_indscub_xw] if b_month >=13 & b_month <=24
gen b_wtd_yr2_extra_needed = r(sum_w)


*Set all as 0 
gen weight_2009 = 0 
*Yr1 people in 2009 academic year A wave weighted normally
replace weight_2009 = a_indscus_xw*(b_wtd_yr1_normal_add/a_wtd_yr1_normal_add) if a_month >=9 & a_month < =12  
*Yrs 2 people in A wave academic year upweighted to account for no 0 wave 
replace weight_2009 = a_indscus_xw*(b_wtd_yr2_extra_needed/a_wtd_yr2_extra_need) if a_month >=13 & a_month < =20  



*The people from wave b can be included as is already possible 
replace weight_2009 = b_indscub_xw*(b_wtd_yr1_add/b_wtd_yr1_add) if b_month >=1 & b_month <= 8 


*** 2010 this requires some people from a, b and c. 
* Being done first as this is a more standard weighting approach. 
gen weight_2010 = 0 
replace weight_2010 = a_indscus_xw*(b_wtd_yr2_add/a_wtd_yr2_add) if a_month >=21 & a_month <=24 
replace weight_2010 = b_indscub_xw*(b_wtd_core/b_wtd_core) if b_month >=9 & b_month < =20  
replace weight_2010 = c_indscub_xw*(b_wtd_yr1_add/c_wtd_yr1_add) if c_month >=1 & c_month <= 8 

*** 2011 
gen weight_2011 = 0 
replace weight_2011 = b_indscub_xw*(b_wtd_yr2_add/b_wtd_yr2_add) if b_month >=21 & b_month <=24 
replace weight_2011 = c_indscub_xw*(b_wtd_core/c_wtd_core) if c_month >=9 & c_month < =20 
replace weight_2011 = d_indscub_xw*(b_wtd_yr1_add/d_wtd_yr1_add) if d_month >=1 & d_month <= 8 

*** 2012 
gen weight_2012 = 0 
replace weight_2012 = c_indscub_xw*(b_wtd_yr2_add/c_wtd_yr2_add) if c_month >=21 & c_month <=24 
replace weight_2012 = d_indscub_xw*(b_wtd_core/d_wtd_core) if d_month >=9 & d_month < =20 
replace weight_2012 = e_indscub_xw*(b_wtd_yr1_add/e_wtd_yr1_add) if e_month >=1 & e_month <= 8 

*** 2013 
gen weight_2013 = 0 
replace weight_2013 = d_indscub_xw*(b_wtd_yr2_add/d_wtd_yr2_add) if d_month >=21 & d_month <=24 
replace weight_2013 = e_indscub_xw*(b_wtd_core/e_wtd_core) if e_month >=9 & e_month < =20 
replace weight_2013 = f_indscui_xw*(b_wtd_yr1_add/f_wtd_yr1_add) if f_month >=1 & f_month <= 8 

*** 2014 
gen weight_2014 = 0 
replace weight_2014 = e_indscub_xw*(b_wtd_yr2_add/e_wtd_yr2_add) if e_month >=21 & e_month <=24 
replace weight_2014 = f_indscui_xw*(b_wtd_core/f_wtd_core) if f_month >=9 & f_month < =20 
replace weight_2014 = g_indscui_xw*(b_wtd_yr1_add/g_wtd_yr1_add) if g_month >=1 & g_month <= 8 

*** 2015 
gen weight_2015 = 0 
replace weight_2015 = f_indscui_xw*(b_wtd_yr2_add/f_wtd_yr2_add) if f_month >=21 & f_month <=24 
replace weight_2015 = g_indscui_xw*(b_wtd_core/g_wtd_core) if g_month >=9 & g_month < =20
replace weight_2015 = h_indscui_xw*(b_wtd_yr1_add/h_wtd_yr1_add) if h_month >=1 & h_month <= 8 

*** 2016 
gen weight_2016 = 0 
replace weight_2016 = g_indscui_xw*(b_wtd_yr2_add/g_wtd_yr2_add) if g_month >=21 & g_month <=24 
replace weight_2016 = h_indscui_xw*(b_wtd_core/h_wtd_core) if h_month >=9 & h_month < =20 
replace weight_2016 = i_indscui_xw*(b_wtd_yr1_add/i_wtd_yr1_add) if i_month >=1 & i_month <= 8 

*** 2017 
gen weight_2017 = 0 
replace weight_2017 = h_indscui_xw*(b_wtd_yr2_add/h_wtd_yr2_add) if h_month >=21 & h_month <=24 
replace weight_2017 = i_indscui_xw*(b_wtd_core/i_wtd_core) if i_month >=9 & i_month < =20 
replace weight_2017 = j_indscui_xw*(b_wtd_yr1_add/j_wtd_yr1_add) if j_month >=1 & j_month <= 8 

*** 2018 
gen weight_2018 = 0 
replace weight_2018 = i_indscui_xw*(b_wtd_yr2_add/i_wtd_yr2_add) if i_month >=21 & i_month <=24 
replace weight_2018 = j_indscui_xw*(b_wtd_core/j_wtd_core) if j_month >=9 & j_month < =20 
replace weight_2018 = k_indscui_xw*(b_wtd_yr1_add/k_wtd_yr1_add) if k_month >=1 & k_month <= 8 



foreach year in 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 {
	label variable weight_`year' "Academic year starting x Sep to Aug"
}




gen a_acad_weight = .
replace a_acad_weight = weight_2009 if a_month >=9 & a_month <= 20  
replace a_acad_weight = weight_2010 if a_month >=21 & a_month <=24 

*Old code to test that the loop was working correctly
*NB it was and it was this code that needed correcting. 
*gen b_acad_weight = .
*replace b_acad_weight = weight_2009 if b_month >= 1 & b_month < =8 
*replace b_acad_weight = weight_2010 if b_month >=9 & b_month < =20  
*replace b_acad_weight = weight_2011 if b_month >=21 & b_month <=24 




*Testing the loop 
*local number = 2009 
*foreach letter in b c d e f g h i j k {
*	display "`letter'"
*	display "weight_`number'"
*	local number = `number' + 1 
*	display "weight_`number'"
*	local number = `number' + 1
*	display "weight_`number'"
*	local number = `number' - 1 
*}


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


save  "$derived\\Academic_year_sample_weights.dta" ,replace  


*** The sensitivity analysis adds an addtional variable for inocome identify if households had imputed data. 
* This resulted in an alternative core file `w'_hhresp_core_v2.dta

foreach w in a b c d e f g h i j k {
use "$original\\`w'_hhresp_protect.dta", clear
keep `w'_hidp `w'_nkids_dv `w'_tenure_dv `w'_fihhmngrs1_dv `w'_fihhmngrs_if `w'_hhsize
save "$derived\\`w'_hhresp_core_v2.dta", replace
}



*********************************************************************************
***** Merging files for further analysis 
*** This was used for the main analyis sensitivity 1, Sensitivity 2,  sensitivity 4 
** Sensitivity 3 changes diverges at this point. 

**********************************************************************************

use "$original\\xwavedat_protect.dta", clear



foreach x in a b c d e f g h i j k   {
merge 1:1 pidp using "$derived\`x'_indresp_selected.dta", gen(`x'_merge)
merge m:1 `x'_hidp using "$derived\`x'_hhresp_core.dta", gen(`x'_hh_merge)
drop if `x'_hh_merge == 2
merge 1:1 pidp using "$derived\`x'_foster_child.dta", gen(`x'_hh_foster)
drop if `x'_hh_foster == 2 
merge 1:1 pidp using "$derived\`x'_par_vars.dta", gen(`x'_par_merge)
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
gen `x'_school_year = 1 if `x'_interview_acad_year == academic_birth_cohort + 17 & academic_birth_cohort !=. & academic_birth_cohort !=. 
replace `x'_school_year = 2 if  `x'_interview_acad_year == academic_birth_cohort + 18 & academic_birth_cohort !=. & academic_birth_cohort !=. 
}



merge 1:1 pidp using "$derived\Academic_year_sample_weights.dta", gen(acad_weights_merge)


**Generate ever in target age group
gen ever_sixth = .
foreach x in a b c d e f g h i j k   {
recode ever_sixth . = 1 if `x'_school_year ==1 | `x'_school_year == 2
}




*********************
* Recoding parental vars
* Merging parental ever done 
recode maedqf (-9/-1 = . ) (1/2  = 9 "No Qualification") (2/3 = 4 "School level") (4 =  2 "Other higher") (5 = 1 "Degree")  (97  = 5 "Other"), gen(mot_ted)
recode paedqf (-9/-1 = . ) (1/2  = 9 "No Qualification") (2/3 = 4 "School level") (4 =  2 "Other higher") (5 = 1 "Degree")  (97  = 5 "Other"), gen(fat_ted)
clonevar par_ted = mot_ted
replace par_ted = fat_ted if fat_ted < par_ted
*recoding wave specific 
foreach x in a b c d e f g h i j k   {
recode `x'_par_educ (1 = 1 "Degree") (2 =  2 "Other higher") (3/4 = 4 "School level")  (5  = 5 "Other") (9  = 9 "No Qualification") (22  = 10 "Not Classifiable"), gen(`x'_par_hqual)
clonevar `x'_par_hedqual = `x'_par_hqual
replace `x'_par_hedqual = par_ted if `x'_par_hedqual ==. 
recode  `x'_par_hedqual .  = 10 
label variable `x'_par_hedqual "High parent ed 4 source"
}


*NOte none of the_sc_weight has inapplicable but htere are still a few who had a proxy response. 

*Creating a binary measure of ghq this will then be used to create a binary random variable with samme distribution to design study without biasing results. 
foreach w in a b c d e f g h i j k {
recode `w'_scghq2_dv -9/-7 = . 0/3 = 0 4/12 = 1 , gen(`w'_ghq_bin)
recode `w'_scghq2_dv -9/-7 = 1 0/12 = 0, gen(`w'_snr_ghq) 
recode `w'_scghq2_dv -9/-7 = 0 0/12 = 1, gen(`w'_has_out)
}





***********************************************
*Generating an anual income measure 
foreach x in a b c d e f g h i j k {
	gen `x'_an_income = `x'_fihhmngrs1_dv *12
}	



********************************************************************************
****e 




keep pidp strata psu /// fixed US design variables 
*_sc_weight_xc /// varing US variables 
*_month weight_* *_acad_weight /// the variables from the weight file 
*_school_year *_interview_acad_year *_interview_cal_month /// Varying study design variables 
*_hh_foster /// varying study selection variables 
*_country  *_racel *_sf1 /// varying person variabels 
*_nkids_dv *_tenure_dv  *_hhsize ///
*_in_ed   *_school  *_fenow  *_paid_job  *_jbhas *_jboff *_neet  *_jbstat  /// education employment vars
*_an_income  /// varying exposure variables 
*_par_hedqual *_par_nssec *_no_workpar *_two_par /// parental vars
academic_birth_cohort sex racel_dv bornuk_dv  ///   fixed person variabels Outcomes on the next line 
*_finnow *_has_out *_ghq_bin *_scghq1_dv *_sf12mcs_dv *_sf12pcs_dv *_sclfsato




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
sc_weight_xc_ /// varing US variables 
acad_weight_ month_ /// The additional variables brought through to correct the weights
school_year_ interview_acad_year_ interview_cal_month_ /// Varying study design variables 
hh_foster_ /// varying study selection variables 
country_  racel_ sf1_ /// varying person variabels 
nkids_dv_ tenure_dv_  hhsize_ ///
in_ed_   school_  fenow_  paid_job_  jbhas_ jboff_ neet_  jbstat_  /// education employment vars
par_hedqual_ par_nssec_ no_workpar_ two_par_ /// parental education and nssec
finnow_  an_income_  /// finance variables next row is health vars
has_out_ ghq_bin_ scghq1_dv_ sf12mcs_dv_ sf12pcs_dv_ sclfsato_, i(pidp) j(wave)


*** Alternative codings for outcomes. 
recode scghq1_dv_  -9/-1 = ., gen(ghq12)

clonevar life_sat = sclfsato_
recode life_sat -10/-1 = . 
 
clonevar sf12_mental = sf12mcs_dv_ 
recode  sf12_mental -9/-7 = . 

clonevar sf12_physical = sf12pcs_dv_
recode sf12_physical  -9/-7 = . 


*** Exposure variables 1 country and period 
*Academic year and period allcoation 
gen acad_year = interview_acad_year
*Note adding sf12_physicla to this makes no difference to results.  
gen any_out = 1 if ghq12 != . | sf12_mental != . | life_sat !=. 


*** Sample selection stage 1 pre_percentile 
gen in_analytic_period = 1 if interview_acad_year >= 2009 & interview_acad_year <= 2018 
gen has_country = 1 if country >0 & country <5 
gen correct_age = 1 if school_year < 3
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



 


*The EMA periods for anlysis
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012 = 3 "Post EMA") (2013/2014 = 4 "Age 17") (2015/2018 = 5 "Age 18" ) (2019/2020 = . ) , gen(detailed )
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012/2018 = 3 "Post EMA" ) (2019/2020 = . ) , gen(three_cat )


 

*Survey setting the data 
svyset psu [pweight = acad_weight_ ], strata(strata)

save   "$derived\\temp3.dta" , replace



*******************************************************************************
*** This is the point where sensitivity 1 & 2 diverge from the main analysis 
********************************************************************************
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
recode  school_year_  1 = 16 2 = 17, gen(age_start)

*Household measures 
recode tenure_dv_  (-9 = 3 "Missing")  (1/2 = 0 "Owners") (3/4  = 1 "Social housing" ) (5/8 = 2 "Rent & Other" ), gen(housing)
recode tenure_dv_  (-9 = . )  (1/2 = 0 "Owners") (3/8 = 1 "Rent & Other" ), gen(owner)
recode hhsize (1/2 = 0  "One or Two") (3/4 =  1 "Three or four") (5/13  = 2 "Five or more"), gen(household_size)
recode nkids_dv_  (0 = 0 "None") (1/2 = 1 "One or two") (3/8 =  3 "Three or more"), gen(kids)
rename  two_par_ two_par
rename no_workpar_ no_workpar
recode par_hedqual_ (1/2 = 0 "Post-School" ) (4 = 1 "School level") (5/9 = 2 "No or Other" ) ( 10 = 3 "Other not classifiable") ,gen(par_ed)
recode par_nssec_  (. = 0 "Not Classifiable") (1 = 1 "Management & professional") (2  = 2 "Intermediate")  (3 = 3 "mall employers & own account") (4 =4 "Lower supervisory & technical") (5 = 5 "Semi-routine & routine") , gen(par_nssec)


*indiviudal measures 
recode racel_dv (1 = 1 "White British") (2/4 = 2 "White Other"  ) (5/8 = 3  "Mixed") (9 = 4 "Indian") (10 = 5 "Pakistani") (11 = 6 "Bangladeshi") (12/13 = 8 "Other") (14/16 = 7 "Black" ) (17/97 = 8) (-9 = 9 "Missing") , gen(ethnicity) 
recode racel_dv (1 = 1 "White British") (2/97 = 2 "Other" ) (-9 = .) , gen(ethnic_bin) 


*Explanatory vars 
recode finnow_ (-9/-1 = 6 "Missing") (1 = 1 "Comfortbale") (2 = 2 "Alright") (3 = 3 "Getting by") (4 = 4 "Quite difficult") (5 = 5 "Very difficult") , gen(fin_strain)
*tab jbstat if analytic_sample == 1, miss
gen neet = jbstat_
recode neet -9/-1 = . 1/2 = 1 3/6 = 2 7 = 0 8 = 2 9 = 1 10 = 2 11 = 1 12 = 1 13 = 2 97 = 2 
label values neet neet 


keep  pidp psu strata sc_weight_xc  acad_weight_ month_ /// Surey administration variables 
/// analytic_sample  /// sample selection variabels 
ghq12 sf12_mental life_sat  sf12_physical /// outcome variables 
country devolved devolved2 three_cat detailed /// exposure variables 
sex age_start month /// main covariates 
fin_strain neet /// explanatory variables + other covariates on the next line
owner kids two_par no_workpar par_ed ethnic_bin Wales NIreland


rename month_ sample_month

save "$derived\\Trial_R_File_15Jan24.dta", replace


**********************************************************************************#
****** Sensitivty analysis 1 
* widened the sample to the bottom 15% of household income. 

use  "$derived\\temp3.dta" , clear


gen bottom_15 = . 
forval i = 2009/2018 {
_pctile an_income_  [pweight=acad_weight_] if   acad_year == `i',  p(15)
display `i'
display r(r1)
replace bottom_15 = 1 if an_income_ <= r(r1) & acad_year == `i' 
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

keep if bottom_15 == 1 
tab sex, miss 

 *key adjustment factors 
rename interview_cal_month_ month
recode  school_year_  1 = 16 2 = 17, gen(age_start)

*Household measures 
recode tenure_dv_  (-9 = 3 "Missing")  (1/2 = 0 "Owners") (3/4  = 1 "Social housing" ) (5/8 = 2 "Rent & Other" ), gen(housing)
recode tenure_dv_  (-9 = . )  (1/2 = 0 "Owners") (3/8 = 1 "Rent & Other" ), gen(owner)
recode hhsize (1/2 = 0  "One or Two") (3/4 =  1 "Three or four") (5/13  = 2 "Five or more"), gen(household_size)
recode nkids_dv_  (0 = 0 "None") (1/2 = 1 "One or two") (3/8 =  3 "Three or more"), gen(kids)
rename  two_par_ two_par
rename no_workpar_ no_workpar
recode par_hedqual_ (1/2 = 0 "Post-School" ) (4 = 1 "School level") (5/9 = 2 "No or Other" ) ( 10 = 3 "Other not classifiable") ,gen(par_ed)
recode par_nssec_  (. = 0 "Not Classifiable") (1 = 1 "Management & professional") (2  = 2 "Intermediate")  (3 = 3 "mall employers & own account") (4 =4 "Lower supervisory & technical") (5 = 5 "Semi-routine & routine") , gen(par_nssec)


*indiviudal measures 
recode racel_dv (1 = 1 "White British") (2/4 = 2 "White Other"  ) (5/8 = 3  "Mixed") (9 = 4 "Indian") (10 = 5 "Pakistani") (11 = 6 "Bangladeshi") (12/13 = 8 "Other") (14/16 = 7 "Black" ) (17/97 = 8) (-9 = 9 "Missing") , gen(ethnicity) 
recode racel_dv (1 = 1 "White British") (2/97 = 2 "Other" ) (-9 = .) , gen(ethnic_bin) 


*Explanatory vars 
recode finnow_ (-9/-1 = 6 "Missing") (1 = 1 "Comfortbale") (2 = 2 "Alright") (3 = 3 "Getting by") (4 = 4 "Quite difficult") (5 = 5 "Very difficult") , gen(fin_strain)
*tab jbstat if analytic_sample == 1, miss
gen neet = jbstat_
recode neet -9/-1 = . 1/2 = 1 3/6 = 2 7 = 0 8 = 2 9 = 1 10 = 2 11 = 1 12 = 1 13 = 2 97 = 2 
label values neet neet 



keep  pidp psu strata sc_weight_xc  acad_weight_ month_ /// Surey administration variables 
/// analytic_sample  /// sample selection variabels 
ghq12 sf12_mental life_sat  sf12_physical /// outcome variables 
country devolved devolved2 three_cat detailed /// exposure variables 
sex age_start month /// main covariates 
fin_strain neet /// explanatory variables + other covariates on the next line
owner kids two_par no_workpar par_ed ethnic_bin Wales NIreland


rename month_ sample_month

save "$derived\\R_File_Bottom15P_percent17Jan24.dta", replace

***********************************************************************************
********************** Sensitivity analys 2 
*we changed the analytic sample to the top 80% of incomes, who would not be expected to be eligible to receive the EMA.
use  "$derived\\temp3.dta" , clear


*Running figures across multiple years 
	
	
*Uses return list to allocate observations to academic year. 
*Note previous version had to include  & analytic_sample_weights == 1
gen top_80 = . 
forval i = 2009/2018 {
_pctile an_income_  [pweight=acad_weight_] if   acad_year == `i',  p(20)
display `i'
display r(r1)
replace top_80 = 1 if an_income_ >= r(r1)  & an_income_  != .  & acad_year == `i' 
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

keep if top_80 == 1 
tab sex, miss 

 *key adjustment factors 
rename interview_cal_month_ month
recode  school_year_  1 = 16 2 = 17, gen(age_start)

*Household measures 
recode tenure_dv_  (-9 = 3 "Missing")  (1/2 = 0 "Owners") (3/4  = 1 "Social housing" ) (5/8 = 2 "Rent & Other" ), gen(housing)
recode tenure_dv_  (-9 = . )  (1/2 = 0 "Owners") (3/8 = 1 "Rent & Other" ), gen(owner)
recode hhsize (1/2 = 0  "One or Two") (3/4 =  1 "Three or four") (5/13  = 2 "Five or more"), gen(household_size)
recode nkids_dv_  (0 = 0 "None") (1/2 = 1 "One or two") (3/8 =  3 "Three or more"), gen(kids)
rename  two_par_ two_par
rename no_workpar_ no_workpar
recode par_hedqual_ (1/2 = 0 "Post-School" ) (4 = 1 "School level") (5/9 = 2 "No or Other" ) ( 10 = 3 "Other not classifiable") ,gen(par_ed)
recode par_nssec_  (. = 0 "Not Classifiable") (1 = 1 "Management & professional") (2  = 2 "Intermediate")  (3 = 3 "mall employers & own account") (4 =4 "Lower supervisory & technical") (5 = 5 "Semi-routine & routine") , gen(par_nssec)


*indiviudal measures 
recode racel_dv (1 = 1 "White British") (2/4 = 2 "White Other"  ) (5/8 = 3  "Mixed") (9 = 4 "Indian") (10 = 5 "Pakistani") (11 = 6 "Bangladeshi") (12/13 = 8 "Other") (14/16 = 7 "Black" ) (17/97 = 8) (-9 = 9 "Missing") , gen(ethnicity) 
recode racel_dv (1 = 1 "White British") (2/97 = 2 "Other" ) (-9 = .) , gen(ethnic_bin) 


*Explanatory vars 
recode finnow_ (-9/-1 = 6 "Missing") (1 = 1 "Comfortbale") (2 = 2 "Alright") (3 = 3 "Getting by") (4 = 4 "Quite difficult") (5 = 5 "Very difficult") , gen(fin_strain)
*tab jbstat if analytic_sample == 1, miss
gen neet = jbstat_
recode neet -9/-1 = . 1/2 = 1 3/6 = 2 7 = 0 8 = 2 9 = 1 10 = 2 11 = 1 12 = 1 13 = 2 97 = 2 
label values neet neet 



keep  pidp psu strata sc_weight_xc  acad_weight_ month_ /// Surey administration variables 
/// analytic_sample  /// sample selection variabels 
ghq12 sf12_mental life_sat  sf12_physical /// outcome variables 
country devolved devolved2 three_cat detailed /// exposure variables 
sex age_start month /// main covariates 
fin_strain neet /// explanatory variables + other covariates on the next line
owner kids two_par no_workpar par_ed ethnic_bin Wales NIreland


rename month_ sample_month

save "$derived\R_File_top_80_percent24Apr24.dta", replace

********************************************************************************
****** Sensitivity analysis 3
* only includes observations for whom less than 10% of household income data was imputed by Understanding Society. 
********************************************************************************




**Trying to merge the first two waves to work out how to allocate people  
use "$original\\xwavedat_protect.dta" , clear

foreach x in a b c d e f g h i j k   {
merge 1:1 pidp using "$derived\`x'_indresp_selected.dta", gen(`x'_merge)
merge m:1 `x'_hidp using "$derived\`x'_hhresp_core_v2.dta", gen(`x'_hh_merge)
drop if `x'_hh_merge == 2
merge 1:1 pidp using "$derived\`x'_foster_child.dta", gen(`x'_hh_foster)
drop if `x'_hh_foster == 2 
merge 1:1 pidp using "$derived\`x'_par_vars.dta", gen(`x'_par_merge)
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
gen `x'_school_year = 1 if `x'_interview_acad_year == academic_birth_cohort + 17 & academic_birth_cohort !=. & academic_birth_cohort !=. 
replace `x'_school_year = 2 if  `x'_interview_acad_year == academic_birth_cohort + 18 & academic_birth_cohort !=. & academic_birth_cohort !=. 
}

merge 1:1 pidp using "$derived\Academic_year_sample_weights.dta", gen(acad_weights_merge)


***Note this just slims the file to get the number of people down. 
**Generate ever in target age group
gen ever_sixth = .
foreach x in a b c d e f g h i j k   {
recode ever_sixth . = 1 if `x'_school_year ==1 | `x'_school_year == 2
}


*********************
* Recoding parental vars
* Merging parental ever done 
recode maedqf (-9/-1 = . ) (1/2  = 9 "No Qualification") (2/3 = 4 "School level") (4 =  2 "Other higher") (5 = 1 "Degree")  (97  = 5 "Other"), gen(mot_ted)
recode paedqf (-9/-1 = . ) (1/2  = 9 "No Qualification") (2/3 = 4 "School level") (4 =  2 "Other higher") (5 = 1 "Degree")  (97  = 5 "Other"), gen(fat_ted)
clonevar par_ted = mot_ted
replace par_ted = fat_ted if fat_ted < par_ted
*recoding wave specific 
foreach x in a b c d e f g h i j k   {
recode `x'_par_educ (1 = 1 "Degree") (2 =  2 "Other higher") (3/4 = 4 "School level")  (5  = 5 "Other") (9  = 9 "No Qualification") (22  = 10 "Not Classifiable"), gen(`x'_par_hqual)
clonevar `x'_par_hedqual = `x'_par_hqual
replace `x'_par_hedqual = par_ted if `x'_par_hedqual ==. 
recode  `x'_par_hedqual .  = 10 
label variable `x'_par_hedqual "High parent ed 4 source"
}


*NOte none of the_sc_weight has inapplicable but htere are still a few who had a proxy response. 

*Creating a binary measure of ghq this will then be used to create a binary random variable with samme distribution to design study without biasing results. 
foreach w in a b c d e f g h i j k {
recode `w'_scghq2_dv -9/-7 = . 0/3 = 0 4/12 = 1 , gen(`w'_ghq_bin)
recode `w'_scghq2_dv -9/-7 = 1 0/12 = 0, gen(`w'_snr_ghq) 
recode `w'_scghq2_dv -9/-7 = 0 0/12 = 1, gen(`w'_has_out)
}





***********************************************
*Generating an anual income measure 
foreach x in a b c d e f g h i j k {
	gen `x'_an_income = `x'_fihhmngrs1_dv *12
}	


* Note I need better race measures and probably one that is static across waves. 


keep pidp strata psu /// fixed US design variables 
*_sc_weight_xc /// varing US variables 
*_month weight_* *_acad_weight /// the variables from the weight file 
*_school_year *_interview_acad_year *_interview_cal_month /// Varying study design variables 
*_hh_foster /// varying study selection variables 
*_country  *_racel *_sf1 /// varying person variabels 
*_nkids_dv *_tenure_dv  *_hhsize ///
*_in_ed   *_school  *_fenow  *_paid_job  *_jbhas *_jboff *_neet  *_jbstat  /// education employment vars
*_an_income *_fihhmngrs_if /// varying exposure variables 
*_par_hedqual *_par_nssec *_no_workpar *_two_par /// parental vars
academic_birth_cohort sex racel_dv bornuk_dv  ///   fixed person variabels Outcomes on the next line 
*_finnow *_has_out *_ghq_bin *_scghq1_dv *_sf12mcs_dv *_sf12pcs_dv *_sclfsato




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
sc_weight_xc_ /// varing US variables 
acad_weight_ month_ /// The additional variables brought through to correct the weights
school_year_ interview_acad_year_ interview_cal_month_ /// Varying study design variables 
hh_foster_ /// varying study selection variables 
country_  racel_ sf1_ /// varying person variabels 
nkids_dv_ tenure_dv_  hhsize_ ///
in_ed_   school_  fenow_  paid_job_  jbhas_ jboff_ neet_  jbstat_  /// education employment vars
par_hedqual_ par_nssec_ no_workpar_ two_par_ /// parental education and nssec
finnow_  an_income_ fihhmngrs_if_ /// finance variables next row is health vars
has_out_ ghq_bin_ scghq1_dv_ sf12mcs_dv_ sf12pcs_dv_ sclfsato_, i(pidp) j(wave)


*** Alternative codings for outcomes. 
recode scghq1_dv_  -9/-1 = ., gen(ghq12)

clonevar life_sat = sclfsato_
recode life_sat -10/-1 = . 
 
clonevar sf12_mental = sf12mcs_dv_ 
recode  sf12_mental -9/-7 = . 

clonevar sf12_physical = sf12pcs_dv_
recode sf12_physical  -9/-7 = . 


*** Exposure variables 1 country and period 
*Academic year and period allcoation 
gen acad_year = interview_acad_year
*Note adding sf12_physicla to this makes no difference to results.  
gen any_out = 1 if ghq12 != . | sf12_mental != . | life_sat !=. 


*** Sample selection stage 1 pre_percentile 
gen in_analytic_period = 1 if interview_acad_year >= 2009 & interview_acad_year <= 2018 
gen has_country = 1 if country >0 & country <5 
gen correct_age = 1 if school_year < 3
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


*The EMA periods for anlysis
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012 = 3 "Post EMA") (2013/2014 = 4 "Age 17") (2015/2018 = 5 "Age 18" ) (2019/2020 = . ) , gen(detailed )
recode acad_year (2008 = . ) (2009/2010 = 1 "EMA Period") (2011 = 2  "Transition" ) (2012/2018 = 3 "Post EMA" ) (2019/2020 = . ) , gen(three_cat )

*** Bottom 10% of sample 

recode fihhmngrs_if_  .1/1 = 0 0/.1 = 1 , gen(min_impute)

tab sex
keep if min_impute == 1
tab sex

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
recode  school_year_  1 = 16 2 = 17, gen(age_start)

*Household measures 
recode tenure_dv_  (-9 = 3 "Missing")  (1/2 = 0 "Owners") (3/4  = 1 "Social housing" ) (5/8 = 2 "Rent & Other" ), gen(housing)
recode tenure_dv_  (-9 = . )  (1/2 = 0 "Owners") (3/8 = 1 "Rent & Other" ), gen(owner)
recode hhsize (1/2 = 0  "One or Two") (3/4 =  1 "Three or four") (5/13  = 2 "Five or more"), gen(household_size)
recode nkids_dv_  (0 = 0 "None") (1/2 = 1 "One or two") (3/8 =  3 "Three or more"), gen(kids)
rename  two_par_ two_par
rename no_workpar_ no_workpar
recode par_hedqual_ (1/2 = 0 "Post-School" ) (4 = 1 "School level") (5/9 = 2 "No or Other" ) ( 10 = 3 "Other not classifiable") ,gen(par_ed)
recode par_nssec_  (. = 0 "Not Classifiable") (1 = 1 "Management & professional") (2  = 2 "Intermediate")  (3 = 3 "mall employers & own account") (4 =4 "Lower supervisory & technical") (5 = 5 "Semi-routine & routine") , gen(par_nssec)


*indiviudal measures 
recode racel_dv (1 = 1 "White British") (2/4 = 2 "White Other"  ) (5/8 = 3  "Mixed") (9 = 4 "Indian") (10 = 5 "Pakistani") (11 = 6 "Bangladeshi") (12/13 = 8 "Other") (14/16 = 7 "Black" ) (17/97 = 8) (-9 = 9 "Missing") , gen(ethnicity) 
recode racel_dv (1 = 1 "White British") (2/97 = 2 "Other" ) (-9 = .) , gen(ethnic_bin) 


*Explanatory vars 
recode finnow_ (-9/-1 = 6 "Missing") (1 = 1 "Comfortbale") (2 = 2 "Alright") (3 = 3 "Getting by") (4 = 4 "Quite difficult") (5 = 5 "Very difficult") , gen(fin_strain)
*tab jbstat if analytic_sample == 1, miss
gen neet = jbstat_
recode neet -9/-1 = . 1/2 = 1 3/6 = 2 7 = 0 8 = 2 9 = 1 10 = 2 11 = 1 12 = 1 13 = 2 97 = 2 
label values neet neet 
drop fihhmngrs_if_ min_impute
rename month_ sample_month
save   "$derived\\\Xclude_imputed_income.dta" , replace

***********************************************************************************
**** BHPS parallel trends analysis 

global original_bhps "[Project_folder]\DataBHPS\Original\"
global derived_bhps  "[Project_folder]\DataBHPS\Derived\"


foreach w in  k l m n o p q r {
use "$original_bhps\\b`w'_indresp_protect.dta", clear
keep  pidp pid b`w'_indin01_xw b`w'_age_dv b`w'_gor_dv b`w'_scghq1_dv b`w'_scghq2_dv b`w'_fihhmn  b`w'_race 
save "$derived_bhps\\`w'_bhps_ind.dta", replace
}




use "$original_bhps\xwaveid_bh_protect.dta" , clear
keep pid sex sampst psu strata

foreach w in  k l m n o p q r {
merge 1:1 pid using "$derived_bhps\`w'_bhps_ind.dta", gen(b`w'_merge)
}

foreach w in  k l m n o p q r {
gen b`w'_in_age = 1  if b`w'_age_dv <18 & b`w'_age_dv >15 
gen b`w'_annual_income =  b`w'_fihhmn * 12
replace  b`w'_annual_income = . if  b`w'_annual_income < 0
recode  b`w'_scghq1_dv -9/-1 =. , gen(b`w'_ghq_cont)
recode b`w'_gor_dv (-9 = .) (1/9 = 1 "England") (10 = 2 "Wales") (11 = 3 "Scotland" ) (12 = 4 "Nothern Ireland" ) (13 = .) , gen(b`w'_country)
}

keep   pid sampst psu strata pidp *_age_dv *_scghq1_dv *_indin01_xw *_ghq_cont *_country *_annual_income *_in_age 
rename bk_* *_2001
rename bl_* *_2002
rename bm_*	*_2003
rename bn_*	*_2004
rename bo_*	*_2005
rename bp_*	*_2006
rename bq_*	*_2007
rename br_* *_2008


reshape long indin01_xw_ country_   age_dv_ scghq1_dv_ ghq_cont_ annual_income_ in_age_  , i(pid) j(wave)
keep if in_age_ == 1


gen sample_with_weights = 1 if country != . & indin01_xw_ !=. & ghq_cont_ != . 


*Uses return list to allocate values to perecentiles 
gen bottom_10 = . 
forval i = 2001/2008 {
display r(r1)
_pctile annual_income_  [pweight=indin01_xw_] if sample_with_weights == 1 & wave == `i',  p(10 11 12 14)
replace bottom_10 = 1 if annual_income_ <= r(r1) & wave == `i' & sample_with_weights == 1
}

gen likely_sample = 1 if country != . & indin01_xw_ !=. & ghq_cont_ != . & bottom_10 == 1



recode country (1 = 1 "England") (2/4 = 2 "Other") , gen(devolved)

svyset psu [pweight = indin01_xw_ ] 
tab  devolved wave if likely_sample == 1 
svy: mean scghq1_dv_  if likely_sample == 1  ,over(devolved wave ) 



*Output then manually used to derive a csv file. 
