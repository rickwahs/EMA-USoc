###### This version of R code initiated on May2024. 
### Written by richard.shaw@glasgow.ac.uk 

##### Background 
### This code is created for paper investigating the impact of the Education Maintenance Allowance using data from understanding Society. 

### Most of the cleaning and coding of data was carried out using Stata, and analysis  was carried out in R. A variety of packages and approaches were initially applied and some vestigial code remains. The logic of some of the function and object names were derived from an earlier draft, and  when writing the paper became clearer how to present the results. So some things such as RQs may not always be consistent. 

# It should be noted that analyses have been run interactively and this file is not simply run continously. 

### Initial project proposal was pre-registered and is available from  https://osf.io/u3avm


### Available outputs
# A pre-print will be available (Insert location here).
# Policy Brief to go here.  
# Final accepted journal draft available from: (insert location here) 
# Abstract for work to be presented at the Society for Social Medicine Annual Scientific meeting available from (Insert location here). 

##### Structure of code. 
## I start with the main file paths for files. Line 40 
## Then packages loaded. Line 44
## Functions created for the main analyses. Line 62
## Importation of data  Line 185
## Descriptive statistics Line 213
## Regression analyses Line 340
## Then revised code for the sensitivity analyses. 

# Sensitivity 1: Line 736
# Sensitivity 2: Line 758
# Sensitivity 3: Line 774 
# Sensitivity 4: Line 790
# Sensitivity 5: Line 886
# BHPS parallel trends graph: line 900



### File pathways 

file_path = "[Project_folder]/Data/AnalysisCheck/"
storage_path = getwd()


### Packages to install. 
library(haven)
library(dplyr)
library(sandwich)
library(forcats)
library(survey)
library(broom)
library(estimatr)
library(arsenal) ## This includes the tableby function 
library(labelled) ## this includes droplevels and unlabelled
library(jtools) ## This includes some alternative options for linear regression 
library(interactions)
library(tibble)
library(ggplot2)
library(writexl)

#Note I think that this may be the option usin the estimator package
## https://search.r-project.org/CRAN/refmans/estimatr/html/predict.lm_robust.html

########################### Functions 





#This code runs a difference in difference model with minimal levels of adjustment 
min_adjust_model <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = acad_weight_ )
}




full_adjust_model <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month + two_par +  no_workpar+ owner + kids + par_ed + ethnic_bin ")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = acad_weight_ )
}


flexy_model <- function(outcome, period, additional_terms  = " " ){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month", additional_terms)
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = acad_weight_ )
}


sex_stratified_min_adjust <- function(outcome, period, gender = "male"){
  stratified_data <- filter(Stata_data, sex == gender)
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = acad_weight_ )
}

sex_interactions_min_adjust <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, "*sex + age_start +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = acad_weight_ )
}



ethnic_stratified_min_adjust <- function(outcome, period, ethnic = "White British"){
  stratified_data <- filter(Stata_data, ethnic_bin == ethnic)
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = acad_weight_ )
}


ethnic_interactions_min_adjust <- function(outcome, period) {
  ethnic_data <- filter(Stata_data, !is.na(ethnic_bin))
  formula <- paste0(outcome," ~ devolved2*", period, "*ethnic_bin + age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = ethnic_data, weights = acad_weight_ )
  }


age_stratified_min_adjust <- function(outcome, period, age_input ){
  stratified_data <- filter(Stata_data, age_start == age_input)
  formula <- paste0(outcome," ~ devolved2*", period, "  + sex +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = acad_weight_ )
}


age_interactions_min_ajust <- function(outcome, period) {
  formula <- paste0(outcome," ~ devolved2*", period, "*age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = acad_weight_ )
}





supplment_output <- function(model, independent_var){
  tidy(model, conf.int = TRUE)|>
  mutate(p_value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, outcome, p_value) |>
  rename_with( ~ paste0(independent_var, .x), .cols = c(estimate, conf.low, conf.high, p_value) )|>
  select(!outcome)
  }

supplment_output2 <- function(model, independent_var){
  tidy(model, conf.int = TRUE)|>
    mutate(p_value = format(round(p.value, 3), nsmall = 3),
           coef = format(round(estimate, 2), nsmall = 2),
           conf_low = format(round(conf.low,2), nsmall =2),
           conf_high = format(round(conf.high,2),nsmall =2)) |>
    mutate(result_col = paste0(coef, " (", conf_low, " to ", conf_high, ") ",p_value))|>
    select(term,result_col)   |>
    rename_with( ~ paste0(independent_var, .x), .cols = c(result_col) )
    }






# This code extract the DID parameters assuming they are the last ones (last 2 parameters by )
did_coef_extract  <- function( model, num_parameters = 2 ) {
  tidy(model,  conf.int = TRUE) |>
    mutate(p_value = round(p.value, 3)) |>
    slice_tail(n = num_parameters) |>
    select(term, estimate, conf.low, conf.high, outcome, p_value)
}



### Prediction graphs 
#Now for the graphs 
predictions_for_graphs <- function(model, new_data, outcome_name) {
  new_data |>
  cbind(predict(model, newdata = new_data ,interval = "confidence")) |>
  mutate(Outcome = outcome_name)
}




###############################
### Importing data files 


#Vector indicate factor variables so that the right labelled variables are converted to factors 
factor_vars <- c("country", "devolved","devolved2", "three_cat", "detailed", "sex", "age_start", "month", "sample_month", "fin_strain" , "neet", "owner", "kids", "two_par", "no_workpar", "par_ed", "ethnic_bin", "Wales", "NIreland") 



## Importing data from Stata
# NB File path may need changing 
Stata_data <- read_dta(paste0(file_path, "Trial_R_File_15Jan24.dta")) |> 
  mutate(across(c(factor_vars), .fns = as_factor),
       sex= droplevels(sex),
       country = droplevels(country),   
       sf12_mental = unlabelled(sf12_mental),
       sf12_physical  = unlabelled(sf12_physical),
       life_sat  = as.double(life_sat ))
#Some missing data in financial strain which need recoding. 

levels(Stata_data$fin_strain)[levels(Stata_data$fin_strain) == 'Missing'] <- NA


#Getting rid of labels for ethnic_bin measure 
var_label(Stata_data$ethnic_bin) <- NULL


### Preliminary svyset 
data_design <- svydesign(data = Stata_data, weights = ~acad_weight_ , id = ~1)



### Coding survey data in r 
##GHQ12 
# svymean( x =  ~ghq12, design = data_design, na.rm = TRUE) 
ghq_mean <- svyby(formula = ~ghq12 , by = ~devolved2 + three_cat ,
      design = data_design, 
      FUN = svymean, na.rm = TRUE ,
      row.names = FALSE)

## Calculating Standard deviation data in R. 
#svyvar(x =  ~ghq12, design = data_design, na.rm = TRUE)
ghq_SD <- svyby(formula = ~ghq12 , by = ~devolved2 + three_cat ,
      design = data_design, 
      FUN = svyvar, na.rm = TRUE ,
      row.names = FALSE) |>
  mutate(SD = sqrt(ghq12))|>
  select(devolved2, three_cat, SD)

ghq_MeanSD <- ghq_mean |>
  left_join(ghq_SD) |>
  rename(ghq12_SE = se,
         ghq12_SD = SD)



### sf12_mental 
sf12_mental_mean <- svyby(formula = ~sf12_mental , by = ~devolved2 + three_cat ,
                  design = data_design, 
                  FUN = svymean, na.rm = TRUE ,
                  row.names = FALSE)

## Calculating Standard deviation data in R. 
sf12_mental_SD <- svyby(formula = ~sf12_mental , by = ~devolved2 + three_cat ,
                design = data_design, 
                FUN = svyvar, na.rm = TRUE ,
                row.names = FALSE) |>
  mutate(SD = sqrt(sf12_mental))|>
  select(devolved2, three_cat, SD)

sf12_mental_MeanSD <- sf12_mental_mean |>
  left_join(sf12_mental_SD)|>
  rename(sf12_mental_SE = se,
         sf12_mental_SD = SD)
View(sf12_mental_MeanSD)

### sf12_physical 
# svymean( x =  ~ghq12, design = data_design, na.rm = TRUE) 
sf12_physical_mean <- svyby(formula = ~sf12_physical , by = ~devolved2 + three_cat ,
                          design = data_design, 
                          FUN = svymean, na.rm = TRUE ,
                          row.names = FALSE)

## Calculating Standard deviation data in R. 
#svyvar(x =  ~ghq12, design = data_design, na.rm = TRUE)
sf12_physical_SD <- svyby(formula = ~sf12_physical , by = ~devolved2 + three_cat ,
                        design = data_design, 
                        FUN = svyvar, na.rm = TRUE ,
                        row.names = FALSE) |>
  mutate(SD = sqrt(sf12_physical))|>
  select(devolved2, three_cat, SD)

sf12_physical_MeanSD <- sf12_physical_mean |>
  left_join(sf12_physical_SD)|>
  rename(sf12_physical_SE = se,
         sf12_physical_SD = SD)
View(sf12_physical_MeanSD)


### life satisfaciton  
# svymean( x =  ~ghq12, design = data_design, na.rm = TRUE) 
life_sat_mean <- svyby(formula = ~life_sat , by = ~devolved2 + three_cat ,
                            design = data_design, 
                            FUN = svymean, na.rm = TRUE ,
                            row.names = FALSE)




## Calculating Standard deviation data in R. 
#svyvar(x =  ~ghq12, design = data_design, na.rm = TRUE)
life_sat_SD <- svyby(formula = ~life_sat , by = ~devolved2 + three_cat ,
                          design = data_design, 
                          FUN = svyvar, na.rm = TRUE ,
                          row.names = FALSE) |>
  mutate(SD = sqrt(life_sat))|>
  select(devolved2, three_cat, SD)

life_sat_MeanSD <- life_sat_mean |>
  left_join(life_sat_SD)|>
  rename(life_sat_SE = se,
         life_sat_SD = SD)

### Continous vars summary 
cont_vars_summary <- ghq_MeanSD %>%
  left_join(life_sat_MeanSD) %>%
  left_join(sf12_mental_MeanSD)%>%
  left_join(sf12_physical_MeanSD)%>%
  arrange(devolved2) %>%
  mutate_if(is.double, round, 2)

View(cont_vars_summary)
write_xlsx(cont_vars_summary,paste0(storage_path, "cont_vars_summary.xlsx") )



### Subsetting the categorical variables for presentation 
descriptive_vars <- Stata_data %>%
  select(all_of(factor_vars)) %>% 
  select(-Wales, -devolved, -NIreland, -month, - detailed, -sample_month)
  
#An esxample of a single vairable and tableby()
tableby(three_cat ~ devolved2, data = Stata_data, test = FALSE) |>
  summary()

#Subsetting all categorical variables for selection
descriptive_vars %>%
  tableby(three_cat ~., data = ., test = FALSE) |>
  summary()

names(Stata_data)      
##############################################################
##### Research Question 1a: Does the mental difference between people in England and RUK change over time corresponding to periods prior to the abolition of EMA, transitionary EMA period, post EMA Period? 

### Minimally adjusted models 
## Generate models 
RQ1a_gh12_minadj_mod <- min_adjust_model("ghq12", "three_cat")
RQ1a_sf12mental_minadj_mod <- min_adjust_model("sf12_mental", "three_cat")
RQ1a_sf12physcial_minadj_mod <- min_adjust_model("sf12_physical", "three_cat")
RQ1a_life_sat_minadj_mod <- min_adjust_model("life_sat", "three_cat")

# Output for supplement (All coefficients)
#Note need to convert to function and convert name. 
ghq_minadj_sup <- tidy(RQ1a_gh12_minadj_mod, conf.int = TRUE)|>
  mutate(p_value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, outcome, p_value) |>
  rename_with( ~ paste0("prefix_", .x), .cols = c(estimate, conf.low, conf.high, p_value) )


RQ1a_ghq12_supplement_output <- supplment_output(RQ1a_gh12_minadj_mod, "GHQ12")
RQ1a_sf12mental_supplement_output <- supplment_output(RQ1a_sf12mental_minadj_mod, "MCS")
RQ1a_sf12physcial_supplement_output <- supplment_output(RQ1a_sf12physcial_minadj_mod, "PCS")
RQ1a_life_sat_supplement_output <- supplment_output(RQ1a_life_sat_minadj_mod, "Life Sat")
View(RQ1a_ghq12_supplement_output)

RQ1a_min_adjust_supplement_table <- RQ1a_ghq12_supplement_output |>
  left_join(RQ1a_sf12mental_supplement_output)|>
  left_join(RQ1a_sf12physcial_supplement_output)|>
  left_join(RQ1a_life_sat_supplement_output)
View(RQ1a_min_adjust_supplement_table)

write_xlsx(RQ1a_min_adjust_supplement_table, paste0(storage_path, "RQ1a_min_adjust_supplement_table.xlsx") )

##Supplement output second version 
RQ1a_ghq12_supplement_output2 <- supplment_output2(RQ1a_gh12_minadj_mod, "GHQ12")
RQ1a_sf12mental_supplement_output2 <- supplment_output2(RQ1a_sf12mental_minadj_mod, "MCS")
RQ1a_sf12physcial_supplement_output2 <- supplment_output2(RQ1a_sf12physcial_minadj_mod, "PCS")
RQ1a_life_sat_supplement_output2 <- supplment_output2(RQ1a_life_sat_minadj_mod, "Life Sat")
View(RQ1a_ghq12_supplement_output2)


RQ1a_min_adjust_supplement_table2 <- RQ1a_ghq12_supplement_output2 |>
  left_join(RQ1a_sf12mental_supplement_output2)|>
  left_join(RQ1a_sf12physcial_supplement_output2)|>
  left_join(RQ1a_life_sat_supplement_output2)
View(RQ1a_min_adjust_supplement_table2)


write_xlsx(RQ1a_min_adjust_supplement_table2, paste0(storage_path, "RQ1a_min_adjust_supplement_table2.xlsx") )




View(RQ1a_ghq12_supplement_output)



#OUtput for main summary results 
rq1a_minadjust_results <- do.call("rbind", list(did_coef_extract(RQ1a_gh12_minadj_mod), 
                      did_coef_extract(RQ1a_sf12mental_minadj_mod),
                      did_coef_extract(RQ1a_sf12physcial_minadj_mod),
                      did_coef_extract(RQ1a_life_sat_minadj_mod)))   
View(rq1a_minadjust_results)

write_xlsx(rq1a_minadjust_results, paste0(storage_path, "RQ1a_min_adjust_main_table.xlsx") )


rq1a_minadjust_results




### Models with increased level of adjustment 
#Generate models
RQ1a_gh12_full_mod <- full_adjust_model("ghq12", "three_cat")
RQ1a_sf12mental_full_mod <- full_adjust_model("sf12_mental", "three_cat")
RQ1a_sf12physcial_full_mod <- full_adjust_model("sf12_physical", "three_cat")
RQ1a_life_sat_full_mod <- full_adjust_model("life_sat", "three_cat")

### Models for supplement 
RQ1a_ghq12_full_supplement_output <- supplment_output(RQ1a_gh12_full_mod, "GHQ12")
RQ1a_sf12mental_full_supplement_output <- supplment_output(RQ1a_sf12mental_full_mod, "MCS")
RQ1a_sf12physcial__full_supplement_output <- supplment_output(RQ1a_sf12physcial_full_mod, "PCS")
RQ1a_life_sat__full_supplement_output <- supplment_output(RQ1a_life_sat_full_mod, "Life Sat")
View(RQ1a_ghq12_full_supplement_output)


RQ1a_full_adjust_supplement_table <- RQ1a_ghq12_full_supplement_output |>
  left_join(RQ1a_sf12mental_full_supplement_output)|>
  left_join(RQ1a_sf12physcial__full_supplement_output)|>
  left_join(RQ1a_ghq12_full_supplement_output)
View(RQ1a_full_adjust_supplement_table)
write_xlsx(RQ1a_full_adjust_supplement_table, paste0(storage_path, "RQ1a_full_supplement_table.xlsx") )



#Did coefficients
rq1a_fulldjust_results <- do.call("rbind", list(did_coef_extract(RQ1a_gh12_full_mod), 
                                                did_coef_extract(RQ1a_sf12mental_full_mod),
                                                did_coef_extract(RQ1a_sf12physcial_full_mod),
                                                did_coef_extract(RQ1a_life_sat_full_mod)))   

View(rq1a_fulldjust_results)


rq1a_both_levels_adjust <- rbind(rq1a_minadjust_results |> mutate(adjustment = "Min Adjust"), rq1a_fulldjust_results |> mutate(adjustment = "Full Adjust"))
rq1a_both_levels_adjust
write_xlsx(rq1a_both_levels_adjust, paste0(storage_path, "rq1a_both_levels_adjust.xlsx") )



### Research Question 1ba: Does the mental difference between people in England and RUK change over time corresponding to periods prior to the abolition of EMA, transitionary EMA period, Raise Education to 17, Raise School leaving age to 18
RQ1b_gh12_minadj_mod <- min_adjust_model("ghq12", "detailed")
RQ1b_sf12mental_minadj_mod <- min_adjust_model("sf12_mental", "detailed")
RQ1b_sf12physcial_minadj_mod <- min_adjust_model("sf12_physical", "detailed")
RQ1b_life_sat_minadj_mod <- min_adjust_model("life_sat", "detailed")


#OUtput for main summary results 
rq1b_minadjust_results <- do.call("rbind", list(did_coef_extract(RQ1b_gh12_minadj_mod, 4), 
                                                did_coef_extract(RQ1b_sf12mental_minadj_mod, 4),
                                                did_coef_extract(RQ1b_sf12physcial_minadj_mod, 4),
                                                did_coef_extract(RQ1b_life_sat_minadj_mod, 4)))   
write_xlsx(rq1b_minadjust_results, paste0(storage_path, "rq1b_minadjust_results.xlsx") )


View(rq1b_minadjust_results)

#Now running for the fully adjusted models 
RQ1b_gh12_fulladj_mod <- full_adjust_model("ghq12", "detailed")
RQ1b_sf12mental_fulladj_mod <- full_adjust_model("sf12_mental", "detailed")
RQ1b_sf12physcial_fulladj_mod <- full_adjust_model("sf12_physical", "detailed")
RQ1b_life_sat_fulladj_mod <- full_adjust_model("life_sat", "detailed")

rq1b_fulladjust_results <- do.call("rbind", list(did_coef_extract(RQ1b_gh12_fulladj_mod, 4), 
                                                did_coef_extract(RQ1b_sf12mental_fulladj_mod, 4),
                                                did_coef_extract(RQ1b_sf12physcial_fulladj_mod, 4),
                                                did_coef_extract(RQ1b_life_sat_fulladj_mod, 4)))   
View(rq1b_fulladjust_results)
write_xlsx(rq1b_fulladjust_results, paste0(storage_path, "rq1b_fulladjust_results.xlsx") )



## RQ2:  Are any difference-in-differences identified above explained by educational status and / or financial strain? 
rq2_ghq_min_adj_mod <- flexy_model("ghq12", "three_cat", "+ fin_strain + neet")
rq2_mcs_min_adj_mod <- flexy_model("sf12_mental", "three_cat", "+ fin_strain + neet")
rq2_pcs_min_adj_mod <- flexy_model("sf12_physical", "three_cat", "+ fin_strain + neet")
rq2_life_sat_min_adj_mod <- flexy_model("life_sat", "three_cat", "+ fin_strain + neet")


RQ2_ghq12_supplement <- supplment_output(rq2_ghq_min_adj_mod, "GHQ12")
RQ2_sf12mental_supplement <- supplment_output(rq2_mcs_min_adj_mod, "MCS")
RQ2_sf12physical_supplement <- supplment_output(rq2_pcs_min_adj_mod, "PCS")
RQ2_life_sat_supplement <- supplment_output(rq2_life_sat_min_adj_mod, "Life Sat")

RQ2_min_adjust_supplement_table <- RQ2_ghq12_supplement |>
  left_join(RQ2_sf12mental_supplement)|>
  left_join(RQ2_sf12physical_supplement)|>
  left_join(RQ2_life_sat_supplement)
View(RQ2_min_adjust_supplement_table)
write_xlsx(RQ2_min_adjust_supplement_table, paste0(storage_path, "RQ2_min_adjust_supplement_table.xlsx") )

#Model 2 supplment 
RQ2_ghq12_supplement2 <- supplment_output2(rq2_ghq_min_adj_mod, "GHQ12")
RQ2_sf12mental_supplement2 <- supplment_output2(rq2_mcs_min_adj_mod, "MCS")
RQ2_sf12physical_supplement2 <- supplment_output2(rq2_pcs_min_adj_mod, "PCS")
RQ2_life_sat_supplement2 <- supplment_output2(rq2_life_sat_min_adj_mod, "Life Sat")

RQ2_min_adjust_supplement_table2 <- RQ2_ghq12_supplement2 |>
  left_join(RQ2_sf12mental_supplement2)|>
  left_join(RQ2_sf12physical_supplement2)|>
  left_join(RQ2_life_sat_supplement2)
View(RQ2_min_adjust_supplement_table2)
write_xlsx(RQ2_min_adjust_supplement_table2, paste0(storage_path, "RQ2_min_adjust_supplement_table2.xlsx") )


RQ2_minadjust_results <- do.call("rbind", list(did_coef_extract(rq2_ghq_min_adj_mod, 2), 
                                                did_coef_extract(rq2_mcs_min_adj_mod, 2),
                                                did_coef_extract(rq2_pcs_min_adj_mod, 2),
                                                did_coef_extract(rq2_life_sat_min_adj_mod, 2)))   


View(RQ2_minadjust_results)
write_xlsx(RQ2_minadjust_results, paste0(storage_path, "RQ2_minadjust_results.xlsx") )









### Research question 2 with additionl vars
full_adj_finstrain_neet <-  " + age_start + sex + month + two_par +  no_workpar+ owner + kids + par_ed + ethnic_bin + Wales + NIreland +  fin_strain + neet"

rq2_ghq_full_adj_mod <- flexy_model("ghq12", "three_cat", full_adj_finstrain_neet)
rq2_mcs_full_adj_mod <- flexy_model("sf12_mental", "three_cat", full_adj_finstrain_neet)
rq2_pcs_full_adj_mod <- flexy_model("sf12_physical", "three_cat", full_adj_finstrain_neet)
rq2_life_sat_full_adj_mod <- flexy_model("life_sat", "three_cat", full_adj_finstrain_neet)

RQ2_fulladjust_results <- do.call("rbind", list(did_coef_extract(rq2_ghq_full_adj_mod, 2), 
                                               did_coef_extract(rq2_mcs_full_adj_mod, 2),
                                               did_coef_extract(rq2_pcs_full_adj_mod, 2),
                                               did_coef_extract(rq2_life_sat_full_adj_mod, 2)))   

summary(rq2_ghq_full_adj_mod)

names(Stata_data)

View(RQ2_fulladjust_results)
write_xlsx(RQ2_fulladjust_results, paste0(storage_path, "RQ2_fulladjust_results.xlsx") )

#supplemnt table for RQ2 or otherwise model 3 



RQ2_ghq12_full_supplement <- supplment_output(rq2_ghq_full_adj_mod, "GHQ12")
RQ2_sf12mental_full_supplement <- supplment_output(rq2_mcs_full_adj_mod, "MCS")
RQ2_sf12physical_full_supplement <- supplment_output(rq2_pcs_full_adj_mod, "PCS")
RQ2_life_sat_full_supplement <- supplment_output(rq2_life_sat_full_adj_mod, "Life Sat")

View(RQ2_ghq12_full_supplement)

RQ2_full_supplement_table <- RQ2_ghq12_full_supplement |>
  left_join(RQ2_sf12mental_full_supplement)|>
  left_join(RQ2_sf12physical_full_supplement)|>
  left_join(RQ2_life_sat_full_supplement)
View(RQ2_full_supplement_table)
write_xlsx(RQ2_full_supplement_table, paste0(storage_path, "RQ2_full_supplement_table.xlsx") )



RQ2_ghq12_full_supplement2 <- supplment_output2(rq2_ghq_full_adj_mod, "GHQ12")
RQ2_sf12mental_full_supplement2 <- supplment_output2(rq2_mcs_full_adj_mod, "MCS")
RQ2_sf12physical_full_supplement2 <- supplment_output2(rq2_pcs_full_adj_mod, "PCS")
RQ2_life_sat_full_supplement2 <- supplment_output2(rq2_life_sat_full_adj_mod, "Life Sat")

View(RQ2_ghq12_full_supplement2)

RQ2_full_supplement_table2 <- RQ2_ghq12_full_supplement2 |>
  left_join(RQ2_sf12mental_full_supplement2)|>
  left_join(RQ2_sf12physical_full_supplement2)|>
  left_join(RQ2_life_sat_full_supplement2)
View(RQ2_full_supplement_table2)
write_xlsx(RQ2_full_supplement_table2, paste0(storage_path, "RQ2_full_supplement_table2.xlsx") )



## RQ3a: Gender differences in results 
ghq_men_mod <- sex_stratified_min_adjust("ghq12", "three_cat")
mcs_men_mod <- sex_stratified_min_adjust("sf12_mental", "three_cat")
pcs_men_mod <- sex_stratified_min_adjust("sf12_physical", "three_cat")
life_sat_men_mod <- sex_stratified_min_adjust("life_sat", "three_cat")

men_results <- do.call("rbind", list(did_coef_extract(ghq_men_mod), 
                                                did_coef_extract(mcs_men_mod),
                                                did_coef_extract(pcs_men_mod),
                                                did_coef_extract(life_sat_men_mod)))  |>
  mutate(subgroup = "male")




ghq_women_mod <- sex_stratified_min_adjust("ghq12", "three_cat", "female")
mcs_women_mod <- sex_stratified_min_adjust("sf12_mental", "three_cat", "female")
pcs_women_mod <- sex_stratified_min_adjust("sf12_physical", "three_cat", "female")
life_sat_women_mod <- sex_stratified_min_adjust("life_sat", "three_cat", "female")

women_results <- do.call("rbind", list(did_coef_extract(ghq_women_mod), 
                                     did_coef_extract(mcs_women_mod),
                                     did_coef_extract(pcs_women_mod),
                                     did_coef_extract(life_sat_women_mod)))  |>
  mutate(subgroup = "female")

rq3_gender_stratified_min_adjust <- rbind(men_results, women_results)
write_xlsx(rq3_gender_stratified_min_adjust, paste0(storage_path, "rq3_gender_stratified_min_adjust.xlsx") )



ghq_sex_interac_mod  <- sex_interactions_min_adjust("ghq12", "three_cat")
mcs_sex_interac_mod  <- sex_interactions_min_adjust("sf12_mental", "three_cat")
pcs_sex_interac_mod  <- sex_interactions_min_adjust("sf12_physical", "three_cat")
life_sat_sex_interac_mod  <- sex_interactions_min_adjust("life_sat", "three_cat")


sex_interaction_results <- do.call("rbind", list(did_coef_extract(ghq_sex_interac_mod, 5), 
                                       did_coef_extract(mcs_sex_interac_mod, 5),
                                       did_coef_extract(pcs_sex_interac_mod, 5),
                                       did_coef_extract(life_sat_sex_interac_mod, 5)))
View(sex_interaction_results)
write_xlsx(sex_interaction_results, paste0(storage_path, "rq3_gender_sex_interaction_results.xlsx") )




##RQ3b : Ethnicity 
RQ3b_ghq12_wb_mod <- ethnic_stratified_min_adjust("ghq12", "three_cat")
RQ3b_sf12_mental_wb_mod <- ethnic_stratified_min_adjust("sf12_mental", "three_cat")
RQ3b_sf12_physical_wb_mod <- ethnic_stratified_min_adjust("sf12_physical", "three_cat")
RQ3b_life_sat_wb_mod <- ethnic_stratified_min_adjust("life_sat", "three_cat")

White_British_results <- do.call("rbind", list(did_coef_extract(RQ3b_ghq12_wb_mod), 
                                               did_coef_extract(RQ3b_sf12_mental_wb_mod),
                                               did_coef_extract(RQ3b_sf12_physical_wb_mod),
                                               did_coef_extract(RQ3b_life_sat_wb_mod)))  |>
  mutate(subgroup = "White British")
View(White_British_results)






RQ3b_ghq12_other_mod <- ethnic_stratified_min_adjust("ghq12", "three_cat", "Other")
RQ3b_sf12_mental_other_mod <- ethnic_stratified_min_adjust("sf12_mental", "three_cat", "Other")
RQ3b_sf12_physical_other_mod <- ethnic_stratified_min_adjust("sf12_physical", "three_cat", "Other")
RQ3b_life_sat_other_mod <- ethnic_stratified_min_adjust("life_sat", "three_cat", "Other")

RQ3b_other_results <- do.call("rbind", list(did_coef_extract(RQ3b_ghq12_other_mod), 
                                               did_coef_extract(RQ3b_sf12_mental_other_mod),
                                               did_coef_extract(RQ3b_sf12_physical_other_mod),
                                               did_coef_extract(RQ3b_life_sat_other_mod)))  |>
  mutate(subgroup = "Other")
View()
View(RQ3b_other_results)

rq3_ethnicity_stratified_min_adjust <- rbind(White_British_results, RQ3b_other_results )
write_xlsx(rq3_ethnicity_stratified_min_adjust, paste0(storage_path, "rq3_ethnicity_stratified_min_adjust.xlsx") )



ghq_ethnic_interac_mod  <- ethnic_interactions_min_adjust("ghq12", "three_cat")
mcs_ethnic_interac_mod  <- ethnic_interactions_min_adjust("sf12_mental", "three_cat")
pcs_ethnic_interac_mod  <- ethnic_interactions_min_adjust("sf12_physical", "three_cat")
life_sat_ethnic_interac_mod  <- ethnic_interactions_min_adjust("life_sat", "three_cat")


ethnic_interaction_results <- do.call("rbind", list(did_coef_extract(ghq_ethnic_interac_mod, 5), 
                                                 did_coef_extract(mcs_ethnic_interac_mod, 5),
                                                 did_coef_extract(pcs_ethnic_interac_mod, 5),
                                                 did_coef_extract(life_sat_ethnic_interac_mod, 5)))
View(ethnic_interaction_results)

write_xlsx(ethnic_interaction_results, paste0(storage_path, "rq3_ethnic_interaction_results.xlsx") )


#RQ3c age interactions 

rq3c_ghq12_16_min_agjust <- age_stratified_min_adjust("ghq12", "three_cat", "16")
rq3c_mcs_16_min_agjust <- age_stratified_min_adjust("sf12_mental", "three_cat", "16")
rq3c_pcs_16_min_agjust <- age_stratified_min_adjust("sf12_physical", "three_cat", "16")
rq3c_life_sat_16_min_agjust <- age_stratified_min_adjust("life_sat", "three_cat", "16")

age_16_results <- do.call("rbind", list(did_coef_extract(rq3c_ghq12_16_min_agjust), 
                                               did_coef_extract(rq3c_mcs_16_min_agjust),
                                               did_coef_extract(rq3c_pcs_16_min_agjust),
                                               did_coef_extract(rq3c_life_sat_16_min_agjust)))  |>
  mutate(age = "16")


View(age_16_results)



rq3c_ghq12_17_min_agjust <- age_stratified_min_adjust("ghq12", "three_cat", "17")
rq3c_mcs_17_min_agjust <- age_stratified_min_adjust("sf12_mental", "three_cat", "17")
rq3c_pcs_17_min_agjust <- age_stratified_min_adjust("sf12_physical", "three_cat", "17")
rq3c_life_sat_17_min_agjust <- age_stratified_min_adjust("life_sat", "three_cat", "17")

age_17_results <- do.call("rbind", list(did_coef_extract(rq3c_ghq12_17_min_agjust), 
                                        did_coef_extract(rq3c_mcs_17_min_agjust),
                                        did_coef_extract(rq3c_pcs_17_min_agjust),
                                        did_coef_extract(rq3c_life_sat_17_min_agjust)))  |>
  mutate(age = "17")


rq3_age_stratified_min_adjust <- rbind( age_16_results, age_17_results)

write_xlsx(rq3_age_stratified_min_adjust, paste0(storage_path, "rq3_age_stratified_min_adjust.xlsx") )




ghq_age_interac_mod  <- age_interactions_min_ajust("ghq12", "three_cat")
mcs_age_interac_mod  <- age_interactions_min_ajust("sf12_mental", "three_cat")
pcs_age_interac_mod  <- age_interactions_min_ajust("sf12_physical", "three_cat")
life_sat_age_interac_mod  <- age_interactions_min_ajust("life_sat", "three_cat")


age_interaction_results <- do.call("rbind", list(did_coef_extract(ghq_age_interac_mod, 5), 
                                                 did_coef_extract(mcs_age_interac_mod, 5),
                                                 did_coef_extract(pcs_age_interac_mod, 5),
                                                 did_coef_extract(life_sat_age_interac_mod, 5)))
View(age_interaction_results)

write_xlsx(age_interaction_results, paste0(storage_path, "rq3_age_interaction_results.xlsx") )


#############################################################################################################
####  Sensitivity analysis 1 
### Two sections of code are changed  


## A different data file is imported. 

Stata_data <- read_dta(paste0(file_path, "R_File_Bottom15P_percent17Jan24.dta")) |> 
  mutate(across(c(factor_vars), .fns = as_factor),
         sex= droplevels(sex),
         country = droplevels(country),   
         sf12_mental = unlabelled(sf12_mental),
         sf12_physical  = unlabelled(sf12_physical),
         life_sat  = as.double(life_sat ))

## And different results are then saved for the estimates. 


write_xlsx(rq1a_minadjust_results, paste0(storage_path, "RQ1A_Sensitivity_analysis.xlsx") )



###############################################################################################################
#### Sensitivity analysis 2 


# Again a different data file is read in. 
Stata_data <- read_dta(paste0(file_path, "R_File_top_80_percent24Apr24.dta")) |> 
  mutate(across(c(factor_vars), .fns = as_factor),
         sex= droplevels(sex),
         country = droplevels(country),   
         sf12_mental = unlabelled(sf12_mental),
         sf12_physical  = unlabelled(sf12_physical),
         life_sat  = as.double(life_sat ))

### And different results are then saved for estimates. 
write_xlsx(rq1a_minadjust_results, paste0(storage_path, "RQ1A_Sensitivity_analysis_Top80Percent.xlsx") )

###############################################################################################################
### Sensitivity analysis 3 removing imputed 
# Again a different data file run in 

## Importing data from Stata
Stata_data <- read_dta(paste0(file_path, "Xclude_imputed_income.dta")) |> 
  mutate(across(c(factor_vars), .fns = as_factor),
         sex= droplevels(sex),
         country = droplevels(country),   
         sf12_mental = unlabelled(sf12_mental),
         sf12_physical  = unlabelled(sf12_physical),
         life_sat  = as.double(life_sat ))

### And different results saved as estimats
write_xlsx(rq1a_minadjust_results, paste0(storage_path, "RQ1A_Sensitivity_excclude_imputed.xlsx") )



########################################################################################################
###  Sensitivity analysis 4 original weights. 
### The main change was changing the functions to use different weights



#This code runs a difference in difference model with minnmal levels of adjustment 
min_adjust_model <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = sc_weight_xc_ )
}




full_adjust_model <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month + two_par +  no_workpar+ owner + kids + par_ed + ethnic_bin ")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = sc_weight_xc_ )
}


flexy_model <- function(outcome, period, additional_terms  = " " ){
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex + month", additional_terms)
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = sc_weight_xc_ )
}


sex_stratified_min_adjust <- function(outcome, period, gender = "male"){
  stratified_data <- filter(Stata_data, sex == gender)
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = sc_weight_xc_ )
}

sex_interactions_min_adjust <- function(outcome, period){
  formula <- paste0(outcome," ~ devolved2*", period, "*sex + age_start +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = sc_weight_xc_ )
}



ethnic_stratified_min_adjust <- function(outcome, period, ethnic = "White British"){
  stratified_data <- filter(Stata_data, ethnic_bin == ethnic)
  formula <- paste0(outcome," ~ devolved2*", period, " + age_start + sex +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = sc_weight_xc_ )
}


ethnic_interactions_min_adjust <- function(outcome, period) {
  ethnic_data <- filter(Stata_data, !is.na(ethnic_bin))
  formula <- paste0(outcome," ~ devolved2*", period, "*ethnic_bin + age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = ethnic_data, weights = sc_weight_xc_ )
}


age_stratified_min_adjust <- function(outcome, period, age_input ){
  stratified_data <- filter(Stata_data, age_start == age_input)
  formula <- paste0(outcome," ~ devolved2*", period, "  + sex +  month")
  print(formula)
  lm_robust(as.formula(formula) , data = stratified_data, weights = sc_weight_xc_ )
}


age_interactions_min_ajust <- function(outcome, period) {
  formula <- paste0(outcome," ~ devolved2*", period, "*age_start + sex + month")
  print(formula)
  lm_robust(as.formula(formula) , data = Stata_data, weights = sc_weight_xc_ )
}




### These were then the commands were rerun.  
sense4_gh12_minadj_mod <- min_adjust_model("ghq12", "three_cat")
sense4_sf12mental_minadj_mod <- min_adjust_model("sf12_mental", "three_cat")
sense4_sf12physcial_minadj_mod <- min_adjust_model("sf12_physical", "three_cat")
sense4_life_sat_minadj_mod <- min_adjust_model("life_sat", "three_cat")



#OUtput for main summary results 
sense4_minadjust_results <- do.call("rbind", list(did_coef_extract(sense4_gh12_minadj_mod), 
                                                  did_coef_extract(sense4_sf12mental_minadj_mod),
                                                  did_coef_extract(sense4_sf12physcial_minadj_mod),
                                                  did_coef_extract(sense4_life_sat_minadj_mod)))   
View(sense4_minadjust_results)

write_xlsx(sense4_minadjust_results, paste0(storage_path, "sense4_minadjust_results_main_table.xlsx") )

#######################################################################################################
## Sensitivity analysis 5 recodign GHQ-12 
Stata_data <- Stata_data |>
  mutate(ghq_sqrt = sqrt(ghq12))

RQ1a_gh12sqrt_minadj_mod <- min_adjust_model("ghq_sqrt", "three_cat")

did_coef_extract(RQ1a_gh12sqrt_minadj_mod)



############################################################################
## BHPS parallel trends graph


### Packages to install. 
library(readr)
library(dplyr)
library(ggplot2)



BHPs_data <- read_csv("Manually coded .csv file here")


### importing files 
BHPs_data |>
  ggplot(aes(x = year, y = mean, color = Country))+ 
  geom_point(position = position_dodge(width = 0.4)) + 
  geom_errorbar(aes(ymin = low_ci, ymax = up_ci), position = position_dodge(width = 0.4))+ 
  labs(y = "Mean GHQ12") + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA), ### This command was needed to get rid of the grey around the background. 
        panel.background = element_blank(),
        axis.line = element_line(size = 0.3))






       
