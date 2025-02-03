###### This is an edit of the main analyses R code to add an addtional sensitiivyt analysis. Started on 11 Dec 2024
### Written by richard.shaw@glasgow.ac.uk 


### Initial project proposal was pre-registered and is available from  https://osf.io/u3avm




### File pathways 

file_path = ###insert file path here
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



########################### Functions 





#This code runs a difference in difference model with minimal levels of adjustment 
min_adjust_model <- function(outcome, period){
  formula <- paste0(outcome," ~ EMA*", period, " + sex + month")
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




###############################
### Importing data files 


#Vector indicate factor variables so that the right labelled variables are converted to factors 


review_factor_vars <- c( "sex", "country", "detailed", "three_cat",  "month", "EMA")


## Importing data from Stata
# NB File path may need changing 
Stata_data <- read_dta(paste0(file_path, "ReviewerAnalysis.dta")) |> 
  mutate(across(c(review_factor_vars), .fns = as_factor),
       sex= droplevels(sex),
       country = droplevels(country),   
       happiness  = as.double(happiness ))

#Some missing data in financial strain which need recoding. 


### Preliminary svyset 
data_design <- svydesign(data = Stata_data, weights = ~acad_weight_ , id = ~1)



### Coding survey data in r 
##Outcome 
# svymean( x =  ~ghq12, design = data_design, na.rm = TRUE) 
happiness_mean <- svyby(formula = ~happiness , by = ~EMA + three_cat ,
      design = data_design, 
      FUN = svymean, na.rm = TRUE ,
      row.names = FALSE)

## Calculating Standard deviation data in R. 
#svyvar(x =  ~ghq12, design = data_design, na.rm = TRUE)
happiness_SD <- svyby(formula = ~happiness , by = ~EMA + three_cat ,
      design = data_design, 
      FUN = svyvar, na.rm = TRUE ,
      row.names = FALSE) |>
  mutate(SD = sqrt(happiness))|>
  select(EMA, three_cat, SD)

happiness_MeanSD <- happiness_mean |>
  left_join(happiness_SD) |>
  rename(happiness_SE = se,
         happiness_SD = SD)

write_xlsx(happiness_MeanSD,paste0(file_path, "happiness_MeanSD.xlsx") )



### Subsetting the categorical variables for presentation 
descriptive_vars <- Stata_data %>%
  select(all_of(review_factor_vars)) %>% 
  select(-month, - detailed, )

glimpse(Stata_data)  

#Subsetting all categorical variables for selection
descriptive_vars %>%
  tableby(three_cat ~., data = ., test = FALSE) |>
  summary()

names(Stata_data)      
##############################################################
##### Sensitivity analysis does: Does the mental difference between 13 and 14 year olds versus 16 and 17 year olds in England change druring  periods prior to the abolition of EMA, transitionary EMA period, post EMA Period? 

### Minimally adjusted models 
## Generate models 
PRE_v_EMA <- min_adjust_model("happiness", "three_cat")
tidy(PRE_v_EMA)
# Output for supplement (All coefficients)
#Note need to convert to function and convert name. 
PRE_v_EMA_sup <- tidy(PRE_v_EMA, conf.int = TRUE)|>
  mutate(p_value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, outcome, p_value) |>
  rename_with( ~ paste0("prefix_", .x), .cols = c(estimate, conf.low, conf.high, p_value) )

View(PRE_v_EMA_sup)


write_xlsx(PRE_v_EMA_sup, paste0(file_path, "PRE_v_EMA_sup.xlsx") )




View(RQ1a_ghq12_supplement_output)



#OUtput for main summary results 
rq1a_minadjust_results <- do.call("rbind", list(did_coef_extract(RQ1a_gh12_minadj_mod), 
                      did_coef_extract(RQ1a_sf12mental_minadj_mod),
                      did_coef_extract(RQ1a_sf12physcial_minadj_mod),
                      did_coef_extract(RQ1a_life_sat_minadj_mod)))   
View(rq1a_minadjust_results)

write_xlsx(rq1a_minadjust_results, paste0(storage_path, "RQ1a_min_adjust_main_table.xlsx") )


rq1a_minadjust_results




       
