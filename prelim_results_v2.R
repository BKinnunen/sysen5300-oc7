# Version 2 for Preliminary Results - Analysis of Variance (ANOVA)

### SYSEN 5300: Six Sigma Project Preliminary Results
### OC 7 - Six Sigma Warriors
# Brandy Kinnunen, Juan Rodriguez, Michael Fizdale

# Version 2 - Use an ANOVA test to measure variance between different clinics in states
# and between states - for all data points, and for specific care types.

library(ggplot2)
library(readr)
library(tidyverse)
library(broom)

## Read the datasets
ca_data = read_csv("01_ca_wait_time.csv")
tx_data = read_csv("03_tx_wait_time.csv")
pop_data = read_csv("05_vet_pop_5_counties_each_state.csv")

## Run an ANOVA test for all new patient wait times for the clinics in CA

ca_anova_new_clinic <- aov(wait_new ~ clinic, data = ca_data)
summary(ca_anova_new_clinic)

### Results: 
# summary(ca_anova_new_clinic)
# Df  Sum Sq Mean Sq F value Pr(>F)    
# clinic         4  308234   77059   53.87 <2e-16 ***
#  Residuals   2868 4102715    1431                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 226 observations deleted due to missingness

### Note, these results show that the ANOVA works and that there is significant variance between clinics
# for all new patient wait times (of all care types). But considering the variability between care types,
# this does not seem to be helpful. So, I think it is better to do this for each individual care type.


## Utilize a loop to run the ANOVA test for each care type.

######## Start by defining a function to be used for looping ANOVA tests

#' @name anova_loop
#' @title ANOVA function to be used with lapply() to evaluate all care types, individually, between clinics
#' @param data [data frame] is the data frame which will be input to the function
#' @param care [string] is the care type being evaluated between clinics
#' @param wait_type [column header] is the type of wait time data evaluated. Either wait_new or wait_established.

anova_loop = function(data, care, wait_type){
  
  data <- data %>% filter(type == care)
  data <- droplevels(data)

  # Determine the degrees of freedom (i.e. number of clinics that have valid, non-NA data)
  dof = data %>%
    select(clinic, {{wait_type}}) %>%
    na.omit() %>%
    summarize(
      dof = length(unique(clinic)) - 1) %>%
    pull()
  
  # Ensure clinics are factors, so that they can be counted and the if/else check works properly
  data$clinic <- as.factor(data$clinic)
  
  # Check if a column has all NA values or is missing
  if(is.null(data[[wait_type]]) || all(is.na(data[[wait_type]]))){
    return(tibble(term = "clinic", df = NA, sumsq = NA, meansq = NA, f.value = NA, p.value = NA, type = care))
  }
  
  # If the column exists and is not all NA, then proceed with the following:
  # if/else just makes sure there is enough data for the ANOVA, otherwise outputs "NA"
  if (dof >= 1 && sum(!is.na(data[[wait_type]])) > 10){
    model <- aov(reformulate("clinic", response = wait_type), data = data)
    tidy(model) %>%
      filter(term == "clinic") %>%
      rename(f.value = statistic) %>%
      mutate(type = care)
  } else {
    tibble(term = "clinic", df = NA, sumsq = NA, meansq = NA, f.value = NA, p.value = NA, type = care)
  }
}
  
## Run Loop for CA Clinics, New Patients
care_type <- unique(ca_data$type)

ca_anova_sum_new <- lapply(care_type, function(care){
  anova_loop(data= ca_data, care = care, wait_type = "wait_new")}) %>%
  bind_rows() %>%
  as.data.frame()

## Run Loop for CA Clinics, Established Patients
ca_anova_sum_est <- lapply(care_type, function(care){
  anova_loop(data= ca_data, care = care, wait_type = "wait_established")}) %>%
  bind_rows() %>%
  as.data.frame()


## Run Loop for TX Clinics, New Patients
tx_anova_sum_new <- lapply(care_type, function(care){
  anova_loop(data= tx_data, care = care, wait_type = "wait_new")}) %>%
  bind_rows() %>%
  as.data.frame()


## Run Loop for TX Clinics, Established Patients
tx_anova_sum_est <- lapply(care_type, function(care){
  anova_loop(data= tx_data, care = care, wait_type = "wait_established")}) %>%
  bind_rows() %>%
  as.data.frame()

##### Now, lets look at the results and see what we have

top_ca_new <- ca_anova_sum_new %>%
  slice_max(order_by = f.value, n = 5)
top_ca_new

top_ca_est <- ca_anova_sum_est %>%
  slice_max(order_by = f.value, n = 5)
top_ca_est

top_tx_new <- tx_anova_sum_new %>%
  slice_max(order_by = f.value, n = 5)
top_tx_new

top_tx_est <- tx_anova_sum_est %>%
  slice_max(order_by = f.value, n = 5)
top_tx_est


