#### The ANOVA Test was completed by creating a function which could allow the ANOVA calculation to be done in a loop. This made for quick analyses for each VA clinic care type.

### SYSEN 5300: Six Sigma Project
### OC 7 - Six Sigma Warriors
# Brandy Kinnunen, Juan Rodriguez, Michael Fizdale

# ANOVA test - measure variance between different clinics in states
# and between states - for all data points, and for specific care types.

library(ggplot2)
library(readr)
library(tidyverse)
library(broom)

## Read the datasets
ca_data = read_csv("01_ca_wait_time.csv")
tx_data = read_csv("03_tx_wait_time.csv")
pop_data = read_csv("05_vet_pop_5_counties_each_state.csv")

######## Define a function to be used for looping ANOVA tests

#' @name anova_loop
#' @title ANOVA function to be used with lapply() to evaluate all care types, individually, between clinics
#' @param data [data frame] is the data frame which will be input to the function
#' @param care [string] is the care type being evaluated between clinics
#' @param wait_type [column header] is the type of wait time data evaluated. Either wait_new or wait_established.

anova_loop = function(data, care, wait_type){
  
  # Get the data that applies to the specified care type
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
ca_care_type <- unique(ca_data$type)

ca_anova_sum_new <- lapply(ca_care_type, function(care){
  anova_loop(data= ca_data, care = care, wait_type = "wait_new")}) %>%
  bind_rows() %>%
  as.data.frame()

## Run Loop for CA Clinics, Established Patients
ca_anova_sum_est <- lapply(ca_care_type, function(care){
  anova_loop(data= ca_data, care = care, wait_type = "wait_established")}) %>%
  bind_rows() %>%
  as.data.frame()


## Run Loop for TX Clinics, New Patients
tx_care_type <- unique(tx_data$type)

tx_anova_sum_new <- lapply(tx_care_type, function(care){
  anova_loop(data= tx_data, care = care, wait_type = "wait_new")}) %>%
  bind_rows() %>%
  as.data.frame()


## Run Loop for TX Clinics, Established Patients
tx_anova_sum_est <- lapply(tx_care_type, function(care){
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

##### Interstate variance comparisons

interstate <- bind_rows(
  ca_data %>% mutate(clinic = "ca"),
  tx_data %>% mutate(clinic = "tx"))

care_type = unique(interstate$type)

## Run Loop for Interstate Data, New Patients
state_anova_sum_new <- lapply(care_type, function(care){
  anova_loop(data= interstate, care = care, wait_type = "wait_new")}) %>%
  bind_rows() %>%
  as.data.frame()

## Run Loop for Interstate Data, Established Patients
state_anova_sum_est <- lapply(care_type, function(care){
  anova_loop(data= interstate, care = care, wait_type = "wait_established")}) %>%
  bind_rows() %>%
  as.data.frame()

### Results:

#Five Care Types with Highest Variance:
top_state_new <- state_anova_sum_new %>%
  slice_max(order_by = f.value, n = 5)
top_state_new

top_state_est <- state_anova_sum_est %>%
  slice_max(order_by = f.value, n = 5)
top_state_est

# Five Care Types with Lowest Variance:
low_state_new <- state_anova_sum_new %>%
  slice_min(order_by = f.value, n = 5)
low_state_new

low_state_est <- state_anova_sum_est %>%
  slice_min(order_by = f.value, n = 5)
low_state_est


