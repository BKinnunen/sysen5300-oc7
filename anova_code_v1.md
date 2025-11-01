#### The ANOVA Test was completed by creating a function which could allow the ANOVA calculation to be done in a loop. This made for quick analyses for each VA clinic care type.

```r
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
ca_data = read_csv("/cloud/project/six_sigma_project/01_ca_wait_time.csv")
tx_data = read_csv("/cloud/project/six_sigma_project/03_tx_wait_time.csv")
#pop_data = read_csv("/cloud/project/six_sigma_project/05_vet_pop_5_counties_each_state.csv")

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

```
#### As the code shows, the ANOVA test was used to compare the wait time variance for each care type between all of the clinics within the same state. The care types with the highest ANOVA F-value are summarized as follows:

``` r
top_ca_new
    term df     sumsq   meansq      f.value       p.value                                       type
  clinic  1  21625.71 21625.71 1.606280e+32 3.921675e-188               PTSD SPECIFIC GROUP PROGRAMS
  clinic  3  75449.10 25149.70 4.947745e+03 1.243860e-100                                     DENTAL
  clinic  4 158439.41 39609.85 3.298596e+03 8.475976e-117 PRIMARY CARE MENTAL HEALTH INTEGRATED CARE
  clinic  4  69495.58 17373.89 2.451140e+03 1.601390e-109                              PAIN MEDICINE
  clinic  4 206744.99 51686.25 2.399918e+03 5.261508e-109                                   PODIATRY

 top_ca_est
    term df      sumsq    meansq  f.value       p.value                              type
  clinic  4 17429.4184 4357.3546 4288.307 3.037284e-123 SUBSTANCE USE DISORDER INDIVIDUAL
  clinic  4  1149.7928  287.4482 2382.739 7.885216e-109                          ONCOLOGY
  clinic  2   876.6333  438.3167 1544.808  2.123663e-57                    WOMEN'S HEALTH
  clinic  4  7289.3579 1822.3395 1465.314  5.577755e-97                     PAIN MEDICINE
  clinic  3  3146.7481 1048.9160 1394.871  6.403309e-76                            DENTAL

top_tx_new
    term df     sumsq   meansq  f.value       p.value          type
  clinic  4 132677.26 33169.31 3176.135 1.251544e-112     AUDIOLOGY
  clinic  3  49738.64 16579.55 2617.159  4.014165e-85      PODIATRY
  clinic  2  26026.79 13013.40 1877.958  3.156758e-58 OPHTHALMOLOGY
  clinic  4  25696.88  6424.22 1474.233  1.552376e-94  PRIMARY CARE
  clinic  3  62019.41 20673.14 1465.229  2.502750e-74     OPTOMETRY
 
top_tx_est
    term df    sumsq    meansq  f.value      p.value           type
  clinic  3 2571.915  857.3050 3323.624 1.360329e-89  OPHTHALMOLOGY
  clinic  3 8037.820 2679.2735 3235.338 4.342547e-89       PODIATRY
  clinic  3 8420.788 2806.9294 2669.172 1.721495e-85      OPTOMETRY
  clinic  4 1713.265  428.3162 1813.988 2.093077e-99   PRIMARY CARE
  clinic  3 4070.940 1356.9800 1689.092 5.775570e-77 WOMEN'S HEALTH
```

#### The ANOVA test was also used to compare wait time variance between states for all care types. The care types with the highest and lowest F-values are summarized as follows:
```r
top_state_new
    term df    sumsq   meansq   f.value      p.value                type
  clinic  1 34266.29 34266.29 227.93999 2.469277e-36            ONCOLOGY
  clinic  1 33966.50 33966.50 137.95312 8.620673e-25         DERMATOLOGY
  clinic  1 56371.09 56371.09 111.72271 2.909038e-19 MENTAL HEALTH GROUP
  clinic  1 45981.32 45981.32  68.39201 1.060809e-14             UROLOGY
  clinic  1 23736.10 23736.10  64.04656 8.572638e-14              OB/GYN

 top_state_est
    term df      sumsq     meansq   f.value      p.value                type
  clinic  1  4990.0350  4990.0350 124.57526 5.678859e-23       OPHTHALMOLOGY
  clinic  1  1857.3465  1857.3465 120.59394 2.007011e-22       ENDOCRINOLOGY
  clinic  1  1155.5400  1155.5400 100.83933 6.183699e-20 MENTAL HEALTH GROUP
  clinic  1 50843.9057 50843.9057  97.45381 2.048114e-19      SLEEP MEDICINE
  clinic  1   357.8431   357.8431  66.83585 1.923672e-14            ONCOLOGY

low_state_new
    term df    sumsq   meansq    f.value   p.value                              type
  clinic  1  36.7160  36.7160 0.05617073 0.8128872                            DENTAL
  clinic  1 185.8982 185.8982 0.05770479 0.8109057 PTSD SPECIFIC INDIVIDUAL PROGRAMS
  clinic  1 141.5850 141.5850 0.33524901 0.5632393 SUBSTANCE USE DISORDER INDIVIDUAL
  clinic  1 402.7342 402.7342 1.34270206 0.2477493                      PRIMARY CARE
  clinic  1 394.4870 394.4870 1.38490367 0.2410154                    WOMEN'S HEALTH

low_state_est
    term df      sumsq     meansq   f.value   p.value                              type
  clinic  1   2.560329   2.560329 0.1368129 0.7118090                           UROLOGY
  clinic  1  25.751747  25.751747 0.6311483 0.4277463          MENTAL HEALTH INDIVIDUAL
  clinic  1  14.992269  14.992269 0.9342595 0.3350272 PTSD SPECIFIC INDIVIDUAL PROGRAMS
  clinic  1  98.640903  98.640903 0.9884871 0.3212676                  PHYSICAL THERAPY
  clinic  1 144.687885 144.687885 1.6449404 0.2010764                            DENTAL
```

