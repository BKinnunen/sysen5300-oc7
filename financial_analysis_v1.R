### Started the code, but definitely not done yet...

### SYSEN 5300: Six Sigma Project - Financial Analysis
### OC 7 - Six Sigma Warriors
# Brandy Kinnunen, Juan Rodriguez, Michael Fizdale

### Financial Analysis - Utilize the defined project functions to conduct analysis on cost per failure

## (Need SPC functions too??)

library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
#install.packages("patchwork")
#install.packages("gtable")
library(patchwork)
library(viridis)
library(glue)

source("project_functions.R")

## Read the datasets
ca_data = read_csv("01_ca_wait_time.csv")
tx_data = read_csv("03_tx_wait_time.csv")
pop_data = read_csv("05_vet_pop_5_counties_each_state.csv")

## Define Cost per failure, where 1 failure is each day that the wait time
# specification limits are violated.

cost = 50  # PLACEHOLDER VALUE --- NEED TO GET AN ACTUAL EST. VALUE!!!

## next steps... utilize functions to continue w/cost analysis....

fail_dat <- fail_data(tx_data)
cost_calc(fail_dat, cost)
tx_boot <- bootstrap_fn(tx_data, 1000)
boot_cost(tx_boot,50)
