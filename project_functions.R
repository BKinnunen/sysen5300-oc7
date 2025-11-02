## R Functions for Evaluating VA Wait Time Data
### Associated Datasets: 01_ca_wait_time.csv ,   03_tx_wait_time.csv
### Code file name is: "project_functions.R"
### Saved in a folder called: "six_sigma_project"
# ### Use filepath to call functions: 
# 
# source("project_functions.R")

### Functions
#### Index (list in order of function definitions in code):
# 
# calc_group_stat()   ### Calculate Group Statistics for Wait Time Data
# calc_tot_stat()     ### Calculate Total Statistics for Wait Time Data
# data_plotter()      ### Scatter Plot of Wait Time data over Observation Dates
# anova_loop()        ### ANOVA function to be used with lapply() to evaluate all care types, individually, between clinics
# fail_data()         ### Create a data frame that captures the number and severity of failures for each care type
# fail_summary()      ### Create a data frame that summarizes the failure statistics for each care type
# fail_plot_single()  ### Create a single bar plot that visually summarizes the failure statistics for each care type
# fail_plot_double()  ### Create a side-by-side bar plot that visually compares failure statistics for two states
# cost_calc ()        ### Calculate the total cost of failures
# bootstrap_fn()      ### Bootstrap the given data to create a single bootstrapped data frame
# boot_cost()         ### Analyze the bootstrapped data, calculating cost, sample distributions, and CI's
# boot_overall_distr() ### Plot the bootstrapped cost data in a histogram to visualize overall sample distribution
# boot_type_distr()   ### Plot the bootstrapped data in a histogram to visualize care type distribution
# 
# 
# ### Code:

# #' @name project_functions.R
#' @title Functions for the Six Sigma Project
#' @description 
#' Below are the functions that were used within the six sigma project code, ranging from 
#' ANOVA analyses to SPC and bar plots.

library(dplyr)      # for data wrangling
library(readr)      # for reading in data
library(ggplot2)    # for visualization
library(tidyverse)  # for processing/cleaning data
#install.packages("patchwork")
#install.packages("gtable")
library(patchwork)  # for combining plots
library(viridis)    # for changing color palettes
library(glue)       # for calling and printing user inputs


################################################################################

#' @name calc_group_stat
#' @title Calculate Group Statistics for Wait Time Data
#' @param data [data frame] is the data frame which will be input to the function
#' @param x [numeric] vector of subgroup values (usually time) that data frame will be grouped by.
#' @param filter_col [string] is the column name within which the filtering will occur (e.g. "type")
#' @param filter_val [string] is the value by which the filtering is desired (e.g. "AUDIOLOGY")

calc_group_stat = function(data, x, filter_col, filter_val){
  
  x <- enquo(x)
  filter_col <- enquo(filter_col)
  
  # Calculate the initial statistics table
  stats_i <- data %>%
    filter( !!filter_col == filter_val) %>%
    group_by(!!x) %>%
    mutate(
      # Calculate mean wait time for new patients
      x_bar_new = mean(wait_new),
      # Calculate std deviation for new patient wait time
      sd_new = sd(wait_new), 
      # Calculate range for new patient wait time
      range_new = max(wait_new) - min(wait_new),
      # Calculate type size (i.e. number of samples)
      group_size_new = n(),
      
      # Calculate mean wait time for established patients
      x_bar_est = mean(wait_established),
      # Calculate std deviation for established patient wait time
      sd_est = sd(wait_established), 
      # Calculate range for established patient wait time
      range_est = max(wait_established) - min(wait_established),
      # Calculate type size (i.e. number of samples)
      group_size_est = n()
    )
  
  # Put together the final total statistics
  output <- stats_i %>%
    ungroup() %>%
    mutate(
      ## Add columns for the new patient data
      # Calculate the mean of the mean
      x_bbar_new = mean(x_bar_new),
      # Calculate the mean of the ranges
      range_bar_new = mean(range_new),
      # Calculate the mean of the std deviation
      sd_bar_new = mean(sd_new),
      # Calculate sigma short and sigma total for the sample
      sigma_s_new = sqrt(mean(sd_new^2)),
      sigma_t_new = sd(data %>% filter(!!filter_col == filter_val) %>% pull(wait_new), na.rm = TRUE),
      
      # Calculate standard error for each clinic
      se_new = sigma_s_new/sqrt(group_size_new),
      
      upper_cl_new = x_bbar_new + 3*se_new,
      lower_cl_new = pmax(x_bbar_new - 3*se_new, 0),
      
      ## Add columns for the established patient data
      # Calculate the mean of the mean
      x_bbar_est = mean(x_bar_est),
      # Calculate the mean of the ranges
      range_bar_est = mean(range_est),
      # Calculate the mean of the std deviation
      sd_bar_est = mean(sd_est),
      # Calculate sigma short and sigma total for the sample
      sigma_s_est = sqrt(mean(sd_est^2)),
      sigma_t_est = sd(data %>% filter(!!filter_col == filter_val) %>% pull(wait_established), na.rm = TRUE),
      
      # Calculate standard error for each clinic
      se_est = sigma_s_est/sqrt(group_size_est),
      
      upper_cl_est = x_bbar_est + 3*se_est,
      lower_cl_est = pmax(x_bbar_est - 3*se_est, 0) 
    )
  
  return(output)
}

################################################################################
################################################################################

#' @name calc_tot_stat
#' @title Calculate Total Statistics for Wait Time Data
#' @param data [data frame] is the data frame which will be input to the function
#' @param x [numeric] vector of subgroup values (usually time) that data frame will be grouped by.
#' @param filter_col [string] is the column name within which the filtering will occur (e.g. "type")
#' @param filter_val [string] is the value by which the filtering is desired (e.g. "AUDIOLOGY")

calc_tot_stat = function(data, x, filter_col, filter_val){
  
  data <- data %>%
    filter( {{filter_col}} == filter_val)
  
  # Get total standard deviation
  sigma_t_new = sd(data$wait_new)
  sigma_t_est = sd(data$wait_established)
  
  # Get statistics for each subgroup
  stat_s = data %>%
    # for each subgroup x (e.g. observed sample date)
    group_by( {{ x }}) %>%
    # Calculate...
    summarize(
      # Within group mean wait time for new patients
      x_bar_new = mean(wait_new),
      # Within group  std deviation for new patient wait time
      sd_new = sd(wait_new), 
      # Within group range for new patient wait time
      range_new = max(wait_new) - min(wait_new),
      # Within group  type size (i.e. number of samples)
      group_size_new = n(),
      # Within group degrees of freedom
      df_new = group_size_new -1,
      
      # Within group  mean wait time for established patients
      x_bar_est = mean(wait_established),
      # Within group  std deviation for established patient wait time
      sd_est = sd(wait_established), 
      # Within group  range for established patient wait time
      range_est = max(wait_established) - min(wait_established),
      # Within group type size (i.e. number of samples)
      group_size_est = n(),
      # Within group degrees of freedom
      df_est = group_size_est -1) %>%
    ungroup()
  
  # Now calculate one row of total statistics
  output = stat_s %>%
    summarize(
      # Type of care
      type = filter_val,
      # average average
      xbbar_new = mean(x_bar_new),
      xbbar_est = mean(x_bar_est),
      # average range
      rbar_new = mean(range_new),
      rbar_est = mean(range_est),
      # average standard deviation
      sbar_new = mean(sd_new),
      sbar_est = mean(sd_est),
      # average within-group standard deviation
      sigma_s_new = sqrt(sum(sd_new^2) / sum(group_size_new)),
      sigma_s_est = sqrt(sum(sd_est^2) / sum(group_size_est)),
      # overall standard deviation
      sigma_t_new = sigma_t_new,
      sigma_t_est = sigma_t_est,
      # total sample size
      n_new = sum(group_size_new),
      n_est = sum(group_size_est)
    )
  
  return(output)
  
}


################################################################################
################################################################################

#' @name data_plotter
#' @title Scatter Plot of Wait Time data over Observation Dates
#' @param data [data frame] is the data to be plotted
#' @param x [numeric] vector of subgroup values that will be on the x-axis (e.g. date).
#' @param y [numeric] vector of metric values that will be plotted on the y-axis (e.g. wait_new).
#' @param lim [numeric] wait time limit value in days (e.g. 20) 
#' @param care_type [string] is the type of care being evaluated (e.g "AUDIOLOGY")
#' @param state [string] is the state for which the clinic data belongs (e.g. "California")
#' @param patient_type [string] is either "New" or "Established"

data_plotter = function(data, x, y, lim, care_type, state, patient_type){
  
  data$date <- as.Date(data$date, format = "%m/%d/%Y")
  data <- data %>% arrange(date)
  
  data <- data %>% filter(type == care_type)
  
  output <-ggplot(data = data, mapping = aes(x = {{ x }}, y = {{ y }}))+
    geom_point(aes(color = clinic), size = 2, alpha = 0.75)+
    geom_hline(mapping = aes(yintercept = lim), color = "magenta4", linewidth = 0.5, linetype = "dashed") +
    scale_color_viridis_d(option = "turbo") +
    labs(title = glue("{care_type} Appointment Wait Time Data for {patient_type} Patients at Five {state} VA Clinics (2025)"),
         subtitle = glue("Dashed line represents { lim } day Wait Time Limit"),
         x = "Date",
         y = "Wait Time (days)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_date(date_labels = "%b %d", date_breaks = "1 day")
  
  return(output)
}

################################################################################
################################################################################

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

################################################################################
################################################################################

#' @name fail_data
#' @title Create a data frame that captures the number and severity of failures for each care type
#' @param data [data frame] is the data frame which will be input to the function
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days

fail_data = function(data, pri_lim = 20, spec_lim = 28){
  
  #PRESET: pri_lim = 20 Days (Primary Care Wait Time Limit)
  #PRESET: spec_lim = 28 Days (Specialty Care Wait Time Limit)
  
  fail_vec <- data %>%
    select(type, wait_new, wait_established) %>%
    mutate(
      # Define limit based on type of care
      lim = case_when(type == "PRIMARY CARE" ~ pri_lim,
                      TRUE ~ spec_lim),
      fail_new = wait_new > lim,
      severity_fail_new = pmax(wait_new - lim, 0),
      fail_est = wait_established > lim,
      severity_fail_est = pmax(wait_established - lim, 0)
    )
  
  return(fail_vec)
}


################################################################################
################################################################################

#' @name fail_summary
#' @title Create a data frame that summarizes the failure statistics for each care type
#' @param data [data frame] is the data frame which will be input to the function
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days
#' @note: Dependency: fail_data()

fail_summary = function(data, pri_lim = 20, spec_lim = 28){
  
  #PRESET: pri_lim = 20 Days (Primary Care Wait Time Limit)
  #PRESET: spec_lim = 28 Days (Specialty Care Wait Time Limit)
  
  fail_vec <- fail_data(data, pri_lim = pri_lim, spec_lim = spec_lim)
  
  summary <- fail_vec %>%
    group_by(type) %>%
    summarize(
      # Create column for limit in days
      lim = unique(lim)[1],
      # Create column for number total data points, number violations, percentage, and severity for new patients
      n_new = sum(!is.na(wait_new)),
      n_fail_new = sum(fail_new, na.rm = TRUE),
      percent_fail_new = (n_fail_new/n_new),
      avg_severity_new = mean(severity_fail_new[fail_new], na.rm = TRUE),
      
      # Create column for number total data points, number violations, percentage, and severity for established patients
      n_est = sum(!is.na(wait_established)),
      n_fail_est = sum(fail_est, na.rm = TRUE), 
      percent_fail_est = (n_fail_est / n_est),
      avg_severity_est = mean(severity_fail_est[fail_est], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary)
}

################################################################################
################################################################################

#' @name fail_plot_single
#' @title Create a single bar plot that visually summarizes the failure statistics for each care type
#' @param data [data frame] is the data frame which will be input to the function
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days
#' @param wait_type [column header] is the type of wait time data evaluated. Either wait_new or wait_established
#' @param state [string] is the two letter state code. Example: "TX", "CA"
#' @note: Dependencies: fail_data(), fail_summary()

fail_plot_single = function(data, wait_type, state, pri_lim = 20, spec_lim = 28){
  
  #PRESET: pri_lim = 20 Days (Primary Care Wait Time Limit)
  #PRESET: spec_lim = 28 Days (Specialty Care Wait Time Limit)
  
  # Check to see if the failure summary has already been completed.
  # If it has not been (i.e. raw data is input), calculate w/our fail functions
  
  if (all(c("percent_fail_new", "percent_fail_est") %in% names(data))){
    data_plot = data
  } else {
    fail_vec <- fail_data(data = data, pri_lim = pri_lim, spec_lim = spec_lim)
    data_plot <- fail_summary(data = fail_vec, pri_lim = pri_lim, spec_lim = spec_lim )
  }
  
  # Select desired column (percent fail) based on wait type
  if (wait_type == "wait_new"){
    data_plot <- data_plot %>% 
      mutate(perc_fail = percent_fail_new,
             perc_fail = replace_na(perc_fail, 0),
             hjust_val = ifelse(perc_fail == 0, 0, 1.25))
    patient = "New"}
  else{
    data_plot <- data_plot %>% 
      mutate(perc_fail = percent_fail_est,
             perc_fail = replace_na(perc_fail, 0),
             hjust_val = ifelse(perc_fail == 0, 0, 1.25))
    patient = "Established"
  }
  
  plot1 <- ggplot(data = data_plot, mapping = aes(x = type, y = perc_fail, fill = type))+
    geom_col() +
    coord_flip()+
    theme(legend.position = "none")+
    scale_fill_viridis_d() +
    labs(title = glue("(%) Wait Time Limits Violated, {patient} Patients ( {state} ) "),
         x = "Care Type",
         y = glue("Percentage of Violations ({patient} Patients)"))+
    geom_label(aes(label = scales::percent(perc_fail, accuracy = 1), hjust = hjust_val), fill = "beige")
  
  return(plot1)
}

################################################################################
################################################################################

#' @name fail_plot_double
#' @title Create a side-by-side bar plot that visually compares failure statistics for two states
#' @param data1 [data frame] is the first data frame which will be input to the function
#' @param data2 [data frame] is the second data frame which will be input to the function
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days
#' @param wait_type [column header] is the type of wait time data evaluated. Either wait_new or wait_established
#' @param state1 [string] is the two letter state code. Example: "TX", "CA"
#' @param state2 [string] is the two letter state code. Example: "TX", "CA"
#' @note: Dependencies: fail_data(), fail_summary(), fail_plot_single()

fail_plot_double = function(data1, data2, wait_type, state1, state2, pri_lim = 20, spec_lim = 28){
  
  # Create individual plots for each state
  p1 = fail_plot_single(data = data1, wait_type = wait_type, state = state1, pri_lim = pri_lim, spec_lim = spec_lim)
  p2 = fail_plot_single(data = data2, wait_type = wait_type, state = state2, pri_lim = pri_lim, spec_lim = spec_lim)
  
  # Use patchwork library to bind the two plots together
  double_plot = p1 + p2 + plot_layout(axes = "collect")
  
  return(double_plot)
  
}

################################################################################
################################################################################

#' @name cost_calc
#' @title Calculate the total cost of failures
#' @param data [data frame] is the first data frame which will be input to the function
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days
#' @param cost [value] is the cost ($) per failure, which is each day over the wait limit
#' @note: Dependency: fail_data()

cost_calc = function(data, cost, pri_lim = 20, spec_lim = 28){
  
  #PRESET: pri_lim = 20 Days (Primary Care Wait Time Limit)
  #PRESET: spec_lim = 28 Days (Specialty Care Wait Time Limit)
  
  # Check to see if the failure data has already been computed.
  # If it has not been (i.e. raw data is input), calculate w/our fail functions
  
  if (all(c("fail_new", "fail_est") %in% names(data))){
    fail_vec = data
  } else {
    fail_vec <- fail_data(data = data, pri_lim = pri_lim, spec_lim = spec_lim)
  }
  
  cost_data <- fail_vec %>%
    mutate(
      # Calculate total cost for each day which was violated
      cost_new = cost*severity_fail_new,
      cost_est = cost*severity_fail_est)%>%
    group_by(type) %>%
    summarize(avg_cost_new = mean(cost_new, na.rm = TRUE),
              total_cost_new = sum(cost_new, na.rm = TRUE),
              avg_cost_est = mean(cost_est, na.rm = TRUE),
              total_cost_est = sum(cost_est, na.rm = TRUE)) %>%
    mutate(grand_total_new = sum(total_cost_new),
           grand_total_est = sum(total_cost_est))
  
  return(cost_data)
  
}

################################################################################
################################################################################

#' @name bootstrap_fn
#' @title Bootstrap the given data to create a single bootstrapped data frame
#' @param data [data frame] is the first data frame which will be input to the function
#' @param boot_reps [value] is the number of bootstrap repetitions desired

bootstrap_fn = function(data, boot_reps){
  
  # Separate wait types, i.e. for new and established patients, so that each
  # can be bootstrapped by itself and recombined into a single data frame later.
  
  new_data <- data %>% select(type, wait_new)
  est_data <- data %>% select(type, wait_established)
  
  # Bootstrap the new patient data
  boot_new = tibble(rep = 1:boot_reps) %>%
    group_by(rep) %>%
    reframe(new_data) %>%
    group_by(rep, type) %>%
    sample_n(size = n(), replace = TRUE) %>%
    ungroup()
  
  # Rearrange boot_new so that it is prepared for joining
  boot_new <- boot_new %>%
    # Arrange to ensure the new and established columns are in same order
    arrange(rep, type) %>%
    group_by(rep, type) %>%
    # Add a row_id to help ensure dataframe joining is done row-by-row
    mutate(row_id = row_number()) %>%
    ungroup()
  
  # Bootstrap the established patient data
  boot_est = tibble(rep = 1:boot_reps) %>%
    group_by(rep) %>%
    reframe(est_data) %>%
    group_by(rep, type) %>%
    sample_n(size = n(), replace = TRUE) %>%
    ungroup()
  
  # Rearrange boot_est so that it is prepared for joining
  boot_est <- boot_est %>%
    # Arrange to ensure the new and established columns are in same order
    arrange(rep, type) %>%
    group_by(rep, type) %>%
    # Add a row_id to help ensure data frame joining is done row-by-row
    mutate(row_id = row_number()) %>%
    ungroup()
  
  # Join the new and established bootstrapped data frames into one for further analysis
  boot <- full_join(
    boot_new %>% select(rep, type, row_id, wait_new),
    boot_est %>% select(rep, type, row_id, wait_established),
    by = c("rep", "type", "row_id")
  ) %>%
    select(-row_id)
  
  return(boot)
}

################################################################################
################################################################################

#' @name boot_cost
#' @title Analyze the bootstrapped data, calculating cost, sample distributions, and CI's
#' @param boot_data [data frame] is the bootstrapped data frame to be analyzed
#' @param pri_lim [constant] Primary Care Wait Time Limit (days), preset = 20 days
#' @param spec_lim [constant] Specialty Care Wait Time Limit (days), preset = 28 days
#' @param cost [value] is the cost ($) per failure, which is each day over the wait limit
#' @note: Dependencies: data already bootstrapped; cost_calc()

boot_cost = function(boot_data, cost, pri_lim=20, spec_lim=28){
  
   n_rep <- sort(unique(boot_data$rep))
   results <- list(length = length(n_rep))
  
   for (i in seq_along(n_rep)){
     # Filter boot data by rep, so that computations are made for each set of rep samples
     boot_i <- boot_data %>% filter(rep == i)
  
     # Cost computation for rep i samples
     results_i <- cost_calc(data = boot_i, cost = cost, pri_lim = pri_lim, spec_lim = spec_lim) %>%
       # Add a column to keep track of which rep these results correspond to
       mutate(rep = i)
     # Add the results to the list
     results[[i]] <- results_i
   }
   # Bind the rows for the results to create a data frame
   results_df <- bind_rows(results)

  # Use results data frame to determine cost statistics (mean, sd, confidence intervals)
  # for each care type and overall

  # Use the total costs for each care type to calculate statistics
  type_stats <- results_df %>%
    group_by(type) %>%
    summarize(
      # Cost stats for new patient wait times
      new_0.5 = quantile(total_cost_new, prob = 0.5),
      new_se = sd(total_cost_new),
      new_lower = quantile(total_cost_new, prob = 0.025),
      new_upper = quantile(total_cost_new, prob = 0.975),
      
      # Cost stats for established patient wait times
      est_0.5 = quantile(total_cost_est, prob = 0.5),
      est_se = sd(total_cost_est),
      est_lower = quantile(total_cost_est, prob = 0.025),
      est_upper = quantile(total_cost_est, prob = 0.975),
      .groups = "drop"
    )
  
  # Use grand total costs for each rep to calculate the distribution statistics  
  total_stats <- results_df %>%
    summarize(
      # Cost stats for new patient wait times
      new_0.5 = quantile(grand_total_new, prob = 0.5),
      new_se = sd(grand_total_new),
      new_lower = quantile(grand_total_new, prob = 0.025),
      new_upper = quantile(grand_total_new, prob = 0.975),
      
      # Cost stats for established patient wait times
      est_0.5 = quantile(grand_total_est, prob = 0.5),
      est_se = sd(grand_total_est),
      est_lower = quantile(grand_total_est, prob = 0.025),
      est_upper = quantile(grand_total_est, prob = 0.975),
      .groups = "drop"
    )
  
  ## Return data as list so users have access to all data frames/summaries computed
  return(list(
    raw = results_df, 
    by_type = type_stats, 
    overall = total_stats))
  
}

################################################################################
################################################################################

#' @name boot_overall_distr
#' @title Plot the bootstrapped cost data in a histogram to visualize overall sample distribution
#' @param boot_results [data frame] are the bootstrapped cost results
#' @param state [string] is the two letter state code. Example: "TX", "CA"
#' @note: Dependencies: data already analyzed by boot_cost()

boot_overall_distr = function(boot_results, state){
  
  results_df <- boot_results$raw 
  
  total_stats <- boot_results$overall
  
  ## Create a histogram plot for the overall cost data for both the new and 
  # established patient data.
  
  ## Plot for new patient data
  overall_new <- ggplot(data = results_df, mapping = aes(x = grand_total_new))+
    geom_histogram(fill = "cyan4")+
    
    # Plot confidence intervals
    geom_vline(data = total_stats, mapping = aes(xintercept = new_lower), color = "orange3", linetype = "dashed", linewidth = 1)+
    geom_vline(data = total_stats, mapping = aes(xintercept = new_upper), color = "orange3", linetype = "dashed", linewidth = 1)+
    
    # Add labels
    labs(title = "Bootstrapped Sample Distribution for Cost of Wait Time Limit Violations",
         subtitle = glue("New patients ({state})"),
         x = "Total Cost of Violations ($)",
         y = "Frequency")
  
  ## Plot for established patient data
  overall_est <- ggplot(data = results_df, mapping = aes(x = grand_total_est))+
    geom_histogram(fill = "darkolivegreen")+
    
    # Plot confidence intervals
    geom_vline(data = total_stats, mapping = aes(xintercept = est_lower), color = "orange3", linetype = "dashed", linewidth = 1)+
    geom_vline(data = total_stats, mapping = aes(xintercept = est_upper), color = "orange3", linetype = "dashed", linewidth = 1)+
    
    # Add labels
    labs(title = "Bootstrapped Sample Distribution for Cost of Wait Time Limit Violations",
         subtitle = glue("Established patients ({state})"),
         x = "Total Cost of Violations ($)",
         y = "Frequency")
  
  ## Combination of plots for output
  overall_plots <- overall_new + overall_est
  
  return(overall_plots)
  
}


################################################################################
################################################################################

#' @name boot_type_distr
#' @title Plot the bootstrapped data in a histogram to visualize care type distribution
#' @param boot_results [data frame] are the bootstrapped cost results
#' @param care_type [string] is the type of care being evaluated (e.g "AUDIOLOGY")
#' @param state [string] is the two letter state code. Example: "TX", "CA"
#' @note: Dependencies: data already analyzed by boot_cost()

boot_type_distr = function(boot_results, state, care_type){

  results_df <- boot_results$raw %>%
    filter(type == care_type)
  
  type_stats <- boot_results$by_type %>%
    filter(type == care_type)
  
  ## Create a histogram plot for the overall cost data for both the new and 
  # established patient data.
  
  ## Plot for new patient data
  plot_new <- ggplot(data = results_df, mapping = aes(x = total_cost_new))+
    geom_histogram(fill = "cyan4")+
    
    # Plot confidence intervals
    geom_vline(data = type_stats, mapping = aes(xintercept = new_lower), color = "orange3", linetype = "dashed", linewidth = 1)+
    geom_vline(data = type_stats, mapping = aes(xintercept = new_upper), color = "orange3", linetype = "dashed", linewidth = 1)+
    
    # Add labels
    labs(title = "Bootstrapped Sample Distribution for Cost of Wait Time Limit Violations",
         subtitle = glue("{care_type}, New patients ({state})"),
         x = "Total Cost of Violations ($)",
         y = "Frequency")
  
  ## Plot for established patient data
  plot_est <- ggplot(data = results_df, mapping = aes(x = total_cost_est))+
    geom_histogram(fill = "darkolivegreen")+
    
    # Plot confidence intervals
    geom_vline(data = type_stats, mapping = aes(xintercept = est_lower), color = "orange3", linetype = "dashed", linewidth = 1)+
    geom_vline(data = type_stats, mapping = aes(xintercept = est_upper), color = "orange3", linetype = "dashed", linewidth = 1)+
    
    # Add labels
    labs(title = "Bootstrapped Sample Distribution for Cost of Wait Time Limit Violations",
         subtitle = glue("{care_type}, Established patients ({state})"),
         x = "Total Cost of Violations ($)",
         y = "Frequency")
  
  ## Combination of plots for output
  comb_plot <- plot_new + plot_est
  
  return(comb_plot)
  
}


