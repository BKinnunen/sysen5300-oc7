```r
 
### SYSEN 5300: Six Sigma Project Preliminary Results
### OC 7 - Six Sigma Warriors
# Brandy Kinnunen, Juan Rodriguez, Michael Fizdale

# This is a version 1 code to help me wrap my mind around the data and see
# where we might be able to go from here... So, if it seems all over the place,
# that's because it is. Just trying different things to see what makes the most
# sense and see what is possible.

library(ggplot2)
library(readr)
library(tidyverse)
#install.packages("patchwork")
#install.packages("gtable")
library(patchwork)
library(viridis)
library(glue)

## Read the datasets
ca_data = read_csv("/cloud/project/six_sigma_project/01_ca_wait_time.csv")
tx_data = read_csv("/cloud/project/six_sigma_project/03_tx_wait_time.csv")
pop_data = read_csv("/cloud/project/six_sigma_project/05_vet_pop_5_counties_each_state.csv")

### NOTE: "NA" values do exist in the datasets. So, whenever we go to run statistics calcs,
# we'll need to include "na.rm = TRUE" to ensure those cell values are passed over. 
# DO NOT use "na.omit()"! This will remove any row that contains an NA value. So, using this
# would cause us to remove other valuable data.

## Create summary tables to get a rough overview of the mean, sd, min, max statistics
ca_summary <- ca_data %>% 
  group_by(type) %>% 
  select(type, wait_new, wait_established) %>%
  summarize(
    u_new = mean(wait_new, na.rm = TRUE),
    sd_new = sd(wait_new, na.rm = TRUE),
    max_new = max(wait_new, na.rm = TRUE),
    min_new = min(wait_new, na.rm = TRUE),
    
    u_est = mean(wait_established, na.rm = TRUE),
    sd_est = sd(wait_established, na.rm = TRUE),
    max_est = max(wait_established, na.rm = TRUE),
    min_est = min(wait_established, na.rm = TRUE)
  )

tx_summary <- tx_data %>% 
  group_by(type) %>% 
  select(type, wait_new, wait_established) %>%
  summarize(
    u_new = mean(wait_new, na.rm = TRUE),
    sd_new = sd(wait_new, na.rm = TRUE),
    max_new = max(wait_new, na.rm = TRUE),
    min_new = min(wait_new, na.rm = TRUE),
    
    u_est = mean(wait_established, na.rm = TRUE),
    sd_est = sd(wait_established, na.rm = TRUE),
    max_est = max(wait_established),
    min_est = min(wait_established, na.rm = TRUE)
  )

###### Box Plots to Visualize Data
## CA new patients
ca_box_new <- ggplot(data = ca_data, mapping = aes(x=type, y=wait_new, fill = type))+
  geom_boxplot() +  
  geom_jitter(width= 0.2, alpha = 0.5)+
  
  # Plot Primary Care 20 day limit
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue", linewidth = 1)+
  # Plot Specialty Care 28 day limit
  geom_hline(yintercept = 28, linetype = "dotted", color = "red", linewidth = 1)+
  
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Box plot for New Patient Wait Times (Days) in Five California VA Clinics",
       subtitle = "Blue Dashed Line = 20 Day Primary Care Limit,
Red Dotted Line = 28 Day Specialty Care Limit",
       y = "Wait Time (Days)",
       x = "Care Type"
       )
ca_box_new

## CA established patients
ca_box_est <- ggplot(data = ca_data, mapping = aes(x=type, y=wait_established, fill = type))+
  geom_boxplot() +  
  geom_jitter(width= 0.2, alpha = 0.5)+
  
  # Plot Primary Care 20 day limit
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue", linewidth = 1)+
  # Plot Specialty Care 28 day limit
  geom_hline(yintercept = 28, linetype = "dotted", color = "red", linewidth = 1)+
  
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Box plot for Established Patient Wait Times (Days) in Five California VA Clinics",
       subtitle = "Blue Dashed Line = 20 Day Primary Care Limit,
Red Dotted Line = 28 Day Specialty Care Limit",
       y = "Wait Time (Days)",
       x = "Care Type"
  )
ca_box_est

####

## TX new patients
tx_box_new <- ggplot(data = tx_data, mapping = aes(x=type, y=wait_new, fill = type))+
  geom_boxplot() +  
  geom_jitter(width= 0.2, alpha = 0.5)+
  
  # Plot Primary Care 20 day limit
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue", linewidth = 1)+
  # Plot Specialty Care 28 day limit
  geom_hline(yintercept = 28, linetype = "dotted", color = "red", linewidth = 1)+
  
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Box plot for New Patient Wait Times (Days) in Five Texas VA Clinics",
       subtitle = "Blue Dashed Line = 20 Day Primary Care Limit,
Red Dotted Line = 28 Day Specialty Care Limit",
       y = "Wait Time (Days)",
       x = "Care Type"
  )
tx_box_new

## TX established patients
tx_box_est <- ggplot(data = tx_data, mapping = aes(x=type, y=wait_established, fill = type))+
  geom_boxplot() +  
  geom_jitter(width= 0.2, alpha = 0.5)+
  
  # Plot Primary Care 20 day limit
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue", linewidth = 1)+
  # Plot Specialty Care 28 day limit
  geom_hline(yintercept = 28, linetype = "dotted", color = "red", linewidth = 1)+
  
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Box plot for Established Patient Wait Times (Days) in Five Texas VA Clinics",
       subtitle = "Blue Dashed Line = 20 Day Primary Care Limit,
Red Dotted Line = 28 Day Specialty Care Limit",
       y = "Wait Time (Days)",
       x = "Care Type"
  )
tx_box_est

###############################################################################
###### How many times were the Wait Time limits violated for each care type?
###### Are there correlations between new and established patients? CA and TX?

pri_lim = 20   # days (primary care limit)
spec_lim = 28  # days (specialty care limit)

ca_lim_violations <- ca_data %>%
  select(type, wait_new, wait_established) %>%
  mutate(
    # Define limit based on type of care
    lim = case_when(type == "PRIMARY CARE" ~ pri_lim,
                    TRUE ~ spec_lim),
    violate_new = wait_new > lim,
    violate_est = wait_established > lim
  ) %>%
  group_by(type) %>%
  summarize(
    # Create column for limit in days
      lim = unique(lim)[1],
    # Create column for number total data points, number violations, and percentage for new patients
      n_new = sum(!is.na(wait_new)),
      n_violate_new = sum(violate_new, na.rm = TRUE),
      percent_violate_new = (n_violate_new/n_new),
    
    # Create column for number total data points, number violations, and percentage for established patients
      n_est = sum(!is.na(wait_established)),
      n_violate_est = sum(violate_est, na.rm = TRUE), 
      percent_violate_est = (n_violate_est / n_est),
      .groups = "drop"
  )

# Create bar plot to show the percentage of violations for new patients
ca_new_violate <- ggplot(data = ca_lim_violations, mapping = aes(x = type, y = percent_violate_new, fill = type)) +
  geom_col() +
  coord_flip()+
  theme(legend.position = "none")+
  scale_fill_viridis_d() +
  labs(title = "(%) Wait Time Limits Violated, New Patients (California)",
       x = "Care Type",
       y = "Percentage of Violations (New Patient)")+
  geom_label(aes(label = scales::percent(percent_violate_new, accuracy = 1)), fill = "beige",hjust = 1.25)

# Create bar plot to show the percentage of violations for established patients
ca_est_violate <- ggplot(data = ca_lim_violations, mapping = aes(x = type, y = percent_violate_est, fill = type)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  scale_fill_viridis_d() +
  labs(title = "(%) Wait Time Limits Violated, Established Patients (California)",
       x = "Care Type",
       y = "Percentage of Violations (Est. Patient)")+
  geom_label(aes(label = scales::percent(percent_violate_est, accuracy = 1)), fill = "beige",hjust =-0.1)

# Use patchwork library to plot both bar plots side-by-side, combining the vertical axis
ca_new_violate + ca_est_violate + plot_layout(axes = "collect")

##### For Texas

tx_lim_violations <- tx_data %>%
  select(type, wait_new, wait_established) %>%
  mutate(
    # Define limit based on type of care
    lim = case_when(type == "PRIMARY CARE" ~ pri_lim,
                    TRUE ~ spec_lim),
    violate_new = wait_new > lim,
    violate_est = wait_established > lim
  ) %>%
  group_by(type) %>%
  summarize(
    # Create column for limit in days
    lim = unique(lim)[1],
    # Create column for number total data points, number violations, and percentage for new patients
    n_new = sum(!is.na(wait_new)),
    n_violate_new = sum(violate_new, na.rm = TRUE),
    percent_violate_new = (n_violate_new/n_new),
    
    # Create column for number total data points, number violations, and percentage for established patients
    n_est = sum(!is.na(wait_established)),
    n_violate_est = sum(violate_est, na.rm = TRUE), 
    percent_violate_est = (n_violate_est / n_est),
    .groups = "drop"
  )

# Create bar plot to show the percentage of violations for new patients
tx_new_violate <- ggplot(data = tx_lim_violations, mapping = aes(x = type, y = percent_violate_new, fill = type)) +
  geom_col() +
  coord_flip()+
  theme(legend.position = "none")+
  scale_fill_viridis_d() +
  labs(title = "(%) Wait Time Limits Violated, New Patients (Texas)",
       x = "Care Type",
       y = "Percentage of Violations (New Patient)")+
  geom_label(aes(label = scales::percent(percent_violate_new, accuracy = 1)), fill = "beige",hjust = 1.25)

# Create bar plot to show the percentage of violations for established patients
tx_est_violate <- ggplot(data = tx_lim_violations, mapping = aes(x = type, y = percent_violate_est, fill = type)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  scale_fill_viridis_d() +
  labs(title = "(%) Wait Time Limits Violated, Established Patients (Texas)",
       x = "Care Type",
       y = "Percentage of Violations (Est. Patient)")+
  geom_label(aes(label = scales::percent(percent_violate_est, accuracy = 1)), fill = "beige",hjust =-0.1)

# Use patchwork library to plot both bar plots side-by-side, combining the vertical axis
tx_new_violate + tx_est_violate + plot_layout(axes = "collect")



######## Utilize Statistical Process Control functions to visualize the raw data???
#### Calculate the overall statistics for the California Data

# For Sleep Medicine:
ca_sleep <- ca_data %>%
  filter(type == "SLEEP MEDICINE") %>%
  group_by(date) %>%
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

ca_sleep_tot <-ca_sleep %>%
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
    sigma_t_new = sd(ca_data %>% filter(type == "SLEEP MEDICINE") %>% pull(wait_new), na.rm = TRUE),
    
    # Calculate standard error for each clinic
    se_new = sigma_s_new/sqrt(group_size_new),
    
    upper_cl_new = x_bbar_new + 3*se_new,
    lower_cl_new = x_bbar_new - 3*se_new,
 
    ## Add columns for the established patient data
    # Calculate the mean of the mean
    x_bbar_est = mean(x_bar_est),
    # Calculate the mean of the ranges
    range_bar_est = mean(range_est),
    # Calculate the mean of the std deviation
    sd_bar_est = mean(sd_est),
    # Calculate sigma short and sigma total for the sample
    sigma_s_est = sqrt(mean(sd_est^2)),
    sigma_t_est = sd(ca_data %>% filter(type == "SLEEP MEDICINE") %>% pull(wait_established), na.rm = TRUE),
    
    # Calculate standard error for each clinic
    se_est = sigma_s_est/sqrt(group_size_est),
    
    upper_cl_est = x_bbar_est + 3*se_est,
    lower_cl_est = x_bbar_est - 3*se_est,   
    
  )

# Average Plot
# Create a data frame that defines plot labels

labels_new = ca_sleep_tot %>%
  group_by(date) %>%
  reframe(
    date = max(date),
    type = c("x_bbar", "Upper", "Lower"),
    name = c("Mean", "+3 S", "-3 S"),
    value = c(unique(x_bbar_new), unique(upper_cl_new), unique(lower_cl_new)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))

avg_plot_new <- ca_sleep_tot %>%
  ggplot(mapping = aes(x = date, y = x_bar_new))+
  geom_hline(mapping = aes(yintercept = unique(x_bbar_new)), color = "magenta4", linewidth = 1) +
  geom_ribbon(mapping = aes(ymin = lower_cl_new, ymax = upper_cl_new), fill = "tan", alpha = 0.2)+
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(mapping = aes(yintercept = spec_lim), color = "red3", linewidth = 0.5, linetype = "dashed") +
 
  # Plot labels
  # Commented the labels out because they were covering too much of the chart...
  #geom_label(data = labels_new, mapping = aes(x = date, y = value, label = text), hjust = 1)+
  labs(x = "Date",
       y = "Average Wait Time (days)",
       title = "Average Chart for Sleep Medicine Wait Time with 3-Sigma Control Limits",
       subtitle = "Data grouped by time for all five California VA clinics. Red Dashed line is 28 Day Wait Time limit.")

avg_plot_new
#########################################################################################################

############  What if we started by plotting the wait times for each day, for each of the top 5 care types
###### for each clinic. Then we can determine how many of those fell outside our control limits.
###### The Cpk/Cp or Ppk/Pp values could then be calculated for each clinic and care type.
###### Comparisons can then be evaluated as to whether wait time violations were due to common or special
###### causes (SPC Tests); as well as what differences there may be between clinics to cause some to
###### have shorter or longer wait times. This could help us identify those factors involved.

########### The next step will be to determine how we can make this useful to the VA. Will we build a
###### dashboard that the VA clinic directors can use to evaluate performance and proactively address
###### issues? Or will we use it to rather highlight areas for targeted improvement? - (e.g. why does sleep
###### medicine have wait times up to a year, but Audiology around a month? Why does it take so long for
###### a new patient to get an appointment vs. an existing patient?)

#### Starting with CA, let's plot sleep medicine data for each clinic

ca_sleep_tot$date <- as.Date(ca_sleep_tot$date, format = "%m/%d/%Y")
ca_sleep_tot <- ca_sleep_tot %>% arrange(date)

ca_sleep_plot <-ggplot(data = ca_sleep_tot, mapping = aes(x = date, y = wait_new))+
  geom_point(aes(color = clinic), size = 2, alpha = 0.75)+
  geom_hline(mapping = aes(yintercept = spec_lim), color = "magenta4", linewidth = 0.5, linetype = "dashed") +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "Sleep Medicine Appointment Wait Time Data for New Patients at Five California VA Clinics (2025)",
       subtitle = "Dashed line represents 28 day wait time limit",
       x = "Date",
       y = "Wait Time (days) [New Patient]")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day")
ca_sleep_plot

################################################################################

### Okay, proof of concept working (sort of?) let's create function that will do this
# so we can start applying this to other care types and to Texas.

#' @name calc_tot_stat
#' @title Calculate Total Statistics for Wait Time Data
#' @parak data [data frame] is the data frame which will be input to the function
#' @param x [numeric] vector of subgroup values (usually time) that data frame will be grouped by.
#' @param filter_col [string] is the column name within which the filtering will occur (e.g. "type")
#' @param filter_val [string] is the value by which the filtering is desired (e.g. "AUDIOLOGY")

calc_tot_stat = function(data, x, filter_col, filter_val){
  
  # Calculate the initial statistics table
  stats_i <- data %>%
    filter( {{filter_col}} == filter_val) %>%
    group_by( {{ x }}) %>%
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
      sigma_t_new = sd(data %>% filter({{ filter_col }} == filter_val) %>% pull(wait_new), na.rm = TRUE),
      
      # Calculate standard error for each clinic
      se_new = sigma_s_new/sqrt(group_size_new),
      
      upper_cl_new = x_bbar_new + 3*se_new,
      lower_cl_new = x_bbar_new - 3*se_new,
      
      ## Add columns for the established patient data
      # Calculate the mean of the mean
      x_bbar_est = mean(x_bar_est),
      # Calculate the mean of the ranges
      range_bar_est = mean(range_est),
      # Calculate the mean of the std deviation
      sd_bar_est = mean(sd_est),
      # Calculate sigma short and sigma total for the sample
      sigma_s_est = sqrt(mean(sd_est^2)),
      sigma_t_est = sd(data %>% filter({{ filter_col }} == filter_val) %>% pull(wait_established), na.rm = TRUE),
      
      # Calculate standard error for each clinic
      se_est = sigma_s_est/sqrt(group_size_est),
      
      upper_cl_est = x_bbar_est + 3*se_est,
      lower_cl_est = x_bbar_est - 3*se_est,   
    )
  
  return(output)
}

#### Let's see how the function works!

tx_sleep_tot <- calc_tot_stat(data = tx_data, x = date, filter_col = type, filter_val = "SLEEP MEDICINE")
tx_sleep_tot

## So far so good, now let's try the plots...


#' @name data_plotter
#' @title Plot of Wait Time data over Dates
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

### Test Test Test ###
data_plotter(data = tx_data, x = date, y = wait_new, lim = 28, 
             care_type = "AUDIOLOGY", state = "Texas", patient_type = "New" )


data_plotter(data = tx_data, x = date, y = wait_established, lim = 28, 
             care_type = "NEUROLOGY", state = "Texas", patient_type = "Established" )

data_plotter(data = ca_data, x = date, y = wait_established, lim = 28, 
             care_type = "NEUROLOGY", state = "California", patient_type = "Established" )










 
### Function for the Average Plot
### Am I doing this right????????????????????????????

avg_plotter_new = function(data, x, y, filter_col, filter_val){
  
  # Create a data frame that defines plot labels
  labels_new = tot_data %>%
    group_by( {{ x }}) %>%
    reframe(
      x_axis = max( {{ x }}),
      type = c("x_bbar", "Upper", "Lower"),
      name = c("Mean", "+3 S", "-3 S"),
      value = c(unique(x_bbar_new), unique(upper_cl_new), unique(lower_cl_new)),
      value = round(value, 2),
      text = paste(name, value, sep = " = "))
  
  avg_plot_new <- tot_data %>%
    ggplot(mapping = aes(x = {{ x }}, y = x_bar_new))+
    geom_hline(mapping = aes(yintercept = unique(x_bbar_new)), color = "magenta4", linewidth = 1) +
    geom_ribbon(mapping = aes(ymin = lower_cl_new, ymax = upper_cl_new), fill = "tan", alpha = 0.2)+
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_hline(mapping = aes(yintercept = spec_lim), color = "red3", linewidth = 0.5, linetype = "dashed") +
    
    # Plot labels
    # Commented the labels out because they were covering too much of the chart...
    #geom_label(data = labels_new, mapping = aes(x = date, y = value, label = text), hjust = 1)+
    labs(x = "Date",
         y = "Average Wait Time (days)",
         title = "Average Chart for Sleep Medicine Wait Time with 3-Sigma Control Limits",
         subtitle = "Data grouped by time for all five California VA clinics. Red Dashed line is 28 Day Wait Time limit.")
  
  avg_plot_new
}

```
