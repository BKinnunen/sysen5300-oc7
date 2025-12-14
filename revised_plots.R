### SYSEN 5300: Six Sigma Project
### OC 7 - Six Sigma Warriors
# Brandy Kinnunen, Juan Rodriguez

# Revise the data visualization for final report and presentation

library(ggplot2)
library(readr)
library(tidyverse)
library(broom)
#install.packages("tidyplots")
library(tidyplots)


######################## Load functions + Read Data
source("/cloud/project/six_sigma_project/project_functions.R")
ca_data = read_csv("/cloud/project/six_sigma_project/01_ca_wait_time.csv")
tx_data = read_csv("/cloud/project/six_sigma_project/03_tx_wait_time.csv")

######################### Failure Plots
## Plot the top five care types for failure rate in each state, then
# join those plots together into one visual.

# Select the top 5 failure rates in CA. Arranging by new patient failure rates
# because they far exceed the established patient failure rates.
top5_ca = fail_summary(ca_data) %>% arrange(desc(percent_fail_new)) %>%
  filter(n_new > 1) %>%
  slice_head(n=5)

# create a side-by-side bar plot of the failure rates
ca_p1 = fail_plot_single(data = top5_ca,  wait_type = "wait_new", state = "CA")
ca_p2 = fail_plot_single(data = top5_ca,  wait_type = "wait_established", state = "CA")
ca_double = ca_p1 + ca_p2 + plot_layout(axes = "collect")

# Select the top 5 failure rates in TX. Arranging by new patient failure rates
# because they far exceed the established patient failure rates.
top5_tx = fail_summary(tx_data) %>% arrange(desc(percent_fail_new)) %>%
  filter(n_new > 1) %>%
  slice_head(n = 5)

# create a side-by-side bar plot of the failure rates
tx_p1 = fail_plot_single(data = top5_tx,  wait_type = "wait_new", state = "TX")
tx_p2 = fail_plot_single(data = top5_tx,  wait_type = "wait_established", state = "TX")
tx_double = tx_p1 + tx_p2 + plot_layout(axes = "collect")

# plot CA and TX bar plots one above the other
ca_double / tx_double + plot_layout(axes = "collect")
###############################################################################

########################## ANOVA Visualization: between States

# Select the five care types for CA with highest failure rate.
top_types_ca = top5_ca %>% pull(type)

# Filter the CA dataset for just the top 5 care types
top_data_ca = ca_data %>%
  filter(type %in% top_types_ca) %>%
  # Rearrange the wait time data so that it is in one column
  pivot_longer(cols = c(wait_established, wait_new),
               names_to = "patient_type",
               values_to = "wait_time")

### Use tidyplots() to visualize the top 5 care types in California
top_data_ca |> 
  tidyplot(x = type, y = wait_time, color = patient_type) |> 
  # Add a violin plot to show the data distribution
  add_violin() |>
  # Add the data points and adjust transparency 
  add_data_points(alpha = 0.5) |>  
  
  # Add a mean line (red) for each group
  add_mean_dash(color = "red") |> 
  # Add an error bar (black) for each group
  add_sem_errorbar(color = "black") |> 
  # Indicate statistical significance between new and established wait times
  add_test_pvalue() |>
  
  # Add the plot title
  add_title("Variance of Care Types with Highest Failure Rates - California") |>
  adjust_title(fontsize = 12) |>
  # Define color scheme
  adjust_colors(new_colors = c("wait_established" = "#480a56ff",
                               "wait_new" = "#218d89ff")) |> 
  # Remove the x axis, since that is just the care type labels
  remove_x_axis_title() |>
  # Rotate the care type labels
  adjust_x_axis(rotate_labels = TRUE) |>
  # Rename the y-axis label
  adjust_y_axis_title("Wait Time (days)") |>
  adjust_y_axis_title(fontsize = 12) |>
  
  # Adjust plot size to make it bigger
  adjust_size(width = 100, height = 75)




### Repeat the same thing for TX
# Select the five care types for TX with highest failure rate.
top_types_tx = top5_tx %>% pull(type)

# Filter the TX dataset for just the top 5 care types
top_data_tx = tx_data %>%
  filter(type %in% top_types_tx) %>%
  # Rearrange the wait time data so that it is in one column
  pivot_longer(cols = c(wait_established, wait_new),
               names_to = "patient_type",
               values_to = "wait_time")

### Use tidyplots() to visualize the top 5 care types in Texas
top_data_tx |> 
  tidyplot(x = type, y = wait_time, color = patient_type) |> 
  # Add a violin plot to show the data distribution
  add_violin() |>
  # Add the data points and adjust transparency 
  add_data_points(alpha = 0.5) |>  
  
  # Add a mean line (red) for each group
  add_mean_dash(color = "red") |> 
  # Add an error bar (black) for each group
  add_sem_errorbar(color = "black") |> 
  # Indicate statistical significance between new and established wait times
  add_test_pvalue() |>
  
  # Add the plot title
  add_title("Variance of Care Types with Highest Failure Rates - Texas") |>
  adjust_title(fontsize = 12) |>
  # Define color scheme
  adjust_colors(new_colors = c("wait_established" = "#3b528b",
                               "wait_new" = "orange")) |> 
  # Remove the x axis, since that is just the care type labels
  remove_x_axis_title() |>
  # Rotate the care type labels
  adjust_x_axis(rotate_labels = TRUE) |>
  # Rename the y-axis label
  adjust_y_axis_title("Wait Time (days)") |>
  adjust_y_axis_title(fontsize = 12) |>
  
  # Adjust plot size to make it bigger
  adjust_size(width = 100, height = 75)
###############################################################################

###############################################################
########################## ANOVA Visualization: between clinics

# Rearrange the wait time data so that it is in one column
long_data_ca = ca_data %>%
  pivot_longer(cols = c(wait_established, wait_new),
               names_to = "patient_type",
               values_to = "wait_time")

### Use tidyplots() to visualize the clinics in California
long_data_ca |> 
  tidyplot(x = clinic, y = wait_time, color = clinic) |> 
  # Add a violin plot to show the data distribution
  add_violin() |>
  # Add the data points and adjust transparency 
  add_data_points(alpha = 0.5) |>  
  
  # Add a mean line (red) for each group
  add_mean_dash(color = "red") |> 
  # Add an error bar (black) for each group
  add_sem_errorbar(color = "black") |> 
  # Indicate statistical significance between new and established wait times
  add_test_pvalue() |>
  
  # Add the plot title
  add_title("Variance of VA Medical Center Performance - California") |>
  adjust_title(fontsize = 12) |>
  # Remove the x axis, since that is just the care type labels
  remove_x_axis_title() |>
  # Rotate the care type labels
  adjust_x_axis(rotate_labels = TRUE) |>
  # Rename the y-axis label
  adjust_y_axis_title("Wait Time (days)") |>
  adjust_y_axis_title(fontsize = 12) |>
  
  # Adjust plot size to make it bigger
  adjust_size(width = 100, height = 75)




### Repeat the same thing for TX

# Rearrange the wait time data so that it is in one column
long_data_tx = tx_data %>%
  pivot_longer(cols = c(wait_established, wait_new),
               names_to = "patient_type",
               values_to = "wait_time")

### Use tidyplots() to visualize the clinics in Texas
long_data_tx |> 
  tidyplot(x = clinic, y = wait_time, color = clinic) |> 
  # Add a violin plot to show the data distribution
  add_violin() |>
  # Add the data points and adjust transparency 
  add_data_points(alpha = 0.5) |>  
  
  # Add a mean line (red) for each group
  add_mean_dash(color = "red") |> 
  # Add an error bar (black) for each group
  add_sem_errorbar(color = "black") |> 
  # Indicate statistical significance between new and established wait times
  add_test_pvalue() |>
  
  # Add the plot title
  add_title("Variance of VA Medical Center Performance - Texas") |>
  adjust_title(fontsize = 12) |>

  # Remove the x axis, since that is just the care type labels
  remove_x_axis_title() |>
  # Rotate the care type labels
  adjust_x_axis(rotate_labels = TRUE) |>
  # Rename the y-axis label
  adjust_y_axis_title("Wait Time (days)") |>
  adjust_y_axis_title(fontsize = 12) |>
  
  # Adjust plot size to make it bigger
  adjust_size(width = 100, height = 75)
