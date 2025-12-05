### SYSEN 5300: Six Sigma Project - Financial Cost Percent Differences
### OC 7 - Six Sigma Warriors - Percent Difference Calculations
# Brandy Kinnunen, Juan Rodriguez
library(dplyr)

# Create a dataframe for the Texas cost data
tx = data.frame(
  cat = c("overall", "neurology", "gastroenterology", "urology", "sleep", "audiology"),
  new_cost = c(3038755, 349416.7, 321858.1, 255133.2, 250787.3, 192987.9),
  est_cost = c(66787.5, 3773.35, 5204.79, 0, 14507.89, 605.56)
)

# Use mutate function to calculate the percent difference between
# the new and established patient costs due to failure

tx %>% mutate(
  perc_diff = (new_cost - est_cost)/((new_cost + est_cost)/2)
)


# Create a dataframe for the Texas cost data
ca = data.frame(
  cat = c("overall", "sleep", "neurology", "optometry", "opthalmology", "podiatry"),
  new_cost = c(3257770, 545632.6, 3773.35, 263023.1, 218628.3, 215927.4),
  est_cost = c(200680, 114174.09, 0, 34141.11, 45.58, 15878.53))

# Use mutate function to calculate the percent difference between
# the new and established patient costs due to failure
ca %>% mutate(
  perc_diff = (new_cost - est_cost)/((new_cost + est_cost)/2)
)
