

---
title: "T-Test Results for Innovations and Correlates"
output: html_document
date: "2024-05-27"
---
  
library(readr)
library(dplyr)
library(writexl)
library(broom)
library(dplyr)
library(tidyr)
library (survey)




# Load ----

df <- read.csv("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/Non-genetics/table5_df.csv")

df <- df %>%
  mutate(
    Sub1 = case_when(
      qSub1 == "[+p]" ~ 1,
      qSub1 == "?" ~ 0,
      qSub1 == "[--]" ~ 0,
      TRUE ~ NA_real_
    ),
    Saltol = case_when(
      Saltol == "[+p]-Aro" ~ 1,
      Saltol == "[+p]-Aus" ~ 1,
      Saltol == "?" ~ 0,
      Saltol == "[--]" ~ 0,
      TRUE ~ NA_real_
    ),
    DMC = case_when(
      DMC == TRUE ~ 1,
      DMC == FALSE ~ 0,
      TRUE ~ NA_real_
    ),
    CMD = case_when(
      CMD == TRUE ~ 1,
      CMD == FALSE ~ 0,
      TRUE ~ NA_real_
    ),
    `GIFT-derived` = case_when(
      StrainB == "GIFT-derived" ~ 1,
      StrainB == "Non-GIFT" ~ 0,
      TRUE ~ NA_real_
    )
  )

df$dist_market_daily[df$dist_market_daily == -1] <- NA
df$dist_market_periodic[df$dist_market_periodic == -1] <- NA
df$dist_market_wholesale[df$dist_market_wholesale == -1] <- NA

# Define the lists of variables
var_list <- c(
  # 2022 Innovations
  "Sub1", "Saltol", "IRRI_Parentage_edited", 
  "d_mech", "mech_mini_combiner", "mech_combine_harvester", "mech_straw_baler",
  # 2023 Innovations
  "GIFT-derived", "DMC", "CMD",  
  "d_coffee_swcp", 
  "d_1m5r_certified", "dummy_2r", "dummy_3r", "dummy_5r",
  "mech_laser_level", 
  # Others
  "d_nrm_vietgap", "d_nrm_1m5r", "d_nrm_awd", "d_nrm_icm", "d_nrm_3r3g", "d_nrm_sri", "d_nrm_other"
)

corr_list <- c(
  "female", "ethnic", "internet", "poor_commune", 
  "dummy_main_str", "dist_main_str", "road_cement_asphalt", "road_dirt_earth", "road_gravel", "road_tar_on_gravel", 
  "dummy_local_market_daily", "dummy_local_market_periodic", "dummy_local_market_wholesale",
  "dummy_irrigation", "dummy_protection_staff"
)

corr_list_num <- c("land_ha", "age", "edu_grade", "n_member", "THUNHAP", "TONGCHITIEU",
                   "dist_market_daily", "dist_market_periodic", "dist_market_wholesale","dist_irrigation") 



# Calculate the number of categories for each variable in var_list
categories_count <- sapply(var_list, function(var) {
  length(unique(na.omit(df[[var]])))
})

# Create a data frame for the categories count
categories_df <- data.frame(
  Variable = var_list,
  Number_of_Categories = categories_count
)



# t-tests for binary correlates -----
perform_t_test <- function(df, innovation, correlate) {
  df_filtered <- df %>%
    filter(!is.na(df[[correlate]]), !is.na(df[[innovation]]))
  
  # Perform t-test if the correlate has exactly 2 levels
  if (length(unique(df_filtered[[correlate]])) == 2 && 
      all(table(df_filtered[[correlate]]) > 1)) {
    t_test <- t.test(df_filtered[[correlate]] ~ df_filtered[[innovation]], na.action = na.omit, weight = df_filtered$wt45)
    
    group_stats <- df_filtered %>%
      group_by_at(innovation) %>%
      summarise(
        weighted_sum = sum(!!sym(correlate) * df_filtered$wt45, na.rm = TRUE),
        sum_of_weights = sum(df_filtered$wt45),
        n_obs = n()
      )
    
    mean_value <- group_stats$weighted_sum / group_stats$sum_of_weights
    
    n_obs_adopters <- group_stats$n_obs[df_filtered[[innovation]] == 1]
    n_obs_non_adopters <- group_stats$n_obs[df_filtered[[innovation]] == 0]
    
    stars <- ifelse(t_test$p.value < 0.001, "***",
                    ifelse(t_test$p.value < 0.01, "**",
                           ifelse(t_test$p.value < 0.05, "*", "")))
    
    result <- data.frame(
      Innovation = innovation,
      Correlate = correlate,
      N_Obs = sum(df_filtered$wt45),
      N_Obs_Adopters = ifelse(nrow(group_stats) > 1, n_obs_adopters, NA),
      Mean_Adopters = ifelse(nrow(group_stats) > 1, round(mean_value[2], 4), NA),
      N_Obs_Non_Adopters = ifelse(nrow(group_stats) > 0, n_obs_non_adopters, NA),
      Mean_Non_Adopters = ifelse(nrow(group_stats) > 0, round(mean_value[1], 4), NA),
      Difference = ifelse(nrow(group_stats) > 1, round(diff(mean_value), 4), NA),
      P_Value = round(t_test$p.value, 4),
      Stars = stars
    )
    
    return(result)
  }
  return(NULL)
}


results <- list()

# Loop through each innovation and correlate
for (innovation in var_list) {
  for (correlate in corr_list) {
    result <- perform_t_test(df, innovation, correlate)
    if (!is.null(result)) {
      results <- append(results, list(result))
    }
  }
}

results_df <- do.call(rbind, results)




# t-tests for numeric correlates ----

perform_t_test <- function(df, innovation, correlate) {
  df_filtered <- df %>%
    filter(!is.na(!!sym(correlate)), !is.na(!!sym(innovation))) # Try modifying this
  
  # Perform t-test if the innovation has exactly 2 levels
  if (length(unique(df_filtered[[innovation]])) == 2 && 
      all(table(df_filtered[[innovation]]) > 1)) {
    t_test <- t.test(df_filtered[[correlate]] ~ df_filtered[[innovation]], na.action = na.omit, weight = df_filtered$wt45)
    
    group_stats <- df_filtered %>%
      group_by_at(innovation) %>%
      summarise(
        weighted_sum = sum(!!sym(correlate) * df_filtered$wt45, na.rm = TRUE),
        sum_of_weights = sum(df_filtered$wt45),
        n_obs = n(),
        .groups = 'drop'
      )
    
    group_stats <- group_stats %>%
      mutate(mean_value = weighted_sum / sum_of_weights)
    
    mean_values <- group_stats$mean_value
    n_obs <- nrow(df_filtered)
    n_obs_adopters <- group_stats$n_obs[group_stats[[innovation]] == 1]
    n_obs_non_adopters <- group_stats$n_obs[group_stats[[innovation]] == 0]
    
    stars <- ifelse(t_test$p.value < 0.001, "***",
                    ifelse(t_test$p.value < 0.01, "**",
                           ifelse(t_test$p.value < 0.05, "*", "")))
    
    result <- data.frame(
      Innovation = innovation,
      Correlate = correlate,
      N_Obs = n_obs,
      N_Obs_Adopters = ifelse(nrow(group_stats) > 1, group_stats$n_obs[2], NA),
      Mean_Adopters = ifelse(nrow(group_stats) > 1, round(group_stats$mean_value[2], 4), NA),
      N_Obs_Non_Adopters = ifelse(nrow(group_stats) > 0, group_stats$n_obs[1], NA),
      Mean_Non_Adopters = ifelse(nrow(group_stats) > 0, round(group_stats$mean_value[1], 4), NA),
      Difference = ifelse(nrow(group_stats) > 1, round(diff(group_stats$mean_value), 4), NA),
      P_Value = round(t_test$p.value, 4),
      Stars = stars
    )
    
    return(result)
  }
  return(NULL)
}


# Initialize results list
results <- list()

# Loop through each innovation and correlate
for (innovation in var_list) {
  for (correlate in corr_list_num) {
    result <- perform_t_test(df, innovation, correlate)
    if (!is.null(result)) {
      results <- append(results, list(result))
    }
  }
}

results_num <- do.call(rbind, results)

results_df <- rbind (results_df, results_num) # Combine with the numeric correlates
results_All <- results_df %>% arrange(Innovation)

write_xlsx(results_df, "t_test_2022_Obs.xlsx")


# Arrange outputs ----

results_Overview <- results_df %>%
  mutate(Diff_Stars = ifelse(Stars == "", "n.s.", paste(Difference, Stars))) %>%
  select(Innovation, Correlate, Diff_Stars) %>%
  spread(key = Correlate, value = Diff_Stars)

# 1. Reorder Innovations (rows) based on the order in l.54 (will work later with 2023 data)
common_vars <- intersect(var_list, results_Overview$Innovation)

results_Overview <- results_Overview %>%
  arrange(factor(Innovation, levels = common_vars))


# 2. Reorder Correlates

corr_list <- c(
  'Innovation',
  # Smallholder context
  "land_ha", "road_cement_asphalt", "road_dirt_earth", "dist_main_str", 
  "internet", "poor_commune", 
  "dist_market_daily", "dist_market_periodic", "dist_market_wholesale", 
  "dummy_local_market_daily", "dummy_local_market_periodic", "dummy_local_market_wholesale",
  "dummy_irrigation", "dist_irrigation","dummy_protection_staff",
  # Gender, social inclusion and youth
  "female", "ethnic", "age", "edu_grade", "n_member", "THUNHAP", "TONGCHITIEU" 
)

results_Overview <- results_Overview %>%
  select(corr_list)


# 3. Rename Innovations and correlates
results_Overview <- results_Overview %>%
  mutate(Innovation = recode(Innovation,
                             'IRRI_Parentage_edited' = 'IRRI-related',
                             'Sub1' = 'Rice Sub1 QTL',
                             'Saltol' ='Rice Saltol QTL',
                             'DMC' = 'Cassava DMC QTL', 
                             'CMD'='Cassava CMD QTL', 
                             'd_coffee_swcp'='Sustainable Water for Coffee Production', 
                             'mech_mini_combiner'='Mini-combine Harvester', 
                             'mech_combine_harvester'='Combine Harvester',
                             'mech_straw_baler'='Straw Baler'
  ))

results_Overview <- results_Overview %>%
  rename(
    'HH head from an ethnic minority' = ethnic,
    'HH head is female' = female,
    'Age of hh head (in years)' = age,
    'HH head highest completed grade' = edu_grade,
    'Household size' = n_member,
    'Annual income (VND)' = THUNHAP,
    'Annual consumption (VND)' = TONGCHITIEU,
    'Total area cultivated per household hh (ha)' = land_ha,
    'Household has internet' = internet,
    'Poor Commune label' = poor_commune,
    'Main road is asphalt' = road_cement_asphalt,
    'Main road is earth' = road_dirt_earth,
    'Distance to main street (km)' = dist_main_str,
    'Distance to daily market (km)' = dist_market_daily,
    'Distance to periodic market (km)' = dist_market_periodic,
    'Distance to wholesale market (km)' = dist_market_wholesale,
    'Commune has daily market' = dummy_local_market_daily,
    'Commune has periodic market' = dummy_local_market_periodic,
    'Commune has wholesale market' = dummy_local_market_wholesale,
    'IRRIGATION_TBD' = dummy_irrigation,
    'Distance to irrigation_TBD' = dist_irrigation,
    'Commune has extension agent' = dummy_protection_staff
  )

write_xlsx(results_Overview, "t_test_Summary.xlsx")




```
