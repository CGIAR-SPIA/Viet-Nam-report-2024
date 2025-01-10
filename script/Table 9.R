

# ---
# Title: "Table 9. Overview of variables associated with the adoption of agricultural innovations + Appendix C"
# Authors email: "f.kosmowski@cgiar.org", "b.thanh@contractors.irri.org"
# Date: "11/2024"
# ---

# This script sources from VH22_data.csv and VH23_data.csv
# Its outputs are Tab9.html / Tab9.docx and Appendix.C.csv / Appendix.C.html
# It also generates OLS plots published in the report.




rm (list = ls()) #start clean
options(scipen = 999) #display of number

# Install packages ----
# Function to check and install packages

# Identify loaded packages (excluding base packages)
loaded_packages <- names(sessionInfo()$otherPkgs)

# Detach each loaded package
if (length(loaded_packages) > 0) {
  for (pkg in loaded_packages) {
    detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
  }
}


packages <- c("rvest", "iai", "jtools", "tidyverse", "officer",
              "dplyr", "summarytools", "broom", "dplyr", "estimatr", 
              "ggplot2", "curl", "patchwork", "flextable", "httr")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))



# Load packages ----
library(rvest)
library(iai)
library(jtools)
library(tidyverse)
library(summarytools)
library(broom)
library(dplyr)
library(estimatr)  
library(ggplot2)
library (curl)
library(patchwork)
library (flextable)
library (officer)
library (httr)


# Function to curl data from GitHub----
# Curl without token:
curl_function <- function(url) {
  url_pasted <- paste0("https://raw.githubusercontent.com/CGIAR-SPIA/Viet-Nam-report-2024/main/", url)
  
  # Ensure the directory exists before saving the file
  dir_path <- dirname(url)  # Extract the directory path from the URL
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)  # Create the directory structure if it doesn't exist
  }
  
  # Use download.file to fetch the file without requiring a token
  download.file(url_pasted, destfile = url, mode = "wb")
}


# Create output folders----

output_dir <- "Output/Multivariate_analysis"

# Check if the directory exists, if not, create it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}



# Load and prepare data ----
# Province ID: 
curl_function ("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Provinces_IDs <- read.csv("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv") 
Provinces_IDs$MATINH <- as.numeric (Provinces_IDs$MATINH)

# VH22: 
curl_function ("data/processed/VH22_data.csv")
df_22 <- read.csv("data/processed/VH22_data.csv")
df_22$MATINH <- as.numeric (df_22$MATINH)
df_22 <- df_22 %>%
  left_join (Provinces_IDs)

# Adjust variables
df_22$THUNHAP_mil <- scale(df_22$THUNHAP) # df_22$THUNHAP / 1000 #convert unit from in thousand dong to in million dong
df_22$land_area_ha <- df_22$land_area_sum / 10000 #convert from sq meters to hectare
df_22$age <- scale (df_22$age) # Scale age
df_22$edu_grade <- scale (df_22$edu_grade) 
df_22$n_member <- scale (df_22$n_member) 

 

# Calculate the 99th percentiles
thunhap_99 <- quantile(df_22$THUNHAP_mil, 0.99, na.rm = TRUE)
land_area_sum_99 <- quantile(df_22$land_area_ha, 0.99, na.rm = TRUE)
df_22$THUNHAP_mil <- ifelse(df_22$THUNHAP_mil > thunhap_99, NA, df_22$THUNHAP_mil)
df_22$land_area_ha <- ifelse(df_22$land_area_ha > land_area_sum_99, NA, df_22$land_area_ha)
df_22$EA_ID <- paste (df_22$MATINH, df_22$MAHUYEN, df_22$MAXA, df_22$MADIABAN, sep='-')


#VH23:
curl_function ("data/processed/VH23_data.csv")
df_23 <- read.csv("data/processed/VH23_data.csv")
df_23$MATINH <- as.numeric (df_23$MATINH)

df_23 <- df_23 %>%
  left_join (Provinces_IDs)

df_23$THUNHAP_mil <- scale (df_23$THUNHAP)
df_23$land_area_ha <- df_23$land_area_sum / 10000
df_23$age <- scale (df_23$age)
df_23$edu_grade <- scale (df_23$edu_grade) 
df_23$n_member <- scale (df_23$n_member) 

df_23$ThreeR <- ifelse(
  df_23$d_1m5r_seed_120kg == 1 & df_23$lenient_2r == 1 & df_23$lenient_3r == 1, 1, 
  ifelse(is.na(df_23$d_1m5r_seed_120kg) & is.na(df_23$lenient_2r) & is.na(df_23$lenient_3r), NA, 0 ))



# Calculate the 99th percentiles
thunhap_99 <- quantile(df_23$THUNHAP_mil, 0.99, na.rm = TRUE)
land_area_sum_99 <- quantile(df_23$land_area_ha, 0.99, na.rm = TRUE)
df_23$THUNHAP_mil <- ifelse(df_23$THUNHAP_mil > thunhap_99, NA, df_23$THUNHAP_mil)
df_23$land_area_ha <- ifelse(df_23$land_area_ha > land_area_sum_99, NA, df_23$land_area_ha)

df_23$EA_ID <- paste (df_23$MATINH, df_23$MAHUYEN, df_23$MAXA, df_23$MADIABAN, sep='-')




# Dependent vars:
All_22 <- c("mech_row_seeder", "mech_mini_combiner", "mech_combine_harvester", "mech_straw_baler", 
         "IRRI_Parentage_edited", "Saltol")


# Define the list of predictor variables
corr_list_22 <- c("female", "ethnic", "internet", "land_area_ha","main_str_asphalt",
               "age", "edu_grade", "n_member", "THUNHAP_mil") 


All_23 <- c(
  "mech_row_drum_seeder", 
  # "mech_seed_blower", 
  "mech_combine_harvester", "mech_straw_baler", "mech_laser_level",
  'CIAT.related', "DMC", "CMD",  
  "StrainB_edited", 
  "SWCP", 
  "d_1m5r_seed_100kg", "d_1m5r_seed_120kg", "strict_2r", "lenient_2r", "strict_3r", "lenient_3r", "ThreeR",
  "awd_1drydown", "awd_2drydown", "csmap_final"
)


corr_list_23 <- c("female", "ethnic", "internet", "land_area_ha",
                  "age", "edu_grade", "n_member", "THUNHAP_mil") 


dependent_23_noCSMAP <- c(
  "mech_row_drum_seeder", 
  # "mech_seed_blower", 
  "mech_combine_harvester", "mech_straw_baler", "mech_laser_level",
  'CIAT.related', "DMC", "CMD",  
  "StrainB_edited", 
  "SWCP", 
  "d_1m5r_seed_100kg", "d_1m5r_seed_120kg", "strict_2r", "lenient_2r", "strict_3r", "lenient_3r", "ThreeR",
  "awd_1drydown", "awd_2drydown")


# Function for SE cluster----

cluster_fn <- function (dataset, dependent_set, corr_list, fixed_effect = TRUE){
  ols_models <- list()
  model_results <- list()
  
  for (dependent_var in dependent_set) {
    # Select appropriate weight variable
    if (dependent_var %in% c("CIAT.related", "DMC", "CMD")) {
      weight_var <- "weight_cass"
    }else if (dependent_var %in% c ("StrainB_edited")) {
      weight_var <- "weight_gift"
    }else if (dependent_var %in% c("IRRI_Parentage_edited", "Saltol")) {
      weight_var <- "weight_rice_DNA"
    }else {
      weight_var <- "weight_final_rice"
    }
    
    
    if (fixed_effect) {
      formula <- as.formula(paste(dependent_var, "~", paste(c(corr_list, "Region"), collapse = " + ")))
    } else {
      formula <- as.formula(paste(dependent_var, "~", paste(corr_list, collapse = " + ")))
    }
    
    # Run weighted OLS model with clustered standard errors
    model <- lm_robust(formula, data = dataset, weights = .data[[weight_var]], clusters = EA_ID)
    ols_models[[dependent_var]] <- model
    
    # Store tidy model result
    model_results[[dependent_var]] <- tidy(model, conf.int = TRUE)
    model_results[[dependent_var]]$dependent_var <- dependent_var
  }
  
  
  # Combine all results into a single data frame
  result <- do.call(rbind, model_results)
  
  # Remove the intercept row
  result <- result %>% filter(term != "(Intercept)")
  
  # Rename Innovations
  result <- result %>%
    mutate(dependent_var = case_when(
      dependent_var == 'IRRI_Parentage_edited' ~ "CGIAR-related Rice Varieties",
      dependent_var == 'mech_mini_combiner' ~ "Mini-Combine Harvester (MCHB)",
      dependent_var == 'mech_combine_harvester' ~ "Combine Harvester (CHB)",
      dependent_var == 'mech_straw_baler' ~ "Rice Straw Baler", 
      dependent_var == 'mech_row_seeder' ~ 'Drum Seeder',
      dependent_var == "mech_row_drum_seeder" ~ "Drum Seeder",
      dependent_var == 'Saltol' ~ "Salt-tolerant Rice Varieties (STRVs)",
      dependent_var == "CIAT.related" ~ "CGIAR-related Cassava Varieties",
      dependent_var == "DMC" ~ "Cassava DMC QTL", 
      dependent_var == "CMD" ~ "Cassava Mosaic Disease (CMD)-resistant Cassava Varieties",
      dependent_var == "SWCP" ~ "Sustainable Water for Coffee Production", 
      dependent_var == "d_1m5r_seed_100kg" ~ "1R: Seed rate (strict)", 
      dependent_var == "mech_seed_blower" ~ "Seed blower", 
      dependent_var == "mech_laser_level" ~ "Laser Land Levelling (LLL)",
      dependent_var == "StrainB_edited" ~ "Genetically Improved Farmed Tilapia (GIFT)-derived strains",
      dependent_var == "d_1m5r_seed_120kg" ~ "1R: Seed rate (lenient)", 
      dependent_var == "strict_2r" ~ "2R: Nitrogen use (strict)", 
      dependent_var == "lenient_2r" ~ "2R: Nitrogen use (lenient)", 
      dependent_var == "strict_3r" ~ "3R: Pesticide use (strict)", 
      dependent_var == "lenient_3r" ~ "3R: Pesticide use (lenient)", 
      dependent_var == "ThreeR" ~ "Three Reductions, Three Gains (3R3G) and One Must Do, Five Reductions (1M5R - lenient)", 
      dependent_var == "awd_1drydown" ~ "Alternate Wetting and Drying (1 dry-down)", 
      dependent_var == "awd_2drydown" ~ "Alternate Wetting and Drying (At least 2 dry-downs)", 
      dependent_var == "csmap_final" ~ "Climate-Smart Mapping and Adaptation Planning (CS-MAP)",
      TRUE ~ dependent_var
    ))
  
  # Rename correlates
  result <- result %>%
    mutate(term = case_when(
      term == 'ethnic' ~ 'HH head from an ethnic minority',
      term == 'female' ~ 'HH head is female',
      term == 'age' ~ 'Age of hh head (std)',
      term == 'edu_grade' ~ 'HH head highest completed grade (std)',
      term == 'n_member' ~ 'Household size (std)',
      term == 'THUNHAP_mil' ~ 'Annual income (std)',
      term == 'TONGCHITIEU' ~ 'Annual consumption (in million VND)',
      term == 'land_area_ha' ~ 'Total agricultural land (in ha)',
      term == 'internet' ~ 'Household has internet',
      term == 'poor_commune' ~ 'Poor Commune label',
      term == 'main_str_asphalt' ~ 'Main road is asphalt',
      term == 'dist_market_wholesale' ~ 'Distance to wholesale market (km)',
      term == 'dummy_protection_staff' ~ 'Commune has extension agent',
      term == 'Bottom_20' ~ '% of households in bottom 20% of annual consumption',
      term == 'Bottom_40' ~ '% of households in bottom 40% of annual consumption',
      TRUE ~ term
    ),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1 ~ "*",
      TRUE ~ ""
    )
    )
}


# Function to plot SE cluster ----

plot_cluster_fn <- function (dataset, name){
  
  Plots_list <- list()
  
  for (innovation in unique(dataset[["dependent_var"]])) {
    
    # Subset data for each innovation
    data_subset <- dataset %>% 
      filter(dependent_var == innovation)
    
    p <- ggplot(data_subset, aes(x = estimate, y = term)) +
    geom_point(size = 5, color = "#2c3e50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color = "#34495e", size = 1) +
    theme_minimal(base_size = 15) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      plot.title = element_blank(),  # Remove plot title
      panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.y = element_blank(),  # Drop y-axis ticks
      axis.ticks.length = unit(0.2, "cm"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
      strip.text = element_text(size = 14)
    ) +
    labs(x = "Coefficient", y = " ") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
    scale_y_discrete(limits = rev(levels(factor(data_subset$term))), expand = c(0, 0.1))  # Reverse order of y-axis labels

  # Store the plot in the list
  Plots_list[[innovation]] <- p
  }

  # Save the plots
  for (innovation in names(Plots_list)) {
    # Generate file name with special case for 3R3G and 1M5R
    file_name <- if (innovation == "Three Reductions, Three Gains (3R3G) and One Must Do, Five Reductions (1M5R - lenient)") {
      paste0("Output/Multivariate_analysis/", 
             name, "_Adopters of_3R3G, 1M5R.png")
    } else {
      paste0("Output/Multivariate_analysis/", 
             name, "_Adopters of_", gsub("[/:?*<>|]", "-", innovation), ".png")
    }
  
    # Save the plot
    png(file_name, width = 1080, height = 600)
    print(Plots_list[[innovation]])  # Print the plot to save
    dev.off()  # Close the device
  }
  return (Plots_list)
}

# Function to convert results to publishable format----
publish_fn <- function (result_table){
  results_Overview <- result_table %>%
    mutate(estimate = round(estimate, 2),  # Round estimates to two decimal places
           estimate_significance = paste(estimate, significance)) %>%
    select(term, dependent_var, estimate_significance) %>%
    spread(key = term, value = estimate_significance)  # Make terms as columns
  
  table (results_Overview$dependent_var)
  
  
  results_Overview <- results_Overview %>%
    mutate(dependent_var = case_when(
      dependent_var == "IRRI-related rice varietie" ~ "CGIAR-related Rice Varieties",
      dependent_var == "CIAT-related cassava varieties" ~ "CGIAR-related Cassava Varieties",
      TRUE ~ dependent_var  # Keep original if no match is found
    ))
  
  if ("Main road is asphalt" %in% names(results_Overview)) {
    results_Overview <- results_Overview %>%
      relocate(c(
        "Age of hh head (std)",
        "Annual income (std)",
        "HH head from an ethnic minority",
        "HH head highest completed grade (std)",
        "HH head is female",
        "Household has internet",
        "Household size (std)",
        "Main road is asphalt",
        "Total agricultural land (in ha)"
      ), .after = "dependent_var")
  } else {
    results_Overview <- results_Overview %>%
      relocate(c(
        "Age of hh head (std)",
        "Annual income (std)",
        "HH head from an ethnic minority",
        "HH head highest completed grade (std)",
        "HH head is female",
        "Household has internet",
        "Household size (std)",
        "Total agricultural land (in ha)"
      ), .after = "dependent_var")
  }
  
  results_Overview <- results_Overview %>%
    rename("Innovation" = "dependent_var")
  
  return (results_Overview)
}


# 1. OLS with SE clustered at the EA level ----
## VH22: SE clustering----
VH22_OLS_Results <- cluster_fn (df_22, All_22, corr_list_22, fixed_effect = FALSE) %>%
  select (-outcome)

rownames(VH22_OLS_Results) <- NULL

write.csv(VH22_OLS_Results, "Output/Multivariate_analysis/VH22_OLS_Results.csv",
          row.names = FALSE)


## VH23: SE clustering----
VH23_OLS_Results <- cluster_fn (df_23, All_23, corr_list_23, fixed_effect = FALSE) 


VH23_OLS_Results <- VH23_OLS_Results %>%
  select (-outcome)

rownames(VH23_OLS_Results) <- NULL


write.csv (VH23_OLS_Results, "Output/Multivariate_Analysis/VH23_OLS_Results.csv",
           row.names = FALSE)



# 2. Add regional fixed effects----
## VH22: Fixed effect ----
VH22_fixed <- cluster_fn (df_22, All_22, corr_list_22, fixed_effect = TRUE)  %>%
  select (-outcome)

rownames(VH22_fixed) <- NULL

write.csv(VH22_fixed, "Output/Multivariate_analysis/VH22_OLS_Results_FE.csv",
          row.names = FALSE)


## VH23: Fixed effect ----
VH23_fixed <- cluster_fn (df_23, dependent_23_noCSMAP, corr_list_23, fixed_effect = TRUE) %>%
  select (-outcome)

rownames(VH23_fixed) <- NULL

write.csv(VH23_fixed, "Output/Multivariate_analysis/VH23_OLS_Results_FE.csv",
          row.names = FALSE)

# 3. Convert the table into publishable formats ----

## Not having regional fixed effects ----

table_22_non_fixed <- publish_fn (VH22_OLS_Results) %>%
  filter (Innovation %in% c("CGIAR-related Rice Varieties", "Salt-tolerant Rice Varieties (STRVs)"))

order_dependent <- c("Genetically Improved Farmed Tilapia (GIFT)-derived strains",
                     "CGIAR-related Cassava Varieties",
                     "Cassava Mosaic Disease (CMD)-resistant Cassava Varieties",
                     "Laser Land Levelling (LLL)",
                     # "Seed blower",
                     "Combine Harvester (CHB)",
                     "Rice Straw Baler",
                     "1R: Seed rate (lenient)",
                     "2R: Nitrogen use (lenient)",
                     "3R: Pesticide use (lenient)",
                     "Three Reductions, Three Gains (3R3G) and One Must Do, Five Reductions (1M5R - lenient)",
                     "Alternate Wetting and Drying (1 dry-down)",
                     'Drum Seeder',
                     "Climate-Smart Mapping and Adaptation Planning (CS-MAP)",
                     "CS-MAPs")

table_23_non_fixed <- publish_fn (VH23_OLS_Results) %>%
  filter (Innovation %in% order_dependent) %>%
  mutate (order_var = factor (Innovation, levels = order_dependent)) %>%
  arrange (order_var) %>%
  select (-order_var)


prep_table <- full_join (table_22_non_fixed, table_23_non_fixed)

sub_title1 <- c("Aqualculture and Capture fisheries", rep (NA, 9))
sub_title2 <- c("Breeding Innovations", rep (NA, 9))
sub_title3 <- c("Climate Change Adaptation Options", rep (NA, 9))
sub_title4 <- c("Mechanisation", rep (NA, 9))
sub_title5 <- c("Sustainable Intensification Practices", rep (NA, 9))

prep_table <- prep_table %>%
  rbind(sub_title1, sub_title2, sub_title3, sub_title4, sub_title5)
  
prep_table_order <- prep_table %>%
  mutate (order = case_when(Innovation == "Aqualculture and Capture fisheries" ~ 0.1,
                            Innovation == "Genetically Improved Farmed Tilapia (GIFT)-derived strains" ~ 0.2,
                            Innovation == "Breeding Innovations" ~ 0.3,
                            Innovation == "Climate Change Adaptation Options" ~ 5.1,
                            Innovation == "Climate-Smart Mapping and Adaptation Planning (CS-MAP)" ~ 5.2,
                            Innovation == "Mechanisation" ~ 4.3,
                            Innovation == "Sustainable Intensification Practices" ~ 8.1,
                            TRUE ~ row_number())) %>%
  arrange(order) %>%
  select (-order)

write.csv (prep_table_order, "Output/Multivariate_analysis/Tab9.csv",
           row.names = FALSE)  #write as csv file

table9_no_region_effect <- flextable (prep_table_order) %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body", j = 2:9) %>%
  colformat_double(j = c(2:9), digits = 2, na_str = "NA") %>%
  merge_at(part = "body", i = 1) %>%
  merge_at(part = "body", i = 3) %>%
  # merge_at(part = "body", i = 6) %>%
  merge_at(part = "body", i = 7) %>%
  merge_at(part = "body", i = 9) %>%
  merge_at(part = "body", i = 14) %>%
  bold(part = "body", i = c(1,3,7, 9,14)) %>%
  # colformat_char(j=3) %>%
  bold(i = 1, part = "header") %>%
  border_inner_h(part = "body", fp_border(width = 0.5)) %>%
  border_inner_h(part = "header", fp_border(width = 0.5)) %>%
  border(i = 1, border.top = fp_border(width = 2)) %>%
  autofit()


table9_no_region_effect

save_as_html(table9_no_region_effect, path = "Output/Multivariate_analysis/Tab9.html" )



## With regional fixed effects ----
table_22_fixed <- publish_fn (VH22_fixed) %>%
  filter (Innovation %in% c("CGIAR-related Rice Varieties", "Salt-tolerant Rice Varieties (STRVs)"))

order_dependent <- c("Genetically Improved Farmed Tilapia (GIFT)-derived strains",
                     "CGIAR-related Cassava Varieties",
                     "Cassava Mosaic Disease (CMD)-resistant Cassava Varieties",
                     "Laser Land Levelling (LLL)",
                      # "Seed blower",
                     "Combine Harvester (CHB)",
                     "Rice Straw Baler",
                     "1R: Seed rate (lenient)",
                     "2R: Nitrogen use (lenient)",
                     "3R: Pesticide use (lenient)",
                     "Three Reductions, Three Gains (3R3G) and One Must Do, Five Reductions (1M5R - lenient)",
                     "Alternate Wetting and Drying (1 dry-down)",
                     "Drum Seeder",
                     "Sustainable Water for Coffee Production",
                     "Climate-Smart Mapping and Adaptation Planning (CS-MAP)")

table_23_fixed <- publish_fn (VH23_fixed) %>%
  filter (Innovation %in% order_dependent) %>%
  mutate (order_var = factor (Innovation, levels = order_dependent)) %>%
  arrange (order_var) %>%
  select (-order_var)


prep_table_fixed <- full_join (table_22_fixed, table_23_fixed)

sub_title1 <- c("Aqualculture and Capture fisheries", rep (NA, 14))
sub_title2 <- c("Breeding Innovations", rep (NA, 14))
sub_title3 <- c("Climate Change Adaptation Options", rep (NA, 14))
sub_title4 <- c("Mechanisation", rep (NA, 14))
sub_title5 <- c("Sustainable Intensification Practices", rep (NA, 14))

prep_table_fixed <- prep_table_fixed %>%
  rbind(sub_title1, sub_title2, sub_title3, sub_title4, sub_title5)

prep_table_order_fixed <- prep_table_fixed %>%
  mutate (order = case_when(Innovation == "Aqualculture and Capture fisheries" ~ 0.1,
                            Innovation == "Genetically Improved Farmed Tilapia (GIFT)-derived strains" ~ 0.2,
                            Innovation == "Breeding Innovations" ~ 0.3,
                            Innovation == "Climate Change Adaptation Options" ~ 5.1,
                            Innovation == "Climate-Smart Mapping and Adaptation Planning (CS-MAP)" ~ 5.2,
                            Innovation == "Mechanisation" ~ 5.3,
                            Innovation == "Sustainable Intensification Practices" ~ 8.1,
                            TRUE ~ row_number())) %>%
  arrange(order) %>%
  select (-order) %>%
  filter (Innovation != "Climate Change Adaptation Options")

colnames(prep_table_order_fixed)[colnames(prep_table_order_fixed)=="Region2_NMMA"] <- "Region: NMMA"
colnames(prep_table_order_fixed)[colnames(prep_table_order_fixed)=="Region3_NCCCA"] <- "Region: NCCCA"
colnames(prep_table_order_fixed)[colnames(prep_table_order_fixed)=="Region4_Central Highlands"] <- "Region: Central Highlands"
colnames(prep_table_order_fixed)[colnames(prep_table_order_fixed)=="Region5_South East"] <- "Region: South East"
colnames(prep_table_order_fixed)[colnames(prep_table_order_fixed)=="Region6_MRD"] <- "Region: MRD"

View(prep_table_order_fixed)

write.csv (prep_table_order_fixed, "Output/Multivariate_analysis/Appendix.C.csv",
           row.names = FALSE)  #write as csv file

table9_fixed <- flextable (prep_table_order_fixed) %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body", j = 2:15) %>%
  colformat_double(j = c(2:15), digits = 2, na_str = "NA") %>%
  merge_at(part = "body", i = 1) %>%
  merge_at(part = "body", i = 3) %>%
  # merge_at(part = "body", i = 6) %>%
  merge_at(part = "body", i = 8) %>%
  merge_at(part = "body", i = 12) %>%
  bold(part = "body", i = c(1,3,8,12)) %>%
  # colformat_char(j=3) %>%
  bold(i = 1, part = "header") %>%
  border_inner_h(part = "body", fp_border(width = 0.5)) %>%
  border_inner_h(part = "header", fp_border(width = 0.5)) %>%
  border(i = 1, border.top = fp_border(width = 2)) %>%
  autofit()


table9_fixed

save_as_html(table9_fixed, path = "Output/Multivariate_analysis/Appendix.C.html" )





# 4. Plotting----
## VH22: Plotting----
plot_cluster_fn(VH22_OLS_Results, "VH22") #plot and save figures


## VH23: Plotting ----

VH23_plot <- plot_cluster_fn(VH23_OLS_Results, "VH23")

### VH23: Combine 3R3G ----
plot_1 <- VH23_plot[[12]] + 
  ggtitle("1R: Seed rate") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )

plot_2 <- VH23_plot[[14]] + 
  ggtitle("2R: Fertilizer") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )

plot_3 <- VH23_plot[[16]] + 
  ggtitle("3R: Pesticide") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )

plot_4 <- VH23_plot[[17]] + 
  ggtitle("3R3G") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )


# Combine the plots with reduced title sizes
combined_3r3g <- plot_1 | plot_2 | plot_3

ggsave (combined_3r3g, filename = "Output/Multivariate_Analysis/3R3G_Combined_OLS.png",
        width = 15, height = 8)



### Rice ----


plot_rice_1 <- VH23_plot[[5]] + 
  ggtitle("CGIAR-related Rice Varieties") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )

plot_rice_2 <- VH23_plot[[6]] + 
  ggtitle("Salt-tolerant Rice Varieties (STRVs)") + 
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 10)),  # Add space below the title
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Optional: add border
  )

# Combine the plots with reduced title sizes
combined_rice <- plot_rice_1 | plot_rice_2 

ggsave (combined_rice, filename = "Output/Multivariate_Analysis/Rice_combined.png",
        width = 15, height = 8)




