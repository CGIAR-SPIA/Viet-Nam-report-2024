

#---
# Title: "Table 5. Overview of socio-economic variables used to characterize the adopters of CGIAR-related innovations in Viet Nam"
# Author email: "b.thanh@contractors.irri.org"
# Date: "11/2024"
# ---

# This script sources from the VH22_data.csv and VH23_data.csv. 
# VH22_data.csv (innovations, weights and correlates), at household level. Its dictionary is VH22_data.dic.csv. | VH23_data.csv (innovations, weights and correlates), at household level. Its dictionary is VH23_data.dic.csv
# Its outputs is Corr_desc_22_23.docx, published in the report as "Table 5. Overview of socio-economic variables used to characterize the adopters of CGIAR-related innovation in Viet Nam"



rm(list = ls()) #start clean

# Install packages ----
# Function to check and install packages

packages <- c("tidyverse", "flextable", "plyr", "ggplot2", "officer")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

# Load packages ----

library (tidyverse)
library (flextable)
library (ggplot2)
library (officer)

# Function to format ID values
format_ID <- function(df, columns, widths, pad_char = "0") {
  # Loop through each column and its corresponding width
  for (i in seq_along(columns)) {
    column <- columns[i]
    width <- widths[i]
    
    # Pad the column with the specified width and character
    df[[column]] <- str_pad(df[[column]], width = width, pad = pad_char)
  }
  return(df)
}

# Define columns and widths
columns <- c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO")
widths <- c(2, 3, 5, 3, 3)

## VH22----

df_22 <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH22_data.csv") %>%
  mutate (THUNHAP_mil = THUNHAP/1000,
          TONGCHITIEU_mil = TONGCHITIEU/1000,
          land_area_ha = land_area_sum / 10000) %>%
  mutate (panel = 2022)
df_22 <- format_ID (df_22, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), 
                    widths = c (2, 3, 5, 3, 3)) %>%
  mutate (IDHO = paste0 (MAXA, MADIABAN, HOSO))


## VH23----
df_23 <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH23_data.csv") %>%
  mutate (THUNHAP_mil = THUNHAP/1000,
          TONGCHITIEU_mil = TONGCHITIEU/1000,
          land_area_ha = land_area_sum / 10000) %>%
  mutate (panel = 2023)
df_23 <- format_ID (df_23, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), 
                    widths = c (2, 3, 5, 3, 3)) %>%
  mutate (IDHO = paste0 (MAXA, MADIABAN, HOSO))


## Combine df----
df <- full_join (df_22, df_23)



## List of results by year----
var_group_stat <- c ("ethnic",
                     "age",
                     "edu_grade",
                     "n_member",
                     "female",
                     "internet",
                     "THUNHAP_mil",
                     "TONGCHITIEU_mil",
                     "land_area_ha",
                     "poor_commune",
                     "main_str_asphalt",
                     "dist_market_wholesale",
                     "dist_ext_center")




var_name_group_stat <- c ("% of household head is an ethnic minority", 
                          "Age of household head (in years)",
                          "Household head's highest completed grade",
                          "Household size",
                          "% of households with female head",
                          "% of households with internet access",
                          "Annual income (in million VND)",
                          "Annual consumption (in million VND)",
                          "Total agricultural land managed or used by the household (in ha)",
                          "Commune is labeled as poor (%)",
                          "Main road is asphalt (%)", 
                          "Distance to wholesale market (in km)",
                          "Distance to extension center (in km)")

result <- list () #to store result


for (i in (1:length(var_group_stat))) {
  stat <- df %>%
    group_by (panel) %>%
    summarise (mean = mean (.data[[var_group_stat[i]]], na.rm = TRUE),
               sd = sd (.data[[var_group_stat[i]]], na.rm = TRUE)) %>%
    mutate (name = var_group_stat[[i]])
  result[[i]] <- stat
}
  
result  #list of mean and sd by year



## Edit result table----
tab_result <- bind_rows(result) %>%
  pivot_wider (names_from = "panel", values_from = c(mean, sd)) %>%
  relocate (ends_with("2023"), .after = ends_with("2022")) #combine result to dataframe

var_pct <- c("ethnic", "female", "internet", "poor_commune", "main_str_asphalt") #list of variables in %

col_pct <- tab_result %>%
  select (starts_with(c("mean", "sd"))) %>%
  colnames()  

for (col in col_pct){ #convert vars to % 
    tab_result[[col]][tab_result$name %in% var_pct] <- tab_result[[col]][tab_result$name %in% var_pct] * 100
}

tab_result <- tab_result %>%
  mutate (Variable = var_name_group_stat) %>%
  mutate (order = case_when (name == "female" ~ 1,
                             name == "ethnic" ~ 2,
                             name == "internet" ~ 3,
                             name == "land_area_ha" ~ 4,
                             name == "age" ~ 5,
                             name == "edu_grade" ~ 6,
                             name == "n_member" ~ 7,
                             name == "THUNHAP_mil" ~ 8,
                             name == "TONGCHITIEU_mil" ~ 9,
                             name == "poor_commune" ~ 10,
                             name == "main_str_asphalt" ~ 11,
                             name == "dist_market_wholesale" ~ 12,
                             name == "dist_ext_center" ~ 13)) %>%
  arrange(order) %>%
  relocate (Variable, .before = everything ()) %>%
  select (-c(order, name))
  

ft_result <- flextable(tab_result) %>%
  delete_part (part = "header") %>%
  add_header_row(values = c("Variable", "Mean", "SD", "Mean", "SD")) %>%
  add_header_row(values = c("", "VHLSS 2022", "VHLSS 2023"), 
                 colwidths = c(1,2,2)) %>%
  align(align = "center", part = "all", j = 2:5) %>%
  colformat_double(j = c(2:5),digits = 2, na_str = "-", nan_str = "-") %>%
  bold(i = 1, part = "header") %>%
  bold (i = 2, part = "header") %>% 
  theme_vanilla() %>%
  autofit()


ft_result
save_as_docx (ft_result, 
              path = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Tab5.docx",
              pr_section = prop_section(page_size(orient = "landscape")))
