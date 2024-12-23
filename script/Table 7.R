

#---
# Title: "Table 7. Adoption rate at enumeration area (EA) and household levels (in %), and estimates for the absolute numbers of households"
# Author email: "b.thanh@contractors.irri.org"
# Date: "11/2024"
# ---

# This script sources from VH22_data.csv and VH23_data.csv
# Its outputs are Tab7.html / Tab7.docx


rm (list = ls()) #start clean


# Install and load packages ----
# Function to check and install packages

packages <- c("tidyverse", "rio", "readxl", "haven", "dplyr", "fastDummies", "flextable", "officer", "plyr")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

library (tidyverse)
library (dplyr)
library (fastDummies)
library (flextable)
library (officer)
library (haven)
library (readxl)


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


# Function to curl data ----


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

output_dir <- "Output"

# Check if the directory exists, if not, create it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Load data----
curl_function ("data/processed/VH22_data.csv")
df_22 <- read.csv ("data/processed/VH22_data.csv")
df_22 <- format_ID(df_22, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) )
df_22$IDHO <- paste0 (df_22$MAXA, df_22$MADIABAN, df_22$HOSO)


curl_function ("data/processed/VH23_data.csv")
df_23 <- read.csv ("data/processed/VH23_data.csv")
df_23 <- format_ID(df_23, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) )
df_23$IDHO <- paste0 (df_23$MAXA, df_23$MADIABAN, df_23$HOSO)

df_23 <- df_23 %>%
  rename ("mech_row_seeder" = "mech_row_drum_seeder")

df <- full_join (df_22, df_23)



# PFES data:
curl_function ("data/raw/VHLSS_2024_Commune/SPIA_ThongTinXa_Q1-3.csv")


pfes <- read.csv ("data/raw/VHLSS_2024_Commune/SPIA_ThongTinXa_Q1-3.csv") %>%
  select (c(MATINH, MAHUYEN, MAXA, M43_C1)) %>%
  mutate (pfes_dummy = case_when (M43_C1 == 1 ~ 1,
                                  TRUE ~ 0)) %>%
  select (-M43_C1) %>%
  mutate (panel = 2024)

pfes <- format_ID(pfes, columns = c("MATINH", "MAHUYEN", "MAXA"), widths = c(2,3,5))


curl_function ("data/raw/Weight/Census_household_communelevel_clean.csv")
n_hh_pop <- read.csv ("data/raw/Weight/Census_household_communelevel_clean.csv") %>%
  select (c(MATINH, MAXA, n_hh)) %>%
  rename (n_hh_pop = n_hh) 
#merge by Commune ID (MAXA) because of some administrative change 
# (486 missing if merge by prov, dist, comm ID --> 470 missing if merge by prov and comm ID)

n_hh_pop <- format_ID(n_hh_pop, columns = c("MATINH", "MAXA"), widths = c(2, 5))

pfes_joined <- pfes %>%
  left_join (n_hh_pop)


# Merge to one dataset:

df <- df%>%
  full_join (pfes_joined)  

#Prep data:
df <- df %>%
  mutate (CMD_edited = case_when(CMD == TRUE ~ 1,
                                 CMD == FALSE ~ 0,
                                 TRUE ~ NA),
          DMC_edited = case_when (DMC == TRUE ~ 1,
                                  DMC == FALSE ~ 0,
                                  TRUE ~ NA)) %>%
  mutate (lenient_3r3g = case_when (d_1m5r_seed_120kg == 1 & lenient_2r == 1 & lenient_3r == 1 ~ 1,
                                    d_1m5r_seed_120kg != 1 | lenient_2r != 1 | lenient_3r != 1 ~ 0,
                                    TRUE ~ NA),
          strict_3r3g = case_when (d_1m5r_seed_100kg == 1 & strict_2r == 1 & strict_3r ==1 ~ 1,
                                   d_1m5r_seed_100kg != 1 | strict_2r != 1 | strict_3r !=1 ~ 0)) %>%
  mutate (ID_EA = paste0 (MAXA, MADIABAN)) 



# Adoption table ----

## Adoption at HH level and reach of adoption ----


### Rice ----

var_list <- c(
  "csmap_final",
  "mech_mini_combiner", "mech_combine_harvester", "mech_straw_baler",
  "mech_laser_level", "mech_row_seeder", "mech_seed_blower",
  
  "lenient_1m", "d_1m5r_certified", 
  "d_1m5r_seed_120kg", "d_1m5r_seed_100kg", 
  "lenient_2r", "strict_2r",
  "lenient_3r", "strict_3r",
  "lenient_3r3g", "strict_3r3g",
  "awd_1drydown",
  "awd_2drydown",
  "straw_rm_livestock", "straw_rm_mushroom", 
  "straw_rm_compost")







# % OF SURVEYED HOUSEHOLDS THAT ADOPT INNOVATION: 

adoption_hh <- list()  #prep result list

for (var in var_list){
  adopt <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (panel, .data[[var]]) %>%
    summarise (twt = sum(weight_final_rice, na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    select (-c(sum_twt))
  
  adoption_hh[[var]] <- adopt
  
}


adoption_hh



### Rice DNA reach: ----


dna_list <- c("Sub1", "Saltol", "IRRI_Parentage_edited")

reach_dna <- list()

for (var in dna_list) {
  
  adopt_dna <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (panel, .data[[var]]) %>%
    summarise (twt = sum(weight_rice_DNA, na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    select (-c(sum_twt))
  
  
  reach_dna[[var]] <- adopt_dna
}

reach_dna




### Cassava reach ----


cassava_list <- c("CMD_edited", "DMC_edited", "CIAT.related")

reach_cassava <- list()

for (var in cassava_list) {
  
  adopt_cassava <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (panel, .data[[var]]) %>%
    summarise (twt = sum(weight_cass, na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    select (-c(sum_twt))
  
  
  reach_cassava[[var]] <- adopt_cassava
}

reach_cassava





### Tilapia reach ----

tilapia_list <- c("StrainB_edited")

reach_tilapia <- list()

for (var in tilapia_list) {
  
  adopt_tilapia <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (panel, .data[[var]]) %>%
    summarise (twt = sum(weight_gift, na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    select (-c(sum_twt))
  
  
  reach_tilapia[[var]] <- adopt_tilapia
}

reach_tilapia




### Coffee reach ----


coffee_list <- c("SWCP")

reach_coffee <- list()

for (var in coffee_list) {
  
  adopt_coffee <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (panel, .data[[var]]) %>%
    summarise (twt = sum(weight_coffee, na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    select (-c(sum_twt))
  
  
  reach_coffee[[var]] <- adopt_coffee
}

reach_coffee



### PFES reach----

reach_pfes <- df %>%
  filter (!is.na(pfes_dummy)) %>%
  group_by (panel, pfes_dummy) %>%
  summarise (twt = sum (n_hh_pop, na.rm = TRUE)) %>%
  group_by (panel) %>%
  mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
  filter (pfes_dummy == 1) %>%
  select (-c(sum_twt))



### CSMAPs----



## Adoption at EA level:----
all_var <-  c(
  "csmap_final",
  "mech_mini_combiner", "mech_combine_harvester", "mech_straw_baler",
  "mech_laser_level", "mech_row_seeder", "mech_seed_blower",
  
  "lenient_1m", "d_1m5r_certified", 
  "d_1m5r_seed_120kg", "d_1m5r_seed_100kg", 
  "lenient_2r", "strict_2r", 
  "lenient_3r", "strict_3r", 
  "lenient_3r3g", "strict_3r3g",
  "awd_1drydown",
  "awd_2drydown",
                        
  "straw_rm_livestock", "straw_rm_mushroom", 
  "straw_rm_compost",
  "Sub1", "Saltol", "IRRI_Parentage_edited",
  "CMD_edited", "DMC_edited", "CIAT.related",
  "StrainB_edited",
  "SWCP",
  "pfes_dummy")

adopt_EA <- list ()

for (var in all_var){
  adopt_ea <- df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(ID_EA, panel) %>%
    dplyr::summarize(mean_adopt_EA = mean(.data[[var]], na.rm = TRUE)) %>%
    group_by (panel) %>%
    mutate (adopt_EA = case_when (mean_adopt_EA > 0 ~ 1,
                                  mean_adopt_EA == 0 ~ 0,
                                  TRUE ~ NA)) %>%
    group_by (panel, adopt_EA) %>%
    summarise (n_EA = n()) %>%
    mutate (pct_adopt_EA = n_EA * 100 / sum(n_EA)) %>%
    filter (adopt_EA == 1 ) %>%
    select (c(panel, pct_adopt_EA))
  
  adopt_EA[[var]] <- adopt_ea
  
}

adopt_EA




## Unlist all results: ----

library (plyr)

tab_adoption_rice <- ldply (adoption_hh) %>%
  select (c(".id", "panel", "twt", "adoption"))

tab_adoption_dna <- ldply (reach_dna) %>%
  select (c(".id", "twt", "adoption")) %>%
  mutate ("panel" = 2022)


tab_adoption_cassava <- ldply (reach_cassava) %>%
  select (c(".id", "twt", "adoption")) %>%
  mutate ("panel" = 2023)

tab_adoption_tilapia <- ldply (reach_tilapia) %>%
  select (c(".id", "twt", "adoption")) %>%
  mutate ("panel" = 2023)

tab_adoption_coffee <- ldply (reach_coffee) %>%
  select (c(".id", "twt", "adoption")) %>%
  mutate ("panel" = 2023)

tab_adoption_ea <- ldply (adopt_EA)



detach("package:plyr", unload = TRUE)



## Edit column names of result table: ----

tab_adoption_rice <- tab_adoption_rice %>%
  rename (reach = twt,
          adopt_hh = adoption) %>%
  pivot_wider(names_from = panel, values_from = c(adopt_hh, reach))


tab_adoption_dna <- tab_adoption_dna %>%
  rename (reach = twt,
          adopt_hh = adoption) %>%
  pivot_wider(names_from = panel, values_from = c(adopt_hh, reach))


tab_adoption_cassava <- tab_adoption_cassava %>%
  rename (reach = twt,
          adopt_hh = adoption) %>%
  pivot_wider(names_from = panel, values_from = c(adopt_hh, reach))


tab_adoption_tilapia <- tab_adoption_tilapia %>%
  rename (reach = twt,
          adopt_hh = adoption) %>%
  pivot_wider(names_from = panel, values_from = c(adopt_hh, reach))

tab_adoption_coffee <- tab_adoption_coffee %>%
  rename (reach = twt,
          adopt_hh = adoption) %>%
  pivot_wider(names_from = panel, values_from = c(adopt_hh, reach))


tab_adoption_ea <- tab_adoption_ea %>%
  rename (adopt_EA = pct_adopt_EA) %>%
  pivot_wider(names_glue = "{.value}_{panel}", names_from = panel, values_from = adopt_EA)


tab_adoption_pfes <- reach_pfes %>%
  rename (c("reach" = "twt",
            ".id" = "pfes_dummy")) %>%
  pivot_wider(names_glue = "{.value}_{panel}", names_from = panel, values_from = reach)
tab_adoption_pfes[1] <- "pfes_dummy"




result <- full_join (tab_adoption_rice, tab_adoption_dna) %>%
  full_join (tab_adoption_cassava) %>%
  full_join (tab_adoption_tilapia) %>%
  full_join (tab_adoption_coffee) %>%
  full_join (tab_adoption_pfes) %>%
  full_join (tab_adoption_ea) %>%
  rename (var = ".id") %>%
  relocate (c("adopt_EA_2022", "adopt_hh_2022", "reach_2022", "adopt_EA_2023", "adopt_hh_2023", "reach_2023", "adopt_EA_2024", "reach_2024"),
            .after = "var")






## Final results output: ----


result <- result %>%
  mutate(
    order = case_when(
      var == "CMD_edited" ~ 0.95,
      var == "DMC_edited" ~ 0.96,  #reorder CMD and DMC
      var == "CIAT.related" ~ 0.94,
      var == "StrainB_edited" ~ 0.4,
      var == "Sub1" ~ 0.99,
      var == "Saltol" ~ 0.98,
      var == "IRRI_Parentage_edited" ~ 0.97,
      var == "mech_laser_level" ~ 2,
      var == "mech_row_seeder" ~ 20.1,
      var == "mech_seed_blower" ~ 2.9,
      var == "mech_combine_harvester" ~ 2.2,
      var == "mech_mini_combiner" ~ 2.3,
      var == "mech_straw_baler" ~ 2.4,
      var == "pfes_dummy" ~ 1.9,
      var == "csmap_final" ~ 1.8,
      TRUE ~ as.numeric(row_number())
    )
  ) %>%
  arrange(order) %>%
  select(-order)








# Print Table 7----

var_name <- c(
  "Genetically Improved Farmed Tilapia (GIFT)-derived strains",
  "CGIAR-related Cassava Varieties",
  "Cassava Mosaic Disease (CMD)-resistant Cassava Varieties",
  "High-starch cassava varieties (DM QTL)",
  "CGIAR-related Rice Varieties",
  "Salt-tolerant Rice Varieties (STRVs)",
  "Submergence-tolerant Rice Varieties",
  "Climate-Smart Mapping and Adaptation Planning (CS-MAP)",
  "Payment for Forest Environmental Services (PFES)",
  "Laser Land Levelling (LLL)",
  "Combine Harvester (CHB)",
  "Mini-Combine Harvester (MCHB)",
  "Rice Straw Baler", 
  "Household used seed blower",
  "Lenient 1M (Households combined certified seeds and own seeds)",
  "Strict 1M (Households used totally certified seeds)", 
  "Lenient 1R (Households adopted maximum 120kg/ha seed rates)",
  "Strict 1R (Households adopted maximum 100kg/ha seed rates)",
  "Lenient 2R (Maximum 110kg/ha of Nitrogen and minimum 2 applications)",
  "Strict 2R (Maximum 100kg/ha of Nitrogen and minimum 3 applications)",
  "Lenient 3R (Maximum 6 applications and not within 20 days before harvest)",
  "Strict 3R (Maximum 3 applications of chemicals, not within 40 days after sowing, not after flowering)",
  "Three Reductions, Three Gains (3R3G) and One Must Do, Five Reductions (1M5R)",
  "Strict 3R3G (Households adopted all the three reductions, using strict criteria)",
  "Alternate Wetting and Drying (AWD)",
  "At least 2 dry-downs, all between reproductive stage, each enduring at least 5 days",
  "Households used removed straws to feed for livestock",
  "Drum Seeder",
  "Off-field Straw Management Practices",
  "Households used removed straws for compost", 
  "Sustainable Water Use for Coffee Production"
  )



table7_print <- result %>%
  mutate (var_name = var_name) %>%
  relocate (var_name, .before = everything()) %>%
  select (-var) 


var_remove <- c("High-starch cassava varieties (DM QTL)",
                "Households used removed straws to feed for livestock",
                "Households used removed straws for compost",
                "Household used seed blower",
                "Lenient 1M (Households combined certified seeds and own seeds)",
                "Strict 1M (Households used totally certified seeds)",
                "Lenient 1R (Households adopted maximum 120kg/ha seed rates)",
                "Strict 1R (Households adopted maximum 100kg/ha seed rates)",
                "Lenient 2R (Maximum 110kg/ha of Nitrogen and minimum 2 applications)",
                "Strict 2R (Maximum 100kg/ha of Nitrogen and minimum 3 applications)",
                "Lenient 3R (Maximum 6 applications and not within 20 days before harvest)",
                "Strict 3R (Maximum 3 applications of chemicals, not within 40 days after sowing, not after flowering)",
                "At least 2 dry-downs, all between reproductive stage, each enduring at least 5 days",
                "Strict 3R3G (Households adopted all the three reductions, using strict criteria)")



table7_print <- table7_print %>%
  filter (!var_name %in% var_remove)


table7_print <- table7_print %>%
  mutate (group_name = c(
    rep ("Aquaculture and Capture Fisheries", 1),
    rep ("Breeding Innovations", 5),
    "Climate Change Adaptation Options",
    "Environmental Conservation",
    rep ("Mechanization", 4),
    rep("Sustainable Intensification Practices", 5))) %>%
  relocate (group_name, .before = "var_name") 






ft <- flextable(table7_print) %>%
  delete_part(part = "header") %>%
  add_header_row(values = c("", "", 
                            "%EA", "%HH", "Estimated number of hh", 
                            "%EA", "%HH", "Estimated number of hh",
                            "%EA", "Estimated number of hh")) %>%
  add_header_row(values = c("Core Domain", "Innovation", "2022", "2023", "2024"), 
                 colwidths = c(1, 1, 3, 3, 2)) %>%
  align(align = "center", part = "header") %>%
  # align(align = "center", part = "header", i = 1) %>%
  merge_v(j = 1) %>% 
  valign(j = 1, valign = "top") %>% 
  colformat_double(j = c(3,4,6,7,9),digits = 1) %>%
  # colformat_double(j = 7:8, digits = 1) %>%
  colformat_double(j = 5, digits = 0) %>%
  colformat_double(j = 8, digits=  0) %>%
  bold(i = 1, part = "header") %>%
  bold (i = 2, part = "header") %>% 
  border_inner_h(part = "body", fp_border(width = 0.5)) %>%
  border_inner_h(part = "header", fp_border(width = 0.5)) %>%
  border(i = 1, border.top = fp_border(width = 2)) %>%
  bold(j = 1, part = "body") %>%
  footnote (part = "header", i = 1, j = 9, value = as_paragraph("Preliminary data - from the first three quarters of VHLSS 2024"), ref_symbols = "1") %>%
  autofit()

ft

print(ft)


save_as_html (ft,
              path = "Output/Tab7.html",
              page_size(orient = "landscape"))


save_as_docx(ft, 
             path = "Output/Tab7.docx",
             pr_section = prop_section(page_size = page_size(orient = "landscape")))



