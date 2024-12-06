
#---
# Title: "VHLSS 2022 dataset: Innovations, correlates and weights"
# Authors email: "b.thanh@contractors.irri.org", "f.kosmowski@cgiar.org"
# Date: "08/2024"
# ---

# This script sources from GSO-SPIA VHLSS modules on DNA fingerprinting of rice at the household level (2022), 
# GSO-SPIA VHLSS modules on mechanization and straw management, at the household level (2022), GSO datasets on 
# socio-economics, at household and commune levels, and Report_weights.csv to generate the two datasets used for the report analysis
# The script outputs are "VH22_data.csv", "VH22_data.dic" and "Desc.stats_VH22.html"


rm(list = ls()) #start clean

getwd() #get current working directory

setwd("YOUR_DESTINATION") #change working directory if needed


token <- "YOUR_TOKEN" #paste your token here if needed. DELETE THIS LINE AFTER REPO IS PUBLIC

# Install and load packages ----
# Function to check and install packages

packages <- c("haven", "tidyverse", "this.path", "flextable", "plyr", "expss", "sjlabelled", "fastDummies", 
              "Hmisc", "summarytools", "httr")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))


# Packages used
library (haven)
library (tidyverse)
library (flextable)
library (expss)
library (sjlabelled)
library (fastDummies)
library (Hmisc)
library (summarytools)
library (httr)

# Download data files from GitHub and save to your working directory----

curl_function <- function (url)
{
  url_pasted <- paste0 ("https://raw.githubusercontent.com/CGIAR-SPIA/Viet-Nam-report-2024/main/", url)
  
  # Ensure the directory exists before saving the file
  dir_path <- dirname(url)  # Extract the directory path from the URL
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)  # Create the directory structure if it doesn't exist
  }
  
  response <- GET(url_pasted, add_headers(Authorization = paste("token", token)))
  writeBin(content(response, as = "raw"), url)
}

# Function to format IDs:
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


# Breeding innovations ----
curl_function("data/processed/Rice.vars.VH24.csv")

breeding_rice <- read.csv ("data/processed/Rice.vars.VH24.csv") %>%
  select (-"X") %>%
  mutate (breeding_certified_seed = case_when (M4B113_C7 ==1 ~ 1,
                                               M4B113_C7 ==2 ~ 0,
                                               TRUE ~ NA)) %>%
  rename (c("breeding_main_variety" = "M4B113_C5",
            "breeding_seed_origin" = "M4B113_C8",
            "breeding_seed_duration" = "M4B113_C9")) %>%
  select (-c("Region", "Province_name", "region", "M4B113_C7")) %>%
  relocate ("Correct_name.DNA2", .after = "breeding_main_variety")


breeding_rice <- format_ID(breeding_rice, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), c(2,3,5,3,3))
breeding_rice$IDHO <- paste0(breeding_rice$MAXA, breeding_rice$MADIABAN, breeding_rice$HOSO)  

breeding_rice$IRRI_Parentage_edited <- ifelse (breeding_rice$IRRI_Parentage %in% c('IRRI-related (P)', 'IRRI-related (P2)', 'IRRI-related line', 'Imported from IRRI Genebank'), 1, 0)



# Mechanization and straw management ----

mech_22 <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Final/1M5R_clean_Mar_2024/mechanization_2022.csv") %>%
  select (c("IDHO" : "KYDIEUTRA", "M4B111_C122", "M4B111_C123", "M4B111_C124", "M4B111_C6", "M4B111_C13", "M4B111_C14")) %>%
  mutate (mech_mini_combiner = case_when (M4B111_C122 == 1 ~ 1,
                                          M4B111_C122 == 2 ~ 0,
                                          TRUE ~ NA),
          mech_combine_harvester = case_when (M4B111_C123 == 1 ~ 1,
                                              M4B111_C123 == 2 ~ 0,
                                              TRUE ~ NA),
          mech_straw_baler = case_when (M4B111_C124 == 1 ~ 1,
                                        M4B111_C124 == 2 ~ 0,
                                        TRUE ~ NA),
          mech_row_seeder = case_when (M4B111_C6 == 2 ~ 1,
                                       M4B111_C6 != 2 ~ 0,
                                       TRUE ~ NA),
          mech_seed_blower = case_when (M4B111_C6 == 3 ~ 1,
                                        M4B111_C6 != 3 ~ 0,
                                        TRUE ~ NA),
          straw = case_when (M4B111_C13 == 1 ~ "burn",
                             M4B111_C13 == 2 ~ "mulching",
                             M4B111_C13 == 3 ~ "incorporated_soil",
                             M4B111_C13 == 4 ~ "remove_partly",
                             M4B111_C13 == 5 ~ "remove_completely",
                             M4B111_C13 == 6 ~ "other",
                             TRUE ~ NA),
          straw_rm = case_when (M4B111_C14 == 1 ~ "livestock",
                                M4B111_C14 == 2 ~ "cooking",
                                M4B111_C14 == 3 ~ "mushroom",
                                M4B111_C14 == 4 ~ "compost",
                                M4B111_C14 == 5 ~ "sold",
                                M4B111_C14 == 6 ~ "other",
                                TRUE ~ NA)) %>%
  select (-starts_with("M4B111_")) %>%
  select(-REGION) %>%
  dummy_cols (select_columns = c("straw", "straw_rm")) %>%
  select (-c(ends_with ("_NA"), "straw", "straw_rm")) 



mech_22 <- format_ID(mech_22, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), c(2,3,5,3,3))
mech_22$IDHO <- paste0(mech_22$MAXA,
                       mech_22$MADIABAN,
                       mech_22$HOSO)

# CSMAP ----
csmap <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Non-genetics/CSMAPs.vars.22.23.csv") %>%
  group_by (MATINH, MAHUYEN, MAXA, MADIABAN, HOSO, panel) %>%
  filter (panel == 2022) %>%
  summarise (mean_csmap = mean(CSMAP_reach, na.rm = TRUE)) %>%
  mutate (csmap_final = case_when(mean_csmap > 0 ~ 1,
                                  mean_csmap == 0 ~ 0,
                                  TRUE ~ NA)) %>%
  ungroup () %>%
  distinct()



csmap <- format_ID(csmap, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) )
csmap$panel <- as.double(csmap$panel)

# Merge innovations

innov <- full_join (breeding_rice, mech_22) %>%
  full_join (csmap) %>%
  mutate (panel = 2022)



# Weights ----

weight <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Report_weights.csv") %>%
  select (-c(weight_cass, weight_gift, weight_coffee)) 

weight <- format_ID(weight, columns = c("MATINH", "MAXA", "MADIABAN"), c(2,5,3))


innov_weight <- left_join (innov, weight) #innovation with weights

innov_weight$weight_rice_DNA[is.na(innov_weight$breeding_main_variety)] <- NA



# HH correlates ----

# VH22----

# Household head's information: 

ho_thanhvien_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Ho_ThanhVien.dta") %>%
  select(c(starts_with(c("MA", "IDHO")),  "HOSO", "M1A_C2", "M1A_C3", "M1A_C5", "M2_C1", "M1A_C10", "KYDIEUTRA")) %>%
  group_by(IDHO) %>%
  mutate (n_member = n()) %>%
  rename (c("gender" = "M1A_C2",
            "rls_head" = "M1A_C3", 
            "age" = "M1A_C5",
            "edu_grade" = "M2_C1")) %>%
  filter (rls_head == 1) %>%
  mutate (male = case_when(gender==1 ~ 1,
                           gender==2 ~ 0,
                           TRUE ~ NA)) %>%
  mutate (female = case_when (male == 1 ~ 0,
                              male == 0 ~ 1,
                              TRUE ~ NA)) %>%
  mutate (internet = case_when (M1A_C10 == 1 ~ 1,
                                M1A_C10 == 2 ~ 0,
                                TRUE ~ NA)) %>%
  select (-c("gender", "M1A_C10", "MAHUYEN")) 


ho_thanhvien_22$edu_grade[ho_thanhvien_22$edu_grade == 99] <- NA

# Summary of correlates
summary(ho_thanhvien_22) 

 


# Household information (ethnic, income, expense): 
ho_thongtinho_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Ho_ThongTinHo.dta") %>%
  select(c(starts_with(c("MA", "IDHO")), "HOSO", "DANTOCCHUHO", "THUNHAP", "TONGCHITIEU")) %>%
  select (-"MAHUYEN") %>%
  mutate (ethnic = case_when (DANTOCCHUHO == 1 ~ 0,
                              DANTOCCHUHO != 1 ~ 1,
                              TRUE ~ NA)) %>%
  select (-DANTOCCHUHO)





ho_muc4b0_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/ho_muc4b0.dta") %>%
  rename(c("id_land" = "M4B0_MA",
           "land_area" = "M4B0_C3")) %>%
  pivot_wider(names_from = id_land,
              values_from = land_area,
              names_prefix = "land_area_") %>%
  mutate (land_area_sum = rowSums(select(., starts_with("land_area_")), na.rm = TRUE)) %>%
  select (- c("land_area_1" : "land_area_6")) %>%
  select (-"MAHUYEN")


# Define the columns to be formated
columns_to_convert <- c("MATINH", "MAXA", "MADIABAN", "HOSO")


# Format IDs:
ho_thongtinho_22 <- format_ID(ho_thongtinho_22, columns_to_convert, c(2,5,3,3))
ho_thongtinho_22$IDHO <- paste0 (ho_thongtinho_22$MAXA, ho_thongtinho_22$MADIABAN, ho_thongtinho_22$HOSO)

ho_thanhvien_22 <- format_ID(ho_thanhvien_22, columns_to_convert, c(2,5,3,3))
ho_thanhvien_22$IDHO <- paste0 (ho_thanhvien_22$MAXA, ho_thanhvien_22$MADIABAN, ho_thanhvien_22$HOSO)

ho_muc4b0_22 <- format_ID(ho_muc4b0_22, columns_to_convert, c(2,5,3,3))
ho_muc4b0_22$IDHO <- paste0 (ho_muc4b0_22$MAXA, ho_muc4b0_22$MADIABAN, ho_muc4b0_22$HOSO)




# Merge all hh information:
hh_df_22 <- full_join (ho_thongtinho_22, ho_thanhvien_22) %>%
  full_join(ho_muc4b0_22) %>%
  group_by (IDHO) 

# Format IDs: 
hh_df_22 <- format_ID(hh_df_22, columns_to_convert, c(2,5,3,3))
hh_df_22$IDHO <- paste0(hh_df_22$MAXA, hh_df_22$MADIABAN, hh_df_22$HOSO)   #hh socio-econs all data


# Commune 2022 ----

xa_thongtinxa_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_ThongTinXa.dta") %>%
  rename (c("dist_main_str" = "M5C5", 
            "type_main_str" = "M5C7")) %>%
  mutate (poor_commune = case_when (M1C14 ==1 ~ 1,
                                    M1C14 ==2 ~ 0,
                                    TRUE ~ NA),
          dummy_main_str = case_when (M5C4 == 1 ~ 1,
                                      M5C4 == 2 ~ 0,
                                      TRUE ~ NA)) %>%
  select (-c("M1C14", "M5C4", starts_with("M7C1"))) %>%
  relocate(c("poor_commune", "dummy_main_str"), .before = "dist_main_str") %>%
  select (-"KYDIEUTRA") %>%
  select (-"MAHUYEN")



xa_muc5a_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_Muc5A.dta") %>%
  rename (c("id_market" = "M5STT1",
            "dist_market" = "M5C25")) %>%
  mutate (dummy_local_market = case_when (M5C24 == 1 ~ 1,
                                          M5C24 == 2 ~ 0,
                                          TRUE ~ NA)) %>%
  select(-"M5C24") %>%
  pivot_wider (names_from = id_market,
               values_from = c("dummy_local_market", "dist_market")) %>%
  rename (c("dummy_local_market_wholesale" = "dummy_local_market_3",
            "dist_market_wholesale" = "dist_market_3"))  %>%
  select (-"KYDIEUTRA") %>%
  select (-c ("dummy_local_market_1", "dummy_local_market_2", "dist_market_1", "dist_market_2")) %>%
  select (-"MAHUYEN")



xa_muc4c_22 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_Muc4C.dta") %>%
  rename (c("dist_ext_center" = "M4C27",
            "farmer_participation_pct" = "M4C29")) %>%
  select (-c(ends_with("GHIRO"), "M4C33", starts_with(c("M4C28", "M4C37")))) %>%
  mutate (dummy_ext_center = case_when (M4C26 ==1 ~ 1,
                                        M4C26 ==2 ~ 0,
                                        TRUE ~ NA),
          dummy_protection_staff = case_when (M4C34 == 1 ~ 1,
                                              M4C34 == 2 ~ 0,
                                              TRUE ~ NA)) %>%
  select (-c("M4C26", "M4C34")) %>%
  relocate ("dummy_ext_center", .before = "dist_ext_center") %>%
  # relocate ("dummy_protection_staff", .before = "irrigation_info_1")  %>%
  select (-"KYDIEUTRA") %>%
  select (-"MAHUYEN")





# Merge datasets: ----

# Concert IDs 
columns_to_convert <- c("MATINH", "MAXA")
ho_thongtinho_22 <- format_ID(hh_df_22, columns_to_convert, c(2,5))
ho_thanhvien_22 <- format_ID(ho_thanhvien_22, columns_to_convert, c(2,5))
ho_muc4b0_22 <- format_ID(ho_muc4b0_22, columns_to_convert, c(2,5))


commune_df_22 <- full_join (xa_thongtinxa_22, xa_muc5a_22) %>%
  full_join(xa_muc4c_22)

hh_commune_df_22 <- full_join (hh_df_22, commune_df_22) 




# Merge with innovations: 
df_22 <- left_join (innov_weight, hh_commune_df_22)



# Recode distance to wholesale market (in km)
df_22$dist_market_wholesale [is.na (df_22$dist_market_wholesale)] <- 5
df_22$dist_market_wholesale [df_22$dist_market_wholesale < 0] <- 0

# Recode Main access road is asphalt
df_22 <- df_22 %>%
  mutate (main_str_asphalt = case_when (type_main_str == 1 ~ 1,
                                        type_main_str != 1 ~ 0,
                                        TRUE ~ NA))

# Recode distance to extension center
df_22$dist_ext_center [is.na (df_22$dist_ext_center)] <- 5

# Convert negative income to NA:
df_22$THUNHAP [df_22$THUNHAP < 0] <- NA





# rm(list=setdiff(ls(), c("df_22")))

# Create Quintiles for annual consumption
df_22$TONGCHITIEU <- ifelse (df_22$TONGCHITIEU == 0, NA, df_22$TONGCHITIEU) # Take out 0, or Quintiles are incorrect

df_22$Quintiles <- Hmisc::cut2(df_22$TONGCHITIEU, g = 5) # Categories cut-offs
df_22$Quintiles <- as.numeric (df_22$Quintiles)

df_22$Bottom_20 <- ifelse (df_22$Quintiles == 1, 1, 0)
df_22$Bottom_40 <- ifelse(is.na(df_22$Quintiles), NA, as.integer(df_22$Quintiles %in% c(1, 2)))


# Recode QTLs 
df_22 <- df_22 %>%
  mutate(
    Sub1 = if_else(qSub1 == "[+p]", 1,
                   if_else(qSub1 == "?" | qSub1 == "[--]", 0, NA_real_))
    ,
    Saltol = case_when(
      Saltol == "[+p]-Aro" ~ 1,
      Saltol == "[+p]-Aus" ~ 1,
      Saltol == "?" ~ 0,
      Saltol == "[--]" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Labelled correlates vars:----
df_22 <- df_22%>%
  apply_labels (IDHO = "HH unique ID",
                HOSO = "HH ID",
                MATINH = "Province ID",
                MAHUYEN = "District ID",
                MAXA = "Commune ID",
                MADIABAN = "EA ID",
                breeding_main_variety = "Self-reported name of the main variety in the plot",
                Correct_name.DNA2 = "DNA-based rice varietal identification",
                breeding_certified_seed = "Self-reported certified seeds for sample (1 = Certified, 0 = Not certified)",
                breeding_seed_origin = "Origin of the sampled seeds (1-Self-produced, 2-Seed club, 3-Seed company, 4-Research institute, 5-Extension center, 6-Cooperative, 7-Retailer, 8-Other)",
                breeding_seed_duration = "Number of season the seed was recycled",
                IRRI_Parentage = "DNA-based IRRI-relatedness",
                Saltol = "Salt-tolerance QTL",
                qSub1 = "Submergence-tolerance QTL",
                mech_mini_combiner = "Dummy if mini-combine harvester is used for harvesting",
                mech_combine_harvester = "Dummy if combine harvester is used for harvesting",
                mech_straw_baler = "Dummy if straw baler is used for harvesting",
                mech_row_seeder = "Dummy if row seeder is used for seeding in the last W-S season",
                mech_seed_blower = "Dummy if seed blower is used for seeding in the last W-S season",
                straw_burn = "Dummy if straws are burned on the plof after harvesting",
                straw_incorporated_soil = "Dummy if straws are incorporated to the soil plot after harvesting",
                straw_mulching = "Dummy if straws are left on the plot for mulching after harvesting",
                straw_other = "Dummy if straws are used for other purposes after harvesting",
                straw_remove_completely = "Dummy if straws are removed completely from the plot after harvesting",
                straw_remove_partly = "Dummy if straws are removed partially from the plot after harvesting",
                straw_rm_compost = "Dummy if straws are removed and used for compost",
                straw_rm_cooking = "Dummy if straws are removed and used for cooking",
                straw_rm_livestock = "Dummy if straws are removed and fed for livestock",
                straw_rm_mushroom = "Dummy if straws are removed and used for mushroom cultivation",
                straw_rm_other = "Dummy if straws are removed and used for other purposes",
                straw_rm_sold = "Dummy is straws are removed and sold",
                CSMAP_reach = "Climate-Smart Mapping and Adaptation Planning (CS-MAP)",
                weight_final_rice = "Weight for rice-related innovations",
                weight_rice_DNA = "Weight for rice DNA subsample",
                ethnic = "Household head from an ethnic minority",
                THUNHAP = "Annual income (in 1,000 VND)",
                TONGCHITIEU = "Annual consumption (in 1,000 VND)",
                Bottom_20 = "% of households in bottom 20% of annual consumption",
                Bottom_40 = "% of households in bottom 40% of annual consumption",
                rls_head = "Relationship to household head (1 = head)",
                age = "Age of household head (in years)",
                edu_grade = "Household head's highest completed grade",
                KYDIEUTRA = "Survey month/quarter",
                n_member = "Household size",
                male = "Dummy if household head is male",
                female= "Household head is female",
                internet = "Household has had access to internet  in the last 6 months",
                land_area_sum = "Total agricultural land managed or used by the household (in sq.meter)",
                poor_commune = "Poor Commune label",
                dummy_main_str = "Dummy if the commune has main access street",
                dist_main_str = "Distance to main access street (=5km if commune has main street)",
                type_main_str = "Type of main access street (1-Asphalt, 2-Tar on gravel, 3-Gravel, 4-Dirth or earth)",
                dummy_local_market_wholesale = "Commune has wholesale market",
                dist_market_wholesale = "Distance to wholesale market (km) (=5km if commune has wholesale market)",
                dummy_ext_center = "Dummy if commune has extension center",
                dist_ext_center = "Distance to extension center (=5km if commune has extension center)",
                farmer_participation_pct = "% farmers' participation and engagement with extension center/staff",
                dummy_protection_staff = "Commune has extension agent",
                panel = "Panel year",
                main_str_asphalt = "Main road is asphalt")


# Export dataset and dictionary of variables (VH22_data.dic)
write_labelled_csv (df_22, filename = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH22_data.csv",
                    single_file = FALSE)






# Generate Descriptive statistics ----
names(df_22)
VH22_summary <- df_22 %>%
  select (c(THUNHAP, TONGCHITIEU, ethnic, age, edu_grade,
            n_member, female, internet,
            land_area_sum, poor_commune, dummy_main_str,
            dist_main_str, dummy_local_market_wholesale,
            dist_market_wholesale, dummy_ext_center,
            dist_ext_center, dummy_protection_staff, main_str_asphalt,
            Bottom_20, Bottom_40)) %>%
  dfSummary()
view(VH22_summary, file = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH22_Desc.stats.html")
