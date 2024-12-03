
#---
# Title: "VHLSS 2023 dataset: Innovations, correlates and weights"
# Authors email: "b.thanh@contractors.irri.org", "f.kosmowski@cgiar.org"
# Date: "11/2024"
# ---

# This script sources from GSO-SPIA VHLSS modules on DNA fingerprinting of cassava and tilapia at the household level (2023), 
# GSO-SPIA VHLSS modules on 1 Must, 5 Reductions (1M5R), mechanization, and coffee at the household level (2023), 
# GSO datasets on socio-economics, at household and commune levels, 
# and Report_weights.csv to generate the two datasets used for the report analysis
# The script outputs are "VH23_data.csv", "VH23_data.dic" and "Desc.stats_VH23.html"

# Note: Some correlates that were available in VH22 were dropped from the VH23: Main road is asphalt,	Household has internet,	Poor Commune label,	
# Distance to wholesale market (km),	Commune has wholesale market and 	Commune has extension agent



rm(list = ls()) #start clean

# Install and load packages ----
# Function to check and install packages

packages <- c("haven", "tidyverse", "flextable", "plyr", "expss", "sjlabelled", "fastDummies", "Hmisc")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))


library (tidyverse)
library (haven)
library (fastDummies)
library (expss)
library (summarytools)

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



# DNA cassava ----
cassava <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Cassava/Cass.vars.VH24.csv") %>%
  select (c(MATINH, MAHUYEN, MAXA, MADIABAN, HOSO, IDHO, Genotype_rec:DMC, CIAT.related)) 



cassava <- format_ID (cassava, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), width = c(2,3,5,3,3))
cassava$IDHO <- paste0 (cassava$MAXA, cassava$MADIABAN, cassava$HOSO)



# Tilapia ----

# GIFT DNA households:

gift <- read.csv("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Tilapia/GIFT.vars.VH24.csv") %>%
  select (c(hhidprovince:HH_ID, StrainB, KYDIEUTRA, Strain, Strain_present, I_Q5)) 

gift[which(gift$I_Q5== "388893714"),]$HH_ID <- 1 #post-survey edit
gift[which(gift$I_Q5== "838834349"),]$hhidcommune <- 32221 #post-survey edit
gift[which(gift$I_Q5 == "395079742"),]$hhidcommune <- 4768 #post-survey edit


gift <- gift %>%
  rename (c("MATINH" = "hhidprovince",
          "MAHUYEN" = "hhiddistrict",
          "MAXA" = "hhidcommune",
          "HOSO" = "HH_ID"))

gift <- format_ID (gift, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO"), widths = c(2,3,5,3)) %>%
  select (-I_Q5)


# GSO dataset to get EA ID:

gift_ea <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Combined_modules/M4B51A.csv")%>%
  select ("MATINH":"MADIABAN")

gift_ea <- format_ID (gift_ea, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN"), widths = c(2,3,5,3)) #format ID

gift_ea_unique <- distinct(gift_ea) #to get EA ID

gift_final <- left_join (gift, gift_ea_unique) 

gift_final$MADIABAN[is.na(gift_final$MADIABAN)] <- "005" #post-survey edit
gift_final$MAHUYEN[gift_final$MAXA == "32221"] <- "972" #post-survey edit

gift_final$IDHO <- paste0(gift_final$MAXA,
                          gift_final$MADIABAN,
                          gift_final$HOSO)


gift_final <- gift_final %>%
  mutate (StrainB_edited = case_when(StrainB == "GIFT-derived" ~ 1,
                                     StrainB != "GIFT-derived" ~ 0,
                                     TRUE ~ NA))


gift_final <- dummy_cols(gift_final, select_columns = "Strain") %>%
  select (-KYDIEUTRA) 



# 1M5R----
df_1m5r <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Final/1M5R_clean_Mar_2024/final_1m5r.csv") %>%
  mutate (lenient_1m = case_when (d_1m5r_certified == 1 | d_1m5r_certified_combined == 1 ~ 1,
                                  d_1m5r_certified != 1 & d_1m5r_certified_combined != 1 ~ 0,
                                  TRUE ~ NA),
          strict_2r = case_when (d_1m5r_nitrogen_3app == 1 & d_1m5r_nitrogen_100kg == 1 ~ 1,
                                 d_1m5r_nitrogen_3app != 1 | d_1m5r_nitrogen_100kg != 1 ~ 0,
                                 TRUE ~ NA),
          lenient_2r = case_when (d_1m5r_nitrogen_2app == 1 & d_1m5r_nitrogen_110kg == 1 ~ 1,
                                  d_1m5r_nitrogen_2app != 1 | d_1m5r_nitrogen_110kg != 1 ~ 0,
                                  TRUE ~ NA),
          strict_3r = case_when (d_1m5r_pest_3app == 1 & d_1m5r_pest_40d_sowing == 1 & d_1m5r_pest_flowering == 1 ~ 1,
                                 d_1m5r_pest_3app != 1 | d_1m5r_pest_40d_sowing != 1 | d_1m5r_pest_flowering != 1 ~ 0,
                                 TRUE ~ NA),
          lenient_3r = case_when (d_1m5r_pest_6app == 1 & d_1m5r_pest_40d_sowing == 1 & d_1m5r_pest_20d_harvest == 1  ~ 1,
                                  d_1m5r_pest_6app != 1 | d_1m5r_pest_40d_sowing != 1 | d_1m5r_pest_20d_harvest != 1  ~ 0,
                                  TRUE ~ NA))

# df_1m5r <- df_1m5r %>%
#   rename ("d_1m5r_awd_intentional" = "dummy_4r")

dup_id <- df_1m5r[which(duplicated(df_1m5r$IDHO)),]$IDHO

df_1m5r <- df_1m5r %>%
  filter (!IDHO %in% dup_id | (IDHO %in% dup_id & KYDIEUTRA == 4))


df_1m5r <- df_1m5r %>%
  select (c("MATINH" : "IDHO", "d_1m5r_certified", "lenient_1m",
            "d_1m5r_seed_100kg", "d_1m5r_seed_120kg", 
            "strict_2r", "lenient_2r",
            "strict_3r", "lenient_3r",
            "harvest_combine", "d_1m5r_harvest_timing", "d_1m5r_harvest_store",
            "awd_1drydown", "awd_1drydown_intentional", "awd_2drydown", "awd_2drydown_intentional",
            "dummy_5r")) 



df_1m5r <- format_ID (df_1m5r, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2,3,5,3,3))

df_1m5r$IDHO <- paste0(df_1m5r$MAXA,
                       df_1m5r$MADIABAN,
                       df_1m5r$HOSO)




# Mechanization ----
mech_23 <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Final/1M5R_clean_Mar_2024/mechanization_2023.csv") %>%
  select (c("MATINH" : "IDHO", "M4B11A5_C3","M4B11A6_C21", "M4B11A6_C22", "M4B11A2_C1", "KYDIEUTRA")) %>%
  mutate (mech_laser_level = case_when (M4B11A5_C3 == 1 ~ 1,
                                        M4B11A5_C3 != 1 ~ 0,
                                        TRUE ~ NA),
          mech_combine_harvester = case_when (M4B11A6_C21 == 1 ~ 1,
                                              M4B11A6_C21 == 2 ~ 0,
                                              TRUE ~ NA),
          mech_straw_baler = case_when (M4B11A6_C22 == 1 ~ 1,
                                        M4B11A6_C22 == 2 ~ 0,
                                        TRUE ~ NA),
          mech_row_drum_seeder = case_when (M4B11A2_C1 == 2 ~ 1,
                                            M4B11A2_C1 != 2 ~ 0,
                                            TRUE ~ NA),
          mech_seed_blower = case_when (M4B11A2_C1 == 3 ~ 1,
                                        M4B11A2_C1 != 3 ~ 0,
                                        TRUE ~ NA)) %>%
  select (-starts_with("M4B11")) %>%
  mutate (panel = 2023) 


mech_23 <- format_ID (mech_23, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2,3,5,3,3))
mech_23$IDHO <- paste0(mech_23$MAXA,
                       mech_23$MADIABAN,
                       mech_23$HOSO)


dup_mech <- mech_23[which(duplicated(mech_23$IDHO)),]$IDHO

mech_23 <- mech_23 %>%
  filter (!IDHO %in% dup_mech | (IDHO %in% dup_mech & KYDIEUTRA == 4)) %>%
  select (-KYDIEUTRA)


# Sustainable water use for coffee production ----

coffee <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Non-genetics/Coffee.vars.VH24.csv")

coffee <- format_ID (coffee, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), width = c(2,3,5,3,3))
coffee$IDHO <- paste0 (coffee$MAXA, coffee$MADIABAN, coffee$HOSO)

coffee <- coffee [, c(3:8,14,15)]


# CSMAP----

csmap <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Non-genetics/CSMAPs.vars.22.23.csv") %>%
  group_by (MATINH, MAHUYEN, MAXA, MADIABAN, HOSO, panel) %>%
  filter (panel == 2023) %>%
  summarise (mean_csmap = mean(CSMAP_reach, na.rm = TRUE)) %>%
  mutate (csmap_final = case_when(mean_csmap > 0 ~ 1,
                                  mean_csmap == 0 ~ 0,
                                  TRUE ~ NA)) %>%
  ungroup () %>%
  distinct()

csmap <- format_ID(csmap, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) )
csmap$panel <- as.double(csmap$panel)


# Merge innovations

innov <- full_join (cassava, gift_final) %>%
  full_join (df_1m5r) %>%
  full_join (mech_23) %>%
  full_join (coffee) %>%
  full_join (csmap) %>%
  mutate (panel = 2023)

# Not all coffee households merge with VH23. Additional households are these who did not grow rice. n=15079 is thus correct. 

table (innov$SWCP) # = +15 households? 
#table (df_23$SWCP)



# Weights ----

weight <- read.csv ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Report_weights.csv") %>%
  select (-c(weight_rice_DNA))

weight <- format_ID(weight, columns = c("MATINH", "MAXA", "MADIABAN"), c(2,5,3))

innov_weight <- innov %>%
  left_join(weight)

innov_weight$weight_cass[is.na(innov_weight$DMC)] <- NA  
innov_weight$weight_gift[is.na(innov_weight$StrainB_edited)] <- NA


# Correlates ----

# Household head's information: 

ho_thanhvien <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH23_Correlates/Ho_ThanhVien.dta") %>%
  select(c(starts_with(c("MA")), "HOSO", "IDHO", "M1A_C2", "M1A_C3", "M1A_C5", "M2_C1", "M1A_C10")) %>%
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
  select (-c("gender", "M1A_C10")) 


ho_thanhvien$edu_grade[ho_thanhvien$edu_grade == 99] <- NA

ho_thanhvien <- format_ID (ho_thanhvien, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2,3,5,3,3))
ho_thanhvien$IDHO <- paste0 (ho_thanhvien$MAXA,
                          ho_thanhvien$MADIABAN,
                          ho_thanhvien$HOSO)


# Household information (ethnic, income, expense): 
ho_thongtinho <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH23_Correlates/Ho_ThongTinHo.dta")

ho_thongtinho <- ho_thongtinho %>%
  rename (c("IDHO" = "idho",
            "MATINH" = "tinh",
            "MAHUYEN" = "huyen",
            "MAXA" = "xa",
            "MADIABAN" = "diaban",
            "HOSO" = "hoso",
            "THUNHAP" = "thunhap",
            "TONGCHITIEU" = "tongchi")) %>%
  select(c(starts_with(c("MA", "IDHO")), "HOSO", "dantocchuho", "THUNHAP", "TONGCHITIEU")) %>%
  mutate (ethnic = case_when (dantocchuho == 1 ~ 0,
                              dantocchuho != 1 ~ 1,
                              TRUE ~ NA)) %>%
  select (-dantocchuho)


ho_thongtinho <- format_ID (ho_thongtinho, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2,3,5,3,3))
ho_thongtinho$IDHO <- paste0 (ho_thongtinho$MAXA,
                          ho_thongtinho$MADIABAN,
                          ho_thongtinho$HOSO)

# Land ownership:

ho_muc4b0 <- read_dta ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH23_Correlates/Ho_muc4b0.dta") %>%
  rename(c("id_land" = "m4b0_ma",
           "land_area" = "m4b0_c3")) %>%
  pivot_wider(names_from = id_land,
              values_from = land_area,
              names_prefix = "land_area_") %>%
  mutate (land_area_sum = rowSums(select(., starts_with("land_area_")), na.rm = TRUE)) %>%
  select (- c(land_area_2 : land_area_6)) %>%
  rename (c("MATINH" = "tinh",
            "MAHUYEN" = "huyen",
            "MAXA" = "xa",
            "MADIABAN" = "diaban",
            "HOSO" = "hoso",
            "KYDIEUTRA" = "ky")) %>%
  select (-c ( idho, KYDIEUTRA)) 



ho_muc4b0 <- format_ID (ho_muc4b0, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2,3,5,3,3))
ho_muc4b0$IDHO <- paste0 (ho_muc4b0$MAXA,
                          ho_muc4b0$MADIABAN,
                          ho_muc4b0$HOSO)

# Recoding of variables

# Convert negative income to NA:
ho_thongtinho$THUNHAP [ho_thongtinho$THUNHAP < 0] <- NA # One value only

# Create Quintiles for annual consumption
ho_thongtinho$TONGCHITIEU <- ifelse (ho_thongtinho$TONGCHITIEU == 0, NA, ho_thongtinho$TONGCHITIEU) # Take out 0, or Quintiles are incorrect
ho_thongtinho$Quintiles <- Hmisc::cut2(ho_thongtinho$TONGCHITIEU, g = 5) # Categories cut-offs
ho_thongtinho$Quintiles <- as.numeric (ho_thongtinho$Quintiles)

#ho_thongtinho$Bottom_20 <- ifelse (ho_thongtinho$Quintiles == 1, 1, 0)
ho_thongtinho$Bottom_20 <- ifelse(is.na(ho_thongtinho$Quintiles), NA, as.integer(ho_thongtinho$Quintiles == 1))
ho_thongtinho$Bottom_40 <- ifelse(is.na(ho_thongtinho$Quintiles), NA, as.integer(ho_thongtinho$Quintiles %in% c(1, 2)))

#table (ho_thongtinho$Bottom_20)
#table (df_23$Bottom_20)
#table (df_23$Bottom_40)


# Merge all hh information:
hh_df_23 <- full_join (ho_thongtinho, ho_thanhvien) %>%
  full_join(ho_muc4b0) %>%
  group_by (IDHO)

df_23 <- innov_weight %>%
  left_join (hh_df_23) #join innovations with hh correlates

df_23 <- df_23 %>%
  mutate(
    DMC = case_when(
      DMC == TRUE ~ 1,
      DMC == FALSE ~ 0,
      TRUE ~ NA_real_
    ),
    CMD = case_when(
      CMD == TRUE ~ 1,
      CMD == FALSE ~ 0,
      TRUE ~ NA_real_
    )
  )


df_23$CIAT.related <- ifelse (df_23$CIAT.related == "Yes", 1, 
                              ifelse (df_23$CIAT.related == "No", 0, NA))

df_23$CIAT.related <- as.integer(df_23$CIAT.related)
df_23$CMD <- as.integer(df_23$CMD)
df_23$DMC <- as.integer(df_23$DMC)

df_23 <- df_23 %>%
  apply_labels (MATINH = "Province ID",
                MAHUYEN = "District ID",
                MAXA = "Commune ID",
                MADIABAN = "EA ID",
                HOSO = "Household ID",
                IDHO = "Household unique ID",
                # KYDIEUTRA = "Survey month/quarter",
                Genotype_rec = "Cassava genotype",
                Genotype = "Cassava genotype",
                CMD = "CMD-resistant cassava varieties (CMD2 QTL)",
                DMC = "High-starch cassava varieties (DM QTL)",
                CIAT.related = "CIAT research efforts on maintaining/improving elite cassava lines",
                StrainB = "Tilapia Strain", 
                Strain = "Tilapia Strain",
                Strain_present = "Tilapia Strain",
                StrainB_edited = "Dummy if tilapia is GIFT-derived",
                Strain_03_BEST = "Dummy if tilapia is BEST variety",
                Strain_05_Molobicus = "Dummy if tilapia is Molobicus variety",
                Strain_10_Mossambicus = "Dummy if tilapia is Mossambicus variety",
                Strain_RIA1 = "Dummy if tilapia is RIA1-derived",
                Strain_Unassigned = "Dummy if tilapia variety is unassigned",
                d_1m5r_certified = "Dummy if hh use certified seeds only",
                lenient_1m = "Dummy if hh use both certified and own seeds",
                d_1m5r_seed_100kg = "Dummy if hh apply a seed rate of 100kg/ha or less",
                d_1m5r_seed_120kg = "Dummy if hh apply a seed rate of 120kg/ha or less",
                strict_2r = "Dummy if hh comply with strict requirement in reducing fertilizer",
                lenient_2r = "Dummy if hh comply with lenient requirement in reducing fertilizer",
                strict_3r = "Dummy if hh comply with strict requirement in reducing pesticides",
                lenient_3r = "Dummy if hh comply with lenient requirement in reducing pesticides",
                harvest_combine = "Dummy if hh use a combine harvester (module 1M5R)",
                d_1m5r_harvest_timing = "Dummy if hh harvest when 80-90% of panicles are yellow-colored",
                d_1m5r_harvest_store = "Dummy if hh use hermetic/aeration bags to store",
                dummy_5r = "Dummy if hh use combine harvester and harvest at 80-90% yellow-colored",
                awd_1drydown = "Dummy if there is only 1 drydown between reproductive stage enduring at least 5 days",
                awd_1drydown_intentional = "Dummy if there is only 1 INTENTIONAL drydown between reproductive stage enduring at least 5 days",
                awd_2drydown = "Dummy if there is >= 2 drydowns, all between reproductive stage, each enduring at least 5 days",
                awd_2drydown_intentional = "Dummy if there is >= 2 INTENTIONAL drydowns, all between reproductive stage, each enduring at least 5 days",
                mech_laser_level = "Dummy if hh use a laser levelling machine",
                mech_combine_harvester = "Dummy if hh use a combine harvester (module Mechanization)",
                mech_straw_baler = "Dummy if hh use a straw baler",
                mech_row_drum_seeder = "Dummy if hh use a row/drum seeder",
                mech_seed_blower = "Dummy if hh use a seed blower",
                CSMAP_reach = "Climate-Smart Mapping and Adaptation Planning (CS-MAP)",
                panel = "Panel",
                weight_final_rice = "Weight of rice-related innovations (non-DNA)",
                weight_cass = "Weight of DNA cassava-related innovations",
                weight_gift = "Weight of DNA tilapia-related innovations",
                weight_coffee = "Weight of coffee-related innovations (non-DNA)",
                THUNHAP = "Annual income (in 1,000 VND)",
                TONGCHITIEU = "Annual consumption (in 1,000 VND)",
                Bottom_20 = "% of households in bottom 20% of annual consumption",
                Bottom_40 = "% of households in bottom 40% of annual consumption",
                ethnic = "Household head from an ethnic minority",
                rls_head = "Relationship to household head (1 = head)",
                age = "Age of household head (in years)",
                edu_grade = "Household head's highest completed grade",
                n_member = "Household size",
                male = "Dummy if household head is male",
                female= "Household head is female",
                internet = "Household has had access to internet  in the last 6 months",
                land_area_sum = "Total agricultural land managed or used by the household (in sq.meter)")


# Export dataset and dictionary of variables (VH23_data.dic)
write_labelled_csv (df_23, filename = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH23_data.csv",
                    single_file = FALSE)




# Generate descriptive statistics ----
names(df_23)
VH23_summary <- df_23 %>%
  select (c(THUNHAP, TONGCHITIEU, ethnic, Bottom_20, Bottom_40, age,
            edu_grade, n_member, female, internet, land_area_sum)) %>%
    dfSummary()
view(VH23_summary, file = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH23_Desc.stats.html")
