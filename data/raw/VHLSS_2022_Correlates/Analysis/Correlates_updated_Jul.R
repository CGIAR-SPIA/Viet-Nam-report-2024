rm(list = ls())

#setwd(here())


library (haven)
library (tidyverse)

# Farming 22----

hh_ag_22 <- read.csv ("C:/Users/simon/Dropbox/DATA/VHLSS_Household_2022/datasets/Ho_Muc4B11_edited.csv") %>%
  select (c("IDHO", "HOSO", starts_with("MA")))



hh_ag_22$MATINH <- str_pad(hh_ag_22$MATINH, width = 2, pad = "0")
hh_ag_22$MAHUYEN <- str_pad(hh_ag_22$MAHUYEN, width = 3, pad = "0")
hh_ag_22$MAXA <- str_pad(hh_ag_22$MAXA, width = 5, pad = "0")
hh_ag_22$MADIABAN <- str_pad(hh_ag_22$MADIABAN, width = 3, pad = "0")
hh_ag_22$HOSO <- str_pad(hh_ag_22$HOSO, width = 3, pad = "0")
hh_ag_22$IDHO <- paste0 (hh_ag_22$MAXA, hh_ag_22$MADIABAN, hh_ag_22$HOSO)

# hh_ag_22 <- hh_ag_22 %>%
#   mutate (panel = 2022)


unique_hh_ag_22 <- hh_ag_22[!duplicated(hh_ag_22$IDHO), ]





# HH correlates ----

## VH22----


ho_thanhvien_22 <- read_dta ("C:/Users/simon/Dropbox/DATA/VH22_Correlates/Ho_ThanhVien.dta") %>%
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
  select (-c("gender", "M1A_C10"))


ho_thanhvien_22$edu_grade[ho_thanhvien_22$edu_grade == 99] <- NA



ho_thongtinho_22 <- read_dta ("C:/Users/simon/Dropbox/DATA//VH22_Correlates/Ho_ThongTinHo.dta") %>%
  select(c(starts_with(c("MA", "IDHO")), "HOSO", "DANTOCCHUHO", "THUNHAP", "TONGCHITIEU"))





ho_muc4b0_22 <- read_dta ("C:/Users/simon/Dropbox/DATA//VH22_Correlates/ho_muc4b0.dta") %>%
  rename(c("id_land" = "M4B0_MA",
             "land_area" = "M4B0_C3")) %>%
  pivot_wider(names_from = id_land,
              values_from = land_area,
              names_prefix = "land_area_") %>%
  mutate (land_area_sum = rowSums(select(., starts_with("land_area_")), na.rm = TRUE)) %>%
  select (- c("land_area_1" : "land_area_6"))





ho_thongtinho_22$MATINH <- as.character(ho_thongtinho_22$MATINH)
ho_thongtinho_22$MAHUYEN <- as.character(ho_thongtinho_22$MAHUYEN)
ho_thongtinho_22$MAXA <- as.character(ho_thongtinho_22$MAXA)
ho_thongtinho_22$MADIABAN <- as.character(ho_thongtinho_22$MADIABAN)
ho_thongtinho_22$IDHO <- as.character(ho_thongtinho_22$IDHO)



ho_thanhvien_22$MATINH <- as.character(ho_thanhvien_22$MATINH)
ho_thanhvien_22$MAHUYEN <- as.character(ho_thanhvien_22$MAHUYEN)
ho_thanhvien_22$MAXA <- as.character(ho_thanhvien_22$MAXA)
ho_thanhvien_22$MADIABAN <- as.character(ho_thanhvien_22$MADIABAN)
ho_thanhvien_22$IDHO <- as.character(ho_thanhvien_22$IDHO)




ho_muc4b0_22$MATINH <- as.character(ho_muc4b0_22$MATINH)
ho_muc4b0_22$MAHUYEN <- as.character(ho_muc4b0_22$MAHUYEN)
ho_muc4b0_22$MAXA <- as.character(ho_muc4b0_22$MAXA)
ho_muc4b0_22$MADIABAN <- as.character(ho_muc4b0_22$MADIABAN)
ho_muc4b0_22$IDHO <- as.character(ho_muc4b0_22$IDHO)


hh_df_22 <- full_join (ho_thongtinho_22, ho_thanhvien_22) %>%
  full_join(ho_muc4b0_22) %>%
  group_by (IDHO) 




hh_df_22$MATINH <- str_pad(hh_df_22$MATINH, width = 2, pad = "0")
hh_df_22$MAHUYEN <- str_pad(hh_df_22$MAHUYEN, width = 3, pad = "0")
hh_df_22$MAXA <- str_pad(hh_df_22$MAXA, width = 5, pad = "0")
hh_df_22$MADIABAN <- str_pad(hh_df_22$MADIABAN, width = 3, pad = "0")
hh_df_22$HOSO <- str_pad(hh_df_22$HOSO, width = 3, pad = "0")


hh_df_22$IDHO <- paste0(hh_df_22$MAXA, hh_df_22$MADIABAN, hh_df_22$HOSO)   #hh socio-econs all data


## Commune 22 ----

xa_thongtinxa_22 <- read_dta ("C:/Users/simon/Dropbox/DATA//VH22_Correlates/Xa_ThongTinXa.dta") %>%
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
  select (-"KYDIEUTRA")





xa_muc5a_22 <- read_dta ("C:/Users/simon/Dropbox/DATA//VH22_Correlates/Xa_Muc5A.dta") %>%
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
  select (-c ("dummy_local_market_1", "dummy_local_market_2", "dist_market_1", "dist_market_2")) 







xa_muc4c_22 <- read_dta ("C:/Users/simon/Dropbox/DATA//VH22_Correlates/Xa_Muc4C.dta") %>%
  rename (c("dist_irrigation" = "M4C27",
            "farmer_participation_pct" = "M4C29")) %>%
  select (-c(ends_with("GHIRO"), "M4C33", starts_with(c("M4C28", "M4C37")))) %>%
  mutate (dummy_irrigation = case_when (M4C26 ==1 ~ 1,
                                        M4C26 ==2 ~ 0,
                                        TRUE ~ NA),
          dummy_protection_staff = case_when (M4C34 == 1 ~ 1,
                                              M4C34 == 2 ~ 0,
                                              TRUE ~ NA)) %>%
  select (-c("M4C26", "M4C34")) %>%
  relocate ("dummy_irrigation", .before = "dist_irrigation") %>%
  # relocate ("dummy_protection_staff", .before = "irrigation_info_1")  %>%
  select (-"KYDIEUTRA")
            



            
# Merge datasets: 

xa_thongtinxa_22$MATINH <- as.character(xa_thongtinxa_22$MATINH)
xa_thongtinxa_22$MAHUYEN <- as.character(xa_thongtinxa_22$MAHUYEN)
xa_thongtinxa_22$MAXA <- as.character(xa_thongtinxa_22$MAXA)


xa_muc5a_22$MATINH <- as.character(xa_muc5a_22$MATINH)
xa_muc5a_22$MAHUYEN <- as.character(xa_muc5a_22$MAHUYEN)
xa_muc5a_22$MAXA <- as.character(xa_muc5a_22$MAXA)



xa_muc4c_22$MATINH <- as.character(xa_muc4c_22$MATINH)
xa_muc4c_22$MAHUYEN <- as.character(xa_muc4c_22$MAHUYEN)
xa_muc4c_22$MAXA <- as.character(xa_muc4c_22$MAXA)

commune_df_22 <- full_join (xa_thongtinxa_22, xa_muc5a_22) %>%
  full_join(xa_muc4c_22)


hh_commune_df_22 <- left_join(hh_df_22, commune_df_22) 





#Final: Panel 2022----

df_22 <- left_join (unique_hh_ag_22, hh_commune_df_22) 

df_22 <- df_22 %>%
  mutate (panel = paste0("2022 (n = ", nrow(df_22), ")"))








### POST-MEETING ###----

# Recode ethnicity 

df_22 <- df_22 %>%
  mutate (ethnic = case_when (DANTOCCHUHO == 1 ~ 0,
                              DANTOCCHUHO != 1 ~ 1,
                              TRUE ~ NA)) %>%
  select (-"DANTOCCHUHO") %>%
  relocate (ethnic, .after = MADIABAN)



# Recode distance to wholesale market (in km)


df_22$dist_market_wholesale [is.na (df_22$dist_market_wholesale)] <- 5
df_22$dist_market_wholesale [df_22$dist_market_wholesale < 0] <- 0

# Recode Main access road is asphalt




df_22 <- df_22 %>%
  mutate (main_str_asphalt = case_when (type_main_str == 1 ~ 1,
                                        type_main_str != 1 ~ 0,
                                        TRUE ~ NA))



# Recode distance to irrigation 


df_22$dist_irrigation [is.na (df_22$dist_irrigation)] <- 5



# Convert negative income to NA:
df_22$THUNHAP [df_22$THUNHAP < 0] <- NA





rm(list=setdiff(ls(), c("df_22")))


## 1.1. Descriptive Summary



var_group_stat <- c ("ethnic", 
                "age",
                "edu_grade",
                "n_member",
                "female",
                "internet",
                "THUNHAP",
                "TONGCHITIEU",
                "land_area_sum",
                "poor_commune",
                "main_str_asphalt", 
                "dist_market_wholesale",
                "dist_irrigation")

var_name_group_stat <- c ("Household head is ethnic minority", 
                     "Age of household head (in yrs)",
                     "Education of hh head: highest completed grade",
                     "Household size",
                     "% of households with female head",
                     "% of households with internet access",
                     "Annual income (in 1,000 VND)",
                     "Annual consumption (in 1,000 VND)",
                     "Area of land managed or used by hh (in sq meters)",
                     "Commune is labeled as poor (%)",
                     "Main access road is asphalt (%)", 
                     "Distance to wholesale market (in km)",
                     "Distance to extension center (in km)")


library (flextable)



list_mean <- list ()
list_sd <- list()

for (i in (1:length(var_group_stat))) {
  mean <- mean (df_22[[var_group_stat[i]]], na.rm = TRUE)
  sd <- sd (df_22[[var_group_stat[i]]], na.rm = TRUE)
  list_mean[[i]] <- mean
  list_sd[[i]] <- sd
  }
  


library (plyr)

mean <- ldply(list_mean)
sd <- ldply (list_sd)

detach("package:plyr", unload = TRUE)


result_22 <- cbind(mean, sd) 

colnames(result_22) <- c("Mean", "SD")

result_22 <- result_22 %>%
  mutate (Name = var_name_group_stat) %>%
  relocate (Name, .before = everything())


descriptive_22 <- flextable(result_22) %>%
  align(align = "center", part = "all", j = 2:3) %>%
  colformat_double(j = c(2,3),digits = 1) %>%
  autofit()

save_as_docx (descriptive_22, path = "descriptive_22_updated.docx")






library(expss)

colnames(df_22)

df_22 <- df_22%>%
  apply_labels (IDHO = "HH unique ID",
                HOSO = "HH ID",
                MATINH = "Province ID",
                MAHUYEN = "District ID",
                MAXA = "Commune ID",
                MADIABAN = "EA ID",
                ethnic = "Dummy if hh head is an ethnic minor (1 = ethnic minority, 0 = Kinh)",
                THUNHAP = "Annual income (in 1,000 VND)",
                TONGCHITIEU = "Annual expenditure (in 1,000 VND)",
                rls_head = "Relationship to hh head (1 = head)",
                age = "Age of hh head (in years)",
                edu_grade = "Highest complete grade of hh head", 
                KYDIEUTRA = "Survey month/quarter",
                n_member = "Number of members in the hh",
                male = "Dummy if hh head is male",
                female= "Dummy if hh head is female",
                internet = "Dummy if hh has access to the Internet in the last 6 months",
                land_area_sum = "Total agricultural land managed or used by the hh (in sq meters)",
                poor_commune = "Dummy if hh is in a poor commune",
                dummy_main_str = "Dummy if the commune has main access street",
                dist_main_str = "Distance to main access street (=5km if commune has main street)",
                dummy_local_market_wholesale = "Dummy if commune has local wholesale market",
                dist_market_wholesale = "Distance to wholesale market (=5km if commune has wholesale market)",
                dummy_irrigation = "Dummy if commune has extension center",
                dist_irrigation = "Distance to extension center (=5km if commune has extension center)",
                farmer_participation_pct = "% farmers' participation and engagement with extension center/staff",
                dummy_protection_staff = "Dummy if commune is plant protection staff",
                panel = "panel",
                main_str_asphalt = "Dummy if main street is from asphalt/cement")


write_dta(df_22, "corr_data_22_Jul.dta")

write.csv (df_22, 
           "corr_data_22_Jul.csv", row.names = FALSE)








# 
# # VH 23----
# # Farming 23----
# 
# hh_ag_23 <- read.csv("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Final/Muc4B11.csv") %>%
#   select (c("IDHO", "HOSO", starts_with("MA"))) 
# 
# 
# 
# 
# hh_ag_23$MATINH <- str_pad(hh_ag_23$MATINH, width = 2, pad = "0")
# hh_ag_23$MAHUYEN <- str_pad(hh_ag_23$MAHUYEN, width = 3, pad = "0")
# hh_ag_23$MAXA <- str_pad(hh_ag_23$MAXA, width = 5, pad = "0")
# hh_ag_23$MADIABAN <- str_pad(hh_ag_23$MADIABAN, width = 3, pad = "0")
# hh_ag_23$HOSO <- str_pad(hh_ag_23$HOSO, width = 3, pad = "0")
# hh_ag_23$IDHO <- paste0 (hh_ag_23$MAXA, hh_ag_23$MADIABAN, hh_ag_23$HOSO)
# 
# # hh_ag_23 <- hh_ag_23 %>%
# #   mutate (panel = 2023)
# 
# 
# unique_hh_ag_23 <- hh_ag_23[!duplicated(hh_ag_23$IDHO), ]
# 
# 
# df_farming <- left_join (unique_hh_ag_22, unique_hh_ag_23, by = c("IDHO", 
#                                                                   "HOSO",
#                                                                   "MATINH",
#                                                                   "MAHUYEN",
#                                                                   "MAXA",
#                                                                   "MADIABAN"))
# 
# 
# 
# 
# ##Uncomment when VH23 is ready:
# ## Final: Panel 2023----
# 
# ho_thanhvien_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Ho_ThanhVien.dta") %>%
#   select(c(starts_with(c("MA", "IDHO")),  "HOSO", "M1A_C2", "M1A_C3", "M1A_C5", "M2_C1", "M1A_C10", "KYDIEUTRA")) %>%
#   group_by(IDHO) %>%
#   mutate (n_member = n()) %>%
#   rename (c("gender" = "M1A_C2",
#             "rls_head" = "M1A_C3",
#             "age" = "M1A_C5",
#             "edu_grade" = "M2_C1")) %>%
#   filter (rls_head == 1) %>%
#   mutate (male = case_when(gender==1 ~ 1,
#                            gender==2 ~ 0,
#                            TRUE ~ NA)) %>%
#   mutate (female = case_when (male == 1 ~ 0,
#                               male == 0 ~ 1,
#                               TRUE ~ NA)) %>%
#   mutate (internet = case_when (M1A_C10 == 1 ~ 1,
#                                 M1A_C10 == 2 ~ 0,
#                                 TRUE ~ NA)) %>%
#   select (-c("gender", "M1A_C10"))
# 
# 
# ho_thanhvien_23$edu_grade[ho_thanhvien_22$edu_grade == 99] <- NA
# 
# 
# 
# ho_thongtinho_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Ho_ThongTinHo.dta") %>%
#   select(c(starts_with(c("MA", "IDHO")), "HOSO", "DANTOCCHUHO", "THUNHAP", "TONGCHITIEU"))
# 
# 
# 
# 
# 
# ho_muc4b0_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/ho_muc4b0.dta") %>%
#   rename(c("id_land" = "M4B0_MA",
#            "land_area" = "M4B0_C3")) %>%
#   pivot_wider(names_from = id_land,
#               values_from = land_area,
#               names_prefix = "land_area_") %>%
#   mutate (land_area_sum = rowSums(select(., starts_with("land_area_")), na.rm = TRUE)) %>%
#   select (-c("land_area_1":"land_area_6"))
# 
# 
# 
# 
# 
# 
# ho_thongtinho_23$MATINH <- as.character(ho_thongtinho_23$MATINH)
# ho_thongtinho_23$MAHUYEN <- as.character(ho_thongtinho_23$MAHUYEN)
# ho_thongtinho_23$MAXA <- as.character(ho_thongtinho_23$MAXA)
# ho_thongtinho_23$MADIABAN <- as.character(ho_thongtinho_23$MADIABAN)
# ho_thongtinho_23$IDHO <- as.character(ho_thongtinho_23$IDHO)
# 
# 
# 
# ho_thanhvien_23$MATINH <- as.character(ho_thanhvien_23$MATINH)
# ho_thanhvien_23$MAHUYEN <- as.character(ho_thanhvien_23$MAHUYEN)
# ho_thanhvien_23$MAXA <- as.character(ho_thanhvien_23$MAXA)
# ho_thanhvien_23$MADIABAN <- as.character(ho_thanhvien_23$MADIABAN)
# ho_thanhvien_23$IDHO <- as.character(ho_thanhvien_23$IDHO)
# 
# 
# 
# 
# ho_muc4b0_23$MATINH <- as.character(ho_muc4b0_23$MATINH)
# ho_muc4b0_23$MAHUYEN <- as.character(ho_muc4b0_23$MAHUYEN)
# ho_muc4b0_23$MAXA <- as.character(ho_muc4b0_23$MAXA)
# ho_muc4b0_23$MADIABAN <- as.character(ho_muc4b0_23$MADIABAN)
# ho_muc4b0_23$IDHO <- as.character(ho_muc4b0_23$IDHO)
# 
# 
# hh_df_23 <- full_join (ho_thongtinho_23, ho_thanhvien_23) %>%
#   full_join(ho_muc4b0_23) %>%
#   group_by (IDHO)
# 
# 
# 
# 
# hh_df_23$MATINH <- str_pad(hh_df_23$MATINH, width = 2, pad = "0")
# hh_df_23$MAHUYEN <- str_pad(hh_df_23$MAHUYEN, width = 3, pad = "0")
# hh_df_23$MAXA <- str_pad(hh_df_23$MAXA, width = 5, pad = "0")
# hh_df_23$MADIABAN <- str_pad(hh_df_23$MADIABAN, width = 3, pad = "0")
# hh_df_23$HOSO <- str_pad(hh_df_23$HOSO, width = 3, pad = "0")
# 
# 
# hh_df_23$IDHO <- paste0(hh_df_23$MAXA, hh_df_23$MADIABAN, hh_df_23$HOSO)   #hh socio-econs all data
# 
# 
# 
# 
# 
# xa_thongtinxa_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_ThongTinXa.dta") %>%
#   rename (c("dist_main_str" = "M5C5",
#             "type_main_str" = "M5C7")) %>%
#   mutate (poor_commune = case_when (M1C14 ==1 ~ 1,
#                                     M1C14 ==2 ~ 0,
#                                     TRUE ~ NA),
#           dummy_main_str = case_when (M5C4 == 1 ~ 1,
#                                       M5C4 == 2 ~ 0,
#                                       TRUE ~ NA)) %>%
#   select (-c("M1C14", "M5C4", "M7C1A" : "M7C1C")) %>%
#   relocate(c("poor_commune", "dummy_main_str"), .before = "dist_main_str") %>%
#   select (-"KYDIEUTRA")
# 
# 
# xa_muc5a_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_Muc5A.dta") %>%
#   rename (c("id_market" = "M5STT1",
#             "dist_market" = "M5C25")) %>%
#   mutate (dummy_local_market = case_when (M5C24 == 1 ~ 1,
#                                           M5C24 == 2 ~ 0,
#                                           TRUE ~ NA)) %>%
#   select(-"M5C24") %>%
#   pivot_wider (names_from = id_market,
#                values_from = c("dummy_local_market", "dist_market")) %>%
#   rename (c("dummy_local_market_wholesale" = "dummy_local_market_3",
#             "dist_market_wholesale" = "dist_market_3"))  %>%
#   select (-c("KYDIEUTRA", "dummy_local_market_1", "dummy_local_market_2", "dist_market_1", "dist_market_2"))
# 
# 
# xa_muc4c_23 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3 - Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Xa_Muc4C.dta") %>%
#   rename (c("dist_irrigation" = "M4C27",
#             "farmer_participation_pct" = "M4C29")) %>%
#   select (-c(ends_with("GHIRO"), "M4C33", starts_with(c("M4C28", "M4C37")))) %>%
#   mutate (dummy_irrigation = case_when (M4C26 ==1 ~ 1,
#                                         M4C26 ==2 ~ 0,
#                                         TRUE ~ NA),
#           dummy_protection_staff = case_when (M4C34 == 1 ~ 1,
#                                               M4C34 == 2 ~ 0,
#                                               TRUE ~ NA)) %>%
#   select (-c("M4C26", "M4C34")) %>%
#   relocate ("dummy_irrigation", .before = "dist_irrigation") %>%
#   # relocate ("dummy_protection_staff", .before = "irrigation_info_1")  %>%
#   select (-"KYDIEUTRA")
# 
# 
# 
# xa_thongtinxa_23$MATINH <- as.character(xa_thongtinxa_23$MATINH)
# xa_thongtinxa_23$MAHUYEN <- as.character(xa_thongtinxa_23$MAHUYEN)
# xa_thongtinxa_23$MAXA <- as.character(xa_thongtinxa_23$MAXA)
# 
# 
# xa_muc5a_23$MATINH <- as.character(xa_muc5a_23$MATINH)
# xa_muc5a_23$MAHUYEN <- as.character(xa_muc5a_23$MAHUYEN)
# xa_muc5a_23$MAXA <- as.character(xa_muc5a_23$MAXA)
# 
# 
# 
# xa_muc4c_23$MATINH <- as.character(xa_muc4c_23$MATINH)
# xa_muc4c_23$MAHUYEN <- as.character(xa_muc4c_23$MAHUYEN)
# xa_muc4c_23$MAXA <- as.character(xa_muc4c_23$MAXA)
# 
# commune_df_23 <- full_join (xa_thongtinxa_23, xa_muc5a_23) %>%
#   full_join(xa_muc4c_23)
# 
# 
# hh_commune_df_23 <- left_join(hh_df_23, commune_df_22) 
# 
# df_23 <- left_join (unique_hh_ag_23, hh_commune_df_23)
# 
# 
# df_23 <- df_23 %>%
#   mutate (panel = paste0("2023 (n = ", nrow(df_23), ")"))
# 
# # Both panel
# data  <- left_join (df_farming, hh_commune_df_22)   #Change to VH23 correlates
# 
# data <- data %>%
#   mutate (panel = paste0("Both year (n = ", nrow(data), ")"))  #data that in both datasets
# 
# 
# ### POST-MEETING ###----
# 
# # Recode distance to wholesale market (in km)
# #table (df_23$dist_market_wholesale, df_23$dummy_local_market_wholesale) # NA on distance when inside the commune
# #mean (df_23$dist_market_wholesale, na.rm=TRUE) # 16km on average when outside commune
# #sum (is.na (df_23$dist_market_wholesale)) # n=732 have market in their commune
# #sum (df_23$dist_market_wholesale <0, na.rm=TRUE) # n=522 have negative distances
# df_23$dist_market_wholesale [is.na (df_23$dist_market_wholesale)] <- 5
# df_23$dist_market_wholesale [df_23$dist_market_wholesale < 0] <- 0
# 
# 
# df_22$dist_market_wholesale [is.na (df_22$dist_market_wholesale)] <- 5
# df_22$dist_market_wholesale [df_22$dist_market_wholesale < 0] <- 0
# 
# # Recode Main access road is asphalt
# #table (df_23$type_main_str)
# # df_23$type_main_str <- ifelse (df_23$type_main_str == 1, TRUE, FALSE)
# 
# df_23 <- df_23 %>%
#   mutate (main_str_asphalt = case_when (type_main_str == 1 ~ 1,
#                                         type_main_str != 1 ~ 0,
#                                         TRUE ~ NA))
# 
# 
# 
# df_22 <- df_22 %>%
#   mutate (main_str_asphalt = case_when (type_main_str == 1 ~ 1,
#                                         type_main_str != 1 ~ 0,
#                                         TRUE ~ NA))
# 
# 
# 
# # Recode distance to irrigation 
# #table (df_23$dist_irrigation, df_23$dummy_irrigation) # NA on distance when inside the commune
# #mean (df_23$dist_irrigation, na.rm=TRUE) # 14km on average when outside commune
# #sum (is.na (df_23$dist_irrigation)) # n=738 have irrigation in their commune
# #sum (df_23$dist_irrigation <0, na.rm=TRUE) # n=522 have negative distances
# 
# df_23$dist_irrigation [is.na (df_23$dist_irrigation)] <- 5
# 
# 
# df_22$dist_irrigation [is.na (df_22$dist_irrigation)] <- 5
# 
# # Recode Distance to extension center ???
# 
# # df_23 <- df_23 [, -c(26,32,33,34,35,36,40)]
# 
# 
# 
# 
# # Convert negative income to NA:
# df_22$THUNHAP [df_22$THUNHAP < 0] <- NA
# df_23$THUNHAP [df_23$THUNHAP < 0] <- NA