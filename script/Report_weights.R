
#---
# Title: "Weights calculations for rice growers and DNA subsamples in the VHLSS"
# Author email: "b.thanh@contractors.irri.org"
# Date: "12/2024"
# ---

# README ----

  
# This script sources from VHLSS 2022 and 2023 datasets

## VHLSS is the survey for general households, however, our study's focus is agricultural households. 
## This weight calculation takes account of structural transformation and under-representation of the VHLSS. 
## In this script, first we calculate the adjustment factors for structural transformation and under-representation.
## Then, we adjusted the VHLSS weights with these factors to come up with new, more correct weights for agricultural households 
## For more information, the weight calculation methods is exposed the Vietnam country report, methods section. 

## It outputs "Report_weight.csv", with variables weight_final_rice, weight_rice_DNA,	weight_cass,	weight_gift used for analysis



  
# List of datasets we are gonna use:

## For adjustment factor for structural changes: 

### Number of rice-growing and cassava-growing households from General Statistics Office (GSO)'s census on agriculture (2016), 
### Number of rice-growing and cassava-growing households from GSO's survey on rural and agriculture (2020). 
### Both are at national level


## For adjustment factor for under-representation: 
### Same as above, but at commune levels
### GSO-calculated weights (for general households), at household level, panel 2022 and 2023
### VHLSS list of households, at household level, panel 2022 and 2023
### VHLSS module M4B11 (rice), at households level. This is a GSO's module, panel 2022 and 2023
### SPIA datasets on DNA fingerprinting of rice (2022), cassava (2023), and tilapia (2023), at household level



rm(list = ls()) #start clean



# 1. Install and load packages ----
# Function to check and install packages

packages <- c("haven", "tidyverse", "readxl", "stringr", "curl")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

library (haven)
library (tidyverse)
library (readxl)
library (stringr)
library (curl)

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


# 2. General calculations for both years  ----

## 2.1. Structural changes ----

### 2.1.1. Rice-growing hhs ---- 

str_change <- read_excel("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Ho_TBB01SR- ho trong lua 2020.xlsx",
                         sheet = "Sheet2") %>%
  select ("Number of rice growing households" : "2020")

colnames (str_change) <- c ("province", "n_rice_2016", "n_rice_2020")

n_rice_2016 <- str_change$n_rice_2016[1] #Number of rice-growing hh in 2016
n_rice_2020 <- str_change$n_rice_2020[1] #Number of rice-growing hh in 2020

# decline_rate <- (n_rice_2020 / n_rice_2016)^(1/4) - 1  #for 4 years, N_rice declines at 6.3%
decline_rate_annual <- (n_rice_2020 / n_rice_2016)^(1/4) - 1   


pred_n_rice_2022 <- n_rice_2020 * (1 + decline_rate_annual)^2  #Predicted number of rice-growing hh in 2022
pred_n_rice_2023 <- n_rice_2020 * (1 + decline_rate_annual)^3 #Predicted 2023



### 2.1.2. Cassava DNA subsample ----

str_change_cass <- read_excel("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Ho_TBB01SR- ho trong lua 2020.xlsx",
                              sheet = "Sheet3") %>%
  select (c("...1", "cassava_2016", "cassava_2020"))


n_cass_2016 <- str_change_cass$cassava_2016[1]
n_cass_2020 <- str_change_cass$cassava_2020[1] #N cassava-growing hh in 2020
decline_rate_annual_cass <- (n_cass_2020 / n_cass_2016) ^ (1/4) - 1 #decline rate of #cassava-growing hh
pred_n_cass_2023 <- n_cass_2020 * (1 + decline_rate_annual_cass) ^ 3




### 2.1.3. Coffee ----

n_coffee_pop <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Final/coffee_household_clean.csv") 

n_coffee_2016 <- sum(n_coffee_pop$n_coffee_hh)

str_change_coffee <- read_excel("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Ho_TBB01SR- ho trong lua 2020.xlsx",
                         sheet = "Sheet4") 

n_coffee_2020 <- str_change_coffee$Coffee[1]

increase_rate_annual_coffee <- (n_coffee_2020 / n_coffee_2016)^(1/4) - 1 # 1.9% increase annually

pred_n_coffee_2023 <- n_coffee_2020 * (1 + increase_rate_annual_coffee)^3 

### 2.2. IN THE POPULATION----

#### 2.2.1. Number of rice-growing households IN THE POPULATION----

n_rice_pop <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Final/rice_household_clean.csv") %>%
  select (MATINH, MAXA, n_rice_hh) %>%
  rename (n_rice_pop = n_rice_hh) #merge by Commune ID (MAXA) because of some administrative change

n_rice_pop <- format_ID(n_rice_pop, columns = c("MATINH", "MAXA"), widths = c(2, 5))


#### 2.2.2. Number of general households IN THE POPULATION----

n_hh_pop <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Final/Census_household_communelevel_clean.csv") %>%
  select (c(MATINH, MAXA, n_hh)) %>%
  rename (n_hh_pop = n_hh) 
#merge by Commune ID (MAXA) because of some administrative change 
# (486 missing if merge by prov, dist, comm ID --> 470 missing if merge by prov and comm ID)

n_hh_pop <- format_ID(n_hh_pop, columns = c("MATINH", "MAXA"), widths = c(2, 5))

fraction_pop <- full_join (n_rice_pop, n_hh_pop) %>%
  mutate (fraction_pop = n_rice_pop / n_hh_pop)



# 3. Recalculating weights 2023----

## 3.1. Load original weights 2023----

wt_2023 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/weight2023.dta")

colnames(wt_2023)[1:4] <- paste0 ("ma", colnames(wt_2023)[1:4]) 
colnames(wt_2023)[1:4] <- toupper (colnames(wt_2023)[1:4])


# Reformat IDs:
wt_2023 <- format_ID(wt_2023, columns = c("MATINH", "MAXA", "MADIABAN"), widths = c(2, 5, 3))


## 3.2. Adjustment for under-representation----

### 3.2.1. IN THE SAMPLE----

#### 3.2.1.1. Rice-growing households IN THE SAMPLE ----

rice_gso <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Final/Muc4B11.csv") %>%
  select (c(MATINH, MAXA, MADIABAN, IDHO, HOSO, KYDIEUTRA, wt45)) %>%
  distinct ()

rice_gso <- rice_gso %>%
  filter (!is.na(wt45))  #N = 13229


rice_gso <- format_ID(rice_gso, columns = c("MATINH", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 5, 3, 3))

rice_gso$IDHO <- paste0 (rice_gso$MAXA, rice_gso$MADIABAN, rice_gso$HOSO)

n_rice_sample <- rice_gso %>%
  group_by (MATINH, MAXA, MADIABAN) %>%
  summarise (n_rice_sample = n()) #Number of rice-growing households by commune




#### 3.2.1.2. Number of general households IN THE SAMPLE ---- 

ho_thongtinho <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH23_Correlates/Ho_ThongTinHo.dta") %>%
  select (c(idho:hoso))

colnames(ho_thongtinho)[2:5] <- paste0 ("ma", colnames(ho_thongtinho)[2:5])

colnames(ho_thongtinho) <- toupper (colnames(ho_thongtinho))


#Reformat IDs:
ho_thongtinho <- format_ID(ho_thongtinho, columns = c("MATINH",  "MAXA", "MADIABAN"), widths = c(2, 5, 3))

n_hh_sample <- ho_thongtinho %>%
  group_by (MATINH, MAXA) %>%
  summarise (n_hh_sample = n()) #Number of general households by commune

fraction_sample <- full_join (n_rice_sample, n_hh_sample) %>%
  mutate (fraction_sample = n_rice_sample / n_hh_sample)




### 3.2.2. Adjustment for under-representation----

adj_underrep <- full_join (fraction_pop, fraction_sample) %>%
  mutate (adj_underrep = fraction_pop / fraction_sample)



## 3.3. Calculating new weights----

### 3.3.1. Calculating k (adjustment for both str change and under-rep)----
new_wt2023 <- rice_gso %>%
  left_join (adj_underrep %>% select (c(MATINH, MAXA, MADIABAN, adj_underrep))) %>%
  mutate (wt45_underrep = case_when (is.na(adj_underrep) ~ wt45, 
                                     adj_underrep == 0 ~ wt45,
                                     !is.na(adj_underrep) & adj_underrep != 0 ~ wt45 * adj_underrep)) %>%
  mutate (k = pred_n_rice_2023 / sum (wt45_underrep, na.rm = TRUE)) %>%
  mutate (weight_final_rice = k * wt45_underrep) %>%
  mutate (panel = 2023)

#Sniff test to check if total new weights sum up to the predicted number of rice-growing households in 2023 
sum(new_wt2023$weight_final_rice, na.rm = TRUE) == pred_n_rice_2023  


#If they don't pass the sniff test, it is because of the floating-point decision issue (i.e: rounding error), check these:
sum(new_wt2023$weight_final_rice, na.rm = TRUE)

pred_n_rice_2023  

all.equal(sum(new_wt2023$weight_final_rice, na.rm = TRUE), pred_n_rice_2023)

any(is.na(new_wt2023$weight_final_rice))  #no NA






# 4. Recalculating weights 2022----


## 4.1. Load original weights 2022----

wt_2022 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2022/datasets/Weights/wt2022_SPIA.dta") %>%
  select (c(tinh:diaban, ky, wt45)) %>%
  select (-huyen)

colnames(wt_2022)[1:3] <- paste0 ("ma", colnames(wt_2022)[1:3]) 

colnames(wt_2022)[1:3] <- toupper (colnames(wt_2022)[1:3])

colnames(wt_2022)[4] <- "KYDIEUTRA"

# Reformat IDs:
wt_2022 <- format_ID(wt_2022, columns = c("MATINH", "MAXA", "MADIABAN"), widths = c(2, 5, 3))


## 4.2. Adjustment for under-representation----

### 4.2.1. IN THE SAMPLE----

#### 4.2.1.1. Rice-growing households IN THE SAMPLE ----

rice_gso_22 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2022/datasets/Ho_Muc4B11.dta") %>%
  select (c(MATINH, MAXA, MADIABAN, IDHO, KYDIEUTRA)) %>%
  distinct() 


rice_gso_22 <- rice_gso_22 %>%
  left_join (wt_2022)


length(which(is.na(rice_gso_22$wt45))) #374 obs missing weights!!!!
#These households were additionally collected for provincial purposes. They don't belong to VHLSS


rice_gso_22 <- rice_gso_22 %>%
  filter (!is.na(wt45))  #N = 13939

n_rice_sample_22 <- rice_gso_22 %>%
  group_by (MATINH, MAXA, MADIABAN) %>%
  summarise (n_rice_sample = n()) #Number of rice-growing households by commune




#### 4.2.1.2. Number of general households IN THE SAMPLE ---- 

ho_thongtinho_22 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VH22_Correlates/Ho_ThongTinHo.dta") %>%
  select (c(IDHO:HOSO))


#Reformat IDs:
ho_thongtinho_22 <- format_ID(ho_thongtinho_22, columns = c("MATINH", "MAXA", "MADIABAN"), widths = c(2, 5, 3))


n_hh_sample_22 <- ho_thongtinho_22 %>%
  group_by (MATINH, MAXA, MADIABAN) %>%
  summarise (n_hh_sample = n()) #Number of general households by commune

fraction_sample_22 <- full_join (n_rice_sample_22, n_hh_sample_22) %>%
  mutate (fraction_sample = n_rice_sample / n_hh_sample)



### 4.2.2. Adjustment for under-representation----

adj_underrep_22 <- full_join (fraction_pop, fraction_sample_22) %>%
  mutate (adj_underrep = fraction_pop / fraction_sample)




## 4.3. Calculating new weights----

### 4.3.1. Calculating k (adjustment for both str change and under-rep)----

new_wt2022 <- rice_gso_22 %>%
  left_join (adj_underrep_22 %>% select (c(MATINH, MAXA, MADIABAN, adj_underrep))) %>%
  mutate (wt45_underrep = case_when (is.na(adj_underrep) ~ wt45,
                                     adj_underrep == 0 ~ wt45,
                                     !is.na(adj_underrep) & adj_underrep != 0 ~ wt45 * adj_underrep)) %>%
  mutate (k = pred_n_rice_2022 / sum (wt45_underrep, na.rm = TRUE)) %>%
  mutate (weight_final_rice = k * wt45_underrep) %>%
  mutate (panel = 2022)


#Again, the sniff test to check if the new weights sum up to the predicted number of rice-growing households in 2022
sum(new_wt2022$weight_final_rice, na.rm = TRUE) == pred_n_rice_2022  

# If not pass the sniff test, it is because of the floating-point precision issue (i.e: rounding error). Check these:
sum(new_wt2022$weight_final_rice, na.rm = TRUE) #7725011

pred_n_rice_2022 #7725011

all.equal(sum(new_wt2022$weight_final_rice, na.rm = TRUE), pred_n_rice_2022)  #now it's true

any(is.na(new_wt2022$weight_final_rice)) # No NA values


# Join two datasets for rice weights
final_weight_rice <- new_wt2022 %>%
  # select (c(MATINH, MAHUYEN, MAXA, MADIABAN, IDHO, weight_2022_final)) %>%
  full_join (new_wt2023 ) %>%
  select (c(MATINH, MAXA, MADIABAN, weight_final_rice, panel)) %>%
  distinct() 




# 4.4. Rice DNA subsample----


#Load rice DNA dataset:
df_rice_dna <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Rice/Rice.vars.VH24.csv") %>%
  select (c(MATINH, MAHUYEN, MAXA, MADIABAN, HOSO)) 



#Reformat IDs to match:
df_rice_dna <- format_ID(df_rice_dna, columns = c("MATINH", "MAXA", "MADIABAN", "HOSO"), widths = c(2,5,3,3))


n_rice_dna <- df_rice_dna %>%
  group_by (MATINH, MAXA) %>%
  summarise (n_rice_dna = n())  #Number of DNA rice-hh in a commune

fraction_dna <- n_rice_dna %>%
  left_join (n_hh_sample) %>%
  mutate (fraction_dna = n_rice_dna / n_hh_sample)

#Normally, in each communes, there are 15 hh surveyed -> replace missing fraction in the sample with 15
fraction_dna$fraction_dna[is.na(fraction_dna$fraction_dna)] <- fraction_dna$n_rice_dna[is.na(fraction_dna$fraction_dna)] / 15


inflation_dna <- fraction_dna %>%
  left_join (fraction_pop) %>%
  mutate (inflation_dna = fraction_pop / fraction_dna) 


dna_weight_rice <- df_rice_dna %>%
  left_join (wt_2022) %>%
  left_join (inflation_dna) %>%
  mutate (wt_underrep = wt45 * inflation_dna) %>%
  mutate (adj_underrep = pred_n_rice_2022 / sum (wt_underrep)) %>%
  mutate (weight_rice_DNA = adj_underrep * wt_underrep) %>%
  mutate (panel = 2022) %>%
  select (c(MATINH, MAXA, MADIABAN, weight_rice_DNA, panel))

# Sniff test to check new weights of DNA rice sum up to predicted N_rice 2022:
sum(dna_weight_rice$weight_rice_DNA, na.rm = TRUE) == pred_n_rice_2022 #passed


final_weight_rice <- final_weight_rice %>%
  full_join (dna_weight_rice)



rm(list = setdiff(ls(), c("final_weight_rice", "n_hh_sample", "n_hh_sample_22", "n_hh_pop", "wt_2023", "pred_n_cass_2023", 'format_ID', "pred_n_coffee_2023")))






# 5. Cassava DNA subsample----

### 5.1.1. In the sample ----

cassava <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Cassava/Cass.vars.VH24.csv") %>%
  select (c(MATINH, MAXA, MADIABAN, HOSO, IDHO)) 

cassava <- format_ID(cassava, columns = c("MATINH", "MAXA", "MADIABAN", "HOSO"), widths = c(2,5,3,3))

cassava <- cassava %>%
  left_join (wt_2023)

n_cass_sample <- cassava %>%
  group_by(MATINH, MAXA, MADIABAN) %>%
  summarise (n_cass_sample = n()) #Number of cassava households in the sample

fraction_sample_cass <- n_cass_sample %>%
  left_join (n_hh_sample) %>%
  mutate (fraction_sample_cass = n_cass_sample / n_hh_sample)


### 5.1.2. In the population ----

n_cass_pop <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Final/cassava_household_clean.csv") %>%
  select (c(MATINH, MAXA, n_cassava_hh))  #GSO list: N_cassava by commune, merge by Commune ID



n_cass_pop <- format_ID(n_cass_pop, columns = c("MATINH", "MAXA"), widths = c(2,5))


fraction_pop_cass <- n_cass_pop %>%
  full_join (n_hh_pop) %>%
  mutate (fraction_pop_cass = n_cassava_hh / n_hh_pop)


adj_underrep_cass <- fraction_sample_cass %>%
  left_join (fraction_pop_cass) %>%
  mutate (adj_underrep_cass = fraction_pop_cass / fraction_sample_cass)


### 5.1.3. New weight----

cass_wt <- cassava %>%
  left_join (adj_underrep_cass %>% select (c(MATINH, MAXA, MADIABAN, adj_underrep_cass))) %>%
  mutate (wt45_underrep = case_when (is.na(adj_underrep_cass) ~ wt45,
                                     adj_underrep_cass == 0 ~ wt45,
                                     !is.na(adj_underrep_cass) & adj_underrep_cass != 0 ~ wt45 * adj_underrep_cass)) %>%
  mutate (k = pred_n_cass_2023 / sum (wt45_underrep, na.rm = TRUE)) %>%
  mutate (weight_cass = k * wt45_underrep) %>%
  mutate (panel = 2023) %>%
  select (c(MATINH:IDHO, weight_cass, panel)) 


#Sniff test to check if new weights add up to the predicted number of cassava-growing households:
sum(cass_wt$weight_cass, na.rm = TRUE) == pred_n_cass_2023  # passed the sniff test


# If not pass the sniff test, it is because of the floating-point precision issue (i.e: rounding error). Check these:
sum(cass_wt$weight_cass, na.rm = TRUE)  #508884.7
pred_n_cass_2023 #508884.7

all.equal(sum(cass_wt$weight_cass, na.rm = TRUE), pred_n_cass_2023)  #TRUE

which(is.na(cass_wt$weight_cass))  #no NA values



cassava_final_weight <- cass_wt %>%
  select (c(MATINH:MADIABAN, panel, weight_cass))


# 6. GIFT-derived tilapia strains ----

gift <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Tilapia/GIFT.vars.VH24.csv") %>%
  select (-hhiddistrict)

gift[which(gift$I_Q5== "388893714"),]$HH_ID <- 1 #post-survey edit
gift[which(gift$I_Q5== "838834349"),]$hhidcommune <- 32221 #post-survey edit
gift[which(gift$I_Q5 == "395079742"),]$hhidcommune <- 4768 #post-survey


gift <- format_ID(gift, columns = c("hhidprovince", "hhidcommune", "HH_ID"), widths = c(2, 5, 3))

gift <- gift %>%
  rename (c("MATINH" = "hhidprovince",
            # "MAHUYEN" = "hhiddistrict",
            "MAXA" = "hhidcommune",
            "HOSO" = "HH_ID"))



# GSO dataset to get EA ID:
gift_ea <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Combined_modules/M4B51A.csv")%>%
  select ("MATINH":"MADIABAN") %>%
  select (-MAHUYEN)

gift_ea <- format_ID(gift_ea, columns = c("MATINH", "MAXA", "MADIABAN"), widths = c(2, 5, 3))

gift_ea_unique <- distinct(gift_ea) #to get EA ID


gift_df <- left_join (gift, gift_ea_unique) %>%
  mutate (panel = 2023)

gift_df$MADIABAN[is.na(gift_df$MADIABAN)] <- "005"
# gift_df$MAHUYEN[gift_df$MAXA == "32221"] <- "972"


gift_final <- gift_df %>%
  left_join (wt_2023) %>%
  rename (weight_gift = wt45) %>%
  select (c(MATINH, MAXA, MADIABAN, weight_gift)) %>%
  mutate (panel = 2023) %>%
  distinct()



# 7. Coffee----
## 7.1.1. In the sample----
coffee_q1 <- read_excel ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Q1.xlsx",
                         sheet = "SPIA_M4B13A")
coffee_q2 <- read_excel ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Q2.xlsx",
                        sheet = "SPIA_M4B13A")
coffee_q3 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Q3/Ho_SPIA_M4B13A.dta")

coffee_q4 <- read_dta ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/Q4/Ho_SPIA_M4B13A.dta")


coffee <- full_join (coffee_q1, coffee_q2) %>%
  full_join (coffee_q3) %>%
  full_join (coffee_q4) %>%
  select (c(MATINH, MAXA, MADIABAN, HOSO, IDHO)) %>%
  left_join (wt_2023) %>%
  select (-MAHUYEN)


any(is.na(coffee$wt45))  #No problems with additional surveyed EAs

n_coffee_sample <- coffee %>%
  group_by(MATINH, MAXA, MADIABAN) %>%
  summarise (n_coffee_sample = n()) #Number of coffee households in the sample

fraction_sample_coffee <- n_coffee_sample %>%
  left_join (n_hh_sample) %>%
  mutate (fraction_sample_coffee = n_coffee_sample / n_hh_sample)


## 7.1.2. In the population----
n_coffee_pop <- read.csv ("C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Weight/Final/coffee_household_clean.csv") %>%
  select (-c(ends_with("name"), MAHUYEN)) 

n_coffee_pop <- format_ID (n_coffee_pop, columns = c("MATINH", "MAXA"), widths = c(2,5))

fraction_pop_coffee <- n_coffee_pop %>%
  left_join (n_hh_pop) %>%
  mutate (fraction_pop_coffee = n_coffee_hh / n_hh_pop)



adj_underrep_coffee <- fraction_sample_coffee %>%
  left_join (fraction_pop_coffee) %>%
  mutate (adj_underrep_coffee = fraction_pop_coffee / fraction_sample_coffee)

### 7.1.3. New weight----

coffee_wt <- coffee %>%
  left_join (adj_underrep_coffee %>% select (c(MATINH, MAXA, MADIABAN, adj_underrep_coffee))) %>%
  mutate (wt45_underrep = case_when (is.na(adj_underrep_coffee) ~ wt45,
                                     adj_underrep_coffee == 0 ~ wt45,
                                     !is.na(adj_underrep_coffee) & adj_underrep_coffee != 0 ~ wt45 * adj_underrep_coffee)) %>%
  mutate (k = pred_n_coffee_2023 / sum (wt45_underrep, na.rm = TRUE)) %>%
  mutate (weight_coffee = k * wt45_underrep) %>%
  mutate (panel = 2023) %>%
    select (c(MATINH:IDHO, weight_coffee, panel)) 


sum(coffee_wt$weight_coffee) == pred_n_coffee_2023  #Test if new weights add up to predicted n_coffee households in 2023


coffee_final_weight <- coffee_wt %>%
  select (c(MATINH, MADIABAN, MAXA, weight_coffee, panel)) %>%
  distinct()

final_weight <- final_weight_rice %>%
  full_join (cassava_final_weight) %>%
  full_join (gift_final) %>%
  full_join (coffee_final_weight) %>%
  distinct()
  


write.csv (final_weight, "C:/Users/BThanh/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Report_weights.csv",
           row.names = FALSE)
