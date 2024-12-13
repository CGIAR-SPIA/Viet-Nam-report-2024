

# ---
# Title: "Section 2. Breeding innovations"
# Author email: "fkosmowski@gmail.com"
# Date: "October 2024"
# ---
rm (list = ls())


# Install and load packages ----
packages <- c("readstata13", "readxl", "foreign", "ggplot2", "forcats", "dplyr",
              "haven", "tidyr", "rlang", "maditr", "stringr", "networkD3", "openxlsx", 
              "scales", "jtools", "officer", "httr")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

library(readstata13)
library(readxl)
library(foreign)
library(ggplot2)
library(forcats)
library(dplyr)
library(haven)
library(tidyr)
library(rlang)
library(maditr)
library(stringr)
library(networkD3)
library(openxlsx)
library(scales)
library(jtools)
library(officer)
library(httr)


# Function to curl data ----
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


# Rice DNA fingerprinting  ----
curl_function("data/processed/Rice.vars.VH24.csv")
dat <- read.csv ("data/processed/Rice.vars.VH24.csv") 

curl_function("data/processed/Rice_Years.csv")
Years <- read.csv ('data/processed/Rice_Years.csv') 
dat <- merge (dat, Years, by.x='Correct_name.DNA2', by.y = 'Name', all.x=TRUE) 

# Figure 13. Evolution of rice production and exports in Viet Nam, from 2003 to 2023, in thousand tonnes ----
data <- data.frame(
  Year = c(2003:2023),
  Production = c(34569, 36149, 35833, 35850, 35943, 38730, 38950, 40006, 42399, 43738, 44039, 44975, 45091, 43109, 42739, 44046, 43495, 42765, 43853, 42661, 43498),
  Exports = c(3810, 4063, 5255, 4642, 4580, 4745, 5969, 6893, 7116, 8017, 6587, 6331, 6582, 6582, 5819, 6107, 6371, 6249, 6242, 7105, 8132)
)

data_melted <- melt(data, id.vars="Year", variable.name="Category", value.name="Value")

ggplot(data=data_melted, aes(x=Year, y=Value, fill=Category)) +
  geom_area(position = "identity", alpha=0.7) +
  scale_fill_manual(values=c("Production"="#6699CC", "Exports"="#336699")) +
  scale_x_continuous(limits=c(2003, 2023), breaks=seq(2003, 2023, by=2)) +
  scale_y_continuous(labels = comma) +  # Add 1,000 separator
  labs(x="Year", y="Tonnes") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        plot.title=element_blank(),
        panel.grid.major = element_blank())  


# Table 15. Distribution of rice by variety planted during the 2022/23 agricultural season ----

freq <- table(dat$Correct_name.DNA2)
sorted_freq <- sort(freq, decreasing = TRUE)
sorted_dat <- data.frame(Correct_name = names(sorted_freq), Frequency = sorted_freq)

# Adding Age var to table
merged_dat <- merge(sorted_dat, Years [,c (1,2,11,6)], by.x = "Correct_name", by.y= "Name", all.x = TRUE)
colnames(merged_dat)[2] <- "Frequency"
merged_dat <- merged_dat[order(-merged_dat$Frequency.Freq), ]

# Clean data
merged_dat$pctn <- round ((merged_dat$Frequency.Freq / sum(merged_dat$Frequency.Freq)) * 100, 1)
merged_dat$Year <- (2022 - merged_dat$Year)
merged_dat <- merged_dat [, c(2,3,7,4,5,6)]
names(merged_dat)[1] <- 'Variety'; names(merged_dat)[2] <- 'Number of rice samples'; names(merged_dat)[3] <- '% rice samples'; names(merged_dat)[4] <- 'Age (years)'; names(merged_dat)[5] <- 'Pedigree'; names(merged_dat)[6] <- 'Origin'

#Note: These are unweighted figures. Weighted adoption statistics are here
#https://www.dropbox.com/scl/fi/t5gonv515g5i25qxu17h7/Rice_adoption_byvar.html?rlkey=mmxr6akxh0c203x72qfrig3vf&dl=0

write.xlsx (merged_dat, 'Output/Tab15.Rice.xlsx') 







# Figure 15. Maps showing the percentage adoption of (a) improved varietal adoption, (b) IRRI-related varietal adoption and c) average age of improved varieties in Viet Nam ----
# Generated in "Rice_Maps_temp.RmD"

# Figure 17. Map showing the intensity of adoption of the saltol gene by province in Vietnam  ----
# See Maps_all.R

# Table 16. Results of the OLS model estimating the relationship between the presence of alleles associated with the Saltol gene on farmer’s plot and salinity risk levels in the MRD ----
curl_function ()
VH22_GPS <- read.csv('C:/Users/FKosmowski/OneDrive - CGIAR/DocumentsRedirected/2023 Activities/C2. CS-Map Ag plans/Georeferenced CS-Map/VH22_CSMAPS_all.csv')

table (VH22_GPS$Salinity_E)

VH22_GPS$Sal_Medium <- ifelse (VH22_GPS$Salinity_E == 2, TRUE, FALSE)
VH22_GPS$Sal_High <- ifelse (VH22_GPS$Salinity_E %in% c(3,4), TRUE, FALSE)
VH22_GPS$Sal_Med_High <- ifelse (VH22_GPS$Salinity_E %in% c(2,3,4), TRUE, FALSE)

VH22_GPS <- VH22_GPS %>%
  mutate(
    Sub1 = if_else(qSub1 == "[+p]", 1,
                   if_else(qSub1 == "?" | qSub1 == "[--]", 0, NA_real_))
    ,
    Saltol = case_when(
      Saltol == "[+p]-Aro" ~ 1,
      Saltol == "[+p]-Aus" ~ 1,
      Saltol == "?" ~ NA,
      Saltol == "[--]" ~ 0,
      TRUE ~ NA_real_
    )
  )

# n=156 Obs, rest are NA on Saltol
summary(lm(Saltol ~ Sal_Medium + Sal_High, data = VH22_GPS))
summ(lm(Sub1 ~ Sal_Medium + Sal_High, data = VH22_GPS), robust = 'HC1', digits = 3) 
summary(lm(Saltol ~ Sal_Med_High, data = VH22_GPS))

VH22_GPS$Sal_High <- ifelse (VH22_GPS$Salinity_E == 3, TRUE, FALSE)
summ(lm(Saltol ~ Sal_Medium + Sal_High, data = VH22_GPS), robust = 'HC1', digits = 3) 
summary_table <- summ(lm(Saltol ~ Sal_Medium + Sal_High, data = VH22_GPS), robust = 'HC1', digits = 3)



summary_table <- summ(lm(Saltol ~ Sal_Med_High, data = VH22_GPS), robust = 'HC1', digits = 3)

export_summs(summary_table, robust = 'HC1', digits = 3, 
             to.file = "docx", file.name = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Tab16.OLS_Saltol.docx")

table (VH22_GPS$Salinity_E, VH22_GPS$Saltol)
table (VH22_GPS$Salinity_E, VH22_GPS$Sub1)
table (VH22_GPS$Salinity_E, VH22_GPS$IRRI_Pare0)

# New version

# a) include 3 dummies — for low risk, medium risk and high risk, 

VH22_GPS$Sal_Low <- ifelse (VH22_GPS$Salinity_E == 1, TRUE, FALSE)
VH22_GPS$Sal_Medium <- ifelse (VH22_GPS$Salinity_E == 2, TRUE, FALSE)
VH22_GPS$Sal_High <- ifelse (VH22_GPS$Salinity_E == 3, TRUE, FALSE)

summary(lm(Saltol ~ Sal_Low + Sal_Medium + Sal_High, data = VH22_GPS))
  
summary_table <- summ(lm(Saltol ~ Sal_Medium + Sal_Low + Sal_Med_High, data = VH22_GPS), robust = 'HC1', digits = 3)
export_summs(summary_table, robust = 'HC1', digits = 3, 
             to.file = "docx", file.name = "C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/Tab16.OLS_Saltol.docx")

# b) 2 dummies — "low risk" and combine "medium risk and high risk", and possibly 

VH22_GPS$Sal_Med_High <- ifelse (VH22_GPS$Salinity_E %in% c(2,3), TRUE, FALSE)

summary(lm(Saltol ~ Sal_Low + Sal_Med_High, data = VH22_GPS))

# c) other combinations. 

VH22_GPS$Sal_Low_Med <- ifelse (VH22_GPS$Salinity_E %in% c(2,1), TRUE, FALSE)
summary(lm(Saltol ~ Sal_Low_Med + Sal_High, data = VH22_GPS))










# Table 13. Overview of cassava varietal adoption in Vietnam ----
#Note: These are unweighted figures 

curl_function ("data/raw/VHLSS_2023_Household/Combined_modules/Cassava.dat.csv")
Cassava.dat <- read.csv ("data/raw/VHLSS_2023_Household/Combined_modules/Cassava.dat.csv")

# Weights
#duplicated_rows <- weight2023[duplicated(weight2023$xa), ] # 21 communes have 2 EAs
# = At the EA level

# Import DNA data

curl_function ("data/raw/Genetics/Cassava/Cassava_assigns_758.csv")
Cass.DNA <- read.csv ("data/raw/Genetics/Cassava/Cassava_assigns_758.csv")
Cass.DNA <- Cass.DNA [, c(1,5,7,8)]
Cass.DNA$ID <- sub("_.*", "", Cass.DNA$Sample_chip)
Cass.DNA <- Cass.DNA [, c(5,2,3,4)]; names (Cass.DNA)[2] <- 'Genotype'; names (Cass.DNA)[3] <- 'CMD'; names (Cass.DNA)[4] <- 'DMC'

table (Cass.DNA$Genotype)
number_of_unique_values <- length(unique(Cass.DNA$Genotype)) # 75 

Cass.DNA <- Cass.DNA %>%
  mutate(Genotype_rec = recode(Genotype,
                               "1" = "Unknown",
                               "Ba Trăng" = "Ba Trăng",
                               "ba trăng" = "Ba Trăng",
                               "cao 94" = "Other landraces",
                               "Cao San" = "Cao San",
                               "cao sản" = "Cao San",
                               "Cao sản" = "Cao San",
                               "cao sẳn" = "Cao San",
                               "cao sản thân trắng" = "Cao San",
                               "địa phương" = "Dịa phương",
                               "địa phương/HL23" = "Dịa phương",
                               "giống địa phương" = "Dịa phương",
                               "giống sắn địa phương" = "Dịa phương",
                               "HL-S11" = "",
                               "khoai mì kè" = "Other landraces",
                               "KM140" = "",
                               "km 140" = "KM140",
                               "km140" = "KM140",
                               "KM297" = "",
                               "KM419" = "",
                               "KM505" = "KM7", # Similar variety with different names (Cong et al. 2016)
                               "KM57_VNM8" = "KM57",
                               "KM60/R60/TAI8" = "KM60",
                               "km94" = "KM94",
                               "KM94" = "KM94",
                               "k94" = "KM94",
                               "k94 " = "KM94",
                               "sắn KM94" = "KM94",
                               "KM94/KU50" = "KM94",
                               "km95" = "KM95",
                               "km 94" = "KM94",
                               "KM98.5/140.8" = "KM98-5",
                               "KM98/CR63/TAI9" = "KM98-5", # From Adriana's report
                               "km98" = "KM98-5",
                               "KM98-1" = "",
                               "KM98-5/BRA1305/ARG49" = "KM98-5",
                               "Pirun2" = "",
                               "R1/TAI1" = "",
                               "sắn" = "Cao San",
                               "Sắn cao" = "Cao San",
                               "Sắn cao sản" = "Cao San",
                               "sắn cao sản xanh" = "Cao San",
                               "mỳ cao sản" = "Cao San",
                               "sắn cao sản/sắn lá tre" = "Cao San",
                               "sắn chuối" = "Other landraces",
                               "Sắn chuối" = "Other landraces",
                               "sắn địa phương" = "Dịa phương",
                               "sắn đỏ" = "Other landraces",
                               "Sắn đỏ ( Sắn tre)" = "Sắn tre",
                               "sắn đỏ địa phương" = "Dịa phương",
                               "sắn lá tre" = "Sắn tre",
                               "sắn lá tre/địa phương" = "Dịa phương",
                               "sắn nếp" = "Other landraces",
                               "sắn nếp/giống địa phương" = "Dịa phương",
                               "sắn thân đỏ" = "Other landraces",
                               "sắn thường/sắn đỏ phú thọ" = "Other landraces",
                               "sắn thường để ăn củ và lá" = "Other landraces",
                               "săn thương" = "Other landraces",
                               "San trang" = "Other landraces",
                               "sắn trắng" = "Other landraces",
                               "sắn trắng/cao sản" = "Cao San",
                               "Sắn tre" = "Sắn tre",
                               "sắn xanh" = "Other landraces",
                               "SC9" = "SC9",
                               "sắn đồng nai" = "Other landraces",
                               "tăng sản" = "Other landraces",
                               "tây ninh" = "Other landraces",
                               "Tai xanh" = "Other landraces",
                               "tây mo" = "Other landraces",
                               "Sắn Chuối" = "Other landraces",
                               "38" = "Other landraces",
                               "lá tre" = "Other landraces",
                               "sắn xanh hòa bình" = "Other landraces",
                               "Unknown" = "Unknown",
                               "VNM1" = "VNM1",
                               .default = Genotype)) 

number_of_unique_values <- length(unique(Cass.DNA$Genotype_rec)) # 20 recoded as 'Other landraces'

Cass.DNA$Genotype_rec <- ifelse (Cass.DNA$Genotype_rec == "", Cass.DNA$Genotype, Cass.DNA$Genotype_rec)
Cass.DNA$Genotype_rec <- ifelse (Cass.DNA$Genotype == "", "Unknown", Cass.DNA$Genotype_rec)

Years <- read.csv ('C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Genetics/Cassava/Cassava_Years.csv') 
Cass.DNA <- merge (Cass.DNA, Years, by.x='Genotype_rec', by.y = 'Name', all.x=TRUE)  

genotype_df <- as.data.frame(table (Cass.DNA$Genotype_rec))
colnames(genotype_df) <- c("Genotype", "Frequency")
ordered_genotype_df <- genotype_df[order(-genotype_df$Frequency), ]
genotype_df$pctn <- round ((genotype_df$Frequency / sum(genotype_df$Frequency)) * 100, 1)

# Adding Age var to table
genotype_df <- merge(genotype_df, Years [,c (1,2,5)], by.x = "Genotype", by.y= "Name", all.x = TRUE) # Add INSTITUIONAL SOURCE LATER
genotype_df$Year <- (2023 - genotype_df$Year)
genotype_df$CIAT.related <- ifelse (is.na(genotype_df$CIAT.related), 0, genotype_df$CIAT.related)
genotype_df <- genotype_df[order(-genotype_df$Frequency), ]
names(genotype_df)[1] <- 'Cultivar'; names(genotype_df)[2] <- 'Number of samples'; names(genotype_df)[3] <- '% samples'; names(genotype_df)[4] <- 'Age (years)'; names(genotype_df)[5] <- 'CGIAR-related' #; names(genotype_df)[6] <- 'Origin'
genotype_df$`CGIAR-related` <- ifelse (genotype_df$`CGIAR-related` == '0', 'No', genotype_df$`CGIAR-related`)

# 24. CMD-resistant cassava varieties
# Marker CMD2, snpME00021 - homozygous favorable allele (TT) or heterozygous (TG)
Cass.DNA$CMD <- ifelse (Cass.DNA$CMD %in% c('T:T','T:G'), TRUE, FALSE)

# 23. High-starch improved cassava varieties
# Marker DM (Dry matter content), snpME00027 - homozygous favorable allele (CC). How about heterozygous (TC) samples? CHECK 
Cass.DNA$DMC <- ifelse (Cass.DNA$DMC == 'C:C', TRUE, FALSE) # 491/611
Cass.DNA$Year <- 2023 - Cass.DNA$Year

# QTL percent per cultivar
summary_table <- Cass.DNA %>%
  group_by(Genotype_rec) %>%
  summarise(
    DMC_QTL = round((sum(DMC == TRUE, na.rm = TRUE) / n()) * 100, 0),
    CMD_QTL = round((sum(CMD == TRUE, na.rm = TRUE) / n()) * 100, 0)
  )

# Add to overview table
genotype_df <- merge (genotype_df, summary_table, by.x='Cultivar', by.y='Genotype_rec', all.x=TRUE)
names(genotype_df)[6] <- 'Dry matter content QTL (in %)'; names(genotype_df)[7] <- 'CMD-resistant QTL (in %)'
genotype_df <- genotype_df[order(-genotype_df$`Number of samples`), ]

write.xlsx (genotype_df, 'Output/Tab13.Cassava.xlsx') 








