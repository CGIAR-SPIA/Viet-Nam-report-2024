

# ---
# Title: "Section 5. Aquaculture and Capture Fisheries"
# Author email: "f,kosmowski@cgiar.org", "b.thanh@contractors.irri.org"
# Date: "October 2024"
# ---

rm (list = ls()) 

# Install and load packages ----
# Function to check and install packages

packages <- c("foreign", "tidyr", "readxl", "ggplot2", "dplyr", "stringr", "networkD3", "sf", "tidyverse", "haven",
              "readr", "curl", "httr", "jsonlite", "reshape", "reshape2", "stringr", "gridExtra")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

library (foreign)
library (tidyr)
library (readxl)
library (ggplot2)
library (dplyr)
library (stringr)
library (networkD3)
library (sf)
library (tidyverse)
library (haven)
library (readr)
library (curl)
library (httr)
library (jsonlite)
library (reshape)
library (reshape2)
library (stringr)
library (gridExtra)


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


# Download data files from GitHub and save to your working directory----
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

# Prepare layering map----
map <- st_read("/vsicurl/https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Shape_file/Shape_file/Province_with_Islands.shp")
IDProv <- read.csv(curl("https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Provinces_IDs.csv"))
names(IDProv)[1] = "MATINH"

map$MATINH <- 0
map$Region <- ""
t <- rep(NA,65)
for (i in 1:length(map$ADM1_EN)) {
  t[[i]] <- ifelse(grep(gsub('( -)','',gsub('( city)','',map$ADM1_EN[[i]])),
                        IDProv$Province_name) %>% length()==0,0,
                   grep(gsub('( -)','',gsub('( city)','',map$ADM1_EN[[i]])),
                        IDProv$Province_name))
  map$MATINH[[i]] <- ifelse(t[[i]]==0,NA,IDProv$MATINH[[t[[i]]]])
  map$Region[[i]] <- ifelse(t[[i]]==0,NA,IDProv$Region[[t[[i]]]])}
map %>% head() %>% print(width = 120) %>% colnames()
TS <- map %>% filter(ADM1_VI=="Truong Sa") %>% st_geometry()
cnTS = st_centroid(TS)
TS_m = (TS-cnTS) * .25 + cnTS + c(-5,0)
HS <- map %>% filter(ADM1_VI=="Hoang Sa") %>% st_geometry()
cnHS = st_centroid(HS)
HS_m =  (HS-cnHS) *.25 + cnHS + c(-2.5,0)
modified_map <- map %>% filter(!(ADM1_VI %in% c("Truong Sa","Hoang Sa")))

crs <- st_crs(modified_map)
TS_map <- map %>% filter(ADM1_VI=="Truong Sa") %>% st_set_geometry(TS_m) %>% st_set_crs(crs)
HS_map <- map %>% filter(ADM1_VI=="Hoang Sa") %>% st_set_geometry(HS_m) %>% st_set_crs(crs)
modified_map <- rbind(modified_map,TS_map,HS_map)
rm(TS,TS_m,TS_map,HS,HS_m,HS_map,cnHS,cnTS,crs)




# Figure 8. Number of large-scale aquaculture facilities in Viet Nam (2011-2023) ----

data <- data.frame(
  Year = factor(2011:2023, levels = 2011:2023),
  Values = c(4440, 4720, 4690, 4644, 4175, 2350, 2627, 2332, 2328, 2710, 2813, 2940, 2985)
)

# Plot the histogram
ggplot(data, aes(x = Year, y = Values)) +
  geom_bar(stat = "identity", fill = "blue4", color = "black") +
  theme_minimal() +
  labs(title = " ",
       x = "Year",
       y = "Aquaculture facilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Figure 11. Map of Sampled Tilapia Households and Hatcheries in 2023 ----

curl_function ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")
HH <- read.csv ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")

curl_function ("data/raw/Genetics/Tilapia/HatcheriesModule_WIDE_anonymized.csv")
Ha <- read.csv ("data/raw/Genetics/Tilapia/HatcheriesModule_WIDE_anonymized.csv")

Ha <- Ha [complete.cases(Ha$S_Q1.Longitude_1, Ha$S_Q1.Latitude_1), ] # 98/101
HH <- HH [complete.cases(HH$S_Q1.Longitude_1, HH$S_Q1.Latitude_1), ] # 215/231

Ha_sf <- st_as_sf(data.frame(longitude = Ha$S_Q1.Longitude_1, latitude = Ha$S_Q1.Latitude_1), coords = c("longitude", "latitude"))
HH_sf <- st_as_sf(data.frame(longitude = HH$S_Q1.Longitude_1, latitude = HH$S_Q1.Latitude_1), coords = c("longitude", "latitude"))

st_crs(Ha_sf) <- st_crs(HH_sf) <- st_crs(modified_map) <- st_crs("+proj=longlat")


ggplot() +
  geom_sf(data = modified_map, fill = "white", color = "black") +
  geom_sf(data = Ha_sf, color = "blue") +
  geom_sf(data = HH_sf, color = "limegreen") +
  scale_color_manual(values = c("Ha Points" = "blue", "HH Points" = "limegreen"),
                     name = "Legend Title") +
  ggtitle("Fig 1. Map of Sampled Tilapia Hatcheries and Households in 2023") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 





# Table 10. Characteristics of tilapia-farming households in Vietnam ----

curl_function ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")
HH <- read.csv ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")
HH <- HH %>%
  dplyr::rename (c("MATINH" = "hhidprovince",
            "MAHUYEN" = "hhiddistrict",
            "MAXA" = "hhidcommune",
            "HOSO" = "HH_ID"))
HH <- format_ID (HH, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO"), widths = c(2,3,5,3))


# Load GIFT DNA data:
curl_function("data/processed/GIFT.vars.VH24.csv")
GIFT <- read.csv("data/processed/GIFT.vars.VH24.csv") %>%
  select (c(MATINH:HOSO, StrainB, KYDIEUTRA, Strain, Strain_present, SubmissionDate, start, end, deviceid)) 
GIFT <- format_ID (GIFT, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO"), widths = c(2,3,5,3))

# Data 1; DEPOCEN-SPIA survey (n=204)
HH <- merge (HH [, -c(7:10)], GIFT , by=c("SubmissionDate", "start", "end", "deviceid"), all.y=TRUE)
# HH data is now n=204, GIFT assignment was added. The device info is used as merger




# Data 2; VHLSS survey (HH that could not be surveyed in DEPOCEN-SPIA survey taken out, from 250 to 204 households)
curl_function("data/raw/VHLSS_2023_Household/Combined_modules/Ho_Muc4B51A.csv")
Ho_Muc4B51A <- read.csv ("data/raw/VHLSS_2023_Household/Combined_modules/Ho_Muc4B51A.csv")
Ho_Muc4B51A <- format_ID (Ho_Muc4B51A, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO", "MADIABAN"), widths = c(2,3,5,3,2)) 
Ho_Muc4B51A$HOSO <- str_pad(Ho_Muc4B51A$HOSO, width = 3, pad = 0)


curl_function ("data/raw/VHLSS_2023_Household/Combined_modules/Ho_Muc4B51A_CS.csv")
Ho_Muc4B51A_CS <- read.csv ("data/raw/VHLSS_2023_Household/Combined_modules/Ho_Muc4B51A_CS.csv")
Ho_Muc4B51A_CS <- format_ID (Ho_Muc4B51A_CS, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO", "MADIABAN"), widths = c(2,3,5,3,2)) 
Ho_Muc4B51A$ID <- paste (Ho_Muc4B51A$MATINH, Ho_Muc4B51A$MAHUYEN, Ho_Muc4B51A$IDHO, sep='-')

HH <- format_ID (HH, columns = c("MATINH", "MAHUYEN", "MAXA", "HOSO"), widths = c(2,3,5,3)) 

# We merge to obtain the sample size of the DEPOCEN survey
Ho_Muc4B51A$ID <- paste (Ho_Muc4B51A$MATINH, Ho_Muc4B51A$MAHUYEN, Ho_Muc4B51A$MAXA, Ho_Muc4B51A$HOSO, sep='-')
Ho_Muc4B51A_CS$ID <- paste (Ho_Muc4B51A_CS$MATINH, Ho_Muc4B51A_CS$MAHUYEN, Ho_Muc4B51A_CS$MAXA, Ho_Muc4B51A_CS$HOSO, sep='-')
HH$ID <- paste (HH$MATINH, HH$MAHUYEN, HH$MAXA, HH$HOSO, sep='-')


Ho_Muc4B51A_filt <- merge (Ho_Muc4B51A [, -c(2:5,7,8)], HH, by = 'ID', all.y=TRUE)

Ho_Muc4B51A_CS <- Ho_Muc4B51A_CS[!duplicated(Ho_Muc4B51A_CS$ID), ]
Ho_Muc4B51A_CS_filt <- merge (Ho_Muc4B51A_CS [, -c(2:5,7,8)], HH, by = 'ID', all.y=TRUE)

# Add wt45 weights
curl_function ("data/raw/Weight/VHLSS_2023_weight.dta")
wt_2023 <- read_dta ("data/raw/Weight/VHLSS_2023_weight.dta")

wt_2023 <- format_ID (wt_2023, columns = c("tinh", "huyen", "xa", "diaban"), widths = c(2,3,5,2)) 
wt_2023$ID2 <- paste (wt_2023$tinh, wt_2023$huyen, wt_2023$xa, wt_2023$diaban, sep='-')

Ho_Muc4B51A_filt$ID2 <- paste (Ho_Muc4B51A_filt$MATINH, Ho_Muc4B51A_filt$MAHUYEN, Ho_Muc4B51A_filt$MAXA, Ho_Muc4B51A_filt$MADIABAN, sep='-')
Ho_Muc4B51A_CS_filt$ID2 <- paste (Ho_Muc4B51A_CS_filt$MATINH, Ho_Muc4B51A_CS_filt$MAHUYEN, Ho_Muc4B51A_CS_filt$MAXA, Ho_Muc4B51A_CS_filt$MADIABAN, sep='-')

Ho_Muc4B51A_filt <- merge (Ho_Muc4B51A_filt, wt_2023, by = 'ID2', all.x=TRUE)
Ho_Muc4B51A_CS_filt <- merge (Ho_Muc4B51A_CS_filt, wt_2023, by = 'ID2', all.x=TRUE)

Ho_Muc4B51A_filt$wt45; Ho_Muc4B51A_CS_filt$wt45


# Tab 10
# Purchased fingerlings in the last 3 years
Ho_Muc4B51A_filt$Purchased <- ifelse (Ho_Muc4B51A_filt$T_Q2_1 == 'option_1' | Ho_Muc4B51A_filt$T_Q2_1 == 'option_2' | Ho_Muc4B51A_filt$T_Q2_1 == 'option_8', FALSE, TRUE)
table (Ho_Muc4B51A_CS_filt$Purchased)

# 1.A Size of land under aquaculture. Note: VHLSS has more households
V1a_Obs <- sum (!is.na(Ho_Muc4B51A_CS_filt$M4B51A_C7)) # Obs.

V1a_mean <- sum(Ho_Muc4B51A_CS_filt$M4B51A_C7 * Ho_Muc4B51A_CS_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_CS_filt$wt45[!is.na(Ho_Muc4B51A_CS_filt$M4B51A_C7)]) / 10000

V1a_sd <- sqrt(sum(Ho_Muc4B51A_CS_filt$wt45 * 
                     ((Ho_Muc4B51A_CS_filt$M4B51A_C7 / 10000) - V1a_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_CS_filt$wt45[!is.na(Ho_Muc4B51A_CS_filt$M4B51A_C7)]))

# 1.B Number of ponds maintained 
V1b_Obs <- sum (!is.na(Ho_Muc4B51A_filt$S_Q0b_1))

V1b_mean <-sum(Ho_Muc4B51A_filt$S_Q0b_1 [Ho_Muc4B51A_filt$S_Q0b_1 != 99]* Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$S_Q0b_1 != 99], na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$S_Q0b_1) & !is.na (Ho_Muc4B51A_filt$wt45)])

V1b_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$S_Q0b_1 != 99] * (Ho_Muc4B51A_filt$S_Q0b_1 [Ho_Muc4B51A_filt$S_Q0b_1 != 99] - V1b_mean)^2, na.rm = TRUE) / 
      sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$S_Q0b_1) & !is.na (Ho_Muc4B51A_filt$wt45)]))


# 1.C Experience of tilapia farming
Ho_Muc4B51A_filt$Experience <- 2023 - Ho_Muc4B51A_filt$M4B51A_C1

V1c_Obs <- sum (!is.na(Ho_Muc4B51A_filt$Experience))

V1c_mean <- sum(Ho_Muc4B51A_filt$Experience * Ho_Muc4B51A_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Experience) & !is.na (Ho_Muc4B51A_filt$wt45)])

V1c_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 * 
                     (Ho_Muc4B51A_filt$Experience - V1c_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Experience) & !is.na (Ho_Muc4B51A_filt$wt45)]))


# 2.A Tilapia monoculture production (in %) 
Ho_Muc4B51A_filt$Prod <- ifelse (Ho_Muc4B51A_filt$P_Q6_1 == 'option_2', TRUE, FALSE)

V2a_Obs <- sum (!is.na(Ho_Muc4B51A_filt$Prod))

V2a_mean <- sum(Ho_Muc4B51A_filt$Prod * Ho_Muc4B51A_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Prod) & !is.na (Ho_Muc4B51A_filt$wt45)]) * 100

V2a_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 * (Ho_Muc4B51A_filt$Prod*100 - V2a_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Prod) & !is.na (Ho_Muc4B51A_filt$wt45)]))

# 2.B Monosex tilapia production (in %) 
Ho_Muc4B51A_filt$Mono <- ifelse (Ho_Muc4B51A_filt$T_Q7_1 == 'option_1', TRUE, FALSE)

V2b_Obs <- sum (!is.na(Ho_Muc4B51A_filt$Mono [Ho_Muc4B51A_filt$Purchased == TRUE]))

V2b_mean <- sum(Ho_Muc4B51A_filt$Mono [Ho_Muc4B51A_filt$Purchased == TRUE] * Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE], na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Mono) & !is.na (Ho_Muc4B51A_filt$wt45)])* 100

V2b_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE] * (Ho_Muc4B51A_filt$Mono [Ho_Muc4B51A_filt$Purchased == TRUE]* 100 - V2b_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$Mono) & !is.na (Ho_Muc4B51A_filt$wt45)]))

# 2.C Household consumes tilapia (in %)
Ho_Muc4B51A_filt$T_Q19b_consumption <- ifelse (grepl('option_1', Ho_Muc4B51A_filt$T_Q19b), TRUE, FALSE)

V2c_Obs <- sum (!is.na(Ho_Muc4B51A_filt$T_Q19b_consumption))

V2c_mean <- sum(Ho_Muc4B51A_filt$T_Q19b_consumption * Ho_Muc4B51A_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q19b_consumption) & !is.na (Ho_Muc4B51A_filt$wt45)]) * 100

V2c_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 * (Ho_Muc4B51A_filt$T_Q19b_consumption * 100 - V2c_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q19b_consumption) & !is.na (Ho_Muc4B51A_filt$wt45)]))

# 2.D Household sells tilapia (in %)
Ho_Muc4B51A_filt$T_Q19b_selling <- ifelse (grepl('option_2', Ho_Muc4B51A_filt$T_Q19b), TRUE, FALSE)

V2d_Obs <- sum (!is.na(Ho_Muc4B51A_filt$T_Q19b_selling))

V2d_mean <- sum(Ho_Muc4B51A_filt$T_Q19b_selling * Ho_Muc4B51A_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q19b_selling) & !is.na (Ho_Muc4B51A_filt$wt45)])* 100

V2d_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 * (Ho_Muc4B51A_filt$T_Q19b_selling * 100 - V2d_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q19b_selling) & !is.na (Ho_Muc4B51A_filt$wt45)]))

# 3.A Year of last purchase
Ho_Muc4B51A_filt$Year <- as.numeric (str_sub(Ho_Muc4B51A_filt$T_Q0, -4))

V3a_Obs <- sum (!is.na(Ho_Muc4B51A_filt$Year [Ho_Muc4B51A_filt$Purchased == TRUE]))

V3a_mean <- round (sum(
  Ho_Muc4B51A_filt$Year[Ho_Muc4B51A_filt$Purchased == TRUE] * 
    Ho_Muc4B51A_filt$wt45[Ho_Muc4B51A_filt$Purchased == TRUE],
  na.rm = TRUE
) / sum(
  Ho_Muc4B51A_filt$wt45[Ho_Muc4B51A_filt$Purchased == TRUE & !is.na(Ho_Muc4B51A_filt$Year)],
  na.rm = TRUE
), 0)

V3a_sd <- NA

# 3.B Size of fingerlings at purchasing (in cm)
V3b_Obs <-  sum (!is.na(Ho_Muc4B51A_filt$T_Q11b_1 [Ho_Muc4B51A_filt$Purchased == TRUE]))

V3b_mean <- sum(Ho_Muc4B51A_filt$T_Q11b_1 [Ho_Muc4B51A_filt$Purchased == TRUE] * Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE], na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q11b_1) & !is.na (Ho_Muc4B51A_filt$wt45)])

V3b_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE] * (Ho_Muc4B51A_filt$T_Q11b_1 [Ho_Muc4B51A_filt$Purchased == TRUE] - V3b_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q11b_1) & !is.na (Ho_Muc4B51A_filt$wt45)]))


mean (Ho_Muc4B51A_filt$T_Q11b_1 [Ho_Muc4B51A_filt$Purchased == TRUE], na.rm=TRUE); 


# 3.C Quantity at stocking (in 1,000)
Ho_Muc4B51A_filt$T_Q9_1_rec <- ifelse (Ho_Muc4B51A_filt$T_Q9a_1 == 'option_3', Ho_Muc4B51A_filt$T_Q9_1*10000, 
                         ifelse (Ho_Muc4B51A_filt$T_Q9a_1 == 'option_2', Ho_Muc4B51A_filt$T_Q9_1*1000, 
                                 ifelse (Ho_Muc4B51A_filt$T_Q9a_1 == 'option_1', Ho_Muc4B51A_filt$T_Q9_1*1, 
                                         ifelse (Ho_Muc4B51A_filt$T_Q9a_1 == 'option_4', Ho_Muc4B51A_filt$T_Q9_1*900, Ho_Muc4B51A_filt$T_Q9_1)))) # Assumes / 3 cm: 800-1000 fishes/ kg


V3c_Obs <- sum (!is.na(Ho_Muc4B51A_filt$T_Q9_1_rec [Ho_Muc4B51A_filt$Purchased == TRUE]))

V3c_mean <- sum(Ho_Muc4B51A_filt$T_Q9_1_rec [Ho_Muc4B51A_filt$Purchased == TRUE] * Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE], na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q9_1_rec) & !is.na (Ho_Muc4B51A_filt$wt45)])

V3c_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 [Ho_Muc4B51A_filt$Purchased == TRUE] * (Ho_Muc4B51A_filt$T_Q9_1_rec [Ho_Muc4B51A_filt$Purchased == TRUE] - V3c_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$T_Q9_1_rec) & !is.na (Ho_Muc4B51A_filt$wt45)]))

mean (Ho_Muc4B51A_filt$T_Q9_1_rec [Ho_Muc4B51A_filt$Purchased == TRUE]); 


# 3.D Size at sampling (cm)

V3d_Obs <- sum (!is.na(Ho_Muc4B51A_filt$S_Q5_1))

V3d_mean <- sum(Ho_Muc4B51A_filt$S_Q5_1 * Ho_Muc4B51A_filt$wt45, na.rm = TRUE) / 
  sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$S_Q5_1) & !is.na (Ho_Muc4B51A_filt$wt45)])

V3d_sd <- sqrt(sum(Ho_Muc4B51A_filt$wt45 * (Ho_Muc4B51A_filt$S_Q5_1 - V3d_mean)^2, na.rm = TRUE) / 
                 sum(Ho_Muc4B51A_filt$wt45[!is.na(Ho_Muc4B51A_filt$S_Q5_1) & !is.na (Ho_Muc4B51A_filt$wt45)]))



# Combine all into a table and clean

var_names <- c("Size of land under aquaculture (in ha)", "Number of ponds",
               "Experience of tilapia farming (in years)", "Tilapia monoculture production (in %)",
               "Monosex tilapia production (in %)", "Household consumes tilapia (in %)", "Household sells tilapia (in %)",
               "Year of last purchase", "Size of fingerlings at purchasing (in cm)", "Quantity at stocking", "Size at sampling (in cm)")

# Observations, Means, and Standard Deviations
Obs <- c(V1a_Obs, V1b_Obs, V1c_Obs, V2a_Obs, V2b_Obs, V2c_Obs, V2d_Obs, V3a_Obs, V3b_Obs, V3c_Obs, V3d_Obs)
Means <- c(V1a_mean, V1b_mean, V1c_mean, V2a_mean, V2b_mean, V2c_mean, V2d_mean, V3a_mean, V3b_mean, V3c_mean, V3d_mean)
SDs <- c(V1a_sd, V1b_sd, V1c_sd, V2a_sd, V2b_sd, V2c_sd, V2d_sd, V3a_sd, V3b_sd, V3c_sd, V3d_sd)

summary_table <- data.frame(
  Variable = var_names,
  Observations = Obs,
  Mean = round (Means, 2),
  SD = round (SDs, 2)
)

summary_table

write.csv(summary_table, "Output/Tab10.csv", row.names = FALSE)

# Hatchery characteristics (text-only)
curl_function ("data/raw/Genetics/Tilapia/Hatch_anonymised.csv")
Ha <- read.csv ("data/raw/Genetics/Tilapia/Hatch_anonymised.csv")

curl_function ("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Provinces_IDs <- read.csv("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Provinces_IDs$MATINH <- as.numeric (Provinces_IDs$MATINH); Ha$Province_ID <- as.numeric (Ha$Province_ID)
Ha <- merge (Ha, Provinces_IDs, by.x='Province_ID', by.y='MATINH', all.x=TRUE)

summary (Ha$numplots); sd (Ha$numplots)# 1.26 strain on average
summary (Ha$S_Q0b_1); sd (Ha$S_Q0b_1) # Nmber of ponds maintained by strain

Ha$R_Q8_rec <- ifelse (Ha$R_Q8a_1 == 'option_3', Ha$R_Q8_1*10000, 
                       ifelse (Ha$R_Q8a_1 == 'option_2', Ha$R_Q8_1*1000, 
                               ifelse (Ha$R_Q8a_1 == 'option_1', Ha$R_Q8_1*1, 
                                       ifelse (Ha$R_Q8a_1 == 'option_4', Ha$R_Q8_1*900, Ha$R_Q8_1)))) # Assumes / 3 cm: 800-1000 fishes/ kg

# Hatchery type
table (Ha$I_Q4) # 57/87 private; 7/87 cooeprative and 23/87 public
table (Ha$I_Q4o)

# Monosex tilapia production 
table (Ha$R_Q5_1) # 14/87 produced the male tilapia.
table (Ha$R_Q6_1) # large majority through hormone treatment

# Hatchery sells GIFT-derived strains
Ha$GIFT_for_sale <- ifelse (Ha$R_Q7_1 == 'option_1' | Ha$R_Q7_2 == 'option_1' | Ha$R_Q7_3 == 'option_1', TRUE, FALSE)
table (Ha$GIFT_for_sale) # 16/87

# In the last 12 months, which percentage of your sales were to other hatcheries? 
Ha$R_Q8c_1 [is.na (Ha$R_Q8c_1)] <- 0
table (Ha$R_Q8c_1) 
mean (Ha$R_Q8c_1) # These who sell to other hatcheries sell 34% on average

by (Ha$R_Q8c_1, Ha$Region, mean, na.rm=TRUE)
mean (Ha$R_Q8b_1 == 'option_1'); sd (Ha$R_Q8b_1== 'option_1')

# Origin of fingerlings is from the same province
Ha$R_Q3a_1 [48] <- '72'; Ha$R_Q3a_1 [Ha$R_Q3a_1 == 'Hai Phong'] <- '31'; Ha$R_Q3a_1 [52] <- '31'
Ha$Origin_fin <- ifelse (Ha$R_Q3a_1 == Ha$Province_ID, TRUE, FALSE)
table (Ha$Origin_fin) # 18/87

# Volumes sold by province
table (Ha$R_Q8_rec) # Sold fingerlings last 12 months
mean (Ha$R_Q8_rec); sd (Ha$R_Q8_rec) # Sold fingerlings last 12 months
by (Ha$R_Q8_rec, Ha$Region.x, mean) # Southeast, RRD and Central Highlands sold the most
sum (Ha$R_Q8_rec)

# Number of stains availabe for sale in the last 12 months
table (Ha$numplot) # 68/87

# Is ${R_Q1} related to the GIFT tilapia strain or not? 13/87 replied yes (15%)
table (Ha$R_Q7_1) 

# Table 11. Percentage assignment of hatcheries and household tilapia samples to modal core populations ----
# Note: Table 3 was taken from Hamilton (2024). R code is included in the study.


# Figure 12. Map of tilapia strain assignments on three different samples: (a) all tilapia-farming households (n=204), (b) households that purchased fingerlings in the last 3 years (n=62), and (c) hatcheries (n=89) ----

# Map C. Strain by hatcheries
curl_function("data/raw/Genetics/Tilapia/HatcheriesModule_WIDE_anonymized.csv")
Ha <- read.csv ("data/raw/Genetics/Tilapia/HatcheriesModule_WIDE_anonymized.csv")

curl_function("data/raw/Genetics/Tilapia/Hatch.vars.csv")
Ha.DNA <- read.csv ("data/raw/Genetics/Tilapia/Hatch.vars.csv") # See l.840 for sourcing
# Note: imported from previous section

Ha <- merge (Ha [, c(11,98,99)], Ha.DNA, by = 'I_Q2', all.y=TRUE);
Ha <- Ha [complete.cases(Ha$S_Q1.Longitude_1, Ha$S_Q1.Latitude_1), ]

Ha$S1_Strain <- ifelse (Ha$S1_Strain == 'RIA1', 'RIA1 lineage', 
                        ifelse (Ha$S1_Strain == '03_BEST', 'BEST', 
                                ifelse (Ha$S1_Strain == 'GenoMAR Gain', 'GenoMar Gain', Ha$S1_Strain)))

desired_order <- c('RIA1 lineage', 'BEST', 'Molobicus', 'GenoMar Gain'); Ha$S1_Strain <- factor(Ha$S1_Strain, levels = desired_order)

Ha_sf <- st_as_sf(data.frame(longitude = Ha$S_Q1.Longitude_1, latitude = Ha$S_Q1.Latitude_1), coords = c("longitude", "latitude"))
st_crs(Ha_sf) <- st_crs(modified_map) <- st_crs("+proj=longlat")

FigC <- ggplot() +
  geom_sf(data = modified_map, fill = "white", color = "black") +
  geom_sf(data = Ha_sf, aes(color = Ha$S1_Strain), size = 2.5) +
  ggtitle("(c) Hatcheries") +
  scale_color_manual(name = "Tilapia strains",
                     values = c("RIA1 lineage" = "dodgerblue",
                                "BEST" = "forestgreen",
                                "Molobicus" = "darkorange",
                                "O. Mossambicus" = "firebrick",
                                'GenoMar Gain' = 'darkorchid1',
                                "Unassigned" = "grey")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3), legend.position = "bottom")  # Position legend at the bottom

FigC

# Map A. Strain by households 
curl_function("data/raw/Genetics/Tilapia/GIFT.vars.VH24.csv")
GIFT <- read.csv ('data/raw/Genetics/Tilapia/GIFT.vars.VH24.csv')

curl_function ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")
HH <- read.csv ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")

HH <- merge (HH [, c(1,2,3,5,15,223,224)], GIFT, by = c("SubmissionDate", "start", "end", "deviceid"), all.y=TRUE)

HH <- HH [complete.cases(HH$S_Q1.Longitude_1, HH$S_Q1.Latitude_1), ]

HH$Strain <- ifelse (HH$Strain == 'RIA1', 'RIA1 lineage', 
                     ifelse (HH$Strain == '03_BEST', 'BEST',
                             ifelse (HH$Strain == '05_Molobicus', 'Molobicus', 
                                     ifelse (HH$Strain == '10_Mossambicus', 'O. Mossambicus', HH$Strain))))

desired_order <- c('RIA1 lineage', 'BEST', 'Molobicus', 'O. Mossambicus', 'Unassigned')
HH$Strain <- factor(HH$Strain, levels = desired_order)

HH_sf <- st_as_sf(data.frame(longitude = HH$S_Q1.Longitude_1, latitude = HH$S_Q1.Latitude_1), coords = c("longitude", "latitude"))
st_crs(HH_sf) <- st_crs(modified_map) <- st_crs("+proj=longlat")

FigA <- ggplot() +
  geom_sf(data = modified_map, fill = "white", color = "black") +
  geom_sf(data = HH_sf, aes (color = HH$Strain), size = 2.5) +
  ggtitle("(a) All households") +
  scale_color_manual(name = "Tilapia strains",
                     values = c("RIA1 lineage" = "dodgerblue",
                                "BEST" = "forestgreen",
                                "Molobicus" = "darkorange",
                                "O. Mossambicus" = "firebrick",
                                "Unassigned" = "grey")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3), legend.position = "bottom")  # Position legend at the bottom


# Map B. Strain by purchasing-households 
#HH <- read.csv ("C:/Users/FKosmowski/OneDrive - CGIAR/DocumentsRedirected/2023 Activities/D. GIFT Experiment/Data/Final datasets/HouseholdModule_WIDE.csv")
# Run previous lines to have 'HH'

# Data 1; DEPOCEN-SPIA survey (n=204)
curl_function ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")
HH <- read.csv ("data/raw/Genetics/Tilapia/HouseholdModule_WIDE_anonymized.csv")

HH <- HH %>%
  rename (c("MATINH" = "hhidprovince",
            "MAHUYEN" = "hhiddistrict",
            "MAXA" = "hhidcommune",
            "HOSO" = "HH_ID"))
HH <- merge (HH, GIFT , by=c("SubmissionDate", "start", "end", "deviceid"), all.y=TRUE) # HH data is now n=204, GIFT assignment was added. The device info is used as merger

HH$Purchased <- ifelse (HH$T_Q2_1.x == 'option_1' | HH$T_Q2_1.x == 'option_2' | HH$T_Q2_1.x == 'option_8', FALSE, TRUE)
table (HH$Purchased) # Correct n=62

HH_purch <- HH [complete.cases(HH$S_Q1.Longitude_1, HH$S_Q1.Latitude_1) & HH$Purchased == TRUE, ] 

HH_purch$Strain <- ifelse (HH_purch$Strain == 'RIA1', 'RIA1 lineage', 
                           ifelse (HH_purch$Strain == '03_BEST', 'BEST',
                                   ifelse (HH_purch$Strain == '05_Molobicus', 'Molobicus', 
                                           ifelse (HH_purch$Strain == '10_Mossambicus', 'O. Mossambicus', HH_purch$Strain))))

desired_order <- c('RIA1 lineage', 'BEST', 'Molobicus', 'O. Mossambicus', 'Unassigned')
HH_purch$Strain <- factor(HH_purch$Strain, levels = desired_order)


HH_purch_sf <- st_as_sf(data.frame(longitude = HH_purch$S_Q1.Longitude_1, latitude = HH_purch$S_Q1.Latitude_1), coords = c("longitude", "latitude"))
st_crs(HH_purch_sf) <- st_crs(modified_map) <- st_crs("+proj=longlat")

FigB  <- ggplot() +
  geom_sf(data = modified_map, fill = "white", color = "black") +
  geom_sf(data = HH_purch_sf, aes (color = HH_purch$Strain), size = 2.5) +
  ggtitle("(b) Households purchased fingerlings") +
  scale_color_manual(name = "Tilapia strains",
                     values = c("RIA1 lineage" = "dodgerblue",
                                "BEST" = "forestgreen",
                                "Molobicus" = "darkorange",
                                "O. Mossambicus" = "firebrick",
                                "Unassigned" = "grey")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3), legend.position = "none")  # Position legend at the bottom

FigB
# Save in 1300 * 915 pixels
# Note: Legends are turned off and some plot, and graphically rearranged later

library (gridExtra)
grid.arrange(FigA, FigB, FigC, ncol = 3)


