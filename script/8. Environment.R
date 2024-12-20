


# ---
# Title: "Section 8. Environment Conservation"
# Author email: fkosmowski@gmail.com, b.thanh@contractors.irri.org
# Date: "November 2024"
# ---

rm (list = ls ())


# Install and load packages ----

# Function to check and install packages

packages <- c("this.path", "tidyverse", "ggplot2", "gridExtra", "fastDummies", "ggpolypath", "eulerr", "readxl", "stringr", "flextable", "rvest", "iai",
              "jtools", "summarytools", "broom", "estimatr", "sf", "curl", "survey", "haven", "httr")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

options (scipen = 999) #format of numbers

#Load packages
library (this.path)
library (tidyverse)
library (ggplot2)
library (gridExtra)
library (fastDummies)
library (ggpolypath)
library (eulerr)
library (readxl)
library (stringr)
library (flextable)
library (sf)
library (curl)
library (survey)
library (haven)
library (httr)

#Set working directory to the location of this file
setwd(this.path::here())   

#Customized function to format ID

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



# Prepare layer maps ----
# Load provincial data: 
province <- read.csv(curl("https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Provinces_IDs.csv")) %>%
  select (-c("CSMAP"))#load provincial data

province$Province_name <- str_remove(province$Province_name, "Tinh ")  #remove "Province" word
province$Province_name <- str_remove(province$Province_name, "Thanh pho ")  #remove "City" word
province$Province_name[province$Province_name=="Ba Ria Vung Tau"] <- "Ba Ria - Vung Tau" #rewording

province$MATINH <- str_pad(province$MATINH, width = 2, pad = 0) # format ID of provincial data


map <- st_read("/vsicurl/https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Shape_file/Shape_file/Province_with_Islands.shp")

map$MATINH <- 0
map$Region <- ""
t <- rep(NA,65)
for (i in 1:length(map$ADM1_EN)) {
  t[[i]] <- ifelse(grep(gsub('( -)','',gsub('( city)','',map$ADM1_EN[[i]])),
                        province$Province_name) %>% length()==0,0,
                   grep(gsub('( -)','',gsub('( city)','',map$ADM1_EN[[i]])),
                        province$Province_name))
  map$MATINH[[i]] <- ifelse(t[[i]]==0,NA,province$MATINH[[t[[i]]]])
  map$Region[[i]] <- ifelse(t[[i]]==0,NA,province$Region[[t[[i]]]])}
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
rm(TS,TS_m,TS_map,HS,HS_m,HS_map,cnHS,cnTS,crs, map, i, t)





# Prepare data----
curl_function ("data/raw/VHLSS_2024_Commune/SPIA_ThongTinXa_Q1-3.csv")
pfes <- read.csv ("data/raw/VHLSS_2024_Commune/SPIA_ThongTinXa_Q1-3.csv") %>%
  select (c(MATINH, MAHUYEN, MAXA, M43_C1, M43_C2))%>%
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


# Estimate of the reach of PFES ----
reach_pfes <- pfes_joined %>%
  filter (!is.na(pfes_dummy)) %>%
  group_by (panel, pfes_dummy) %>%
  summarise (twt = sum (n_hh_pop, na.rm = TRUE)) %>%
  group_by (panel) %>%
  mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
  filter (pfes_dummy == 1) %>%
  select (-c(sum_twt))

# Figure 24a. Percentage of communes per province with operational Payments for Forest Environmental Services areas ----
pfes_prep <- pfes_joined %>%
  filter (!is.na(pfes_dummy)) %>%
  group_by (pfes_dummy, MATINH) %>%
  summarise (twt = sum (n_hh_pop, na.rm = TRUE)) %>%
  group_by (MATINH) %>%
  mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
  mutate (pct = twt * 100 /sum_twt) %>%
  filter (pfes_dummy == 1) %>%
  select (-c(pfes_dummy, twt, sum_twt))

pfes_prep <- left_join (pfes_prep, province)


# Maps ----



modified_map <- modified_map %>% 
  left_join(pfes_prep,by="MATINH")

pfes_map <- modified_map %>%
  ggplot() + 
  aes(fill = pct) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In %", limits = c(0, 100)) +  
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


ggsave (pfes_map, filename = "Output/Figure 24a.png",
        width = 6, height = 10, dpi = 1024)



# Figure 24b. Percentage of communes per province and total forest area under PFES in 2023 ----
pfes_area <- pfes %>%
  filter (!is.na(M43_C2)) %>%
  group_by (MATINH) %>%
  summarise (sum_area = sum(M43_C2, na.rm = TRUE)) 


modified_map <- modified_map %>% 
  left_join(pfes_area,by="MATINH")

pfes_map_area <- modified_map %>%
  ggplot() + 
  aes(fill = sum_area) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In hectare") +  
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

pfes_map_area

ggsave (pfes_map_area, filename = "Figure 24b.png",
        width = 6, height = 10, dpi = 1024)

