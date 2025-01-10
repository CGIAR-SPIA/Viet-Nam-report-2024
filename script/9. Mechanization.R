# ---
# Title: "Section 9. Mechanization"
# Author email: "simonchpa@gmail.com", "b.thanh@contractors.irri.org"
# Date: "December 2024"
# ---



rm(list=ls()) ## Clean work space


# Install and load package: 
packages <- c("sf", "ggplot2", "ggspatial", "cowplot", "haven", "rmapshaper",
             "readxl", "openxlsx", "readr", "tidyverse", "curl", "httr")
check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))

library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot) 
library(haven)
library(rmapshaper)
library(readxl)
library(openxlsx)
library(readr)
library(tidyverse)
library(curl)
library(httr)

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




# Function to download datasets from GitHub----

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




# Load and merge data----
## VH22
curl_function ("data/processed/VH22_data.csv")
vh22 <- read.csv ("data/processed/VH22_data.csv")
vh22 <- format_ID (vh22, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), 
                   widths = c(2, 3, 5, 3, 3))
vh22$IDHO <- paste0 (vh22$MAXA, vh22$MADIABAN, vh22$HOSO)


## VH23
curl_function ("data/processed/VH23_data.csv")
vh23 <- read.csv ("data/processed/VH23_data.csv") %>%
  select (-c (starts_with("straw"), Genotype_rec,
              Genotype, CMD, DMC, CIAT.related, starts_with("Strain"), SWCP, SWCP_confirmed, weight_cass, 
              weight_gift, weight_coffee)) %>%
  distinct() 
vh23 <- format_ID (vh23, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), 
                   widths = c(2, 3, 5, 3, 3))
vh23$IDHO <- paste0 (vh23$MAXA, vh23$MADIABAN, vh23$HOSO)

vh <- full_join (vh22, vh23) %>%
  filter (IDHO != "NANANA") #merge them


# Match them with province name
province <- read.csv(curl("https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Provinces_IDs.csv")) %>%
  select (-c("CSMAP"))#load provincial data

province$Province_name <- str_remove(province$Province_name, "Tinh ")  #remove "Province" word
province$Province_name <- str_remove(province$Province_name, "Thanh pho ")  #remove "City" word
province$Province_name[province$Province_name=="Ba Ria Vung Tau"] <- "Ba Ria - Vung Tau" #rewording

province$MATINH <- str_pad(province$MATINH, width = 2, pad = 0) # format ID of provincial data


vh <- left_join (vh, province, by = "MATINH") %>%
  relocate (c("Province_name", "Region"), .after = MATINH) #join two datasets


# Adoption at province level
adopt_prep <- vh %>%
  #filter(!is.na(weight_final_rice)) %>% 
  group_by(MATINH, panel) %>%
  summarize(
    adopt_mcbh = sum((mech_mini_combiner == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    adopt_cbh = sum((mech_combine_harvester == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    adopt_straw_baler = sum((mech_straw_baler == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    adopt_lll = sum((mech_laser_level == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    adopt_row_seeder = sum((mech_row_seeder == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    adopt_seed_blower = sum((mech_seed_blower == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
  ) %>%
  pivot_wider(names_from = panel, values_from = starts_with("adopt")) %>%
  select (-c("adopt_mcbh_2023", "adopt_lll_2022", "adopt_row_seeder_2023"))


# Mapping:
# Function to plot and save maps:
if (!dir.exists("Output/Mechanisation")) {
  dir.create("Output/Mechanisation", recursive = TRUE)
}

plot_map <- function(var) {
  # Set gradient limits based on the variable
  if (var %in% c("adopt_lll_2023", "adopt_lll_2022")) {
    gradient_limits <- c(0, 100)
    midpoint <- 50
  } else {
    gradient_limits <- c(0, 50)
    midpoint <- 25
  }
  
  # Create the plot
  plot <- modified_map %>%
    ggplot() + 
    aes(fill = .data[[var]]) + 
    geom_sf() + 
    scale_fill_gradient2(midpoint = midpoint, 
                         low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                         na.value = "#dbd0d0", space = "Lab", name = "In %", 
                         limits = gradient_limits) +  
    theme(plot.title = element_text(size = 8, hjust = 0.5)) +
    annotate("rect", xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10, 
             linewidth = 0.1, color = "black", fill = NA) +
    annotate("rect", xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7, 
             linewidth = 0.1, color = "black", fill = NA) + 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  # Save the plot
  ggsave(plot, filename = paste0("Output/Mechanisation/", var, ".png"), 
         width = 1080, height = 600, units = "px")
  
  return(plot)
}




# Prepare layer maps ----
map <- st_read("/vsicurl/https://raw.githubusercontent.com/CGIAR-SPIA/Vietnam-pre-report-2023/main/datasets/Shape_file/Shape_file/Province_with_Islands.shp")

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
rm(TS,TS_m,TS_map,HS,HS_m,HS_map,cnHS,cnTS,crs, map, IDProv, i, t)


modified_map <- format_ID (modified_map, c("MATINH"), c(2))
modified_map <- modified_map %>% left_join(adopt_prep,by="MATINH")


var_list_noise <- adopt_prep %>%
  colnames ()

var_list <- var_list_noise[-1]

map_list <- list ()
for (var in var_list) 
  {
  plot <- plot_map(var)
  map_list[[var]] <- plot
}

