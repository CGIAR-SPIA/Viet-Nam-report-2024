
#---
# Title: "Section 10. Sustainable Intensification Practices'"
# Authors email: b.thanh@contractors.irri.org, fkosmowski@gmail.com
# Date: "11/2024"
# ---


# This script sources from these following datasets:
# First is the "VH23_data.csv" dataset that includes VHLSS 2023 modules on 1M5R generally
# Second is the dataset "df_1m5r.csv" that includes more information about the practices of farmers related to 1M5R
# Third is a spatial dataset under shapefile format that has geospatial information of provinces in Vietnam
# Last is a file extracted and recoded from 37 provincial agricultural plans from 2020-2024 (not all years are included in each province) 
# under the name "ag_plan_recode_1m5r.xlsx". Please be noted that this file is used for the purpose of this subsection only.  

rm(list = ls()) #start clean

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




# Function to plot maps: ----
create_plot <- function(modified_map, fill_var, title) {
  ggplot(modified_map) + 
    aes_string(fill = paste0("factor(", fill_var, ")")) +  # Convert fill_var to factor
    geom_sf() + 
    scale_fill_manual(
      values = c("0" = "#ffffff", "1" = "#1c4c6f"),  # Define colors for each factor level
      na.value = "#dbd0d0",                          # Color for NA values
      name = NULL,                                    # Remove legend title
      breaks = c("1", "0"),                           # Define which levels to display
      labels = c("Yes", "No")                         # Custom labels for the legend
    ) +  
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),  # Increase title size to 14
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      legend.title = element_blank(), # Ensure legend title is removed
      axis.ticks = element_blank() 
    ) +
    geom_rect(
      aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
      linewidth = 0.1, color = "black", fill = NA
    ) +
    geom_rect(
      aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
      linewidth = 0.1, color = "black", fill = NA
    )
}


# Function to plot by component:----
plot_component_fn <- function(data, footnote) {
  plot <- data %>%
    ggplot() +
    geom_bar(aes(x=position, y=adoption, fill = group), stat="identity", alpha=0.8) +
    labs (caption = footnote,
          fill = "") +
    xlab("") +
    ylab ("(%)") +
    theme (axis.text.y = element_text (size = 12),
           legend.text = element_text(size = 12),
           legend.title = element_text(size = 15),
           title = element_text (size = 14),
           axis.text.x = element_blank (),
           axis.ticks.x = element_blank(),
           legend.position = "bottom",
           strip.text = element_text (size = 12),
           plot.caption = element_text (hjust = 0)) +
    scale_y_continuous(breaks = seq(0, 100, 20),
                       expand = c(0,1)) +
    scale_fill_manual (labels = c("Strict", "Lenient"),
                       values = c("#006D5B", "#FFBF00")) +
    facet_wrap(~component, scales = "fixed") 
  
  return (plot)
}


# Load and merge data----
curl_function ("data/processed/VH23_data.csv")

df_23 <- read.csv ("data/processed/VH23_data.csv") %>%
  select (-c (starts_with("mech"), starts_with("straw"), Genotype_rec,
              Genotype, CMD, DMC, CIAT.related, starts_with("Strain"), SWCP, SWCP_confirmed, weight_cass, 
              weight_gift, weight_coffee)) %>%
  distinct() 

df_23 <- format_ID(df_23, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) ) #format admin-ID

df_23$IDHO <- paste0 (df_23$MAXA, df_23$MADIABAN, df_23$HOSO) #unique household ID




curl_function("data/raw/VHLSS_2023_Household/Final/Final_1M5R/final_1m5r.csv")
df_1m5r <- read.csv ("data/raw/VHLSS_2023_Household/Final/Final_1M5R/final_1m5r.csv") %>%
  select (c(MATINH:IDHO, rice_name, ws_rate_ha, seed_rate_duration, pre_rate_ha, KYDIEUTRA, 
            ws_method, n_app_fert, sum_fert, 
            d_1m5r_nitrogen_2app, d_1m5r_nitrogen_110kg, d_1m5r_nitrogen_3app, d_1m5r_nitrogen_100kg,
            n_app_drug, n_app_insect_fungi, starts_with("n_drug_mix_"),
            d_1m5r_pest_3app, d_1m5r_pest_6app, d_1m5r_pest_40d_sowing, 
            d_1m5r_pest_flowering, d_1m5r_pest_20d_harvest,
            starts_with ("purpose")))

df_1m5r <- format_ID(df_1m5r, columns = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO"), widths = c(2, 3, 5, 3, 3) ) 

df_1m5r$IDHO <- paste0 (df_1m5r$MAXA, df_1m5r$MADIABAN, df_1m5r$HOSO)

dup_id <- df_1m5r[which(duplicated(df_1m5r$IDHO)),]$IDHO

df_1m5r <- df_1m5r %>%
  filter (!IDHO %in% dup_id | (IDHO %in% dup_id & KYDIEUTRA == 4)) #there are some households surveyed in two quarters, but quarter 4 seems to be purposefully collected, so we keep values of Q4


df <- left_join (df_23, df_1m5r) #join two dataset


df <- df %>%
   dplyr::rename (c("strict_1m" = "d_1m5r_certified",
            "strict_1r" = "d_1m5r_seed_100kg",
            "lenient_1r" = "d_1m5r_seed_120kg"))  #rename vars

df$MATINH <- str_pad(df$MATINH, width = 2, pad = 0) # format ID of newly merged data


df <- left_join (df, province, by = "MATINH") %>%
  relocate (c("Province_name", "Region"), .after = MATINH) #join two datasets



# Figure 34: Vietnamese provinces referencing 1M5R/3R3G components in provincial agriculture plans  ----
curl_function ("data/processed/ag_plan_1m5r_recode.xlsx")
ag_plan <- read_excel ("data/processed/ag_plan_1m5r_recode.xlsx") #load ag_plan data

# Add MATINH to the original excel file

ag_plan <- ag_plan %>%
  mutate (package = case_when (mention_1M5R == 1 | mention_3R3G == 1 ~ 1,
                               mention_1M5R == 0 & mention_3R3G == 0 ~ 0,
                               TRUE ~ NA))  #recode variable
ag_plan$MATINH <- as.character(ag_plan$MATINH)

ag_plan <- ag_plan %>%
  left_join (province)




ag_plan <- ag_plan %>%
  group_by(Province, Year) %>%
  summarize (
    MATINH = first(MATINH),
    package_prv = case_when (mean(package) > 0 ~ 1,
                             mean(package) == 0 ~ 0),
    certified_prv = case_when (mean(certified) > 0 ~ 1,
                               mean(certified) == 0 ~ 0),
    seeding_prv = case_when (mean(seeding) > 0 ~ 1,
                             mean(seeding) == 0 ~ 0),
    pesticide_prv = case_when (mean(pesticide) > 0 ~ 1,
                               mean(pesticide) == 0 ~ 0),
    fertilizer_prv = case_when (mean(fertilizer) > 0 ~ 1,
                                mean(fertilizer) == 0 ~ 0)) %>%
  mutate (n = n()) %>%
  pivot_wider (names_from = Year, values_from = c(package_prv, certified_prv, seeding_prv, pesticide_prv, fertilizer_prv)) %>%
  mutate (mention_package = case_when (mean(c_across(starts_with("package")), na.rm = TRUE) > 0 ~ 1, #if they mention at least once
                                       TRUE ~ 0),
          mention_certified = case_when (mean(c_across(starts_with("certified")), na.rm = TRUE) > 0 ~ 1,
                                         TRUE ~ 0),
          mention_seeding = case_when (mean(c_across(starts_with("seeding")), na.rm = TRUE) > 0 ~ 1,
                                       TRUE ~ 0),
          mention_pest = case_when (mean(c_across(starts_with("pest")), na.rm = TRUE) > 0 ~ 1,
                                    TRUE ~ 0),
          mention_fert = case_when (mean(c_across(starts_with("fert")), na.rm = TRUE) > 0 ~ 1,
                                    TRUE ~ 0)) %>%
  mutate (repeat_package = case_when (mean(c_across(starts_with("package")), na.rm = TRUE) > 1/n ~ 1, #if they repeatedly mention (more than once)
                                      TRUE ~ 0),
          repeat_certified = case_when (mean(c_across(starts_with("certified")), na.rm = TRUE) > 1/n ~ 1,
                                        TRUE ~ 0),
          repeat_seeding = case_when (mean(c_across(starts_with("seeding")), na.rm = TRUE) > 1/n ~ 1,
                                      TRUE ~ 0),
          repeat_pest = case_when (mean(c_across(starts_with("pest")), na.rm = TRUE) > 1/n ~ 1,
                                   TRUE ~ 0),
          repeat_fert = case_when (mean(c_across(starts_with("fert")), na.rm = TRUE) > 1/n ~ 1,
                                   TRUE ~ 0)) 

modified_map <- modified_map %>% left_join(ag_plan,by="MATINH")




plot_info <- list(
  list(title = "Certified seeds (Mentioned)", fill_var = "mention_certified"),
  list(title = "Certified seeds (Repeated)", fill_var = "repeat_certified"),
  list(title = "1R: Seed rate (Mentioned)", fill_var = "mention_seeding"),
  list(title = "1R: Seed rate (Repeated)", fill_var = "repeat_seeding"),
  list(title = "2R: Fertilizer (Mentioned)", fill_var = "mention_fert"),
  list(title = "2R: Fertilizer (Repeated)", fill_var = "repeat_fert"),
  #list(title = "3R: Pesticide (Mentioned)", fill_var = "mention_pest"),
  #list(title = "3R: Pesticide (Repeated)", fill_var = "repeat_pest"),
  list(title = "3R3G/1M5R (Mentioned)", fill_var = "mention_package"),
  list(title = "3R3G/1M5R (Repeated)", fill_var = "repeat_package")
)


plots <- lapply(plot_info, function(info) {
  create_plot(modified_map, info$fill_var, info$title) +
    theme(axis.ticks = element_blank()) # Remove axis ticks
})

# gridExtra::grid.arrange(grobs = c(plots[1], plots[2], plots[3], plots[4], ncol = 2)
map_ag_plan <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)

ggsave (plot = map_ag_plan,
        "Output/Fig_33.png", 
        width = 6, height = 15, dpi = 1024)





## Figure 35: Adoption Rates of 1M5R/3R3G Practices by lenient and strict Criteria in Viet Nam in 2023  ----

var_lenient <- c("lenient_1m", "lenient_1r", "lenient_2r", "lenient_3r")
var_strict <- c("strict_1m", "strict_1r", "strict_2r", "strict_3r")

adopt_all <- list()

calculate_adoption <- function(var_set, criterion_type) {
  adopt_list <- list()
  for (var in var_set) {
    adopt <- df %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(.data[[var]]) %>%
      summarise(twt = sum(weight_final_rice, na.rm = TRUE)) %>%
      mutate(sum_twt = sum(twt, na.rm = TRUE)) %>%
      mutate(adoption = twt * 100 / sum_twt) %>%
      filter(.data[[var]] == 1) %>%
      select(-c(sum_twt, twt)) %>%
      mutate(group = var, criterion = criterion_type)
    adopt_list[[var]] <- adopt
  }
  return(adopt_list)
}

adopt_lenient <- calculate_adoption(var_lenient, "Lenient")
adopt_strict <- calculate_adoption(var_strict, "Strict")

adopt_all <- c(adopt_lenient, adopt_strict)
tab_adopt <- bind_rows(adopt_all) %>%
  select(group, adoption, criterion)

tab_adopt$group <- factor(tab_adopt$group, 
                          levels = c("lenient_1m", "lenient_1r", "lenient_2r", "lenient_3r",
                                     "strict_1m", "strict_1r", "strict_2r", "strict_3r"),
                          labels = c("Certified seeds", "1R: Seed rate", "2R: Fertilizer", "3R: Pesticide",
                                     "Certified seeds", "1R: Seed rate", "2R: Fertilizer", "3R: Pesticide"))

# Plot with separate bars for lenient and strict criteria
plot <- tab_adopt %>%
  ggplot(aes(x = group, y = adoption, fill = criterion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +  # Increase dodge width to avoid overlap
  labs(x = " ", y = "Percentage of Adopters", fill = "Criteria") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),  # Keep x-axis labels horizontal
    axis.text.y = element_text(size = 12),  # Adjust y-axis font size
    panel.grid = element_blank(),  # Remove background grid
    legend.position = "top"  # Position legend at the top
  ) +
  scale_fill_manual(values = c("Lenient" = "#1c4c6f", "Strict" = "#d95f02"))  # Different colors for criteria

ggsave (plot, filename = "Output/Fig_34.png", width = 10, height = 6, dpi = 1024)


## Figure 36: Household adoption rates at the province-level for (a) Certified seeds, (b) 1R: Seed rate, (c) 2R: Fertilizer, (d) 3R: Pesticide----

tab_fig4 <- df %>%
  #filter(!is.na(weight_final_rice)) %>% 
  group_by(MATINH) %>%
  summarize(
    lenient_1m = sum((lenient_1m == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    lenient_1r = sum((lenient_1r == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    lenient_2r = sum((lenient_2r == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100,
    lenient_3r = sum((lenient_3r == 1) * weight_final_rice, na.rm = TRUE) / sum(weight_final_rice, na.rm = TRUE) * 100
  )


modified_map <- modified_map %>% left_join(tab_fig4,by="MATINH")

OneM <- modified_map %>%
  ggplot() + 
  aes(fill = lenient_1m) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In %", limits = c(0, 100)) +  
  ggtitle('(a) Certified seeds') +
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

R1 <- modified_map %>%
  ggplot() + 
  aes(fill = lenient_1r) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In %", limits = c(0, 100)) +  
  ggtitle('(a) 1R: Seed rate') +
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())

R2 <- modified_map %>%
  ggplot() + 
  aes(fill = lenient_2r) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In %", limits = c(0, 100)) +  
  ggtitle('(c) 2R: Fertilizer') +
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())


R3 <- modified_map %>%
  ggplot() + 
  aes(fill = lenient_3r) + 
  geom_sf() + 
  scale_fill_gradient2(midpoint = 50, low = "#f7efef", mid = "#9ecae1", high = "#1c4c6f", 
                       na.value = "#dbd0d0", space = "Lab", name = "In %", limits = c(0, 100)) +  
  ggtitle('(d) 3R: Pesticide') +
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  geom_rect(aes(xmin = 107.5, xmax = 110, ymin = 8.5, ymax = 10), 
            linewidth = 0.1, color = "black", fill = NA) +
  geom_rect(aes(xmin = 109.3, xmax = 110, ymin = 16.1, ymax = 16.7), 
            linewidth = 0.1, color = "black", fill = NA) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())


OneM <- OneM + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
R1 <- R1 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
R2 <- R2 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
R3 <- R3 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


a <- grid.arrange(OneM, R1, R2, R3, nrow = 2, ncol = 2)

ggsave (a, filename = "Output/Fig_35.png", 
        height = 15, width = 8, 
        dpi = 1024)

## Figure 38: Incidence of different rice seeding and transplanting methods among (a) non-adopters (b) adopters of 120kg of seed per ha (lenient criterion) and (c) adopters of 100kg of seed per ha (strict criterion)	165 ----

df <- df %>%
  mutate (ws_method_lab = case_when (ws_method == 1 ~ "Hand seeding",
                                     ws_method == 2 ~ "Row seeder/drum seeder",
                                     ws_method == 3 ~ "Seed blower",
                                     ws_method == 4 ~ "Transplanting by hand using hard plating",
                                     ws_method == 5 ~ "Transplanting by hand using soft plating",
                                     ws_method == 6 ~ "Transplanting by machine",
                                     ws_method == 7 ~ "Others",
                                     TRUE ~ "Unknown"))

df$ws_method_lab <- factor(df$ws_method_lab, 
                           levels = rev(c("Hand seeding", "Row seeder/drum seeder", "Seed blower",
                                          "Transplanting by hand using hard plating",
                                          "Transplanting by hand using soft plating",
                                          "Transplanting by machine",
                                          "Others", 
                                          "Unknown")))

seed_rate <- df %>%
  filter (lenient_1r == 1) 

seed_rate_strict <- df %>%
  filter (strict_1r == 1)

seed_rate_non_adopt <- df %>%
  filter (strict_1r == 0 & lenient_1r == 0)

seed_rate$ws_method[seed_rate$ws_method == 99] <- NA
seed_rate_strict$ws_method[seed_rate_strict$ws_method == 99] <- NA
seed_rate_non_adopt$ws_method[seed_rate_non_adopt$ws_method == 99] <- NA

seed_method_group <- list ("(a) Non adopters" = seed_rate_non_adopt,
                           "(b) Adopters (lenient criterion)" = seed_rate,
                           "(c) Adopters (strict criterion)" = seed_rate_strict)

list_seed_method <- list () 
for (data in (1:length(seed_method_group))){
  adopt <- seed_method_group[[data]] %>%
    group_by(ws_method_lab) %>%
    summarise (twt = sum(weight_final_rice, na.rm = TRUE)) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    mutate (group = names (seed_method_group[data]))
  
  list_seed_method[[data]] <- adopt
}

list_seed_method

tab_seed_method <- bind_rows(list_seed_method) %>%
  select (c(ws_method_lab, adoption, group))

plot_seed_method <- tab_seed_method %>%
  ggplot(aes(x = ws_method_lab, y = adoption, fill = ws_method_lab)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),  # Remove background grid
    panel.background = element_rect(fill = "gray99", color = NA)  # Lighten background to a very light grey
  ) +
  geom_text(aes(label = round(adoption, 0)),  # Remove % symbol from label
            hjust = -0.1,
            color = "black") +
  scale_fill_manual(values = c("lightgrey",
                               "#dbe9f6",
                               "#b3d7e8",
                               "#87bdd8",
                               "#5697c6",
                               "#336b87",
                               "#24496e",
                               "#16324f")) +
  facet_wrap(~group, scales = "fixed") +
  ylab("%") +
  ylim(c(0, 100))

plot_seed_method

ggsave (plot_seed_method, filename = "Output/Fig_37.png.png", dpi = 1024, height = 6, width = 14)




## Figure 39: Adoption of fertiliser use recommendations under strict and lenient criteria: (a) number of fertiliser applications, (b) amount of Nitrogen per application, and (c) both -----
# Non-Nitrogen fertilizer: 

df %>%
  filter(!is.na(sum_fert)) %>%
  group_by(sum_fert == 0) %>%
  summarise(twt = sum(weight_final_rice, na.rm = TRUE)) %>%
  mutate(sum_twt = sum(twt, na.rm = TRUE)) %>%
  mutate(adoption = twt * 100 / sum_twt)  #This supports numbers used in part "Willingness to change" of the Fertilizer

list_2r <- c("d_1m5r_nitrogen_100kg", "d_1m5r_nitrogen_110kg", 
             "d_1m5r_nitrogen_3app", "d_1m5r_nitrogen_2app",
             "strict_2r", "lenient_2r")


adopt_list <- list ()

for (var in list_2r){
  adopt <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (.data[[var]]) %>%
    summarise (twt = sum(weight_final_rice, na.rm = TRUE)) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    mutate (name = var) %>%
    select (c(name, adoption)) 
  
  adopt_list[[var]] <- adopt
}

tab_2r_component <- bind_rows(adopt_list) %>%
  mutate(
    group = case_when(
      str_detect(name, "110kg") ~ "Lenient",
      str_detect(name, "100kg") ~ "Strict",
      str_detect(name, "2app") ~ "Lenient",
      str_detect(name, "3app") ~ "Strict",
      str_detect(name, "strict") ~ "Strict",
      str_detect(name, "lenient") ~ "Lenient"
    ),
    # Directly set the component levels
    component = factor(
      case_when(
        str_detect(name, "app") ~ "(a) Number of applications",
        str_detect(name, "kg") ~ "(b) Amount of Nitrogen per application",
        TRUE ~ "(a) + (b)"
      ),
      levels = c("(a) Number of applications", 
                 "(b) Amount of Nitrogen per application",
                 "(a) + (b)")
    )
  ) %>%
  select(-name) %>%
  mutate(position = case_when(group == "Strict" ~ 0.5,
                              group == "Lenient" ~ 1))

tab_2r_component$group <- factor(tab_2r_component$group, levels = c("Strict", "Lenient"))


plot_2r_component <- plot_component_fn(tab_2r_component, footnote = "") +  # Provide an empty footnote
  scale_fill_manual(values = c("Lenient" = "#1c4c6f", "Strict" = "#d95f02")) +  # Set colors
  theme_minimal() +  # Use a minimal theme as a base
  theme(
    panel.grid = element_blank(),  # Drop all grid lines
    legend.title = element_blank(),
    axis.text.x = element_blank(),  # Drop x labels
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size if needed
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    axis.title.x = element_blank()  # Drop x-axis title
  ) +
  theme(axis.ticks = element_line(colour = NA),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 1), 
        plot.title = element_text(size = 14),  # Set title size to 14
        panel.background = element_rect(fill = "gray97")) +
  labs(y = "% adopters", x = NULL, fill = NULL, caption = NULL) +  # Set y-axis label
  ylim(c(0, 100))  # Ensure y-axis extends from 0 to 100


ggsave(plot_2r_component, filename = "Output/Fig_38.png", dpi = 1024,
       width = 10, height = 5)


## Figure 40: Comparison of 1M5R pesticide application requirements under strict and lenient criteria ----

df <- df %>%
  mutate (d_1m5r_pest_timing_lenient = case_when (d_1m5r_pest_40d_sowing == 1 & d_1m5r_pest_20d_harvest == 1 ~ 1,
                                                  d_1m5r_pest_40d_sowing != 1 | d_1m5r_pest_20d_harvest != 1 ~ 0,
                                                  TRUE ~ NA),
          d_1m5r_pest_timing_strict = case_when (d_1m5r_pest_40d_sowing == 1 & d_1m5r_pest_flowering == 1 ~ 1,
                                                 d_1m5r_pest_40d_sowing != 1 | d_1m5r_pest_flowering != 1 ~ 0,
                                                 TRUE ~ NA))

list_3r <- c("d_1m5r_pest_6app", "d_1m5r_pest_timing_lenient",
             "d_1m5r_pest_3app", "d_1m5r_pest_timing_strict",
             "lenient_3r", "strict_3r")


adopt_list <- list ()

for (var in list_3r){
  adopt <- df %>%
    filter (!is.na(.data[[var]])) %>%
    group_by (.data[[var]]) %>%
    summarise (twt = sum(weight_final_rice, na.rm = TRUE)) %>%
    mutate (sum_twt = sum(twt, na.rm = TRUE)) %>%
    mutate (adoption = twt * 100 / sum_twt) %>%
    filter (.data[[var]] == 1) %>%
    mutate (name = var) %>%
    select (c(name, adoption)) 
  
  adopt_list[[var]] <- adopt
}

tab_3r_component <- bind_rows(adopt_list) %>%
  mutate(
    group = case_when(
      str_detect(name, "6app") ~ "Lenient",
      str_detect(name, "3app") ~ "Strict",
      str_detect(name, "strict") ~ "Strict",
      str_detect(name, "lenient") ~ "Lenient"
    ),
    # Directly set the component levels
    component = factor(
      case_when(
        str_detect(name, "app") ~ "(a) Number of applications",
        str_detect(name, "3r") ~ "(a) + (b)",
        TRUE ~ "(b) Timing of applications"
      ),
      levels = c("(a) Number of applications", 
                 "(b) Timing of applications",
                 "(a) + (b)")
    )
  ) %>%
  select(-name) %>%  # Uncomment if you want to drop the 'name' column
  mutate(position = case_when(group == "Strict" ~ 0.5,
                              group == "Lenient" ~ 1))

tab_3r_component$group <- factor(tab_3r_component$group, levels = c("Strict", "Lenient"))

# Plot without the title
plot_3r_component <- plot_component_fn(tab_3r_component, footnote = "") +  # Provide an empty footnote
  scale_fill_manual(values = c("Lenient" = "#1c4c6f", "Strict" = "#d95f02")) +  # Set colors
  theme_minimal() +  # Use a minimal theme as a base
  theme(
    panel.grid = element_blank(),  # Drop all grid lines
    legend.title = element_blank(),
    axis.text.x = element_blank(),  # Drop x labels
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size if needed
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    axis.title.x = element_blank()  # Drop x-axis title
  ) +
  theme(axis.ticks = element_line(colour = NA),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 1), 
        plot.title = element_text(size = 14),  # Set title size to 14
        panel.background = element_rect(fill = "gray97")) +
  labs(y = "% adopters", x = NULL, fill = NULL, caption = NULL) +  # Set y-axis label
  ylim(c(0, 100))  # Ensure y-axis extends from 0 to 100


ggsave(plot_3r_component, filename = "Output/Fig_39.png", dpi = 1024,
       width = 10, height = 5)

## Average pesticide applications (text only) ----

df <- df %>%
  mutate(n_drug_mix = rowSums(select(., starts_with("n_drug_mix_")), na.rm = TRUE))

pest_tab_prep <- df %>%
  filter (!is.na(weight_final_rice)) %>%
  summarise (avg_pp = weighted.mean(n_app_drug, weight_final_rice, na.rm = TRUE),
             self_n_app = weighted.mean (n_app_insect_fungi, weight_final_rice, na.rm = TRUE),
             self_max = max (n_app_drug, na.rm = TRUE),
             self_min = min (n_app_drug, na.rm = TRUE),
             actual_n_app = weighted.mean(n_drug_mix, weight_final_rice, na.rm = TRUE),
             actual_max = max (n_app_drug, na.rm = TRUE),
             actual_min = min (n_app_drug, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               values_to = "value") %>%
  mutate (order = c(1,3,4,5,7,8,9)) %>%
  arrange () 

newrow <- data.frame (name = c("text1", "text2"),
                      value = NA,
                      order = c(2,6))

pest_tab <- rbind (pest_tab_prep, newrow) %>%
  arrange(order) %>%
  select (-order) %>%
  mutate ("str" = c("Average applications of plant protection drugs at household level:",
                    "Self-reported applications of insecticides and fungicides at household level:",
                    "Average number of applications:",
                    "Maximum number of applications:",
                    "Minimum number of applications:",
                    "Actual applications of insecticides and fungicides at household level:",
                    "Average number of applications:",
                    "Maximum number of applications:",
                    "Minimum number of applications:")) %>%
  relocate (str, .before = everything()) %>%
  select (-name)


table_df_3r <- flextable (pest_tab) %>%
  delete_part(part = "header") %>%
  align(align = "center", part = "header") %>%
  colformat_double(j = c(2) ,digits = 2) %>%
  
  theme_vanilla() %>%
  autofit()


## Figure 46. Adoption of Water Saving Practices for Coffee Production in the Central Highlands ----

curl_function ("data/raw/VHLSS_2023_Household/Combined_modules/M4B13A.csv")
M4B13A <- read.csv ("data/raw/VHLSS_2023_Household/Combined_modules/M4B13A.csv")

M4B13A$SWCP <- ifelse (M4B13A$M4B13A_C2 <=3 & M4B13A$M4B13A_C3 <= 400, 1, 0); table (M4B13A$SWCP) # 830/1279 = 65% 
#M4B13A$SWCP_confirmed <- ifelse (M4B13A$M4B13A_C2 <=3 & M4B13A$M4B13A_C3 <= 400 & M4B13A$M4B13A_C4 != 4 , 1, 0); table (M4B13A$SWCP_confirmed) # 357/1279 = 17%. FIltering out hhs not knowing quantity of water used


M4B13A <- merge (M4B13A, province, all.x =TRUE)

M4B13A <- M4B13A [M4B13A$Province_name %in% c('Dac Nong','Dak Lak','Gia Lai',
                                              'Kon Tum','Lam Dong') ,] 

percentage_data <- M4B13A %>%
  group_by(Province_name) %>%
  summarize(##Lower.bound = mean(SWCP_confirmed == TRUE, na.rm = TRUE) * 100,
    Higher.bound = mean(SWCP == TRUE, na.rm = TRUE) * 100)

percentage_data_long <- gather(percentage_data, key = "SWCP_percentage", value = "Percentage", -Province_name)


Fig46 <- ggplot(percentage_data_long, aes(x = Province_name, y = Percentage)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1c4c6f") +  # Same color for all bars
  labs(x = " ",  # X-axis shows Province names
       y = "Percentage of Adopters") +  # Y-axis label
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis font size
    panel.grid = element_blank(),  # Remove background grid
    legend.position = "none"  # Drop the legend
  )


# Table 21. Overview of AWD adoption rates by measurement method ----
# Note: Table is constructed based on R output


curl_function ("data/raw/VHLSS_2023_Household/Final/SPIA_M4B11A.csv")
M4B11A <- read.csv ("data/raw/VHLSS_2023_Household/Final/SPIA_M4B11A.csv")
M4B11A <- format_ID(M4B11A, columns = c("MATINH", "MAXA", "MADIABAN"), c(2,5,3))

curl_function ("Output/Report_weights.csv")
weight <- read.csv ("Output/Report_weights.csv") 
weight <- weight[!is.na (weight$weight_final_rice),]
weight <- format_ID(weight, columns = c("MATINH", "MAXA", "MADIABAN"), c(2,5,3))


weight$ID <- paste (weight$MATINH, weight$MAXA, weight$MADIABAN, sep="-"); M4B11A$ID <- paste (M4B11A$MATINH, M4B11A$MAXA, M4B11A$MADIABAN, sep="-")
weight <- weight %>% distinct(ID, .keep_all = TRUE)

M4B11A <- merge (M4B11A, weight [, c(10,4)], by="ID", all.x=TRUE)



M4B11A <- M4B11A %>% filter(!is.na(weight_final_rice))
survey_design <- svydesign(ids = ~1, data = M4B11A, weights = ~weight_final_rice)


# 1. Calculate the weighted percentage of adopters in VH23(M4B11A7_C12 == 1)
adopter_percentage <- svymean(~I(M4B11A$M4B11A7_C12 == 1), survey_design, na.rm = TRUE) * 100
print(adopter_percentage)

# 2. Calculate the weighted percentage of communes with at least one adopter
# First, create a new variable at the commune level that indicates if there is at least one adopter
commune_adoption <- M4B11A %>%
  group_by(ID) %>%
  summarise(has_adopter = as.numeric(any(M4B11A7_C12 == 1)), 
            weight_final_rice = mean(weight_final_rice, na.rm = TRUE))

# Create a new survey design object for commune-level data
commune_design <- svydesign(ids = ~1, data = commune_adoption, weights = ~weight_final_rice)

# Calculate the weighted percentage of communes with at least one adopter
commune_adopter_percentage <- svymean(~has_adopter, commune_design, na.rm = TRUE) * 100
print(commune_adopter_percentage)


# AWD 2023

missing_per_value <- df_23 %>%
  group_by(awd_1drydown) %>%
  summarise(missing_weights = sum(is.na(weight_final_rice)),
            total_rows = n())

df_23_clean <- df_23[!is.na(df_23$weight_final_rice) & !is.na(df_23$awd_1drydown), ]

# HH-level | One drydown
svy_design <- svydesign(ids = ~1, data = df_23_clean, weights = ~weight_final_rice)
weighted_table <- svytable(~awd_1drydown, design = svy_design)
weighted_df <- as.data.frame(weighted_table)
weighted_df$percentage <- round (prop.table(weighted_df$Freq) * 100, 1)
weighted_df

# HH-level | At least two dry-downs of 5 days minimum
svy_design <- svydesign(ids = ~1, data = df_23_clean, weights = ~weight_final_rice)
weighted_table2 <- svytable(~awd_2drydown, design = svy_design)
weighted_df2 <- as.data.frame(weighted_table2)
weighted_df2$percentage <- round (prop.table(weighted_df2$Freq) * 100, 1)
weighted_df2


# EA-level | One drydown
df_23_clean$IDCo <- paste (df_23_clean$MATINH, df_23_clean$MAHUYEN, df_23_clean$MAXA, df_23_clean$MADIABAN, sep='-') 

commune_adoption <- df_23_clean %>%
  group_by(IDCo) %>%  # Replace `ID` with the commune-level identifier (`MATINH`)
  summarise(
    has_adopter = as.numeric(any(awd_1drydown == 1)),  # Check if at least one household adopted
    weight_final_rice = mean(weight_final_rice, na.rm = TRUE)  # Compute mean weight
  )

commune_design <- svydesign(ids = ~1, data = commune_adoption, weights = ~weight_final_rice)

commune_adopter_percentage <- svymean(~has_adopter, commune_design, na.rm = TRUE) * 100
print(commune_adopter_percentage)


# EA-level | At least two dry-downs of 5 days minimum
df_23_clean <- df_23[!is.na(df_23$weight_final_rice) & !is.na(df_23$awd_2drydown), ]
df_23_clean$IDCo <- paste (df_23_clean$MATINH, df_23_clean$MAHUYEN, df_23_clean$MAXA, df_23_clean$MADIABAN, sep='-') 

commune_adoption <- df_23_clean %>%
  group_by(IDCo) %>%  # Replace `ID` with the commune-level identifier (`MATINH`)
  summarise(
    has_adopter = as.numeric(any(awd_2drydown == 1)),  # Check if at least one household adopted
    weight_final_rice = mean(weight_final_rice, na.rm = TRUE)  # Compute mean weight
  )

commune_design <- svydesign(ids = ~1, data = commune_adoption, weights = ~weight_final_rice)

commune_adopter_percentage <- svymean(~has_adopter, commune_design, na.rm = TRUE) * 100
print(commune_adopter_percentage)



# AWD 2022
curl_function ("data/raw/VHLSS_2022_Household/datasets/Ho_TTC_M4B11_season_edit.csv")
TTC_M4B111 <- read.csv ("data/raw/VHLSS_2022_Household/datasets/Ho_TTC_M4B11_season_edit.csv") # Crop cut module

TTC_M4B111$Onedd <- ifelse (TTC_M4B111$M4B111_C7 == 1 & TTC_M4B111$M4B111_C8 >=5, TRUE, FALSE)
TTC_M4B111$Twodd <- ifelse (TTC_M4B111$M4B111_C7 > 1 & TTC_M4B111$M4B111_C8 >=5, TRUE, FALSE)

# HH-level | One drydown
svy_design <- svydesign(ids = ~1, data = TTC_M4B111, weights = ~wt45)
weighted_table <- svytable(~Onedd, design = svy_design)
weighted_df <- as.data.frame(weighted_table)
weighted_df$percentage <- round (prop.table(weighted_df$Freq) * 100, 1)
weighted_df

# HH-level | At least two dry-downs of 5 days minimum
svy_design <- svydesign(ids = ~1, data = TTC_M4B111, weights = ~wt45)
weighted_table2 <- svytable(~Twodd, design = svy_design)
weighted_df2 <- as.data.frame(weighted_table2)
weighted_df2$percentage <- round (prop.table(weighted_df2$Freq) * 100, 1)
weighted_df2










