

# ---
# Title: "Section 4. Climate change adaptation options"
# Author email: "fkosmowski@gmail.com"
# Date: "November 2024"
# ---

# Input dataset: CS-MAPS Phase1_WIDE.xlsx, 
# Adm_CSMAP_spatial_join_dataset.xlsx, VH22_data, VH23_data, M4B11A5
# GSO_dstricts.xls, M4B11.dta, Muc4B11.csv, result_gregorian_final.csv
# Output: VH_merged.csv, CSMAPs.vars.22.23.csv

rm (list = ls()) #start clean


# Install and load packages ----
# Function to check and install packages

packages <- c("tidyverse", "cowplot", "gridExtra", "httr", "readxl", "e1071",
              "estimatr", "jtools", "haven", "summarytools")

check_and_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

invisible(sapply(packages, check_and_install_package))
library (tidyverse)
library (cowplot)
library (gridExtra)
library (httr)
library (readxl)
library (e1071)
library (estimatr)
library (jtools)
library (haven)
library (summarytools)

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





# Figure 21. Number of Vietnamese provinces referencing forecasted reservoir water levels in yearly agricultural plans, by region (2021-2024) ----

data <- data.frame(
  Year = c(2021, 2022, 2023, 2024, 2021, 2022, 2023, 2024, 2021, 2022, 2023, 2024),
  Region = c("RRD", "RRD", "RRD", "RRD", "SCC-CH", "SCC-CH", "SCC-CH", "SCC-CH", "NC", "NC", "NC", "NC"),
  Normal = c(0, 0, 1, 1, 2, 3, 3, 1,2,1,0,0),
  Insufficient = c(1, 5, 4, 7, 6, 4, 4, 0,2,1,1,0),
  Extreme_Insufficient = c(0, 0, 1, 0, 1, 0, 0, 0,0,0,0,0),
  Total = c(8, 9, 10, 3, 14, 10, 10, 1,1,1,1,1)
)

# Reshape data to long format
data_long <- data %>%
  pivot_longer(cols = c(Normal, Insufficient, Extreme_Insufficient),
               names_to = "Status",
               values_to = "CoInt")

# Rename and reorder the Status levels
data_long$Status <- factor(data_long$Status, 
                           levels = c("Normal", "Insufficient", "Extreme_Insufficient"),
                           labels = c("Normal", "Insufficient", "Extremely Insufficient"))

# Define new color palette
color_palette <- scale_fill_manual(values = c("Normal" = "#6baed6",    # Blue for Normal
                                              "Insufficient" = "#fdae61", # Orange for Insufficient
                                              "Extremely Insufficient" = "#f46d43")) # Pale red for Extremely Insufficient

# Create RRD plot without axis labels and without legend, y-axis scaled to 8
plot_rrd <- ggplot(data_long[data_long$Region == "RRD", ], aes(x = as.factor(Year), y = CoInt, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  color_palette +
  ylim(0, 8) +  # Set y-axis scale to be between 0 and 8
  labs(title = "RRD-NMMA provinces (n=10)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Create SCC-CH plot without axis labels and without legend, y-axis scaled to 8
plot_sccch <- ggplot(data_long[data_long$Region == "SCC-CH", ], aes(x = as.factor(Year), y = CoInt, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  color_palette +
  ylim(0, 8) +  # Set y-axis scale to be between 0 and 8
  labs(title = "SCC-CH provinces (n=8)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title

plot_nc <- ggplot(data_long[data_long$Region == "NC", ], aes(x = as.factor(Year), y = CoInt, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  color_palette +
  ylim(0, 8) +  # Set y-axis scale to be between 0 and 8
  labs(title = "NC provinces (n=6)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title


# Extract the legend and rename 'Status' to 'Risk level'
legend <- get_legend(
  ggplot(data_long, aes(x = as.factor(Year), y = CoInt, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    color_palette +
    labs(fill = "Water reservoirs capacity") +  # Rename legend title to "Risk level"
    theme_minimal() +
    theme(legend.position = "bottom")
)

# Arrange the plots horizontally with the legend centered at the bottom
grid.arrange(
  arrangeGrob(plot_rrd, plot_sccch, plot_nc, ncol = 3),
  legend,
  nrow = 2,
  heights = c(8, 1)  # Adjust the height ratio to give more space to the plots
)
# Saved 650 * 464



# Figure 20. Number of Vietnamese provinces referencing the strengh of El-Niño in yearly agricultural plans, by region (2021-2024) ----

el_nino_data <- data.frame(
  Year = c(2021, 2021, 2021, 2022, 2022, 2022, 2023, 2023, 2023, 2024, 2024, 2024),
  Region = c("MRD", "NM-RRD", "SCC-CH", "MRD", "NM-RRD", "SCC-CH", "MRD", "NM-RRD", "SCC-CH", "MRD", "NM-RRD", "SCC-CH"),
  Weak_El_Nino = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Moderate_El_Nino = c(1, 0, 0, 1, 0, 1, 4, 1, 1, 2, 3, 1),
  Strong_El_Nino = c(0, 0, 0, 1, 0, 0, 3, 0, 0, 3, 2, 0)
)

# Reshape data to long format
el_nino_data_long <- el_nino_data %>%
  pivot_longer(cols = c(Weak_El_Nino, Moderate_El_Nino, Strong_El_Nino),
               names_to = "El_Nino_Type",
               values_to = "Count")

# Rename and reorder the El Nino Type levels
el_nino_data_long$El_Nino_Type <- factor(el_nino_data_long$El_Nino_Type, 
                                         levels = c("Weak_El_Nino", "Moderate_El_Nino", "Strong_El_Nino"),
                                         labels = c("Weak El Niño", "Moderate El Niño", "Strong El Niño"))

# Define a color palette for El Nino types
el_nino_color_palette <- scale_fill_manual(values = c("Weak El Niño" = "#6baed6",    # Blue for Weak El Nino
                                                      "Moderate El Niño" = "#fdae61", # Orange for Moderate El Nino
                                                      "Strong El Niño" = "#f46d43"))  # Pale red for Strong El Nino

# Create plot for MRD
plot_mrd <- ggplot(el_nino_data_long[el_nino_data_long$Region == "MRD", ], aes(x = as.factor(Year), y = Count, fill = El_Nino_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  el_nino_color_palette +
  ylim(0, 5) +  # Set y-axis scale to be between 0 and 8
  labs(title = "MRD provinces (n=11)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Create plot for RRD
plot_rrd <- ggplot(el_nino_data_long[el_nino_data_long$Region == "NM-RRD", ], aes(x = as.factor(Year), y = Count, fill = El_Nino_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  el_nino_color_palette +
  ylim(0, 5) +  # Set y-axis scale to be between 0 and 8
  labs(title = "NM-RRD provinces (n=10)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Create plot for NCC-CH
plot_nccch <- ggplot(el_nino_data_long[el_nino_data_long$Region == "SCC-CH", ], aes(x = as.factor(Year), y = Count, fill = El_Nino_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  el_nino_color_palette +
  ylim(0, 5) +  # Set y-axis scale to be between 0 and 8
  labs(title = "SCC-CH provinces (n=8)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Extract the legend and rename 'El_Nino_Type' to 'El Niño Category'
legend_el_nino <- get_legend(
  ggplot(el_nino_data_long, aes(x = as.factor(Year), y = Count, fill = El_Nino_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    el_nino_color_palette +
    labs(fill = " ") +  # Rename legend title to "El Niño Category"
    theme_minimal() +
    theme(legend.position = "bottom")
)

# Arrange the three plots horizontally with the legend centered at the bottom
grid.arrange(
  arrangeGrob(plot_mrd, plot_rrd, plot_nccch, ncol = 3),
  legend_el_nino,
  nrow = 2,
  heights = c(8, 1)  # Adjust the height ratio to give more space to the plots
)




# Figure 16. Heatmap showing the the occurrences of recommending no rice cultivation or shift to another crop in a given season from 2021 to 2024 in agricultural plannings ----

# Example data frame
data <- data.frame(
  Province = c("Dong Thap", "Bac Lieu", "Ben Tre", "Ca Mau", "Can Tho", "An Giang", "Hau Giang", "Kien Giang", "Long An", "Soc Trang", "Tra Vinh"),
  WS_2021_2022 = c(0, 2, 2, 0, 0, 0, 0, 0, 1, 1, 0),
  WS_2022_2023 = c(0, 1, 1, 1, 0, 0, 0, 0, 2, 1, 0),
  WS_2023_2024 = c(0, 1, 0, 1, 0, 0, 0, 0, 2, 1, 0),
  AW_2021 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  AW_2022 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  AW_2023 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0)
)

# Convert to long format and order provinces alphabetically
data_long <- data %>%
  pivot_longer(cols = -Province,
               names_to = "Season",
               values_to = "Count") %>%
  mutate(Province = factor(Province, levels = rev(sort(unique(Province)))),
         Season = factor(Season, levels = c("WS_2021_2022", "WS_2022_2023", "WS_2023_2024", "AW_2021", "AW_2022", "AW_2023"),
                         labels = c("WS 2021-2022", "WS 2022-2023", "WS 2023-2024",
                                    "AW 2021", "AW 2022", "AW 2023")))

# Plot heatmap with custom color scale and ordered provinces
ggplot(data_long, aes(x = Season, y = Province, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(name = "Count", low = "white", high = "deepskyblue3", breaks = c(0, 1, 2), labels = scales::label_comma(accuracy = 1)) +
  labs(title = " ",
       x = "Cropping Seasons",
       y = "Provinces") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(margin = margin(b = 10))) + # Adds space below the legend title
  guides(fill = guide_colorbar(reverse = TRUE))


# Table 18. Frequency and percentage of respondents who declared having used CS-MAPs in the last six cropping seasons by region ----
curl_function ("data/processed/CS-MAPS%20Phase1_WIDE.xlsx")
CS <- read_excel("data/processed/CS-MAPS%20Phase1_WIDE.xlsx")
names(CS)

CS <- CS %>%
  mutate(Region_CSMaps = case_when(
    hhidprovince %in% c("Thanh Hoa", "Nghe An", "Ha Tinh", "Quang Binh", "Quang Tri", "Thua Thien Hue") ~ "NC",
    hhidprovince %in% c("Da Nang", "Quang Ngai", "Quang Nam", "Binh Dinh", "Khanh Hoa", "Phu Yen", "Binh Thuan", "Ninh Thuan", "Dak Lak", "Gia Lai") ~ "SCC-CH",
    hhidprovince %in% c("Ninh Binh", "Nam Dinh", "Thai Binh", "Vinh Phuc", "Hai Duong", "Hung Yen", "Bac Ninh", "Ha Nam", "Ha Noi", "Hai Phong", "Bac Giang", "Phu Tho") ~ "NM-RRD",
    hhidprovince %in% c("Can Tho", "An Giang", "Tien Giang", "Dong Thap", "Long An", "Vinh Long", "Ben Tre", "Tra Vinh", "Soc Trang", "Hau Giang", "Bac Lieu", "Ca Mau", "Kien Giang") ~ "MRD",
    TRUE ~ NA_character_ # Optional: Assign NA for unmatched provinces
  ))

table (CS$hhidprovince, CS$Region_CSMaps)

# Repondents involved in the design of agricultural plans and sowing schedules in [hhidprovince] ?
table (CS$S1_Q5)

# 19/40 respondents declared using drought categoriy to assess the upcoming risk
table (CS$S2a_Q2_Drought [CS$S1_Q5 == 'yes'])

# 10./38 declared the maps were also handed over at lower administrative layers (districts), with proper guidance and instructions on how to use them.
table (CS$S3_Q1b [CS$S1_Q5 == 'yes']) # Guidance and instructions
table (CS$region [CS$S1_Q5 == 'yes'], CS$S3_Q1b [CS$S1_Q5 == 'yes']) # Guidance and instructions



# When asked about the use of the CS-Maps in the next cropping season following the map design/release, 40% of respondents in the MRD, 22% in the NM-RRD and 31% in the SCC-CH responded positively. 
table (CS$S2a_Q7[CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
round (prop.table (table (CS$S2a_Q7[CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes']), 2), 2)

# In the last three years, which years were categorised as severe or moderate years in [hhidprovince]?
table (CS$hhidprovince, CS$S2a_Q100b) # 2023
table (CS$hhidprovince, CS$S2a_Q100c) # 2022
table (CS$hhidprovince, CS$S2a_Q100d) # 2021

# Frequency and percentage of respondents who declared having used the CS-Maps in the last three years, by region. 
table (CS$S2a_Q11b [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q11c [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q11d [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q11e [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q11f [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q11g [CS$S1_Q5 == 'yes'], CS$Region_CSMaps [CS$S1_Q5 == 'yes'])

# Assuming CS is your data frame and 'Yes' is coded as 1, 'No' as 0
CS$aggreg <- rowSums(CS[, c("S2a_Q11b", "S2a_Q11c", "S2a_Q11d", "S2a_Q11e", "S2a_Q11f", "S2a_Q11g")] == 'yes', na.rm = TRUE)
table (CS$aggreg, CS$Region_CSMaps)
by (CS$aggreg, CS$Region_CSMaps, sum)


# A total of 9 respondents could precisely detail the adaptation options recommended. 
table (CS$S2a_Q10a[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10b[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10c[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10d[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10e[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10f[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])
table (CS$S2a_Q10g[CS$S1_Q5 == 'yes'], CS$region [CS$S1_Q5 == 'yes'])




# Change in planting dates in the MRD provinces: merging georeferenced maps with VHLSS ----
curl_function("data/raw/VHLSS_2022_Household/datasets/Ho_Muc4B11_edited.csv")
VH22 <- read.csv ("data/raw/VHLSS_2022_Household/datasets/Ho_Muc4B11_edited.csv") # 4B Dates
curl_function("data/raw/VHLSS_2023_Household/Combined_modules/M4B11A5.csv")
VH23 <- read.csv ("data/raw/VHLSS_2023_Household/Combined_modules/M4B11A5.csv")

VH22$P_date <- as.Date(paste(VH22$M4B11_C3AY, VH22$M4B11_C3AM, VH22$M4B11_C3AD, sep = "-"))

VH22$month <- as.numeric(format(VH22$P_date, "%m"))
VH22$day <- as.numeric(format(VH22$P_date, "%d"))
VH22$P_date_fortnight <- ifelse(VH22$day <= 14, VH22$month, VH22$month + 0.5) # Converting to fortnight format

# Recode the lunar dates..
VH22 <- VH22[VH22$M4B11_MA == 2 & VH22$M4B11_C3B == 2, ]
table (VH22$M4B11_MA) # 2 =WS season
table (VH22$M4B11_C3B)  # 2 = Gregorian calendar is 15% of Obs. 


# MRD household merging
curl_function("data/processed/VH22_data.csv")
df_22 <- read.csv("data/processed/VH22_data.csv")

curl_function("data/processed/VH23_data.csv")
df_23 <- read.csv("data/processed/VH23_data.csv")

curl_function("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Provinces_IDs <- read.csv("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")

Provinces_IDs$MATINH <- as.numeric (Provinces_IDs$MATINH); df_22$MATINH <- as.numeric (df_22$MATINH); df_23$MATINH <- as.numeric (df_23$MATINH)
df_22 <- merge (df_22, Provinces_IDs, all.x =TRUE)
df_23 <- merge (df_23, Provinces_IDs, all.x =TRUE)

table (df_22$Region)
table (df_23$Region)


# CS-Maps merging - Generate clean dataset with District names for merging with CS-Maps georeferences
curl_function ("data/processed/Adm_CSMAP_spatial_join_dataset.xlsx")
Merge <- read_excel("data/processed/Adm_CSMAP_spatial_join_dataset.xlsx")

col_from<-c("Land_Use_c", "Flood_norm", "Flood_extr", "Salinity_N", "Salinity_E",
            "LandUse_No", "LandUse_Ex", "LU_code_No", "LU_code_Ex", "Planting_W",
            "Planting_S", "Planting_A", "Planting_1", "Planting_2", "Planting_3",
            "Planting_4", "Planting_5")

col_to<-c("Land_Use_code",     "Flood_normal",      "Flood_extreme",    
          "Salinity_Normal",   "Salinity_Extreme",  "LandUse_Normal",   
          "LandUse_Extreme",   "LU_code_Normal",    "LU_code_Extreme",  
          "Planting_WS_N",     "Planting_SA_N",     "Planting_AW_N",    
          "Planting_Summer_N", "Planting_WS_E",     "Planting_SA_E",    
          "Planting_AW_E",     "Planting_Summer_E")

Merge <- Merge %>% rename_at(vars(col_from), ~col_to)

Merge$ProDis <- paste (Merge$NAME_1, Merge$NAME_2, sep='-')

names(Merge)

# Calculate the MODE and MEAN of planting dates at the district-level (n=140 districts)
# Check the distribution and conclude on mean or mode

# Replace 0 values with NA for the specified columns
Merge <- Merge %>%
  mutate(
    Planting_SA_E = ifelse(Planting_SA_E == 0, NA, Planting_SA_E),
    Planting_SA_N = ifelse(Planting_SA_N == 0, NA, Planting_SA_N),
    Planting_WS_E = ifelse(Planting_WS_E == 0, NA, Planting_WS_E),
    Planting_WS_N = ifelse(Planting_WS_N == 0, NA, Planting_WS_N)
  )

# Define a function to calculate the mode
calculate_mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  if (length(x) == 0) return(NA)  # Return NA if no values are present
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Calculate the mean and mode at the district level
district_summary <- Merge %>%
  group_by(ProDis) %>%
  summarise(
    avg_Planting_SA_E = mean(Planting_SA_E, na.rm = TRUE),
    mode_Planting_SA_E = calculate_mode(Planting_SA_E),
    
    avg_Planting_SA_N = mean(Planting_SA_N, na.rm = TRUE),
    mode_Planting_SA_N = calculate_mode(Planting_SA_N),
    
    avg_Planting_WS_E = mean(Planting_WS_E, na.rm = TRUE),
    mode_Planting_WS_E = calculate_mode(Planting_WS_E),
    
    avg_Planting_WS_N = mean(Planting_WS_N, na.rm = TRUE),
    mode_Planting_WS_N = calculate_mode(Planting_WS_N)
  )

# PLace NaN by NAs in averages
district_summary <- district_summary %>%
  mutate(
    avg_Planting_WS_N = ifelse(is.nan(avg_Planting_WS_N), NA, avg_Planting_WS_N),
    avg_Planting_WS_E = ifelse(is.nan(avg_Planting_WS_E), NA, avg_Planting_WS_E),
    avg_Planting_SA_N = ifelse(is.nan(avg_Planting_SA_N), NA, avg_Planting_SA_N),
    avg_Planting_SA_E = ifelse(is.nan(avg_Planting_SA_E), NA, avg_Planting_SA_E)
  )

print(district_summary)


# Test of significance between mean and mode
t_test_SA <- t.test(
  district_summary$avg_Planting_SA_E,
  district_summary$mode_Planting_SA_E,
  paired = TRUE,
  alternative = "two.sided"
)

t_test_WS <- t.test(
  district_summary$avg_Planting_WS_E,
  district_summary$mode_Planting_WS_E,
  paired = TRUE,
  alternative = "two.sided"
)

# Print the results
print("Significance test for Planting_SA_E (mean vs. mode):")
print(t_test_SA)

print("Significance test for Planting_WS_E (mean vs. mode):")
print(t_test_WS)
# = Sign differences for WS but not for SA

# Skewness by District
skewness_summary <- Merge %>%
  group_by(ProDis) %>%
  summarise(
    skewness_Planting_SA_E = skewness(Planting_SA_E, na.rm = TRUE),
    skewness_Planting_SA_N = skewness(Planting_SA_N, na.rm = TRUE),
    skewness_Planting_WS_E = skewness(Planting_WS_E, na.rm = TRUE),
    skewness_Planting_WS_N = skewness(Planting_WS_N, na.rm = TRUE)
  )

# View the skewness summary
# NaNs indicate a similar distribution, no variation
print(skewness_summary)

# Plots
hist (skewness_summary$skewness_Planting_WS_E , col = "red", breaks = 50, xlim = c(-50, 50)) # Basic Histogramme (Breaks = nb of bars)
hist (skewness_summary$skewness_Planting_SA_E, col = "blue", breaks = 50, xlim = c(-50, 50)) # Basic Histogramme (Breaks = nb of bars)

print (mean_skewness_Planting_WS_E <- mean(
  skewness_summary$skewness_Planting_WS_E[!is.nan(skewness_summary$skewness_Planting_WS_E)],
  na.rm = TRUE
)) # Mean skewness by district = 0.42

print (mean_skewness_Planting_SA_E <- mean(
  skewness_summary$skewness_Planting_SA_E[!is.nan(skewness_summary$skewness_Planting_SA_E)],
  na.rm = TRUE
)) # Mean skewness by district = 0.94

# Conclusion: mean and mode are significantly different
# skewness is positive for both seasons, the distribution has a right tail (more lower values), so the mode (the most frequent value) is used
# Can also Split the provinces by those with homogeneous planting dates (use the mean) and those with disparate dates (use the mode).
# Use variable district_summary for merging




# Input GSO commune data
curl_function ("data/processed/GSO_dstricts.xls")
GSO_Dist <- read_excel("data/processed/GSO_dstricts.xls")

GSO_Dist$Province_name <- gsub("^Tỉnh\\s*", "", GSO_Dist$Province_name, ignore.case = TRUE)
GSO_Dist$Province_name <- gsub("^Thành phố\\s*", "", GSO_Dist$Province_name, ignore.case = TRUE)

GSO_Dist$District_name <- gsub("^Thành phố\\s*", "", GSO_Dist$District_name, ignore.case = TRUE)
GSO_Dist$District_name <- gsub("^Quận\\s*", "", GSO_Dist$District_name, ignore.case = TRUE)
GSO_Dist$District_name <- gsub("^Thị xã\\s*", "", GSO_Dist$District_name, ignore.case = TRUE)
GSO_Dist$District_name <- gsub("^Huyện\\s*", "", GSO_Dist$District_name, ignore.case = TRUE)

Merge$NAME_2 <- gsub("\\s*Thị xã", "", Merge$NAME_2, ignore.case = TRUE); 
Merge$NAME_2 <- gsub("\\s*Thành phố", "", Merge$NAME_2, ignore.case = TRUE)
Merge$NAME_2 <- gsub("\\s*\\(.*?\\)", "", Merge$NAME_2, ignore.case = TRUE); 

GSO_Dist$ProDis <- paste (GSO_Dist$Province_name, GSO_Dist$District_name, sep='-')


# 1a. Calculate district-level mode and means

# Replace 0 values with NA for the specified columns
Merge <- Merge %>%
  mutate(
    Planting_SA_E = ifelse(Planting_SA_E == 0, NA, Planting_SA_E),
    Planting_SA_N = ifelse(Planting_SA_N == 0, NA, Planting_SA_N),
    Planting_WS_E = ifelse(Planting_WS_E == 0, NA, Planting_WS_E),
    Planting_WS_N = ifelse(Planting_WS_N == 0, NA, Planting_WS_N)
  )

# Define a function to calculate the mode
calculate_mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  if (length(x) == 0) return(NA)  # Return NA if no values are present
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Calculate the mean and mode at the district level
district_summary <- Merge %>%
  group_by(ProDis) %>%
  summarise(
    avg_Planting_SA_E = mean(Planting_SA_E, na.rm = TRUE),
    mode_Planting_SA_E = calculate_mode(Planting_SA_E),
    
    avg_Planting_SA_N = mean(Planting_SA_N, na.rm = TRUE),
    mode_Planting_SA_N = calculate_mode(Planting_SA_N),
    
    avg_Planting_WS_E = mean(Planting_WS_E, na.rm = TRUE),
    mode_Planting_WS_E = calculate_mode(Planting_WS_E),
    
    avg_Planting_WS_N = mean(Planting_WS_N, na.rm = TRUE),
    mode_Planting_WS_N = calculate_mode(Planting_WS_N)
  )

# PLace NaN by NAs in averages
district_summary <- district_summary %>%
  mutate(
    avg_Planting_WS_N = ifelse(is.nan(avg_Planting_WS_N), NA, avg_Planting_WS_N),
    avg_Planting_WS_E = ifelse(is.nan(avg_Planting_WS_E), NA, avg_Planting_WS_E),
    avg_Planting_SA_N = ifelse(is.nan(avg_Planting_SA_N), NA, avg_Planting_SA_N),
    avg_Planting_SA_E = ifelse(is.nan(avg_Planting_SA_E), NA, avg_Planting_SA_E)
  )


# 1b. We first merge commune names to obtain GSO admin codes
district_summary <- merge(district_summary, GSO_Dist [, c(1:4,9)], by = "ProDis", all.x = TRUE); 
district_summary <- district_summary %>% distinct(ProDis, .keep_all = TRUE)

Unmatched <- district_summary [is.na (district_summary$MATINH) & !duplicated(district_summary$ProDis),] # Test if unmatched CS-MAPs Districts exist in the data
Unmatched

# STILL has 5 UNMATCHED DISTRICTS TO CORRECT ................................................



# 2. Add lunar dates from VHLSS; 
curl_function ("data/raw/VHLSS_2022_Household/datasets/Ho_Muc4B11_edited.csv")
VH22 <- read.csv ("data/raw/VHLSS_2022_Household/datasets/Ho_Muc4B11_edited.csv") # 2022

curl_function ("data/raw/VHLSS_2023_Household/Final/Muc4B11.csv")
VH23 <- read.csv ("data/raw/VHLSS_2023_Household/Final/Muc4B11.csv") # 2023
VH22$Year <- '2022'; VH23$Year <- '2023'

district_summary$ID <- paste (district_summary$MATINH, district_summary$MAHUYEN, sep='-')
VH22$ID <- paste (VH22$MATINH, VH22$MAHUYEN, sep='-'); VH23$ID <- paste (VH23$MATINH, VH23$MAHUYEN, sep='-')

curl_function ("data/processed/result_gregorian_final.csv")
Lunar <- read.csv ("data/processed/result_gregorian_final.csv")
# = Dataset converting Lunar dates into gregorian dates

# Convert lunar dates into Dates format
VH22$Dates_VH <- as.Date(paste(VH22$M4B11_C3AY, VH22$M4B11_C3AM, VH22$M4B11_C3AD, sep = "-"), format = "%Y-%m-%d")
VH23$Dates_VH <- as.Date(paste(VH23$M4B11_C3AY, VH23$M4B11_C3AM, VH23$M4B11_C3AD, sep = "-"), format = "%Y-%m-%d")
Lunar$Date_collected <- as.Date(paste(Lunar$sowing_year, Lunar$sowing_month, Lunar$sowing_day, sep = "-"), format = "%Y-%m-%d")
Lunar$Date_converted <- as.Date(paste(Lunar$gre_year, Lunar$gre_month, Lunar$sowing_day, sep = "-"), format = "%Y-%m-%d")

VH22 <- VH22[!is.na (VH22$Dates_VH), ]; VH23 <- VH23[!is.na (VH23$Dates_VH), ]

VH22 <- merge (VH22, Lunar, by.x = "Dates_VH", by.y = "Date_collected", all.x = TRUE); VH22 <- VH22[!is.na (VH22$Dates_VH), ] # 20,616 planting dates in VH22
VH23 <- merge (VH23, Lunar, by.x = "Dates_VH", by.y = "Date_collected", all.x = TRUE); VH23 <- VH23[!is.na (VH23$Dates_VH), ] # 21,800 planting dates in VH23

VH22$Pl_date <- ifelse (VH22$M4B11_C3B.x == 1, VH22$Date_converted, VH22$Dates_VH)
VH23$Pl_date <- ifelse (VH23$M4B11_C3B.x == 1, VH23$Date_converted, VH23$Dates_VH)

VH22$Pl_date <- as.Date(VH22$Pl_date)
VH23$Pl_date <- as.Date(VH23$Pl_date)


# 3. Merge VHLSS with CS- Maps dataset
VH23 <- VH23 [, -17]
VH <- full_join (VH22, VH23)
VH <- VH [VH$MATINH %in% c(80, 82, 83, 84, 86, 87 ,89, 91, 92, 93,94,95,96) & VH$M4B11_MA %in% c(2, 3), ] # Drop non MRD procvinces - 6,037 Obs (4,504 in two crop seasons WS and SA)

VH_merged <- merge (district_summary, VH, by= "ID", all.y=TRUE)
VH_merged <- VH_merged[!is.na (VH_merged$Pl_date), ] # NAs on Planting dates. Total = 3,264 Obs

VH$M4B11_MA <- as.character(VH$M4B11_MA); VH[VH$M4B11_MA == "2", "M4B11_MA"] <- 'WS'
VH$M4B11_MA <- as.character(VH$M4B11_MA); VH[VH$M4B11_MA == "3", "M4B11_MA"] <- 'SA'


# Describe events-HHs
non_unique_IDHO <- VH$IDHO[duplicated(VH$IDHO) | duplicated(VH$IDHO, fromLast = TRUE)]

VH_non_unique <- VH[VH$IDHO %in% non_unique_IDHO, ]
summary_stats <- aggregate(cbind(M4B11_MA, Year) ~ IDHO, data = VH_non_unique, 
                           function(x) c(Count = length(x), Unique = length(unique(x)), 
                                         Values = paste(unique(x), collapse = ", ")))  # Describe the events per hhs [2:4 rows/events]

table (summary_stats$Year) # 357 events are repeated in 2022 and 2023
length(unique(VH$IDHO)) # 2038 hhs have more than one date. Often two seasons

write.csv (VH_merged, 'Output/VH_merged.csv')
rm(list=setdiff(ls(), c("VH_merged", "curl_function", "token")))


# We keep Planting_WS_E as it is (numeric)



# Merge with VHLSS hhs to get sample size



# Reformat at district level
VH_Dis <- VH_merged %>% distinct(ProDis, .keep_all = TRUE) # 92 districts

VH_Dis$Comp_WS <- ifelse (VH_Dis$mode_Planting_WS_N == VH_Dis$mode_Planting_WS_E, 'No change', 
                          ifelse (VH_Dis$mode_Planting_WS_N > VH_Dis$mode_Planting_WS_E, 'Early planting',
                                  ifelse (VH_Dis$mode_Planting_WS_N < VH_Dis$mode_Planting_WS_E, 'Late planting', NA)))

VH_Dis <- VH_Dis %>%
  mutate(Comp_WS_magn = case_when(
    Comp_WS != 'No change' & mode_Planting_WS_E == 0 & mode_Planting_WS_N != 0 ~ NA_real_,
    Comp_WS != 'No change' ~ mode_Planting_WS_E - mode_Planting_WS_N,
    TRUE ~ 0
  ))


VH_Dis$Comp_SA <- ifelse (VH_Dis$mode_Planting_SA_N == VH_Dis$mode_Planting_SA_E, 'No change', 
                          ifelse (VH_Dis$mode_Planting_SA_N > VH_Dis$mode_Planting_SA_E, 'Early planting',
                                  ifelse (VH_Dis$mode_Planting_SA_N < VH_Dis$mode_Planting_SA_E, 'Late planting', NA)))

VH_Dis <- VH_Dis %>%
  mutate(Comp_SA_magn = case_when(
    Comp_SA != 'No change' & mode_Planting_SA_E == 0 & mode_Planting_SA_N != 0 ~ NA_real_,
    Comp_SA != 'No change' ~ mode_Planting_SA_E - mode_Planting_SA_N,
    TRUE ~ 0
  ))

VH_merged2 <- merge (VH_merged, VH_Dis [, c(2,42:45)], by='ProDis', all.x=TRUE)
table (VH_merged2$Comp_WS); table (VH_merged2$Comp_SA);                                     

# Irregulariries
# ~25 Obs go from 5.5 to 0 in extreme scenario (SA). Idem in WS from 11.5 to 0



# Figure 23. Magnitude of change in planting dates recommended by the CS-MAPs in extreme scenarios -----

round (prop.table (table (VH_merged2$Comp_WS_magn)), 2) * 100
round (prop.table (table (VH_merged2$Comp_SA_magn)), 2) * 100

data <- data.frame(
  Diff = c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3),
  WS = c(0, 0, 2, 3, 11, 14, 64, 6, 0, 0, 0, 0, 0),
  SA = c(0, 2, 2, 3, 2, 7, 64, 16, 0, 4, 0, 0, 0)
)


# Transform the data into long format for ggplot2
data_long <- data %>%
  pivot_longer(cols = c("WS", "SA"), names_to = "Season", values_to = "Percentage") %>%
  mutate(Season = recode(Season, "WS" = "Winter-Spring", "SA" = "Summer-Autumn"))

# Plot using ggplot2
ggplot(data_long, aes(x = Diff, y = Percentage, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("cornflowerblue", "salmon")) +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # Add horizontal lines at each 10 points on y-axis
  labs(
    x = "Diff. in planting dates (months)",
    y = "% households"
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),  # Remove major grid lines on x-axis
    panel.grid.minor = element_blank(),     # Remove minor grid lines
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),  # Add space at the bottom of the plot
    axis.title.x = element_text(margin = margin(t = 15))   # Add space between x-axis label and axis
  )


# Table 20. Association of households’ planting dates with CS-MAP recommended dates: Winter-Spring and Summer-Autoumn seasons ----

VH_merged <- VH_merged %>%
  mutate(
    # Recode Pl_date to numeric format based on day ranges
    Pl_date_numeric = case_when(
      day(Pl_date) >= 1 & day(Pl_date) <= 15 ~ month(Pl_date) + 0.0,  # First half of the month
      day(Pl_date) > 15 ~ month(Pl_date) + 0.5                          # Second half of the month
    )
  )


VH_merged$Pl_date_year <- year (VH_merged$Pl_date) # Indicate the Year of hh planting date

convert_to_fortnight <- function(date) { # Events are number as fortnight per year, from 1 to 26.
  day <- day(date)
  month <- month(date)
  fortnight <- ifelse(day <= 15, 0, 1) 
  return((month - 1) * 2 + fortnight + 1)
}

VH_merged$Pl_date_fortnight <- sapply(VH_merged$Pl_date, convert_to_fortnight)

# Convert CS-Maps dates
convert_to_date <- function(variable) {
  as.Date(paste("2000",  # Use a placeholder year
                floor(as.numeric(variable)), 
                ifelse(as.numeric(variable) %% 1 == 0, 1, 16), 
                sep = "-"))
}

# Apply the helper function to each planting variable
VH_merged$Planting_WS_E <- convert_to_date(VH_merged$mode_Planting_WS_E)
VH_merged$Planting_SA_E <- convert_to_date(VH_merged$mode_Planting_SA_E)
VH_merged$Planting_WS_N <- convert_to_date(VH_merged$mode_Planting_WS_N)
VH_merged$Planting_SA_N <- convert_to_date(VH_merged$mode_Planting_SA_N)

# Apply the conversion to fortnights
VH_merged$Planting_SA_E_fortnight <- sapply(VH_merged$Planting_SA_E, convert_to_fortnight)
VH_merged$Planting_WS_E_fortnight <- sapply(VH_merged$Planting_WS_E, convert_to_fortnight)
VH_merged$Planting_SA_N_fortnight <- sapply(VH_merged$Planting_SA_N, convert_to_fortnight)
VH_merged$Planting_WS_N_fortnight <- sapply(VH_merged$Planting_WS_N, convert_to_fortnight)

# Covnert from fortnight to days to faciliate regression readings
VH_merged$Pl_date_fortnight <- VH_merged$Pl_date_fortnight * 14
VH_merged$Planting_SA_E_fortnight <- VH_merged$Planting_SA_E_fortnight * 14
VH_merged$Planting_WS_E_fortnight <- VH_merged$Planting_WS_E_fortnight * 14
VH_merged$Planting_SA_N_fortnight <- VH_merged$Planting_SA_N_fortnight * 14
VH_merged$Planting_WS_N_fortnight <- VH_merged$Planting_WS_N_fortnight * 14


# Add Ag plans
NAME_1 <- c("Bạc Liêu", "Cà Mau", "An Giang", "Kiên Giang", "Long An", 
            "Trà Vinh", "Bến Tre", "Cần Thơ", "Hậu Giang", 
            "Sóc Trăng", "Đồng Tháp", "Vinh Long", "Tien Giang")

years <- c(2021, 2022, 2023, 2024)

# Create a data frame with all combinations of provinces and years
elnino_data <- expand.grid(Province = NAME_1, Year = years)

# Set Ave_High_mention to 1 for the specified conditions
high_mentions <- data.frame(
  Province = c("Bạc Liêu", "Cà Mau", "Bạc Liêu", "An Giang", "An Giang", 
               "Kiên Giang", "Kiên Giang", "Long An", "Long An", 
               "Trà Vinh"),
  Year = c(2021, 2022, 2023, 2023, 2024, 2023, 2024, 2023, 2024, 2023),
  Ave_High_mention = 1
)

# Merge the two data frames and set Ave_High_mention to 0 where not present
elnino_data <- merge(elnino_data, high_mentions, by = c("Province", "Year"), all.x = TRUE)
elnino_data$Ave_High_mention[is.na(elnino_data$Ave_High_mention)] <- 0
names (elnino_data)[1] <- 'NAME_1'

table (elnino_data$Ave_High_mention)

elnino_data$Ave_High_mention_2021 <- ifelse(elnino_data$Year == 2021, elnino_data$Ave_High_mention, 0)
elnino_data$Ave_High_mention_2022 <- ifelse(elnino_data$Year == 2022, elnino_data$Ave_High_mention, 0)
elnino_data$Ave_High_mention_2023 <- ifelse(elnino_data$Year == 2023, elnino_data$Ave_High_mention, 0)

# Merge based on VH_merged$Pl_date_year and elnino_data$Year
VH_merged <- merge(VH_merged, elnino_data, 
                   by.x = c('Province_name', 'Pl_date_year'), 
                   by.y = c('NAME_1', 'Year'), 
                   all.x = TRUE)


# Modelling

VH_merged$WS_Advice_received <- ifelse (VH_merged$Pl_date_year == '2021' & VH_merged$Ave_High_mention_2021 == 1, VH_merged$Planting_WS_E_fortnight, 
                                        ifelse (VH_merged$Pl_date_year == '2021' & VH_merged$Ave_High_mention_2021 ==0, VH_merged$Planting_WS_N_fortnight,   
                                                ifelse (VH_merged$Pl_date_year == '2022' & VH_merged$Ave_High_mention_2022 == 1, VH_merged$Planting_WS_E_fortnight, 
                                                        ifelse (VH_merged$Pl_date_year == '2022' & VH_merged$Ave_High_mention_2022 ==0, VH_merged$Planting_WS_N_fortnight,   
                                                                ifelse (VH_merged$Pl_date_year == '2023' & VH_merged$Ave_High_mention_2023 == 1, VH_merged$Planting_WS_E_fortnight, 
                                                                        ifelse (VH_merged$Pl_date_year == '2023' & VH_merged$Ave_High_mention_2023 ==0, VH_merged$Planting_WS_N_fortnight, NA))))))


VH_merged$SA_Advice_received <- ifelse (VH_merged$Pl_date_year == '2021' & VH_merged$Ave_High_mention_2021 == 1, VH_merged$Planting_SA_E_fortnight, 
                                        ifelse (VH_merged$Pl_date_year == '2021' & VH_merged$Ave_High_mention_2021 ==0, VH_merged$Planting_SA_N_fortnight,   
                                                ifelse (VH_merged$Pl_date_year == '2022' & VH_merged$Ave_High_mention_2022 == 1, VH_merged$Planting_SA_E_fortnight, 
                                                        ifelse (VH_merged$Pl_date_year == '2022' & VH_merged$Ave_High_mention_2022 ==0, VH_merged$Planting_SA_N_fortnight,   
                                                                ifelse (VH_merged$Pl_date_year == '2023' & VH_merged$Ave_High_mention_2023 == 1, VH_merged$Planting_SA_E_fortnight, 
                                                                        ifelse (VH_merged$Pl_date_year == '2023' & VH_merged$Ave_High_mention_2023 ==0, VH_merged$Planting_SA_N_fortnight, NA))))))


VH_merged$Year.2022 <- ifelse (VH_merged$Pl_date_year == '2022', 1, 0)
VH_merged$Year.2023 <- ifelse (VH_merged$Pl_date_year == '2023', 1, 0)

VH_merged$SA_Ext_year <- ifelse (VH_merged$Ave_High_mention_2021 == 1 | VH_merged$Ave_High_mention_2022 == 1 | VH_merged$Ave_High_mention_2023 == 1, 1, 0)
VH_merged$WS_Ext_year <- ifelse (VH_merged$Ave_High_mention_2021 == 1 | VH_merged$Ave_High_mention_2022 == 1 | VH_merged$Ave_High_mention_2023 == 1, 1, 0)

VH_merged$EA_ID <- paste (VH_merged$MATINH.x, VH_merged$MAHUYEN.x, VH_merged$MAXA, VH_merged$MADIABAN, sep='-')


# A) SUmmer-Automn
VH_SA <- VH_merged [VH_merged$M4B11_MA == 3 ,]

# OLS 1 - DO farmer follow the advice?
#model_1 <- lm (Pl_date_numeric ~ SA_Advice_received,  clusters = EA_ID, data = VH_SA )
model_1 <- lm_robust(Pl_date_numeric ~ SA_Advice_received, data = VH_SA, clusters = EA_ID)

# OLS 2 - DO farmer follow the advice, after controlling for year variations?
model_2 <- lm_robust (Pl_date_numeric ~ SA_Advice_received + Year.2022 + Year.2023, clusters = EA_ID, data = VH_SA )

# OLS 3 - DO farmer better follow the advice in extreme vs normal year?
model_3 <- lm_robust (Pl_date_numeric ~ SA_Advice_received + SA_Ext_year + SA_Advice_received * SA_Ext_year + Year.2022 + Year.2023, clusters = EA_ID, data = VH_SA )

models <- list(model_1, model_2, model_3)
models <- models[!sapply(models, is.null)]

SA_table <- export_summs(models, 
                         error_format = "({std.error})", 
                         statistics = c(N = "nobs", R2 = "r.squared"),
                         digits = 3)

write.xlsx(SA_table, "Output/Tab20.CSMAP_model_SA.xlsx")


# B) Winter_Spring
VH_WS <- VH_merged [VH_merged$M4B11_MA == 2 ,]

# OLS 1 - DO farmer follow the advice?
model_1b <- lm_robust (Pl_date_numeric ~ WS_Advice_received, clusters = EA_ID, data = VH_WS )

# OLS 2 - DO farmer follow the advice, after controlling for year variations?
model_2b <- lm_robust (Pl_date_numeric ~ WS_Advice_received + Year.2022 + Year.2023, clusters = EA_ID, data = VH_WS )

# OLS 3 - DO farmer better follow the advice in extreme vs normal year?
model_3b <- lm_robust (Pl_date_numeric ~ WS_Advice_received + WS_Ext_year + WS_Advice_received * WS_Ext_year + Year.2022 + Year.2023, clusters = EA_ID, data = VH_WS )

models <- list(model_1b, model_2b, model_3b)
models <- models[!sapply(models, is.null)]

# Method 2: Display summary using export_summs from jtools
WS_table <- export_summs(models, 
                         error_format = "({std.error})", 
                         statistics = c(N = "nobs", R2 = "r.squared"),
                         digits = 3)

write.xlsx(WS_table, "Output/Tab20.CSMAP_model_WS.xlsx")



# CS-MAPs number of adopters ----

VH_merged <- VH_merged %>%
  mutate(
    # Recode Pl_date to numeric format based on day ranges
    Pl_date_numeric = case_when(
      day(Pl_date) >= 1 & day(Pl_date) <= 15 ~ month(Pl_date) + 0.0,  # First half of the month
      day(Pl_date) > 15 ~ month(Pl_date) + 0.5                          # Second half of the month
    )
  )


VH_merged$Pl_date_year <- year (VH_merged$Pl_date) # Indicate the Year of hh planting date

convert_to_fortnight <- function(date) { # Events are number as fortnight per year, from 1 to 26.
  day <- day(date)
  month <- month(date)
  fortnight <- ifelse(day <= 15, 0, 1) 
  return((month - 1) * 2 + fortnight + 1)
}
convert_to_date <- function(variable) {
  as.Date(paste("2000",  # Use a placeholder year
                floor(as.numeric(variable)), 
                ifelse(as.numeric(variable) %% 1 == 0, 1, 16), 
                sep = "-"))
}

VH_merged$Pl_date_fortnight <- sapply(VH_merged$Pl_date, convert_to_fortnight)

# Apply the helper function to each planting variable
VH_merged$Planting_WS_E <- convert_to_date(VH_merged$mode_Planting_WS_E)
VH_merged$Planting_SA_E <- convert_to_date(VH_merged$mode_Planting_SA_E)
VH_merged$Planting_WS_N <- convert_to_date(VH_merged$mode_Planting_WS_N)
VH_merged$Planting_SA_N <- convert_to_date(VH_merged$mode_Planting_SA_N)

# Apply the conversion to fortnights
VH_merged$Planting_SA_E_fortnight <- sapply(VH_merged$Planting_SA_E, convert_to_fortnight)
VH_merged$Planting_WS_E_fortnight <- sapply(VH_merged$Planting_WS_E, convert_to_fortnight)
VH_merged$Planting_SA_N_fortnight <- sapply(VH_merged$Planting_SA_N, convert_to_fortnight)
VH_merged$Planting_WS_N_fortnight <- sapply(VH_merged$Planting_WS_N, convert_to_fortnight)

#VH_merged$Planting_SA_E_fortnight; VH_merged$Planting_SA_E_fortnight; 
VH_merged$Planting_SA_E # date
VH_merged$Pl_date # date

table(VH_merged$Pl_date, VH_merged$Pl_date)



# SUmmer-Automn
VH_merged$CS_reach_SA <- ifelse (VH_merged$M4B11_MA == 3 & VH_merged$Pl_date_fortnight == VH_merged$Planting_SA_E_fortnight, TRUE, FALSE) # 230 hhs


table (VH_merged$M4B11_MA, VH_merged$CS_reach_SA) # 69 hhs

# Winter-SPring
VH_merged$CS_reach_WS <- ifelse (VH_merged$M4B11_MA == 2 & VH_merged$Pl_date_fortnight == VH_merged$Planting_WS_E_fortnight, TRUE, FALSE) # 234 hh


table (VH_merged$M4B11_MA, VH_merged$CS_reach_WS) # 5 hhs


VH_merged$CSMAP_reach <- ifelse (VH_merged$CS_reach_WS == TRUE | VH_merged$CS_reach_SA == TRUE, 1, 0)

table (VH_merged$CSMAP_reach)

CS <- VH_merged %>%
  rename(
    MAHUYEN = MAHUYEN.y,
    MATINH = MATINH.y,
    MAXA = MAXA,
    MADIABAN = MADIABAN,
    HOSO = HOSO,
    panel = Year
  )

write.csv (CS [, c(17:21,31,41,53:55)], 
           'Output/CSMAPs.vars.22.23.csv') 

table (VH_merged$CSMAP_reach [VH_merged$Year == 2023])

names(VH_merged)

Check <- VH_merged [VH_merged$CS_reach_SA == TRUE | VH_merged$CS_reach_WS == TRUE ,]
Check <- Check [!is.na (Check$ID) ,]
Check <- Check [!duplicated(Check$IDHO) ,] # 69 hhs are concerned
# 417 hhs-cropping seasons. ~50 have 2 cropping seasons



# Add the weights variable
curl_function ("data/processed/VH22_data.csv")
curl_function ("data/processed/VH23_data.csv")
df_22 <- read.csv("data/processed/VH22_data.csv")
df_23 <- read.csv("data/processed/VH23_data.csv")

df_22 <- df_22 %>%
  select (c("MATINH":"HOSO", IDHO, csmap_final, weight_final_rice))

df_23 <- df_23 %>%
  select (c("MATINH":"HOSO", IDHO, csmap_final, weight_final_rice))

Merge.22 <- merge (Check [Check$Year == 2022 ,], df_22 , by='IDHO', all.x=TRUE)
Merge.23 <- merge (Check [Check$Year == 2023 ,], df_23, by='IDHO', all.x=TRUE)

table (Merge.23$CSMAP_reach)

Check <- rbind (Merge.22, Merge.23)

Check$CSMAP_reach <- ifelse (Check$CS_reach_WS == TRUE | Check$CS_reach_SA == TRUE, TRUE, FALSE)
table (Check$CSMAP_reach) # 69 hhs are there

weighted_table <- tapply(Check$weight_final_rice, Check$CSMAP_reach, sum, na.rm = TRUE) 
weighted_table_23 <- tapply(Check$weight_final_rice [Check$Year == 2023], Check$CSMAP_reach [Check$Year == 2023], sum, na.rm = TRUE) # 
table (Check$Province_name)


Merge.23$CSMAP_reach <- ifelse (Merge.23$CS_reach_WS == TRUE | Merge.23$CS_reach_SA == TRUE, TRUE, FALSE)
weighted_table <- tapply(Merge.23$weight_final_rice, Merge.23$CSMAP_reach, sum, na.rm = TRUE) # 27,199 hhs



# Export the CS-MAP hhs, from 2022 and 2023, two cropping seasons
write.csv (Check, 'Output/CSMAPs.vars.22.23.csv') 


setdiff(names(Merge.22), names(Merge.23))

# Columns in Merge.23 but not in Merge.22
setdiff(names(Merge.23), names(Merge.22))




names(df_22)

# Agro-Climatic Bulletins (ACB) ----


# Table 18. Percentage of communes in which ACBs were disseminated in 2024 according to project M&E and VHLSS data. 


# Ag plans 
# curl_function ("data/raw/VHLSS_2024_Commune/Q1/SPIA_Phan42.dta")
# Q1 <- read_dta ("data/raw/VHLSS_2024_Commune/Q1/SPIA_Phan42.dta")
# 
# curl_function ("data/raw/VHLSS_2024_Commune/Q2/SPIA_Phan42.dta")
# Q2 <- read_dta ("data/raw/VHLSS_2024_Commune/Q2/SPIA_Phan42.dta")
# df_24_C <- rbind (Q1, Q2)


curl_function ("data/raw/VHLSS_2024_Commune/Q1/SPIA_ThongTinXa.dta")
curl_function ("data/raw/VHLSS_2024_Commune/Q2/SPIA_ThongTinXa.dta")
curl_function ("data/raw/VHLSS_2024_Commune/Q3/SPIA_ThongTinXa.dta")
curl_function ("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Q1 <- read_dta ("data/raw/VHLSS_2024_Commune/Q1/SPIA_ThongTinXa.dta")
Q2 <- read_dta ("data/raw/VHLSS_2024_Commune/Q2/SPIA_ThongTinXa.dta")
Q3 <- read_dta ("data/raw/VHLSS_2024_Commune/Q3/SPIA_ThongTinXa.dta")
df_24 <- rbind (Q1, Q2, Q3)


Provinces_IDs <- read.csv ("data/raw/VHLSS_2022_Household/datasets/Provinces_IDs.csv")
Provinces_IDs$MATINH <- as.character(Provinces_IDs$MATINH)

df_24 <- df_24 %>%
  left_join (Provinces_IDs)

# Do farmers in this commune receive Agro-Climatic Bulletins that deliver recommendations based on weather forecasts ? 
df_24$Bulletins <- ifelse (df_24$M42_C13 == 1, TRUE, 
                           ifelse (df_24$M42_C13 %in% c(2,3), FALSE, df_24$M42_C13))

table (df_24$Bulletins) # 144/347 (41%)

# Agro-Climatic Bulletins were in 7 MRD provinces in 2022. # All but Dong Thap, Ben Tre, Vinh Lang, Bac Lieu and Ca Mau
table (df_24$Province_name [df_24$Region == '6_MRD'], df_24$Bulletins [df_24$Region == '6_MRD']) 
table (df_24$Province_name [df_24$Region == '6_MRD'], df_24$M42_C141 [df_24$Region == '6_MRD']) # Seasonal
table (df_24$Province_name [df_24$Region == '6_MRD'], df_24$M42_C142 [df_24$Region == '6_MRD']) # Monthly
table (df_24$Province_name [df_24$Region == '6_MRD'], df_24$M42_C143 [df_24$Region == '6_MRD']) # Every 10 days

df_summary <- dfSummary(df_24 [, 22:32])
view(df_summary, file = "Output/Bulletins_summary.html")

# Farmers mostly receive seasonal bulletins (74%), followed monthly (20%) and 10-days (14.6%) 
# 98% of bulletins about rice


# Table 
# Subset data for Region == '6_MRD'
subset_MRD <- df_24[df_24$Region == '6_MRD', ]

# Create tables for each time period
seasonal_table <- table(subset_MRD$Province_name, subset_MRD$M42_C141)
monthly_table <- table(subset_MRD$Province_name, subset_MRD$M42_C142)
ten_days_table <- table(subset_MRD$Province_name, subset_MRD$M42_C143)

# Calculate total respondents per province
total_respondents <- table(subset_MRD$Province_name)

# Calculate the count of Bulletins == TRUE per province
bulletins_true <- table(subset_MRD$Province_name[subset_MRD$Bulletins == TRUE])

# Combine all tables into one data frame
combined_table <- data.frame(
  Province = rownames(total_respondents),  # Province names
  Total.EAs = as.vector(total_respondents),  # Total respondents
  Receive.Bulletins = as.vector(bulletins_true[rownames(total_respondents)]),  # TRUE Bulletins per province
  Type.Seasonal = as.vector(seasonal_table),  # Seasonal values
  Type.Monthly = as.vector(monthly_table),    # Monthly values
  Type.10_days = as.vector(ten_days_table)  # Every 10 days values
)

# Replace NA values with 0 (in case some provinces don't have Bulletins == TRUE)
combined_table$Receive.Bulletins[is.na(combined_table$Receive.Bulletins)] <- 0
combined_table <- combined_table [1:13 ,]

write.csv (combined_table, "Output/Bulletins.csv")

names(df_22)

