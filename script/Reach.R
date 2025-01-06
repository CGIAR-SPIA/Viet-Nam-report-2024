

# ---
# Title: "Reach estimates"
# Author email: "fkosmowski@gmail.com"
# Date: "November 2024"
# ---


# Install and load packages ----

# Function to check and install packages

packages <- c("this.path", "tidyverse", "ggplot2", "gridExtra", "fastDummies", "ggpolypath", "eulerr", "readxl", "stringr", "flextable", "rvest", "iai",
              "jtools", "summarytools", "broom", "estimatr", "sf", "curl", "survey", "haven")

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
library (readstata13)
library (scales)



# Reach of the CGIAR estimates ----

# a) Innovations in VHLSS 2023 ----
df_23 <- read.csv("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH23_data.csv")

# ADD CS-MAPs. Later integrate the variable into df_23
CS <- read.csv("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/Non-genetics/CSMAPs.vars.22.23.csv")
CS <- CS [CS$panel == 2023 ,] # From 406 to 267 in 2023

CS <- CS[order(CS$MATINH, CS$MAHUYEN, CS$MAXA, CS$MADIABAN, CS$HOSO, -CS$CSMAP_reach), ]
CS <- CS[!duplicated(CS[c("MATINH", "MAHUYEN", "MAXA", "MADIABAN", "HOSO")]), ]

df_23 <- merge(df_23, CS, by = intersect(names(df_23), names(CS)), all.x = TRUE)
df_23$CSMAP_reach <- as.integer(df_23$CSMAP_reach)

CG.reach.23 <- df_23 [, c(1:6,12,16,25,27,29,33,38,43,69,47:50)]

CG.reach.23$ThreeR <- ifelse(
  CG.reach.23$d_1m5r_seed_120kg == 1 & CG.reach.23$lenient_2r == 1 & CG.reach.23$lenient_3r == 1, 1, 
  ifelse(is.na(CG.reach.23$d_1m5r_seed_120kg) & is.na(CG.reach.23$lenient_2r) & is.na(CG.reach.23$lenient_3r), NA, 0 ))


CG.reach.23$CG.reach.23 <- ifelse (CG.reach.23$CIAT.related == 1 | CG.reach.23$StrainB_edited == 1 | CG.reach.23$awd_1drydown == 1 | CG.reach.23$SWCP ==1  | CG.reach.23$ThreeR == 1 | CG.reach.23$CSMAP_reach == 1, 1, 0)
CG.reach.23$CG.reach.23 [is.na (CG.reach.23$CG.reach.23)] <- 0
table (CG.reach.23$CG.reach.23)

CG.reach.23$sum_CG <- rowSums(
  cbind(
    ifelse(is.na(CG.reach.23$CIAT.related), 0, CG.reach.23$CIAT.related),
    ifelse(is.na(CG.reach.23$StrainB_edited), 0, CG.reach.23$StrainB_edited),
    ifelse(is.na(CG.reach.23$awd_1drydown), 0, CG.reach.23$awd_1drydown),
    ifelse(is.na(CG.reach.23$SWCP), 0, CG.reach.23$SWCP),
    ifelse(is.na(CG.reach.23$ThreeR), 0, CG.reach.23$ThreeR),
    ifelse(is.na(CG.reach.23$CSMAP_reach), 0, CG.reach.23$CSMAP_reach)
    
  )
)

table (CG.reach.23$sum_CG) # Non adopters, single adopters and multi-adopters

# What are the two innovations adopted together?
#subset_two_innovations <- CG.reach.23[CG.reach.23$sum_CG == 2, ]
#subset_two_innovations$two_innovations <- apply(
#  subset_two_innovations[, c("CIAT.related", "StrainB_edited", "awd_1drydown", "SWCP", "ThreeR", "CSMAP_reach", "mech_laser_level")],
#  1,
#  function(row) paste(names(row)[which(row == 1)], collapse = " & ")
#)

#table_of_two_innovations <- table(subset_two_innovations$two_innovations)
#table_of_two_innovations


# Add weights
w <- read.dta13 ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2023/weight2023.dta")
w$ID <- paste (w$tinh, w$huyen, w$xa, w$diaban, sep='-')
CG.reach.23$ID <- paste (CG.reach.23$MATINH, CG.reach.23$MAHUYEN, CG.reach.23$MAXA, CG.reach.23$MADIABAN, sep='-')

CG.reach.23 <- merge (CG.reach.23, w, by="ID", all=TRUE)
CG.reach.23 <- CG.reach.23 [!is.na (CG.reach.23$MATINH) ,]

CG.reach.23$weighted_frequency <- 0


# Apply weights only where sum_CG is 1, using only wt35 weights for the calculation
CG.reach.23$weighted_frequency[CG.reach.23$sum_CG == 1] <- rowSums(
  cbind(
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$CIAT.related), 
           CG.reach.23$CIAT.related * CG.reach.23$weight_cass, 0),
    
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$StrainB_edited), 
           CG.reach.23$StrainB_edited * CG.reach.23$wt45, 0),
    
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$awd_1drydown), 
           CG.reach.23$awd_1drydown * CG.reach.23$wt45, 0),
    
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$SWCP), 
           CG.reach.23$SWCP * CG.reach.23$wt45, 0),
    
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$ThreeR), 
           CG.reach.23$ThreeR * CG.reach.23$wt45, 0),
    
    ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$CSMAP_reach), 
           CG.reach.23$CSMAP_reach * CG.reach.23$wt45, 0)
   )[CG.reach.23$sum_CG == 1, ],
  na.rm = TRUE
)


# Apply weights for sum_CG == 2 where CIAT.related is 1, using weight_cass
CG.reach.23$weighted_frequency <- ifelse(
  CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 1 & !is.na(CG.reach.23$CIAT.related),
  CG.reach.23$CIAT.related * CG.reach.23$weight_cass,
  CG.reach.23$weighted_frequency  # Keep existing value if condition isn't met
)

# Apply weights for sum_CG == 2 where CIAT.related is 0, using wt45
CG.reach.23$weighted_frequency <- ifelse(
  CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 0 & !is.na(CG.reach.23$CIAT.related),
  rowSums(
    cbind(
      ifelse(!is.na(CG.reach.23$StrainB_edited), CG.reach.23$StrainB_edited * CG.reach.23$wt45, 0),
      ifelse(!is.na(CG.reach.23$awd_1drydown), CG.reach.23$awd_1drydown * CG.reach.23$wt45, 0),
      ifelse(!is.na(CG.reach.23$SWCP), CG.reach.23$SWCP * CG.reach.23$wt45, 0),
      ifelse(!is.na(CG.reach.23$ThreeR), CG.reach.23$ThreeR * CG.reach.23$wt45, 0) # Covers all multi-adoption patterns in the data
    ),
    na.rm = TRUE
  ),
  CG.reach.23$weighted_frequency  # Keep existing value if condition isn't met
)

# Total reach in 2023
sum(CG.reach.23$weighted_frequency) # Unique adopters = 1,763,580

# Substract the multi-adopters that adopted Cassava with wt45
sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 1], na.rm=TRUE) # 23,098

# Substract the multi-adopters that adopted coffee with wt45
sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 2 & CG.reach.23$SWCP == 1], na.rm=TRUE) # 18,269

# Final estimates for reach in VH23 = 1,722,213
Reach.VH23 <- sum(CG.reach.23$weighted_frequency) - (sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 1], na.rm=TRUE) + sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 2 & CG.reach.23$SWCP == 1], na.rm=TRUE))



# 2. Innovation weighted frequency 
total_weighted_frequency <- sum(CG.reach.23$weighted_frequency, na.rm = TRUE)

CIAT_related_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$CIAT.related),
         CG.reach.23$CIAT.related * CG.reach.23$weight_cass, 0), na.rm = TRUE
) + sum(
  ifelse(CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 1 & !is.na(CG.reach.23$CIAT.related),
         CG.reach.23$CIAT.related * CG.reach.23$weight_cass, 0), na.rm = TRUE
)

StrainB_edited_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$StrainB_edited),
         CG.reach.23$StrainB_edited * CG.reach.23$wt45, 0), na.rm = TRUE
)

awd_1drydown_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$awd_1drydown),
         CG.reach.23$awd_1drydown * CG.reach.23$wt45, 0), na.rm = TRUE
)

SWCP_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$SWCP),
         CG.reach.23$SWCP * CG.reach.23$wt45, 0), na.rm = TRUE
)

ThreeR_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$ThreeR),
         CG.reach.23$ThreeR * CG.reach.23$wt45, 0), na.rm = TRUE
)

CSMAP_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$CSMAP_reach),
         CG.reach.23$CSMAP_reach * CG.reach.23$wt45, 0), na.rm = TRUE
)

LLL_contribution <- sum(
  ifelse(CG.reach.23$sum_CG == 1 & !is.na(CG.reach.23$mech_laser_level),
         CG.reach.23$mech_laser_level * CG.reach.23$wt45, 0), na.rm = TRUE
)


# For sum_CG == 2 & CIAT.related == 0 cases:
StrainB_edited_contribution <- StrainB_edited_contribution + sum(
  ifelse(CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 0 & !is.na(CG.reach.23$StrainB_edited),
         CG.reach.23$StrainB_edited * CG.reach.23$wt45, 0), na.rm = TRUE
)

awd_1drydown_contribution <- awd_1drydown_contribution + sum(
  ifelse(CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 0 & !is.na(CG.reach.23$awd_1drydown),
         CG.reach.23$awd_1drydown * CG.reach.23$wt45, 0), na.rm = TRUE
)

SWCP_contribution <- SWCP_contribution + sum(
  ifelse(CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 0 & !is.na(CG.reach.23$SWCP),
         CG.reach.23$SWCP * CG.reach.23$wt45, 0), na.rm = TRUE
)

ThreeR_contribution <- ThreeR_contribution + sum(
  ifelse(CG.reach.23$sum_CG == 2 & CG.reach.23$CIAT.related == 0 & !is.na(CG.reach.23$ThreeR),
         CG.reach.23$ThreeR * CG.reach.23$wt45, 0), na.rm = TRUE
)

# Calculate the percentage contribution of each innovation
contributions <- data.frame(
  Innovation = c("CIAT.related", "StrainB_edited", "awd_1drydown", "SWCP", "ThreeR", "CSMAP_reach"),
  Contribution = c(
    CIAT_related_contribution,
    StrainB_edited_contribution,
    awd_1drydown_contribution,
    SWCP_contribution,
    ThreeR_contribution,
    CSMAP_contribution
  ),
  Percentage = c(
    CIAT_related_contribution,
    StrainB_edited_contribution,
    awd_1drydown_contribution,
    SWCP_contribution,
    ThreeR_contribution,
    CSMAP_contribution
  ) / total_weighted_frequency * 100
)

# Print results
print(contributions) 




# b) Innovations integrated in VHLSS 2022 ----

df_22 <- read.csv("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/Report 2024/Reproducible Scripts/Output/VH22_data.csv")
CG.reach.22 <- df_22 [, c(1:6,33,17,39,40)]

names(df_22)

w <- read.dta13 ("C:/Users/FKosmowski/SPIA Dropbox/SPIA General/SPIA 2019-2024/5. OBJ.3-Data collection/Country teams/Vietnam/DATA/VHLSS_Household_2022/datasets/Weights/wt2022_SPIA.dta")

w$ID <- paste (w$tinh, w$huyen, w$xa, w$diaban, sep='-')
CG.reach.22$ID <- paste (CG.reach.22$MATINH, CG.reach.22$MAHUYEN, CG.reach.22$MAXA, CG.reach.22$MADIABAN, sep='-')
CG.reach.22 <- merge (CG.reach.22, w, by="ID", all=TRUE)
CG.reach.22 <- CG.reach.22 [!is.na (CG.reach.22$MATINH) ,]

# Since there is no multi-adopters, we sum-up both innovations
sum(CG.reach.22$weight_rice_DNA [CG.reach.22$IRRI_Parentage_edited == 1], na.rm=TRUE) # 
sum(CG.reach.22$wt45 [CG.reach.22$straw_rm_mushroom == 1], na.rm=TRUE) # 

Reach.VH22 <- sum(CG.reach.22$weight_rice_DNA [CG.reach.22$IRRI_Parentage_edited == 1], na.rm=TRUE) + sum(CG.reach.22$wt45 [CG.reach.22$straw_rm_mushroom == 1], na.rm=TRUE) 



# Calculate CSMAp reach and LLL reach (additional hh not counted in other adoption figures). 
sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 1 & CG.reach.23$mech_laser_level == 1], na.rm=TRUE) # 66,460 hhs 
sum(CG.reach.23$wt45 [CG.reach.23$sum_CG == 1 & CG.reach.23$CSMAP_reach == 1], na.rm=TRUE) # 135,676

# Lower bound = 
Reach.lower <- Reach.VH22 + Reach.VH23 + 227331 - 135676 # PFES from VH24 is added. CS_MAPs were already part so we substract it 

# Higher bound =
Reach.higher <- Reach.VH22 + Reach.VH23 + 227331 + 66460 # PFES from VH24 is added + Lazer-land levelling

paste ('The reach CGIAR research is estimated to be between', round (Reach.lower,0), "and", round (Reach.higher,0), "households in Viet Nam")

# Note that using population weights and taking multi=adoption into account leads to different estimates than Tab 7 of the report





# Fig 1. Number of households adopting each CGIAR-related innovation in Viet Nam in 2023, in millions  ----

options(scipen = 999)

data <- data.frame(
  Innovation = c(
    "Genetically Improved Farmed Tilapia (GIFT) derived strains", 
    "Improved Rice Varieties", 
    #"Salt-Tolerant Rice Varieties", 
    #"Submergence-Tolerant Rice Varieties", 
    "Improved Cassava Varieties", 
    #"CMD-resistant cassava varieties", 
    "Climate-Smart Mapping and Adaptation Planning (CS-MAP)", 
    #"Agro-Climatic Bulletins (ABCs)", 
    "Laser Land Leveling (LLL)", 
    # "Drum Seeder", 
    # "Combine Harvester", 
    #"Mini-Combine Harvester", 
    # "Straw Baler", 
    "Off-field Straw Management practices", 
    "3 Reductions 3 Gains (3R3G) and 1 Must do, 5 Reductions (1M5R)", 
    "Alternate Wetting and Drying (AWD)", 
    "Sustainable Water Use for Coffee production", 
    "Payments for Forest Environmental Services (PFES)"
  ),
  Reach = c(27291, 1926902, 270869, 148000, 409359, 4949, 787775, 408689, 362010, 227331)
)

# Specify the innovations to be highlighted
highlighted_innovations <- c(
  "Genetically Improved Farmed Tilapia (GIFT) derived strains", 
  "Improved Rice Varieties", 
  "Improved Cassava Varieties", 
  "Off-field Straw Management practices", 
  "3 Reductions 3 Gains (3R3G) and 1 Must do, 5 Reductions (1M5R)", 
  "Alternate Wetting and Drying (AWD)", 
  "Sustainable Water Use for Coffee production", 
  "Payments for Forest Environmental Services (PFES)"
  #"Climate-Smart Mapping and Adaptation Planning (CS-MAP)"
)


# Add a new column to indicate if an innovation should be highlighted
data <- data %>%
  mutate(Highlight = ifelse(Innovation %in% highlighted_innovations, "Highlighted", "Normal"))

# Remove rows with NA in Reach
data <- na.omit(data)

# Order data by Reach in ascending order
data <- data %>% arrange(Reach)

ggplot(data, aes(x = Reach / 1e6, y = reorder(Innovation, Reach, FUN = function(x) -x), fill = Highlight)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Highlighted" = "darkblue", "Normal" = "lightblue")) +
  scale_x_continuous(labels = label_number(scale = 1),  # Format x-axis labels
                     breaks = seq(0, 3, by = 1)) +  # Set breaks to end at 6
  labs(#title = "Fig 1. Number of households adopting each CGIAR-related innovation in Vietnam in 2023, in millions",
    x = " ",
    y = "Innovation") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"))












