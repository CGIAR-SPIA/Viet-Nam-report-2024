library(readxl)
library(this.path)
library(haven)
library(tidyverse)

rm(list = ls())

setwd(here())

#Muc4B11 - Rice (GSO)----

rice_q1 <- read_excel("Q1.xlsx", sheet = "Muc4B11")
rice_q2 <- read_excel("Q2.xlsx", sheet = "Muc4B11")
rice_q3 <- read_dta("Q3/Ho_Muc4B11.dta")
rice_q4 <- read_dta("Q4/Ho_Muc4B11.dta")

rice <- full_join(rice_q1, rice_q2)
rice <- full_join(rice, rice_q3)
rice <- full_join(rice, rice_q4)

write.csv (rice, "Final/Muc4B11.csv")


#SPIA_M4B11A - Rice----
rice_spia_q1 <- read_excel("Q1.xlsx", sheet = "SPIA_M4B11A")
rice_spia_q2 <- read_excel("Q2.xlsx", sheet = "SPIA_M4B11A")
rice_spia_q3 <- read_dta ("Q3/Ho_SPIA_M4B11A.dta")
rice_spia_q4 <- read_dta ("Q4/Ho_SPIA_M4B11A.dta")

rice_spia <- full_join (rice_spia_q1, rice_spia_q2) %>%
  full_join (rice_spia_q3) %>%
  full_join (rice_spia_q4)

write.csv(rice_spia, "Final/SPIA_M4B11A.csv")



#SPIA_M4B11A1 - CSMAP----
csmap_q1 <- read_excel("Q1.xlsx", sheet = "SPIA_M4B11A1")
csmap_q2 <- read_excel("Q2.xlsx", sheet = "SPIA_M4B11A1")
csmap_q3 <- read_dta ("Q3/Ho_SPIA_M4B11A1.dta")
csmap_q4 <- read_dta ("Q4/Ho_SPIA_M4B11A1.dta")

csmap <- full_join (csmap_q1, csmap_q2) %>%
  full_join(csmap_q3) %>%
  full_join(csmap_q4)

write.csv (csmap, "Final/SPIA_M4B11A1.csv")

#SPIA_M4B11A3 - Pesticides ----
pest_q1 <- read_excel("Q1.xlsx", sheet = "SPIA_M4B11A3")
pest_q2 <- read_excel("Q2.xlsx", sheet = "SPIA_M4B11A3")
pest_q3 <- read_dta ("Q3/Ho_SPIA_M4B11A3.dta")
pest_q4 <- read_dta ("Q4/Ho_SPIA_M4B11A3.dta")

pest <- full_join (pest_q1, pest_q2) %>%
  full_join (pest_q3) %>%
  full_join (pest_q4)

write.csv(pest, "Final/SPIA_M4B11A3.csv")


# SPIA_M4B11A4 - Fertilizer ----
fert_q1 <- read_excel("Q1.xlsx", sheet = "SPIA_M4B11A4")
fert_q2 <- read_excel("Q2.xlsx", sheet = "SPIA_M4B11A4")
fert_q3 <- read_dta ("Q3/Ho_SPIA_M4B11A4.dta")
fert_q4 <- read_dta ("Q4/Ho_SPIA_M4B11A4.dta")

fert <- full_join (fert_q1, fert_q2) %>%
  full_join (fert_q3) %>%
  full_join (fert_q4)

write.csv (fert, "Final/SPIA_M4B11A4.csv")



# SPIA_M4B11A5 - Water----
water_q1 <- read_excel("Q1.xlsx", sheet = "SPIA_M4B11A5")
water_q2 <- read_excel("Q2.xlsx", sheet = "SPIA_M4B11A5")
water_q3 <- read_dta ("Q3/Ho_SPIA_M4B11A5.dta")
water_q4 <- read_dta ("Q4/Ho_SPIA_M4B11A5.dta")


water <- full_join (water_q1, water_q2) %>%
  full_join (water_q3) %>%
  full_join (water_q4)


write.csv(water, "Final/SPIA_M4B11A5.csv")
