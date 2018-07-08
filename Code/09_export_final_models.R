
# Load Libraries and Data -------------------------------------------------

library(tidyverse)
library(lubridate)

pm25 <- read_csv("Data/Cleaned/pm25_model_trimmed_long.csv")
no   <- read_csv("Data/Cleaned/no_model_trimmed_long.csv")
no2  <- read_csv("Data/Cleaned/no2_model_trimmed_long.csv")

# Drop Unused Temporally Interpolated Models ------------------------------

# MEAN.TRUE.LUR
#
# Temporal factor: MEAN
# Imputation: TRUE
# Truncation: At LUR
#
# Temporal adjustment using factors based on a ratio between
# the monthly mean of air pollutant concentrations across all air monitors
# for the month of interest over the annual mean of air pollutant
# concentrations across all air monitors for the LUR year. Missing air
# monitor data has been imputed prior to developing temporal factors. And
# LUR models have been truncated to prevent zeros prior to temporal
# adjustment.

pm25 <- select(pm25, YEAR, MONTH, PC, MEAN.TRUE.LUR) %>%
  mutate(DATE        = ymd(as.Date(paste0(YEAR,"-",MONTH,"-01"))),
         POSTAL.CODE = PC,
         PM25        = MEAN.TRUE.LUR) %>%
  select(DATE, POSTAL.CODE, PM25)

no <- select(no, YEAR, MONTH, PC, MEAN.TRUE.LUR) %>%
  mutate(DATE        = ymd(as.Date(paste0(YEAR,"-",MONTH,"-01"))),
         POSTAL.CODE = PC,
         NO          = MEAN.TRUE.LUR) %>%
  select(DATE, POSTAL.CODE, NO)

no2 <- select(no2, YEAR, MONTH, PC, MEAN.TRUE.LUR) %>%
  mutate(DATE        = ymd(as.Date(paste0(YEAR,"-",MONTH,"-01"))),
         POSTAL.CODE = PC,
         NO2         = MEAN.TRUE.LUR) %>%
  select(DATE, POSTAL.CODE, NO2)

# Merge Pollutants --------------------------------------------------------

model <- full_join(pm25,  no,  by = c("DATE", "POSTAL.CODE"))
model <- full_join(model, no2, by = c("DATE", "POSTAL.CODE"))

model <- arrange(model, POSTAL.CODE, DATE)

summary(model)

# Save File ---------------------------------------------------------------

write_csv(model, "Results/temporally_adjusted_air_pollution_models.csv")

rm(list = ls())

# Delete Temporay Files ---------------------------------------------------

file.remove(file.path("Data/Temp/", list.files("Data/Temp/")))
