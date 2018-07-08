#' ---
#' Trim and subset data to prepare for visualization and analyses. Drop
#' "Value" and "Factor" as unnecessary variables, and subset data to only
#' Years 2003-2009.
#' ---

library(tidyverse)

save.files <- function(df, dir, filename) {
  write_csv(df,   paste0(dir, filename, ".csv"))
}

dir <- "Data/Cleaned/"

prep.data <- function(df.path) {
  min.year <- 2003
  max.year <- 2009
  df <- read_csv(df.path)
  df.trim <- select(df, -c(Value, Factor)) %>%
    filter(Year >= min.year & Year <= max.year) %>%
    select(Measure, Imputed, Truncation, Year, Month, PC, Modelled) %>%
    mutate(Measure    = factor(Measure),
           Imputed    = factor(Imputed),
           Truncation = factor(Truncation),
           Year       = factor(Year),
           Month      = factor(Month),
           PC         = factor(PC)) %>%
    arrange(Measure, Imputed, Truncation, Year, PC, Month)
}

# Trim and save data: PM2.5

model.pm25 <- prep.data("Data/Cleaned/pm25_model.csv")
save.files(model.pm25, dir, "pm25_model_trimmed")
rm(model.pm25); gc()

# Trim and save data: NO

model.no <- prep.data("Data/Cleaned/no_model.csv")
save.files(model.no, dir, "no_model_trimmed")
rm(model.no); gc()

# Trim and save data: NO2

model.no2 <- prep.data("Data/Cleaned/no2_model.csv")
save.files(model.no2, dir, "no2_model_trimmed")
rm(model.no2); gc()

# Clear global environment

rm(list = ls())
