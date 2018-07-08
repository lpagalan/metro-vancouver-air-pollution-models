
# Description -------------------------------------------------------------

# Create spatiotemporal model of pollutants for Metro Van from 2003-2014.
# Take LUR values at a given postal code and multiple it by temporal factor
# of a given month and year.

# Load libraries and prepare data -----------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

dir      <- "Data/Cleaned/"
dir.temp <- "Data/Temp/"

save.files <- function(df, dir, filename) {
  write_csv(df,   paste0(dir, filename, ".csv"))
}

remove.files <- function(dir, filename) {
  file.remove(paste0(dir, filename, ".csv"))
}

if(!dir.exists(dir.temp)) { dir.create(dir.temp) }

# Load cleaned spatial air pollution data

lur.val.pm25 <- read_csv("Data/Cleaned/pm25_lur_val_at_pc.csv")
lur.val.no   <- read_csv("Data/Cleaned/no_lur_val_at_pc.csv")
lur.val.no2  <- read_csv("Data/Cleaned/no2_lur_val_at_pc.csv")

# Load temporal factors

temp.factor.pm25 <- read_csv("Data/Cleaned/pm25_temporal_factor.csv")
temp.factor.no   <- read_csv("Data/Cleaned/no_temporal_factor.csv")
temp.factor.no2  <- read_csv("Data/Cleaned/no2_temporal_factor.csv")

# Split temporal factor data frames

pm25.mean       <- filter(temp.factor.pm25, Measure == "Mean",   Imputed == F)
pm25.mean.imp   <- filter(temp.factor.pm25, Measure == "Mean",   Imputed == T)
pm25.median     <- filter(temp.factor.pm25, Measure == "Median", Imputed == F)
pm25.median.imp <- filter(temp.factor.pm25, Measure == "Median", Imputed == T)

no.mean         <- filter(temp.factor.no,   Measure == "Mean",   Imputed == F)
no.mean.imp     <- filter(temp.factor.no,   Measure == "Mean",   Imputed == T)
no.median       <- filter(temp.factor.no,   Measure == "Median", Imputed == F)
no.median.imp   <- filter(temp.factor.no,   Measure == "Median", Imputed == T)

no2.mean        <- filter(temp.factor.no2,  Measure == "Mean",   Imputed == F)
no2.mean.imp    <- filter(temp.factor.no2,  Measure == "Mean",   Imputed == T)
no2.median      <- filter(temp.factor.no2,  Measure == "Median", Imputed == F)
no2.median.imp  <- filter(temp.factor.no2,  Measure == "Median", Imputed == T)

# Prepare PC data frame ---------------------------------------------------

# Prepare data frame with PC-extracted LUR values for vectorized operations.
# First create monthly columns, and then reshape data frame to long format.

add.months <- function(df) {

  # Add month columns

  df.months <- df
  for(month in 1:12) {
    df.months[ , paste0("M", as.character(month))] = month
  }

  # Gather month columns to long

  df.long <- gather(df.months, Column, Month, M1:M12) %>%
    select(Year, Month, PC, Value) %>%
    arrange(Year, PC, Month)

  return(df.long)
}

lur.val.pm25 <- add.months(lur.val.pm25)
lur.val.no   <- add.months(lur.val.no)
lur.val.no2  <- add.months(lur.val.no2)

summary.pm25 <- group_by(lur.val.pm25, Year, PC) %>%
  summarise(SD = sd(Value), N = n())

summary.no <- group_by(lur.val.no, Year, PC) %>%
  summarise(SD = sd(Value), N = n())

summary.no2 <- group_by(lur.val.no2, Year, PC) %>%
  summarise(SD = sd(Value), N = n())

all(summary.pm25$SD == 0,  na.rm = T)
all(summary.no$SD   == 0,  na.rm = T)
all(summary.no2$SD  == 0,  na.rm = T)

all(summary.pm25$N  == 12, na.rm = T)
all(summary.no$N    == 12, na.rm = T)
all(summary.no2$N   == 12, na.rm = T)

rm(list = ls(pattern = "^summary."))

# Temporally adjusted estimates: no truncation ----------------------------

# Temporally adjust pollutants to create spatiotemporal models. Develop
# temporally adjusted estimate by multiplying LUR value at PC by temporal
# factor for each month for each year.

model.pollutant <- function(df.lur.val, df.factor) {

  # Merge temporal factor to data frame with LUR values at PC

  df <- merge(df.lur.val,
              select(df.factor, Year, Month, Measure, Imputed, Factor),
              by = c("Year", "Month"),
              all.x = T) %>%

    # Select variables of interest

    select(Measure, Imputed, Year, Month, PC, Value, Factor) %>%

    # Sort columns

    arrange(Measure, Imputed, Year, PC, Month) %>%

    # Calculate temporally adjusted estimate

    mutate(Modelled = Value * Factor)
  return(df)
}

model.pm25.mean       <- model.pollutant(lur.val.pm25, pm25.mean)
model.pm25.mean.imp   <- model.pollutant(lur.val.pm25, pm25.mean.imp)
model.pm25.median     <- model.pollutant(lur.val.pm25, pm25.median)
model.pm25.median.imp <- model.pollutant(lur.val.pm25, pm25.median.imp)

model.no.mean         <- model.pollutant(lur.val.no, no.mean)
model.no.mean.imp     <- model.pollutant(lur.val.no, no.mean.imp)
model.no.median       <- model.pollutant(lur.val.no, no.median)
model.no.median.imp   <- model.pollutant(lur.val.no, no.median.imp)

model.no2.mean        <- model.pollutant(lur.val.no2, no2.mean)
model.no2.mean.imp    <- model.pollutant(lur.val.no2, no2.mean.imp)
model.no2.median      <- model.pollutant(lur.val.no2, no2.median)
model.no2.median.imp  <- model.pollutant(lur.val.no2, no2.median.imp)

save.files(model.pm25.mean,       dir.temp, "model.pm25.mean")
save.files(model.pm25.mean.imp,   dir.temp, "model.pm25.mean.imp")
save.files(model.pm25.median,     dir.temp, "model.pm25.median")
save.files(model.pm25.median.imp, dir.temp, "model.pm25.median.imp")

save.files(model.no.mean,         dir.temp, "model.no.mean")
save.files(model.no.mean.imp,     dir.temp, "model.no.mean.imp")
save.files(model.no.median,       dir.temp, "model.no.median")
save.files(model.no.median.imp,   dir.temp, "model.no.median.imp")

save.files(model.no2.mean,        dir.temp, "model.no2.mean")
save.files(model.no2.mean.imp,    dir.temp, "model.no2.mean.imp")
save.files(model.no2.median,      dir.temp, "model.no2.median")
save.files(model.no2.median.imp,  dir.temp, "model.no2.median.imp")

rm(list = c(ls(pattern = "^model.pm"),
            ls(pattern = "^model.no")))

# Temporally adjusted estimates: LUR truncation ---------------------------

# Temporally adjust pollutants, but first truncate LUR value floor based on
# observed air monitoring station values.

# Load data with min and max observed values

monthly.bounds.pm25 <- read_csv("Data/Cleaned/pm25_bounds.csv")
monthly.bounds.no   <- read_csv("Data/Cleaned/no_bounds.csv")
monthly.bounds.no2  <- read_csv("Data/Cleaned/no2_bounds.csv")

# Subset data to only minimum observations for LUR year

get.lur.min <- function(df, year, is.imputed) {
  df.min <- filter(df,
                   Year    == year,
                   Measure == "Min",
                   Imputed == is.imputed) %>%
    select(Measure, Imputed, Monitor, Year, Month, Value) %>%
    arrange(Measure, Imputed, Monitor, Year, Month)
  return(df.min)
}

lur.min.pm25       <- get.lur.min(monthly.bounds.pm25, 2003, F)
lur.min.no.03      <- get.lur.min(monthly.bounds.no,   2003, F)
lur.min.no.10      <- get.lur.min(monthly.bounds.no,   2010, F)
lur.min.no2.03     <- get.lur.min(monthly.bounds.no2,  2003, F)
lur.min.no2.10     <- get.lur.min(monthly.bounds.no2,  2010, F)

lur.min.pm25.imp   <- get.lur.min(monthly.bounds.pm25, 2003, T)
lur.min.no.03.imp  <- get.lur.min(monthly.bounds.no,   2003, T)
lur.min.no.10.imp  <- get.lur.min(monthly.bounds.no,   2010, T)
lur.min.no2.03.imp <- get.lur.min(monthly.bounds.no2,  2003, T)
lur.min.no2.10.imp <- get.lur.min(monthly.bounds.no2,  2010, T)

# Calculate floor based on median-median & imp of min values for LUR year

annual.mean.min.pm25         <- mean(lur.min.pm25$Value,         na.rm = T)
annual.mean.min.no.03        <- mean(lur.min.no.03$Value,        na.rm = T)
annual.mean.min.no.10        <- mean(lur.min.no.10$Value,        na.rm = T)
annual.mean.min.no2.03       <- mean(lur.min.no2.03$Value,       na.rm = T)
annual.mean.min.no2.10       <- mean(lur.min.no2.10$Value,       na.rm = T)

annual.mean.min.pm25.imp     <- mean(lur.min.pm25.imp$Value,     na.rm = T)
annual.mean.min.no.03.imp    <- mean(lur.min.no.03.imp$Value,    na.rm = T)
annual.mean.min.no.10.imp    <- mean(lur.min.no.10.imp$Value,    na.rm = T)
annual.mean.min.no2.03.imp   <- mean(lur.min.no2.03.imp$Value,   na.rm = T)
annual.mean.min.no2.10.imp   <- mean(lur.min.no2.10.imp$Value,   na.rm = T)

annual.median.min.pm25       <- median(lur.min.pm25$Value,       na.rm = T)
annual.median.min.no.03      <- median(lur.min.no.03$Value,      na.rm = T)
annual.median.min.no.10      <- median(lur.min.no.10$Value,      na.rm = T)
annual.median.min.no2.03     <- median(lur.min.no2.03$Value,     na.rm = T)
annual.median.min.no2.10     <- median(lur.min.no2.10$Value,     na.rm = T)

annual.median.min.pm25.imp   <- median(lur.min.pm25.imp$Value,   na.rm = T)
annual.median.min.no.03.imp  <- median(lur.min.no.03.imp$Value,  na.rm = T)
annual.median.min.no.10.imp  <- median(lur.min.no.10.imp$Value,  na.rm = T)
annual.median.min.no2.03.imp <- median(lur.min.no2.03.imp$Value, na.rm = T)
annual.median.min.no2.10.imp <- median(lur.min.no2.10.imp$Value, na.rm = T)

# Save LUR floors

floor.1  <- c(annual.mean.min.pm25,         "Mean",   FALSE, "PM25", 2003)
floor.2  <- c(annual.mean.min.no.03,        "Mean",   FALSE, "NO",   2003)
floor.3  <- c(annual.mean.min.no.10,        "Mean",   FALSE, "NO",   2010)
floor.4  <- c(annual.mean.min.no2.03,       "Mean",   FALSE, "NO2",  2003)
floor.5  <- c(annual.mean.min.no2.10,       "Mean",   FALSE, "NO2",  2010)

floor.6  <- c(annual.mean.min.pm25.imp,     "Mean",   TRUE,  "PM25", 2003)
floor.7  <- c(annual.mean.min.no.03.imp,    "Mean",   TRUE,  "NO",   2003)
floor.8  <- c(annual.mean.min.no.10.imp,    "Mean",   TRUE,  "NO",   2010)
floor.9  <- c(annual.mean.min.no2.03.imp,   "Mean",   TRUE,  "NO2",  2003)
floor.10 <- c(annual.mean.min.no2.10.imp,   "Mean",   TRUE,  "NO2",  2010)

floor.11 <- c(annual.median.min.pm25,       "Median", FALSE, "PM25", 2003)
floor.12 <- c(annual.median.min.no.03,      "Median", FALSE, "NO",   2003)
floor.13 <- c(annual.median.min.no.10,      "Median", FALSE, "NO",   2010)
floor.14 <- c(annual.median.min.no2.03,     "Median", FALSE, "NO2",  2003)
floor.15 <- c(annual.median.min.no2.10,     "Median", FALSE, "NO2",  2010)

floor.16 <- c(annual.median.min.pm25.imp,   "Median", TRUE,  "PM25", 2003)
floor.17 <- c(annual.median.min.no.03.imp,  "Median", TRUE,  "NO",   2003)
floor.18 <- c(annual.median.min.no.10.imp,  "Median", TRUE,  "NO",   2010)
floor.19 <- c(annual.median.min.no2.03.imp, "Median", TRUE,  "NO2",  2003)
floor.20 <- c(annual.median.min.no2.10.imp, "Median", TRUE,  "NO2",  2010)

lur.floors <- as.data.frame(cbind(floor.1,  floor.2,
                                  floor.3,  floor.4,
                                  floor.5,  floor.6,
                                  floor.7,  floor.8,
                                  floor.9,  floor.10,
                                  floor.11, floor.12,
                                  floor.13, floor.14,
                                  floor.15, floor.16,
                                  floor.17, floor.18,
                                  floor.19, floor.20))

lur.floors <- as.data.frame(t(lur.floors))

names(lur.floors) <- c("Floor", "Measure", "Imputed", "Pollutant", "Year")

rownames(lur.floors) <- 1:nrow(lur.floors)

lur.floors <- mutate(lur.floors,
                     Floor     = as.numeric(as.character(Floor)),
                     Measure   = as.character(Measure),
                     Imputed   = as.logical(as.character(Imputed)),
                     Pollutant = as.character(Pollutant),
                     Year      = as.numeric(as.character(Year)))

save.files(lur.floors, dir, "lur_floors")

rm(list = c(ls(pattern = "^lur.min."),
            ls(pattern = "^annual.mean."),
            ls(pattern = "^annual.median."),
            ls(pattern = "^floor.")))

# Truncate LUR value floors

ltrunc.floor <- function(df, df.floor, measure, imputed, pollutant, year) {
  df.floor <- filter(df.floor,
                     Measure   == measure,
                     Imputed   == imputed,
                     Pollutant == pollutant,
                     Year      == year)$Floor[1]
  df <- mutate(df, Value = ifelse(Value < df.floor, df.floor, Value))
  return(df)
}

lur.val.no.03  <- filter(lur.val.no, Year <  2010)
lur.val.no.10  <- filter(lur.val.no, Year >= 2010)

lur.val.no2.03 <- filter(lur.val.no2, Year <  2010)
lur.val.no2.10 <- filter(lur.val.no2, Year >= 2010)

# PM2.5

lur.trunc.pm25.mean       <- ltrunc.floor(lur.val.pm25, lur.floors,
                                          "Mean", F,
                                          "PM25", 2003)

lur.trunc.pm25.mean.imp   <- ltrunc.floor(lur.val.pm25, lur.floors,
                                          "Mean", T,
                                          "PM25", 2003)

lur.trunc.pm25.median     <- ltrunc.floor(lur.val.pm25, lur.floors,
                                          "Median", F,
                                          "PM25",   2003)

lur.trunc.pm25.median.imp <- ltrunc.floor(lur.val.pm25, lur.floors,
                                          "Median", T,
                                          "PM25",   2003)

filter(lur.floors, Pollutant == "PM25", Year == 2003)

summary(lur.trunc.pm25.mean$Value)
summary(lur.trunc.pm25.mean.imp$Value)
summary(lur.trunc.pm25.median$Value)
summary(lur.trunc.pm25.median.imp$Value)

# NO - 2003

lur.trunc.no.03.mean       <- ltrunc.floor(lur.val.no.03, lur.floors,
                                           "Mean", F,
                                           "NO",   2003)

lur.trunc.no.03.mean.imp   <- ltrunc.floor(lur.val.no.03, lur.floors,
                                           "Mean", T,
                                           "NO",   2003)

lur.trunc.no.03.median     <- ltrunc.floor(lur.val.no.03, lur.floors,
                                           "Median", F,
                                           "NO",     2003)

lur.trunc.no.03.median.imp <- ltrunc.floor(lur.val.no.03, lur.floors,
                                           "Median", T,
                                           "NO",     2003)

filter(lur.floors, Pollutant == "NO", Year == 2003)

summary(lur.trunc.no.03.mean$Value)
summary(lur.trunc.no.03.mean.imp$Value)
summary(lur.trunc.no.03.median$Value)
summary(lur.trunc.no.03.median.imp$Value)

# NO - 2010

lur.trunc.no.10.mean       <- ltrunc.floor(lur.val.no.10, lur.floors,
                                           "Mean", F,
                                           "NO",   2010)

lur.trunc.no.10.mean.imp   <- ltrunc.floor(lur.val.no.10, lur.floors,
                                           "Mean", T,
                                           "NO",   2010)

lur.trunc.no.10.median     <- ltrunc.floor(lur.val.no.10, lur.floors,
                                           "Median", F,
                                           "NO",     2010)

lur.trunc.no.10.median.imp <- ltrunc.floor(lur.val.no.10, lur.floors,
                                           "Median", T,
                                           "NO",     2010)

filter(lur.floors, Pollutant == "NO", Year == 2010)

summary(lur.trunc.no.10.mean$Value)
summary(lur.trunc.no.10.mean.imp$Value)
summary(lur.trunc.no.10.median$Value)
summary(lur.trunc.no.10.median.imp$Value)

# NO2 - 2003

lur.trunc.no2.03.mean       <- ltrunc.floor(lur.val.no2.03, lur.floors,
                                            "Mean",  F,
                                            "NO2",   2003)

lur.trunc.no2.03.mean.imp   <- ltrunc.floor(lur.val.no2.03, lur.floors,
                                            "Mean",  T,
                                            "NO2",   2003)

lur.trunc.no2.03.median     <- ltrunc.floor(lur.val.no2.03, lur.floors,
                                            "Median",  F,
                                            "NO2",     2003)

lur.trunc.no2.03.median.imp <- ltrunc.floor(lur.val.no2.03, lur.floors,
                                            "Median",  T,
                                            "NO2",     2003)

filter(lur.floors, Pollutant == "NO2", Year == 2003)

summary(lur.trunc.no2.03.mean$Value)
summary(lur.trunc.no2.03.mean.imp$Value)
summary(lur.trunc.no2.03.median$Value)
summary(lur.trunc.no2.03.median.imp$Value)

# NO2 - 2010

lur.trunc.no2.10.mean       <- ltrunc.floor(lur.val.no2.10, lur.floors,
                                            "Mean",  F,
                                            "NO2",   2010)

lur.trunc.no2.10.mean.imp   <- ltrunc.floor(lur.val.no2.10, lur.floors,
                                            "Mean",  T,
                                            "NO2",   2010)

lur.trunc.no2.10.median     <- ltrunc.floor(lur.val.no2.10, lur.floors,
                                            "Median",  F,
                                            "NO2",     2010)

lur.trunc.no2.10.median.imp <- ltrunc.floor(lur.val.no2.10, lur.floors,
                                            "Median",  T,
                                            "NO2",     2010)

filter(lur.floors, Pollutant == "NO2", Year == 2010)

summary(lur.trunc.no2.10.mean$Value)
summary(lur.trunc.no2.10.mean.imp$Value)
summary(lur.trunc.no2.10.median$Value)
summary(lur.trunc.no2.10.median.imp$Value)

rm(list = ls(pattern = "^lur.val"))

# Bind respective NO and NO2 columns

lur.trunc.no.mean       <- rbind(lur.trunc.no.03.mean,
                                 lur.trunc.no.10.mean)

lur.trunc.no.mean.imp   <- rbind(lur.trunc.no.03.mean.imp,
                                 lur.trunc.no.10.mean.imp)

lur.trunc.no.median     <- rbind(lur.trunc.no.03.median,
                                 lur.trunc.no.10.median)

lur.trunc.no.median.imp <- rbind(lur.trunc.no.03.median.imp,
                                 lur.trunc.no.10.median.imp)

rm(list = c(ls(pattern = "^lur.trunc.no.03"),
            ls(pattern = "^lur.trunc.no.10")))

lur.trunc.no2.mean       <- rbind(lur.trunc.no2.03.mean,
                                  lur.trunc.no2.10.mean)

lur.trunc.no2.mean.imp   <- rbind(lur.trunc.no2.03.mean.imp,
                                  lur.trunc.no2.10.mean.imp)

lur.trunc.no2.median     <- rbind(lur.trunc.no2.03.median,
                                  lur.trunc.no2.10.median)

lur.trunc.no2.median.imp <- rbind(lur.trunc.no2.03.median.imp,
                                  lur.trunc.no2.10.median.imp)

rm(list = c(ls(pattern = "^lur.trunc.no2.03"),
            ls(pattern = "^lur.trunc.no2.10")))

# Check LUR truncation

check.min <- function(df, floor, pollutant, measure, imputed) {
  min <- filter(floor,
                Pollutant == pollutant,
                Measure   == measure,
                Imputed   == imputed);
  print(min)
  drop_na(df, Value) %>%
    group_by(Year) %>%
    summarise(Min = min(Value))
}

check.min(lur.trunc.pm25.mean,       lur.floors, "PM25", "Mean",   F)
check.min(lur.trunc.pm25.mean.imp,   lur.floors, "PM25", "Mean",   T)
check.min(lur.trunc.pm25.median,     lur.floors, "PM25", "Median", F)
check.min(lur.trunc.pm25.median.imp, lur.floors, "PM25", "Median", T)

check.min(lur.trunc.no.mean,         lur.floors, "NO",   "Mean",   F)
check.min(lur.trunc.no.mean.imp,     lur.floors, "NO",   "Mean",   T)
check.min(lur.trunc.no.median,       lur.floors, "NO",   "Median", F)
check.min(lur.trunc.no.median.imp,   lur.floors, "NO",   "Median", T)

check.min(lur.trunc.no2.mean,        lur.floors, "NO2",  "Mean",   F)
check.min(lur.trunc.no2.mean.imp,    lur.floors, "NO2",  "Mean",   T)
check.min(lur.trunc.no2.median,      lur.floors, "NO2",  "Median", F)
check.min(lur.trunc.no2.median.imp,  lur.floors, "NO2",  "Median", T)

# Check number of postal codes that were LUR floor truncated

lur.floors.check <- filter(lur.floors,
                           Year == 2003,
                           Measure == "Mean",
                           Imputed == TRUE) %>%
  select(Floor, Pollutant)

check.num.pc.lur.trunc <- function(df, pollutant) {
  floor <- lur.floors.check$Floor[lur.floors.check$Pollutant == pollutant]
  total <- filter(df, Year == 2003) %>%
    select(Value)
  filtered <- filter(df, Year  == 2003,
                         Value == floor) %>%
    select(Value) %>%
    na.omit()
  print(paste0("Total PC's filtered: ", nrow(filtered),
         " out of ", nrow(total),
         " (", round(nrow(filtered) / nrow(total) * 100, 1), "%)"))
}

check.num.pc.lur.trunc(lur.trunc.pm25.mean.imp, "PM25")
check.num.pc.lur.trunc(lur.trunc.no.mean.imp,   "NO")
check.num.pc.lur.trunc(lur.trunc.no2.mean.imp,  "NO2")

rm(lur.floors.check, check.num.pc.lur.trunc)

# Temporally adjust LUR truncated values

model.pm25.mean.ltrc       <- model.pollutant(lur.trunc.pm25.mean,
                                                        pm25.mean)

model.pm25.mean.imp.ltrc   <- model.pollutant(lur.trunc.pm25.mean.imp,
                                                        pm25.mean.imp)

model.pm25.median.ltrc     <- model.pollutant(lur.trunc.pm25.median,
                                                        pm25.median)

model.pm25.median.imp.ltrc <- model.pollutant(lur.trunc.pm25.median.imp,
                                                        pm25.median.imp)

model.no.mean.ltrc         <- model.pollutant(lur.trunc.no.mean,
                                                        no.mean)

model.no.mean.imp.ltrc     <- model.pollutant(lur.trunc.no.mean.imp,
                                                        no.mean.imp)

model.no.median.ltrc       <- model.pollutant(lur.trunc.no.median,
                                                        no.median)

model.no.median.imp.ltrc   <- model.pollutant(lur.trunc.no.median.imp,
                                                        no.median.imp)

model.no2.mean.ltrc        <- model.pollutant(lur.trunc.no2.mean,
                                                        no2.mean)

model.no2.mean.imp.ltrc    <- model.pollutant(lur.trunc.no2.mean.imp,
                                                        no2.mean.imp)

model.no2.median.ltrc      <- model.pollutant(lur.trunc.no2.median,
                                                        no2.median)

model.no2.median.imp.ltrc  <- model.pollutant(lur.trunc.no2.median.imp,
                                                        no2.median.imp)

rm(list = ls(pattern = "^lur.trunc."))

save.files(model.pm25.mean.ltrc,       dir.temp, "model.pm25.mean.ltrc")
save.files(model.pm25.mean.imp.ltrc,   dir.temp, "model.pm25.mean.imp.ltrc")
save.files(model.pm25.median.ltrc,     dir.temp, "model.pm25.median.ltrc")
save.files(model.pm25.median.imp.ltrc, dir.temp, "model.pm25.median.imp.ltrc")

save.files(model.no.mean.ltrc,       dir.temp, "model.no.mean.ltrc")
save.files(model.no.mean.imp.ltrc,   dir.temp, "model.no.mean.imp.ltrc")
save.files(model.no.median.ltrc,     dir.temp, "model.no.median.ltrc")
save.files(model.no.median.imp.ltrc, dir.temp, "model.no.median.imp.ltrc")

save.files(model.no2.mean.ltrc,       dir.temp, "model.no2.mean.ltrc")
save.files(model.no2.mean.imp.ltrc,   dir.temp, "model.no2.mean.imp.ltrc")
save.files(model.no2.median.ltrc,     dir.temp, "model.no2.median.ltrc")
save.files(model.no2.median.imp.ltrc, dir.temp, "model.no2.median.imp.ltrc")

rm(list = c(ls(pattern = "^model.pm"),
            ls(pattern = "^model.no")))

# Temporally adjusted estimates: temporal truncation ----------------------

# Temporally adjust pollutants then truncate temporally adjusted floor based
# on observed air monitoring station values.

# Calculate monthly floors based on central tendency and imputation

get.month.min <- function(df) {
  df <- filter(df, Measure == "Min") %>%
    drop_na(Value)
  df.min <- group_by(df, Imputed, Year, Month) %>%
    summarise(Mean       = mean(Value),
              Median     = median(Value),
              N.Monitors = n())
  df.min <- gather(df.min, Measure, Value, c(Mean, Median)) %>%
    arrange(Imputed, Measure, Year, Month) %>%
    rename(Floor = Value)
  return(df.min)
}

tem.month.floors.pm25 <- get.month.min(monthly.bounds.pm25)
tem.month.floors.no   <- get.month.min(monthly.bounds.no)
tem.month.floors.no2  <- get.month.min(monthly.bounds.no2)

save.files(tem.month.floors.pm25, dir, "pm25_tem_floors_monthly")
save.files(tem.month.floors.no,   dir, "no_tem_floors_monthly")
save.files(tem.month.floors.no2,  dir, "no2_tem_floors_monthly")

floor.pm25.mean       <- filter(tem.month.floors.pm25,
                                Measure == "Mean",
                                Imputed == F)

floor.pm25.mean.imp   <- filter(tem.month.floors.pm25,
                                Measure == "Mean",
                                Imputed == T)

floor.pm25.median     <- filter(tem.month.floors.pm25,
                                Measure == "Median",
                                Imputed == F)

floor.pm25.median.imp <- filter(tem.month.floors.pm25,
                                Measure == "Median",
                                Imputed == T)

floor.no.mean         <- filter(tem.month.floors.no,
                                Measure == "Mean",
                                Imputed == F)

floor.no.mean.imp     <- filter(tem.month.floors.no,
                                Measure == "Mean",
                                Imputed == T)

floor.no.median       <- filter(tem.month.floors.no,
                                Measure == "Median",
                                Imputed == F)

floor.no.median.imp   <- filter(tem.month.floors.no,
                                Measure == "Median",
                                Imputed == T)

floor.no2.mean        <- filter(tem.month.floors.no2,
                                Measure == "Mean",
                                Imputed == F)

floor.no2.mean.imp    <- filter(tem.month.floors.no2,
                                Measure == "Mean",
                                Imputed == T)

floor.no2.median      <- filter(tem.month.floors.no2,
                                Measure == "Median",
                                Imputed == F)

floor.no2.median.imp  <- filter(tem.month.floors.no2,
                                Measure == "Median",
                                Imputed == T)

# Trucante floor of temporally adjusted models

ttrunc.floor <- function(df, floor) {
  df.ttrunc <- merge(df, floor, by = c("Year",
                                       "Month",
                                       "Measure",
                                       "Imputed")) %>%
    mutate(Truncated = ifelse(Modelled < Floor, Floor, Modelled)) %>%
    select(Measure, Imputed,
           Year,    Month,
           PC,      Value,
           Factor,  Truncated) %>%
    arrange(Measure, Imputed, Year, PC, Month) %>%
    rename(Modelled = Truncated)
  return(df.ttrunc)
}

# Truncate floor of temporally adjusted models: PM2.5

model.pm25.mean       <- read_csv("Data/Temp/model.pm25.mean.csv")
model.pm25.mean.imp   <- read_csv("Data/Temp/model.pm25.mean.imp.csv")
model.pm25.median     <- read_csv("Data/Temp/model.pm25.median.csv")
model.pm25.median.imp <- read_csv("Data/Temp/model.pm25.median.imp.csv")

model.pm25.mean.ttrc       <- ttrunc.floor(model.pm25.mean,
                                           floor.pm25.mean)

model.pm25.mean.imp.ttrc   <- ttrunc.floor(model.pm25.mean.imp,
                                           floor.pm25.mean.imp)

model.pm25.median.ttrc     <- ttrunc.floor(model.pm25.median,
                                           floor.pm25.median)

model.pm25.median.imp.ttrc <- ttrunc.floor(model.pm25.median.imp,
                                           floor.pm25.median.imp)

save.files(model.pm25.mean.ttrc,       dir.temp, "model.pm25.mean.ttrc")
save.files(model.pm25.mean.imp.ttrc,   dir.temp, "model.pm25.mean.imp.ttrc")
save.files(model.pm25.median.ttrc,     dir.temp, "model.pm25.median.ttrc")
save.files(model.pm25.median.imp.ttrc, dir.temp, "model.pm25.median.imp.ttrc")

rm(list = ls(pattern = "^model.pm"))

# Truncate floor of temporally adjusted models: NO

model.no.mean       <- read_csv("Data/Temp/model.no.mean.csv")
model.no.mean.imp   <- read_csv("Data/Temp/model.no.mean.imp.csv")
model.no.median     <- read_csv("Data/Temp/model.no.median.csv")
model.no.median.imp <- read_csv("Data/Temp/model.no.median.imp.csv")

model.no.mean.ttrc       <- ttrunc.floor(model.no.mean,
                                         floor.no.mean)

model.no.mean.imp.ttrc   <- ttrunc.floor(model.no.mean.imp,
                                         floor.no.mean.imp)

model.no.median.ttrc     <- ttrunc.floor(model.no.median,
                                         floor.no.median)

model.no.median.imp.ttrc <- ttrunc.floor(model.no.median.imp,
                                         floor.no.median.imp)

save.files(model.no.mean.ttrc,       dir.temp, "model.no.mean.ttrc")
save.files(model.no.mean.imp.ttrc,   dir.temp, "model.no.mean.imp.ttrc")
save.files(model.no.median.ttrc,     dir.temp, "model.no.median.ttrc")
save.files(model.no.median.imp.ttrc, dir.temp, "model.no.median.imp.ttrc")

rm(list = ls(pattern = "^model.no"))

# Truncate floor of temporally adjusted models: NO2

model.no2.mean       <- read_csv("Data/Temp/model.no2.mean.csv")
model.no2.mean.imp   <- read_csv("Data/Temp/model.no2.mean.imp.csv")
model.no2.median     <- read_csv("Data/Temp/model.no2.median.csv")
model.no2.median.imp <- read_csv("Data/Temp/model.no2.median.imp.csv")

model.no2.mean.ttrc       <- ttrunc.floor(model.no2.mean,
                                          floor.no2.mean)

model.no2.mean.imp.ttrc   <- ttrunc.floor(model.no2.mean.imp,
                                          floor.no2.mean.imp)

model.no2.median.ttrc     <- ttrunc.floor(model.no2.median,
                                          floor.no2.median)

model.no2.median.imp.ttrc <- ttrunc.floor(model.no2.median.imp,
                                          floor.no2.median.imp)

save.files(model.no2.mean.ttrc,       dir.temp, "model.no2.mean.ttrc")
save.files(model.no2.mean.imp.ttrc,   dir.temp, "model.no2.mean.imp.ttrc")
save.files(model.no2.median.ttrc,     dir.temp, "model.no2.median.ttrc")
save.files(model.no2.median.imp.ttrc, dir.temp, "model.no2.median.imp.ttrc")

rm(list = ls(pattern = "^model.no"))

# Bind pollutant: PM25 ----------------------------------------------------

model.pm25.mean            <- read_csv("Data/Temp/model.pm25.mean.csv")
model.pm25.mean.imp        <- read_csv("Data/Temp/model.pm25.mean.imp.csv")
model.pm25.median          <- read_csv("Data/Temp/model.pm25.median.csv")
model.pm25.median.imp      <- read_csv("Data/Temp/model.pm25.median.imp.csv")
model.pm25.mean.ltrc       <- read_csv("Data/Temp/model.pm25.mean.ltrc.csv")
model.pm25.mean.imp.ltrc   <- read_csv("Data/Temp/model.pm25.mean.imp.ltrc.csv")
model.pm25.median.ltrc     <- read_csv("Data/Temp/model.pm25.median.ltrc.csv")
model.pm25.median.imp.ltrc <- read_csv("Data/Temp/model.pm25.median.imp.ltrc.csv")
model.pm25.mean.ttrc       <- read_csv("Data/Temp/model.pm25.mean.ttrc.csv")
model.pm25.mean.imp.ttrc   <- read_csv("Data/Temp/model.pm25.mean.imp.ttrc.csv")
model.pm25.median.ttrc     <- read_csv("Data/Temp/model.pm25.median.ttrc.csv")
model.pm25.median.imp.ttrc <- read_csv("Data/Temp/model.pm25.median.imp.ttrc.csv")

# Compare all models: PM2.5

summary(model.pm25.mean)
summary(model.pm25.mean.imp)
summary(model.pm25.median)
summary(model.pm25.median.imp)

summary(model.pm25.mean.ltrc)
summary(model.pm25.mean.imp.ltrc)
summary(model.pm25.median.ltrc)
summary(model.pm25.median.imp.ltrc)

summary(model.pm25.mean.ttrc)
summary(model.pm25.mean.imp.ttrc)
summary(model.pm25.median.ttrc)
summary(model.pm25.median.imp.ttrc)

# Add a column for imputation method

model.pm25.mean$Truncation            = "None"
model.pm25.mean.imp$Truncation        = "None"
model.pm25.median$Truncation          = "None"
model.pm25.median.imp$Truncation      = "None"

model.pm25.mean.ltrc$Truncation       = "LUR"
model.pm25.mean.imp.ltrc$Truncation   = "LUR"
model.pm25.median.ltrc$Truncation     = "LUR"
model.pm25.median.imp.ltrc$Truncation = "LUR"

model.pm25.mean.ttrc$Truncation       = "Temporal"
model.pm25.mean.imp.ttrc$Truncation   = "Temporal"
model.pm25.median.ttrc$Truncation     = "Temporal"
model.pm25.median.imp.ttrc$Truncation = "Temporal"

# Bind models together

model.pm25 <- bind_rows(list(model.pm25.mean,
                             model.pm25.mean.imp,
                             model.pm25.median,
                             model.pm25.median.imp,
                             model.pm25.mean.ltrc,
                             model.pm25.mean.imp.ltrc,
                             model.pm25.median.ltrc,
                             model.pm25.median.imp.ltrc,
                             model.pm25.mean.ttrc,
                             model.pm25.mean.imp.ttrc,
                             model.pm25.median.ttrc,
                             model.pm25.median.imp.ttrc))

# Save file

save.files(model.pm25, dir, "pm25_model")

rm(list = ls(pattern = "^model.pm25"))

# Bind pollutant: NO ------------------------------------------------------

model.no.mean            <- read_csv("Data/Temp/model.no.mean.csv")
model.no.mean.imp        <- read_csv("Data/Temp/model.no.mean.imp.csv")
model.no.median          <- read_csv("Data/Temp/model.no.median.csv")
model.no.median.imp      <- read_csv("Data/Temp/model.no.median.imp.csv")
model.no.mean.ltrc       <- read_csv("Data/Temp/model.no.mean.ltrc.csv")
model.no.mean.imp.ltrc   <- read_csv("Data/Temp/model.no.mean.imp.ltrc.csv")
model.no.median.ltrc     <- read_csv("Data/Temp/model.no.median.ltrc.csv")
model.no.median.imp.ltrc <- read_csv("Data/Temp/model.no.median.imp.ltrc.csv")
model.no.mean.ttrc       <- read_csv("Data/Temp/model.no.mean.ttrc.csv")
model.no.mean.imp.ttrc   <- read_csv("Data/Temp/model.no.mean.imp.ttrc.csv")
model.no.median.ttrc     <- read_csv("Data/Temp/model.no.median.ttrc.csv")
model.no.median.imp.ttrc <- read_csv("Data/Temp/model.no.median.imp.ttrc.csv")

# Compare all models: NO

summary(model.no.mean)
summary(model.no.mean.imp)
summary(model.no.median)
summary(model.no.median.imp)

summary(model.no.mean.ltrc)
summary(model.no.mean.imp.ltrc)
summary(model.no.median.ltrc)
summary(model.no.median.imp.ltrc)

summary(model.no.mean.ttrc)
summary(model.no.mean.imp.ttrc)
summary(model.no.median.ttrc)
summary(model.no.median.imp.ttrc)

# Add a column for imputation method

model.no.mean$Truncation            = "None"
model.no.mean.imp$Truncation        = "None"
model.no.median$Truncation          = "None"
model.no.median.imp$Truncation      = "None"

model.no.mean.ltrc$Truncation       = "LUR"
model.no.mean.imp.ltrc$Truncation   = "LUR"
model.no.median.ltrc$Truncation     = "LUR"
model.no.median.imp.ltrc$Truncation = "LUR"

model.no.mean.ttrc$Truncation       = "Temporal"
model.no.mean.imp.ttrc$Truncation   = "Temporal"
model.no.median.ttrc$Truncation     = "Temporal"
model.no.median.imp.ttrc$Truncation = "Temporal"

# Bind models together

model.no <- bind_rows(list(model.no.mean,
                           model.no.mean.imp,
                           model.no.median,
                           model.no.median.imp,
                           model.no.mean.ltrc,
                           model.no.mean.imp.ltrc,
                           model.no.median.ltrc,
                           model.no.median.imp.ltrc,
                           model.no.mean.ttrc,
                           model.no.mean.imp.ttrc,
                           model.no.median.ttrc,
                           model.no.median.imp.ttrc))

# Save file

save.files(model.no, dir, "no_model")

rm(list = ls(pattern = "^model.no"))

# Bind pollutant: NO2 -----------------------------------------------------

model.no2.mean            <- read_csv("Data/Temp/model.no2.mean.csv")
model.no2.mean.imp        <- read_csv("Data/Temp/model.no2.mean.imp.csv")
model.no2.median          <- read_csv("Data/Temp/model.no2.median.csv")
model.no2.median.imp      <- read_csv("Data/Temp/model.no2.median.imp.csv")
model.no2.mean.ltrc       <- read_csv("Data/Temp/model.no2.mean.ltrc.csv")
model.no2.mean.imp.ltrc   <- read_csv("Data/Temp/model.no2.mean.imp.ltrc.csv")
model.no2.median.ltrc     <- read_csv("Data/Temp/model.no2.median.ltrc.csv")
model.no2.median.imp.ltrc <- read_csv("Data/Temp/model.no2.median.imp.ltrc.csv")
model.no2.mean.ttrc       <- read_csv("Data/Temp/model.no2.mean.ttrc.csv")
model.no2.mean.imp.ttrc   <- read_csv("Data/Temp/model.no2.mean.imp.ttrc.csv")
model.no2.median.ttrc     <- read_csv("Data/Temp/model.no2.median.ttrc.csv")
model.no2.median.imp.ttrc <- read_csv("Data/Temp/model.no2.median.imp.ttrc.csv")

# Compare all models: NO2

summary(model.no2.mean)
summary(model.no2.mean.imp)
summary(model.no2.median)
summary(model.no2.median.imp)

summary(model.no2.mean.ltrc)
summary(model.no2.mean.imp.ltrc)
summary(model.no2.median.ltrc)
summary(model.no2.median.imp.ltrc)

summary(model.no2.mean.ttrc)
summary(model.no2.mean.imp.ttrc)
summary(model.no2.median.ttrc)
summary(model.no2.median.imp.ttrc)

# Add a column for imputation method

model.no2.mean$Truncation            = "None"
model.no2.mean.imp$Truncation        = "None"
model.no2.median$Truncation          = "None"
model.no2.median.imp$Truncation      = "None"

model.no2.mean.ltrc$Truncation       = "LUR"
model.no2.mean.imp.ltrc$Truncation   = "LUR"
model.no2.median.ltrc$Truncation     = "LUR"
model.no2.median.imp.ltrc$Truncation = "LUR"

model.no2.mean.ttrc$Truncation       = "Temporal"
model.no2.mean.imp.ttrc$Truncation   = "Temporal"
model.no2.median.ttrc$Truncation     = "Temporal"
model.no2.median.imp.ttrc$Truncation = "Temporal"

# Bind models together

model.no2 <- bind_rows(list(model.no2.mean,
                            model.no2.mean.imp,
                            model.no2.median,
                            model.no2.median.imp,
                            model.no2.mean.ltrc,
                            model.no2.mean.imp.ltrc,
                            model.no2.median.ltrc,
                            model.no2.median.imp.ltrc,
                            model.no2.mean.ttrc,
                            model.no2.mean.imp.ttrc,
                            model.no2.median.ttrc,
                            model.no2.median.imp.ttrc))

# Save file

save.files(model.no2,  dir, "no2_model")

rm(list = ls(pattern = "^model.no2"))

rm(list = ls())

# Verify rbind and count truncated ----------------------------------------

count.truncated <- function(model) {

  filtered.lur <- mutate(model,
                         Measure = as.factor(Measure),
                         Year = as.factor(Year),
                         Month = as.factor(Month),
                         Truncation = as.factor(Truncation)) %>%
    filter(Measure    == "Mean",
           Imputed    == T,
           Truncation == "LUR")

  filtered.non <- mutate(model,
                         Measure = as.factor(Measure),
                         Year = as.factor(Year),
                         Month = as.factor(Month),
                         Truncation = as.factor(Truncation)) %>%
    filter(Measure    == "Mean",
           Imputed    == T,
           Truncation == "None")

  filtered.join <- full_join(filtered.lur, filtered.non, by = c("Measure",
                                                                "Imputed",
                                                                "Year",
                                                                "Month",
                                                                "PC",
                                                                "Factor"))
  filtered.join <- mutate(filtered.join,
                          LUR.Val.Diff = Value.x - Value.y,
                          Truncated = ifelse(LUR.Val.Diff != 0, T, F))

  summary <- group_by(filtered.join, Year, Truncated) %>%
    summarise(N = n()) %>%
    spread(Truncated, N) %>%
    select(-`<NA>`) %>%
    mutate(Total = `TRUE` + `FALSE`,
           Percent = `TRUE` / Total * 100)

  return(summary)
}

calc.percent.trunc <- function(summary) {
  summary.total <- c("Total",
                     sum(summary$`FALSE`,      na.rm = T),
                     sum(summary$`TRUE`,       na.rm = T),
                     sum(summary$`TRUE`,       na.rm = T) +
                       sum(summary$`FALSE`,    na.rm = T),
                     sum(summary$`TRUE`,       na.rm = T) /
                       (sum(summary$`TRUE`,    na.rm = T) +
                          sum(summary$`FALSE`, na.rm = T)) * 100)

  names(summary.total) <- c("Year", "FALSE", "TRUE", "Total", "Percent")
  summary.total <- as.data.frame(t(summary.total))
  return(summary.total)
}

# PM25

model.pm25 <- read_csv("Data/Cleaned/pm25_model.csv")

group_by(model.pm25, Measure, Imputed, Truncation) %>%
  summarise(Max    = max(Modelled,    na.rm = T),
            Mean   = mean(Modelled,   na.rm = T),
            Median = median(Modelled, na.rm = T),
            Min    = min(Modelled,    na.rm = T),
            N      = n())

trunc.count.pm25 <- count.truncated(model.pm25)
trunc.summary.pm25 <- calc.percent.trunc(trunc.count.pm25)

rm(model.pm25)

# NO

model.no <- read_csv("Data/Cleaned/no_model.csv")

group_by(model.no, Measure, Imputed, Truncation) %>%
  summarise(Max    = max(Modelled,    na.rm = T),
            Mean   = mean(Modelled,   na.rm = T),
            Median = median(Modelled, na.rm = T),
            Min    = min(Modelled,    na.rm = T),
            N      = n())

trunc.count.no <- count.truncated(model.no)
trunc.summary.no <- calc.percent.trunc(trunc.count.no)

rm(model.no)

# NO2

model.no2 <- read_csv("Data/Cleaned/no2_model.csv")

group_by(model.no2, Measure, Imputed, Truncation) %>%
  summarise(Max    = max(Modelled,    na.rm = T),
            Mean   = mean(Modelled,   na.rm = T),
            Median = median(Modelled, na.rm = T),
            Min    = min(Modelled,    na.rm = T),
            N      = n())

trunc.count.no2 <- count.truncated(model.no2)
trunc.summary.no2 <- calc.percent.trunc(trunc.count.no2)

rm(model.no2)

rm(list = ls())
