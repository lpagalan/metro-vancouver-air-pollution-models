#' ---
#' Load temporal air monitoring data, and calculate monthly-spatial and
#' baseline-spatial averages to create a spatially homogenous temporal factor
#' used to model LUR values across time. Temporal factor is based on air
#' monitoring station data, which has missing at random data and will first
#' require multiple imputation. Baseline-spatial average is based on
#' LUR year.
#' ---

library(dplyr)
library(readr)
library(reshape2)
library(mice)
library(lattice)
library(VIM)

ptm <- proc.time()

#' ---
#' Load data and analyze missing data in preparation multiple imputation
#' ---

# Load Metro Vancouver air monitoring data

load.data <- function(filename) {
  filepath <- "Data/Original/Air Quality Monitoring Network/"
  df <- read.csv(paste0(filepath, filename),
                 header           = T,
                 stringsAsFactors = F,
                 na.strings       = c("<Samp", "InVld", "NoData", "Down",
                                      "Purge", "RS232", "FailPwr"))
  colnames(df)[1] <- "Time"
  df <- mutate(df,
               Year  = as.numeric(substr(Time, 7, 10)),
               Month = as.numeric(substr(Time, 4, 5 )),
               Day   = as.numeric(substr(Time, 1, 2 ))) %>%
    select(-Time)
  return(df)
}

pm25.A <- load.data("PM25_A.csv")
pm25.B <- load.data("PM25_B.csv")

temporal.pm25 <- rbind(pm25.A, pm25.B)
temporal.no   <- load.data("NO.csv")
temporal.no2  <- load.data("NO2.csv")

rm(pm25.A, pm25.B, load.data)

# Make all columns numeric

make.cols.numeric <- function(df) {
  for(i in 1:length(df)) { df[ , i] <- as.numeric(df[ , i]) }
  return(df)
}

temporal.pm25 <- make.cols.numeric(temporal.pm25)
temporal.no   <- make.cols.numeric(temporal.no)
temporal.no2  <- make.cols.numeric(temporal.no2)

rm(make.cols.numeric)

# Reshape date from wide to long

reshape.long <- function(df) {
  df <- melt(df, id.vars = c("Year", "Month", "Day"),
             na.rm = F,
             variable.name = "Monitor",
             value.name    = "Value")
  return(df)
}

long.pm25 <- reshape.long(temporal.pm25)
long.no   <- reshape.long(temporal.no)
long.no2  <- reshape.long(temporal.no2)

# Calcuate number of missing values

count.missing <- function(df) {

  # Filter all NAs and count observations

  df.missing <- filter(df, is.na(df["Value"])) %>%
    group_by(Year, Month, Monitor) %>%
    summarise(Missing = n())

  # Filter all observed and count observations

  df.observed <- filter(df, !is.na(df["Value"])) %>%
    group_by(Year, Month, Monitor) %>%
    summarise(Observed = n())

  df <- merge(df.missing, df.observed,
              by = c("Year", "Month", "Monitor"), all = T)

  df[is.na(df)] <- 0

  # Caulcate percentage of missing observations

  df <- mutate(df, Total = Missing + Observed,
               Pct.Missing = signif(Missing / Total * 100, 0))

  df[df == 0] <- NA

  return(df)
}

missing.pm25 <- count.missing(long.pm25)
missing.no   <- count.missing(long.no)
missing.no2  <- count.missing(long.no2)

# Summary of missing data

Pollutant <- c("PM25", "NO", "NO2")

Missing <- c(sum(missing.pm25$Missing, na.rm = T),
             sum(missing.no$Missing,   na.rm = T),
             sum(missing.no2$Missing,  na.rm = T))

Observed <- c(sum(missing.pm25$Observed, na.rm = T),
              sum(missing.no$Observed,   na.rm = T),
              sum(missing.no2$Observed,  na.rm = T))

Total <- c(sum(missing.pm25$Total,   na.rm = T),
              sum(missing.no$Total,  na.rm = T),
              sum(missing.no2$Total, na.rm = T))

missing.data <- data.frame(Pollutant, Missing, Observed, Total) %>%
  mutate(Percent = Missing / Total * 100)

rm(Pollutant, Missing, Observed, Total, missing.data)

# Reshape data

reshape.wide <- function(df, value.var) {
  df <- dcast(df, Year + Month ~ Monitor, value.var = value.var)
  return(df)
}

missing.wide.pm25 <- reshape.wide(missing.pm25, "Missing")
missing.wide.no   <- reshape.wide(missing.no,   "Missing")
missing.wide.no2  <- reshape.wide(missing.no2,  "Missing")

# Export summary of missing values to be analyzed in Excel

# write_csv(missing.wide.pm25, path="Results/pm25_missing_summary.csv", na="")
# write_csv(missing.wide.no,   path="Results/no_missing_summary.csv",   na="")
# write_csv(missing.wide.no2,  path="Results/no2_missing_summary.csv",  na="")

rm(list = c(ls(pattern = "^long"), ls(pattern = "^missing"),
            "count.missing", "reshape.wide"))

# Plot missing data

plot.missing.data <- function(df) {
  plot <- aggr(select(df, -(Year:Day)),
               col       = "red",
               numbers   = TRUE,
               labels    = names(select(df, -(Year:Day))),
               cex.axis  = 0.8,
               gap       = 3,
               ylab      = c("Histogram of Missing Data",
                             "Patterns"),
               digits    = 2)
  return(plot)
}

plot.missing.pm25 <- plot.missing.data(temporal.pm25)
plot.missing.no   <- plot.missing.data(temporal.no)
plot.missing.no2  <- plot.missing.data(temporal.no2)

rm(list = ls(pattern = "^plot.missing"))

#' ---
#' Impute missing data using single imputation
#' ---

impute.na <- function(df, num.imp) {
  exclude.var = c("Year", "Month", "Day")
  imp.result <- mice(df,
                     m    = num.imp,
                     seed = 170703,
                     pred = quickpred(df, exclude = exclude.var))
  return(imp.result)
}

imp.pm25 <- impute.na(temporal.pm25, 1)
imp.no   <- impute.na(temporal.no,   1)
imp.no2  <- impute.na(temporal.no2,  1)

summary(imp.pm25)
summary(imp.no)
summary(imp.no2)

# Make density plot observed and imputed values: red-imputed, blue-observed

pdf("Figures/imputation_density_plot.pdf", width = 11, height = 8.5)

densityplot(imp.pm25,
              grid        = T,
              thicker     = 1,
              plot.points = T,
              xlab        = "PM2.5")

  densityplot(imp.no,
              grid        = T,
              thicker     = 1,
              plot.points = T,
              xlab        = "NO")

  densityplot(imp.no2,
              grid        = T,
              thicker     = 1,
              plot.points = T,
              xlab        = "NO2")
dev.off()

# Fill in original NA data with imputation results

temporal.imp.pm25 <- complete(imp.pm25, 1)
temporal.imp.no   <- complete(imp.no,   1)
temporal.imp.no2  <- complete(imp.no2,  1)

# Verify that no NAs remain

verify.na <- function(df) {
  results <- sapply(df, function(x) sum(is.na(x))) %>%
    data.frame()
  colnames(results) <- "NA.Count"
  return(results)
}

na.pm25 <- verify.na(temporal.imp.pm25)
na.no   <- verify.na(temporal.imp.no)
na.no2  <- verify.na(temporal.imp.no2)

rm(list = c(ls(pattern = "^na"), ls(pattern = "^imp"), "verify.na"))

#' ---
#' Develop temporal factors for LUR adjustements over time
#' ---

# Convert data to long, rbind, and save

temporal.pm25 <- reshape.long(temporal.pm25) %>% mutate(Imputed = F)
temporal.no   <- reshape.long(temporal.no)   %>% mutate(Imputed = F)
temporal.no2  <- reshape.long(temporal.no2)  %>% mutate(Imputed = F)

temporal.imp.pm25 <- reshape.long(temporal.imp.pm25) %>% mutate(Imputed = T)
temporal.imp.no   <- reshape.long(temporal.imp.no)   %>% mutate(Imputed = T)
temporal.imp.no2  <- reshape.long(temporal.imp.no2)  %>% mutate(Imputed = T)

daily.pm25 <- rbind(temporal.pm25, temporal.imp.pm25)
daily.no   <- rbind(temporal.no,   temporal.imp.no)
daily.no2  <- rbind(temporal.no2,  temporal.imp.no2)

write_csv(daily.pm25, "Data/Cleaned/pm25_daily.csv")
write_csv(daily.no,   "Data/Cleaned/no_daily.csv")
write_csv(daily.no2,  "Data/Cleaned/no2_daily.csv")

rm(list = c(ls(pattern = "^temporal"), "reshape.long"))

#' ---
#' Calculate yearly averages and medians with and without imputation
#' ---

calc.mean.year <- function(df, is.imputed) {
  if(is.imputed) { df <- filter(df, Imputed == T)
  } else { df <- filter(df, Imputed == F) }
  mean <- group_by(df, Year) %>%
    summarise(Value = mean(Value, na.rm = T)) %>%
    mutate(Measure = "Mean", Imputed = is.imputed)
  return(mean)
}

calc.median.year <- function(df, is.imputed) {
  if(is.imputed) { df <- filter(df, Imputed == T)
  } else { df <- filter(df, Imputed == F) }
  median <- group_by(df, Year) %>%
    summarise(Value = median(Value, na.rm = T)) %>%
    mutate(Measure = "Median", Imputed = is.imputed)
  return(median)
}

# Calculate yearly averages with no imputation

yearly.mean.pm25 <- calc.mean.year(daily.pm25, F)
yearly.mean.no   <- calc.mean.year(daily.no,   F)
yearly.mean.no2  <- calc.mean.year(daily.no2,  F)

# Calculate yearly averages with imputation

yearly.mean.imp.pm25 <- calc.mean.year(daily.pm25, T)
yearly.mean.imp.no   <- calc.mean.year(daily.no,   T)
yearly.mean.imp.no2  <- calc.mean.year(daily.no2,  T)

# Calculate yearly medians with no imputation

yearly.median.pm25 <- calc.median.year(daily.pm25, F)
yearly.median.no   <- calc.median.year(daily.no,   F)
yearly.median.no2  <- calc.median.year(daily.no2,  F)

# Calculate yearly medians with imputation

yearly.median.imp.pm25 <- calc.median.year(daily.pm25, T)
yearly.median.imp.no   <- calc.median.year(daily.no,   T)
yearly.median.imp.no2  <- calc.median.year(daily.no2,  T)

# Bind data frames into one

yearly.pm25 <- bind_rows(yearly.mean.pm25,
                         yearly.mean.imp.pm25,
                         yearly.median.pm25,
                         yearly.median.imp.pm25)

yearly.no <- bind_rows(yearly.mean.no,
                       yearly.mean.imp.no,
                       yearly.median.no,
                       yearly.median.imp.no)

yearly.no2 <- bind_rows(yearly.mean.no2,
                        yearly.mean.imp.no2,
                        yearly.median.no2,
                        yearly.median.imp.no2)

rm(list = c(ls(pattern = "^yearly.mean"), ls(pattern = "^yearly.median")))

#' ---
#' Calculate monthly averages and medians with and without imputation
#' ---

calc.mean.month <- function(df, is.imputed) {
  if(is.imputed) { df <- filter(df, Imputed == T)
  } else { df <- filter(df, Imputed == F) }
  mean <- group_by(df, Year, Month) %>%
    summarise(Value = mean(Value, na.rm = T)) %>%
    mutate(Measure = "Mean", Imputed = is.imputed)
  return(mean)
}

calc.median.month <- function(df, is.imputed) {
  if(is.imputed) { df <- filter(df, Imputed == T)
  } else { df <- filter(df, Imputed == F) }
  median <- group_by(df, Year, Month) %>%
    summarise(Value = median(Value, na.rm = T)) %>%
    mutate(Measure = "Median", Imputed = is.imputed)
  return(median)
}

# Calculate monthly averages with no imputation

monthly.mean.pm25 <- calc.mean.month(daily.pm25, F)
monthly.mean.no   <- calc.mean.month(daily.no,   F)
monthly.mean.no2  <- calc.mean.month(daily.no2,  F)

# Calculate monthly averages with imputation

monthly.mean.imp.pm25 <- calc.mean.month(daily.pm25, T)
monthly.mean.imp.no   <- calc.mean.month(daily.no,   T)
monthly.mean.imp.no2  <- calc.mean.month(daily.no2,  T)

# Calculate monthly medians with no imputation

monthly.median.pm25 <- calc.median.month(daily.pm25, F)
monthly.median.no   <- calc.median.month(daily.no,   F)
monthly.median.no2  <- calc.median.month(daily.no2,  F)

# Calculate monthly medians with imputation

monthly.median.imp.pm25 <- calc.median.month(daily.pm25, T)
monthly.median.imp.no   <- calc.median.month(daily.no,   T)
monthly.median.imp.no2  <- calc.median.month(daily.no2,  T)

# Bind data frames into one

monthly.pm25 <- bind_rows(monthly.mean.pm25,
                          monthly.mean.imp.pm25,
                          monthly.median.pm25,
                          monthly.median.imp.pm25)

monthly.no <- bind_rows(monthly.mean.no,
                        monthly.mean.imp.no,
                        monthly.median.no,
                        monthly.median.imp.no)

monthly.no2 <- bind_rows(monthly.mean.no2,
                         monthly.mean.imp.no2,
                         monthly.median.no2,
                         monthly.median.imp.no2)

rm(list = c(ls(pattern = "^monthly.mean"), ls(pattern = "^monthly.median")))

#' ---
#' Calc monthly minimum and max values
#' ---

calc.min.month <- function(df, is.imputed) {
  df <- na.omit(df, cols = "Value") %>%
    filter(Imputed == is.imputed)
  min <- group_by(df, Year, Month, Monitor) %>%
    summarise(Value = min(Value, na.rm = T)) %>%
    mutate(Measure = "Min", Imputed = is.imputed)
  return(min)
}

calc.max.month <- function(df, is.imputed) {
  df <- na.omit(df, cols = "Value") %>%
    filter(Imputed == is.imputed)
  max <- group_by(df, Year, Month, Monitor) %>%
    summarise(Value = max(Value, na.rm = T)) %>%
    mutate(Measure = "Max", Imputed = is.imputed)
  return(max)
}

# Get observed monthly min with no imputation

monthly.min.pm25 <- calc.min.month(daily.pm25, F)
monthly.min.no   <- calc.min.month(daily.no,   F)
monthly.min.no2  <- calc.min.month(daily.no2,  F)

monthly.min.imp.pm25 <- calc.min.month(daily.pm25, T)
monthly.min.imp.no   <- calc.min.month(daily.no,   T)
monthly.min.imp.no2  <- calc.min.month(daily.no2,  T)

# Get observed monthly max with no imputation

monthly.max.pm25 <- calc.max.month(daily.pm25, F)
monthly.max.no   <- calc.max.month(daily.no,   F)
monthly.max.no2  <- calc.max.month(daily.no2,  F)

monthly.max.imp.pm25 <- calc.max.month(daily.pm25, T)
monthly.max.imp.no   <- calc.max.month(daily.no,   T)
monthly.max.imp.no2  <- calc.max.month(daily.no2,  T)

# Bind data frames into one

monthly.bounds.pm25 <- bind_rows(list(monthly.min.pm25,
                                      monthly.max.pm25,
                                      monthly.min.imp.pm25,
                                      monthly.max.imp.pm25))

monthly.bounds.no <- bind_rows(list(monthly.min.no,
                                    monthly.max.no,
                                    monthly.min.imp.no,
                                    monthly.max.imp.no))

monthly.bounds.no2 <- bind_rows(list(monthly.min.no2,
                                     monthly.max.no2,
                                     monthly.min.imp.no2,
                                     monthly.max.imp.no2))

rm(list = c(ls(pattern = "^monthly.min"), ls(pattern = "^monthly.max")))

# Save min and max bounds

write_csv(monthly.bounds.pm25, "Data/Cleaned/pm25_bounds.csv")
write_csv(monthly.bounds.no,   "Data/Cleaned/no_bounds.csv")
write_csv(monthly.bounds.no2,  "Data/Cleaned/no2_bounds.csv")

rm(list = ls(pattern = "^monthly.bounds"))

#' ---
#' Calc temporal factor based on monthly and baseline avg
#' ---

# Establish baseline years based on LUR creation dates

baseline.pm25 <- filter(yearly.pm25, Year == 2003) %>%
  rename(Baseline = Value, LUR = Year)

baseline.no <- filter(yearly.no, Year == 2003 | Year == 2010) %>%
  rename(Baseline = Value, LUR = Year)

baseline.no2 <- filter(yearly.no2, Year == 2003 | Year == 2010) %>%
  rename(Baseline = Value, LUR = Year)

# Prepare temporal factor data frame with baseline year key

temp.factor.pm25 <- mutate(monthly.pm25,
                           LUR = 2003)

temp.factor.no   <- mutate(monthly.no,
                           LUR = ifelse(Year < 2010, 2003, 2010))

temp.factor.no2  <- mutate(monthly.no2,
                           LUR = ifelse(Year < 2010, 2003, 2010))

# Merge monthly average dataframe to yearly baseline dataframe

merge.factor.baseline <- function(df.x, df.y) {
  df <- merge(df.x, df.y, all.x = T) %>%
    select(LUR, Year, Month, Measure, Imputed, Value, Baseline)
  return(df)
}

temp.factor.pm25 <- merge.factor.baseline(temp.factor.pm25, baseline.pm25)
temp.factor.no   <- merge.factor.baseline(temp.factor.no,   baseline.no)
temp.factor.no2  <- merge.factor.baseline(temp.factor.no2,  baseline.no2)

# Calculate temporal factor

calc.temp.factor <- function(df) {
  return(mutate(df, Factor = Value / Baseline))
}

temp.factor.pm25 <- calc.temp.factor(temp.factor.pm25)
temp.factor.no   <- calc.temp.factor(temp.factor.no)
temp.factor.no2  <- calc.temp.factor(temp.factor.no2)

rm(list = c(ls(pattern = "^calc."), ls(pattern = "^merge.")))

# Save output

write_csv(temp.factor.pm25, "Data/Cleaned/pm25_temporal_factor.csv")
write_csv(temp.factor.no,   "Data/Cleaned/no_temporal_factor.csv")
write_csv(temp.factor.no2,  "Data/Cleaned/no2_temporal_factor.csv")

proc.time() - ptm

rm(list = ls())
