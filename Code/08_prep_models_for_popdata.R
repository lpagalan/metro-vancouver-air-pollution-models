
# Libraries ---------------------------------------------------------------

library(tidyverse)

# Export Models -----------------------------------------------------------

file.export <- function(pollutant) {

  df <- read_csv(paste0("Data/Cleaned/", pollutant, "_model_trimmed.csv"))

  # Make data wide and rename columns based on model variant

  df.model <- filter(df, Imputed == T) %>%
    rename(YEAR     = Year,
           MONTH    = Month,
           MODELLED = Modelled) %>%
    mutate(MODEL = paste0(toupper(Measure),".",
                          toupper(as.character(Imputed)),".",
                          toupper(substr(Truncation, 1, 3)))) %>%
    select(YEAR, MONTH, PC, MODEL, MODELLED) %>%
    spread(MODEL, MODELLED) %>%
    select(YEAR, MONTH, PC, MEAN.TRUE.NON,
                            MEAN.TRUE.LUR,
                            MEAN.TRUE.TEM,
                            MEDIAN.TRUE.NON,
                            MEDIAN.TRUE.LUR,
                            MEDIAN.TRUE.TEM) %>%
    arrange(YEAR, PC, MONTH)

  write_csv(df.model, paste0("Data/Cleaned/",
                             pollutant,
                             "_model_trimmed_long.csv"))

  print(summary(df.model))

  # Create summary of results for comparison check

  summary <- bind_rows(summary(df.model$MEAN.TRUE.NON),
                       summary(df.model$MEAN.TRUE.LUR),
                       summary(df.model$MEAN.TRUE.TEM),
                       summary(df.model$MEDIAN.TRUE.NON),
                       summary(df.model$MEDIAN.TRUE.LUR),
                       summary(df.model$MEDIAN.TRUE.TEM))
  model.names <- data.frame(Model = c("MEAN.TRUE.NON",
                                      "MEAN.TRUE.LUR",
                                      "MEAN.TRUE.TEM",
                                      "MEDIAN.TRUE.NON",
                                      "MEDIAN.TRUE.LUR",
                                      "MEDIAN.TRUE.TEM"))
  summary <- bind_cols(model.names, summary)
  return(summary)
}

summary.a.pm25 <- file.export("pm25")
summary.a.no   <- file.export("no")
summary.a.no2  <- file.export("no2")

# Check Transformation ----------------------------------------------------

check.transform <- function(pollutant) {

  df <- read_csv(paste0("Data/Cleaned/", pollutant, "_model_trimmed.csv"))

  df <- filter(df, Imputed == T)

  # Split models

  mean.non   <- filter(df, Measure == "Mean",   Truncation == "None")
  mean.lur   <- filter(df, Measure == "Mean",   Truncation == "LUR")
  mean.tem   <- filter(df, Measure == "Mean",   Truncation == "Temporal")
  median.non <- filter(df, Measure == "Median", Truncation == "None")
  median.lur <- filter(df, Measure == "Median", Truncation == "LUR")
  median.tem <- filter(df, Measure == "Median", Truncation == "Temporal")

  rm(df)

  # Create summaries

  summary <- (bind_rows(summary(mean.non$Modelled),
                        summary(mean.lur$Modelled),
                        summary(mean.tem$Modelled),
                        summary(median.non$Modelled),
                        summary(median.lur$Modelled),
                        summary(median.tem$Modelled)))
  model.names <- data.frame(Model = c("MEAN.TRUE.NON",
                                      "MEAN.TRUE.LUR",
                                      "MEAN.TRUE.TEM",
                                      "MEDIAN.TRUE.NON",
                                      "MEDIAN.TRUE.LUR",
                                      "MEDIAN.TRUE.TEM"))
  summary <- bind_cols(model.names, summary)
  return(summary)
}

summary.b.pm25 <- check.transform("pm25")
summary.b.no   <- check.transform("no")
summary.b.no2  <- check.transform("no2")

# Check Export ------------------------------------------------------------

check.export <- function(pollutant) {

  df.model <- read_csv(paste0("Data/Cleaned/",
                              pollutant,
                              "_model_trimmed_long.csv"))

  summary <- bind_rows(summary(df.model$MEAN.TRUE.NON),
                       summary(df.model$MEAN.TRUE.LUR),
                       summary(df.model$MEAN.TRUE.TEM),
                       summary(df.model$MEDIAN.TRUE.NON),
                       summary(df.model$MEDIAN.TRUE.LUR),
                       summary(df.model$MEDIAN.TRUE.TEM))
  model.names <- data.frame(Model = c("MEAN.TRUE.NON",
                                      "MEAN.TRUE.LUR",
                                      "MEAN.TRUE.TEM",
                                      "MEDIAN.TRUE.NON",
                                      "MEDIAN.TRUE.LUR",
                                      "MEDIAN.TRUE.TEM"))
  summary <- bind_cols(model.names, summary)
  return(summary)
}

summary.c.pm25 <- check.export("pm25")
summary.c.no   <- check.export("no")
summary.c.no2  <- check.export("no2")

# Compare Summaries -------------------------------------------------------

summary.a.pm25

all(all.equal(summary.a.pm25, summary.b.pm25) &
    all.equal(summary.b.pm25, summary.c.pm25) &
    all.equal(summary.c.pm25, summary.a.pm25))

summary.a.no

all(all.equal(summary.a.no, summary.b.no) &
    all.equal(summary.b.no, summary.c.no) &
    all.equal(summary.c.no, summary.a.no))

summary.a.no2

all(all.equal(summary.a.no2, summary.b.no2) &
    all.equal(summary.b.no2, summary.c.no2) &
    all.equal(summary.c.no2, summary.a.no2))

rm(list = ls())
