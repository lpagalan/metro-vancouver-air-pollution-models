#' ---
#' Import and geocode cleaned postal code data for Metro Vancouver, overlay it
#' on land use regression layer, and get pollutant value at each postal code.
#' ---

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)
library(ggplot2)

# Load Metro Vancouver postal codes

pccf.data.van <- read_csv("Data/Cleaned/pccf_data_van.csv")

# Load LUR as rasters

lur.2003.pm25 <- raster("Data/Original/LUR Surfaces/PM25_2003/pm25_2003.tif")
lur.2003.no   <- raster("Data/Original/LUR Surfaces/NO_2003/no_2003.tif")
lur.2010.no   <- raster("Data/Original/LUR Surfaces/NO_2010/no_2010.tif")
lur.2003.no2  <- raster("Data/Original/LUR Surfaces/NO2_2003/no2_2003.tif")
lur.2010.no2  <- raster("Data/Original/LUR Surfaces/NO2_2010/no2_2010.tif")

extent.van <- readOGR("Data/Original/Metro Vancouver Open Data",
                      "AdminBoundaryDissolved")

#' ---
#' Analyze LUR air pollution values. Extract values from LUR and plot
#' their distribution. Look for zero and NA values.
#' ---

# Extract cell values from rasters

val.lur.2003.pm25 <- data.frame(Value = getValues(lur.2003.pm25))
val.lur.2003.no   <- data.frame(Value = getValues(lur.2003.no))
val.lur.2010.no   <- data.frame(Value = getValues(lur.2010.no))
val.lur.2003.no2  <- data.frame(Value = getValues(lur.2003.no2))
val.lur.2010.no2  <- data.frame(Value = getValues(lur.2010.no2))

# Trim precision

trim.precision <- function(df) {
  var <- "Value"
  digits <- 4
  df[ , var] <- round(df[ , var], digits)
  return(df)
}

val.lur.2003.pm25 <- trim.precision(val.lur.2003.pm25)
val.lur.2003.no   <- trim.precision(val.lur.2003.no)
val.lur.2010.no   <- trim.precision(val.lur.2010.no)
val.lur.2003.no2  <- trim.precision(val.lur.2003.no2)
val.lur.2010.no2  <- trim.precision(val.lur.2010.no2)

# Summarise LUR values

summary(val.lur.2003.pm25)
summary(val.lur.2003.no)
summary(val.lur.2010.no)
summary(val.lur.2003.no2)
summary(val.lur.2010.no2)

# Count zeros

filter(val.lur.2003.pm25, Value == 0) %>% summarise(N.Zeros = n())
filter(val.lur.2003.no,   Value == 0) %>% summarise(N.Zeros = n())
filter(val.lur.2010.no,   Value == 0) %>% summarise(N.Zeros = n())
filter(val.lur.2003.no2,  Value == 0) %>% summarise(N.Zeros = n())
filter(val.lur.2010.no2,  Value == 0) %>% summarise(N.Zeros = n())

# Look at smallest values after filtering out for zero

filter(val.lur.2003.pm25, Value > 0) %>% arrange(Value) %>% head()
filter(val.lur.2003.no,   Value > 0) %>% arrange(Value) %>% head()
filter(val.lur.2010.no,   Value > 0) %>% arrange(Value) %>% head()
filter(val.lur.2003.no2,  Value > 0) %>% arrange(Value) %>% head()
filter(val.lur.2010.no2,  Value > 0) %>% arrange(Value) %>% head()

# Plot density distribution of pollutants

plot.lur <- function(val.lur, pollutant, year, note, binwidth) {
  n <- base::nrow(val.lur)
  plot <- ggplot(val.lur, aes(Value)) +
    geom_histogram(binwidth = binwidth) +
    xlab(paste0(pollutant, " (", year, "). ",
                "N-Cells = ", prettyNum(n, big.mark = ","),". ", note)) +
    ylab("Count")
  return(plot)
}

note <- ""

plot.lur(val.lur.2003.pm25, "PM2.5","2003", note, 1)
plot.lur(val.lur.2003.no,   "NO",   "2003", note, 10)
plot.lur(val.lur.2010.no,   "NO",   "2010", note, 10)
plot.lur(val.lur.2003.no2,  "NO2",  "2003", note, 1)
plot.lur(val.lur.2010.no2,  "NO2",  "2010", note, 1)

# For 2010 NO2, change NA to zero for plotting purposes

val.lur.2010.no2.noNA <- val.lur.2010.no2
val.lur.2010.no2.noNA$Value[is.na(val.lur.2010.no2.noNA$Value)] <- 0

note <- "NAs set as zero"

plot.lur(val.lur.2010.no2.noNA, "NO2", "2010", note, 1)

# Plot with zeros and NA removed

note <- "Zeros & NAs Excluded."

val.lur.2003.pm25.trim <- val.lur.2003.pm25      %>% filter(Value > 0)
val.lur.2003.no.trim   <- val.lur.2003.no        %>% filter(Value > 0)
val.lur.2010.no.trim   <- val.lur.2010.no        %>% filter(Value > 0)
val.lur.2003.no2.trim  <- val.lur.2003.no2       %>% filter(Value > 0)
val.lur.2010.no2.trim  <- val.lur.2010.no2.noNA  %>% filter(Value > 0)

plot.lur(val.lur.2003.pm25.trim, "PM2.5","2003", note, 1)
plot.lur(val.lur.2003.no.trim,   "NO",   "2003", note, 10)
plot.lur(val.lur.2010.no.trim,   "NO",   "2010", note, 10)
plot.lur(val.lur.2003.no2.trim,  "NO2",  "2003", note, 1)
plot.lur(val.lur.2010.no2.trim,  "NO2",  "2010", note, 1)

rm(list = c(ls(pattern='^val'), "note", "plot.lur"))

#' ---
#' Function performs GIS functions to calculate pollutant values at a given
#' postal code. It transforms list of PCs into geographic object,
#' matches projection between PCs and LUR, overlays the PCs unto LUR, and then
#' selects LUR value at given PC location.
#'
#' pccf.data.van : data frame with postal codes
#' lur           : raster layer
#' map.label     : string
#' ---

get.pollutant.at.cord <- function(df.pc, lur, map.label) {

  # Give each observation a unique ID, used later to check merged data frames

  df.pc <- mutate(df.pc, ID = as.numeric(rownames(df.pc)))

  # Duplicate original GEOGCS NAD83 lat long vars to preserve post-geocoding

  df.pc.spatial <- mutate(df.pc, Lat_GCSNAD83 = Lat, Long_GCSNAD83 = Long) %>%
    mutate(Lat = Lat_UTM10, Long = Long_UTM10)

  # Transform PCs to spatial object using UTM1O coordinate system for LUR

  coordinates(df.pc.spatial) <- c("Long_UTM10", "Lat_UTM10")
  proj4string(df.pc.spatial) <- lur@crs

  # Map postal codes on top of LUR to verify transformation

  plot(lur, main = map.label)
  plot(extent.van, add = T, border = "red")
  plot(df.pc.spatial, add = T, pch = 1, cex = 0.1, col = "blue")
  legend("bottomleft",
         legend = c("Active Postal Codes", "Metro Vancouver Boundary"),
         col = c("blue", "red"),
         lty = c(NA, 1),
         pch = c(16, NA),
         bty = "n")

  # Extract LUR value at postal code coordinates

  pc.lur.values <- data.frame(df.pc.spatial$Year,
                              df.pc.spatial$PostalCode,
                              coordinates(df.pc.spatial),
                              df.pc.spatial$ID,
                              extract(lur, df.pc.spatial))

  names(pc.lur.values) <- c("Year",
                            "PostalCode",
                            "Long_UTM10",
                            "Lat_UTM10",
                            "ID",
                            "Value")

  # Merge postal code and lur value data frames based

  df.pollutant <- merge(df.pc,
                        pc.lur.values,
                        by = c("Year",
                               "PostalCode",
                               "Long_UTM10",
                               "Lat_UTM10",
                               "ID"),
                        all = T)

  # Clean variables of interest, rename columns, and sort data frame

  df.pollutant <- dplyr::select_(df.pollutant,
                                "Year",
                                "PC" = "PostalCode",
                                "FSA",
                                "PR",
                                "Lat_NAD83" = "Lat",
                                "Lon_NAD83" = "Long",
                                "Lat_UTM10",
                                "Lon_UTM10" = "Long_UTM10",
                                "Birth",
                                "Retired",
                                "SLI",
                                "Value") %>%
    arrange(Year, PC)

  return(df.pollutant)
}

# Get LUR value at postal code coordinate

png("Figures/lur_postalcode_map.png", width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 3),
    mar = c(4.1, 4.1, 4.1, 6.1))

lur.pc.val.pm25 <- get.pollutant.at.cord(
  pccf.data.van,
  lur.2003.pm25,
  "PM2.5 (2003)")

lur.pc.val.no.2003 <- get.pollutant.at.cord(
  filter(pccf.data.van, Year < 2010),
  lur.2003.no,
  "NO (2003)")

lur.pc.val.no.2010 <- get.pollutant.at.cord(
  filter(pccf.data.van, Year >= 2010),
  lur.2010.no,
  "NO (2010)")

lur.pc.val.no2.2003 <- get.pollutant.at.cord(
  filter(pccf.data.van, Year < 2010),
  lur.2003.no2,
  "NO2 (2003)")

lur.pc.val.no2.2010 <- get.pollutant.at.cord(
  filter(pccf.data.van, Year >= 2010),
  lur.2010.no2,
  "NO2 (2010)")

dev.off()

# Merge NO and NO2 data frames, respectively

lur.pc.val.no  <- rbind(lur.pc.val.no.2003,  lur.pc.val.no.2010)
lur.pc.val.no2 <- rbind(lur.pc.val.no2.2003, lur.pc.val.no2.2010)

rm(lur.pc.val.no.2003, lur.pc.val.no2.2003,
   lur.pc.val.no.2010, lur.pc.val.no2.2010)

# Trim observations

lur.pc.val.pm25 <- filter(lur.pc.val.pm25, Year >= 2003 & Year <= 2014)
lur.pc.val.no   <- filter(lur.pc.val.no,   Year >= 2003 & Year <= 2014)
lur.pc.val.no2  <- filter(lur.pc.val.no2,  Year >= 2003 & Year <= 2014)

# Check values

summary(lur.pc.val.pm25)
summary(lur.pc.val.no)
summary(lur.pc.val.no2)

filter(lur.pc.val.pm25, Value == 0) %>% summarise(N.Zeros = n())
filter(lur.pc.val.no,   Value == 0) %>% summarise(N.Zeros = n())
filter(lur.pc.val.no2,  Value == 0) %>% summarise(N.Zeros = n())

# Save files

save.files <- function(df, filename) {
  write_csv(df,   paste0("Data/Cleaned/", filename, ".csv"))
}

save.files(lur.pc.val.pm25, "pm25_lur_val_at_pc")
save.files(lur.pc.val.no,   "no_lur_val_at_pc")
save.files(lur.pc.val.no2,  "no2_lur_val_at_pc")

# Plot density distribution of pollutant values at pc

lur.pc.val.pm25$Year <- factor(lur.pc.val.pm25$Year)
lur.pc.val.no$Year   <- factor(lur.pc.val.no$Year)
lur.pc.val.no2$Year  <- factor(lur.pc.val.no2$Year)

ggplot(lur.pc.val.pm25, aes(x = Value, colour = Year)) + geom_density()
ggplot(lur.pc.val.no,   aes(x = Value, colour = Year)) + geom_density()
ggplot(lur.pc.val.no2,  aes(x = Value, colour = Year)) + geom_density()

# Clear global environment

rm(list = ls())
