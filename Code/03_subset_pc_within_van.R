#' ---
#' Import cleaned postal code data for all of BC, and select only
#' postal codes within Metro Vancouver geographic extent.
#' ---

library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)

# Load BC postal code data

pccf.data.bc <- read_csv("Data/Cleaned/pccf_data_bc.csv")
pccf.data.ret.bc <-  read_csv("Data/Cleaned/pccf_data_ret_bc.csv")

# Load shape file of Metro Vancouver boundary to serve as geographic extent

extent.van <- readOGR("Data/Original/Metro Vancouver Open Data",
                      "AdminBoundaryDissolved")

# Load shape file of BC boundary to visually test geographic transformations

extent.bc  <- readOGR("Data/Original/Stats Can Provincial Boundaries",
                      "bc_boundary")

# Duplicate original GEOGCS NAD83 lat long vars to preserve post-geocoding

pccf.data.bc <- mutate(pccf.data.bc,
                       Long_GCSNAD83 = Long,
                       Lat_GCSNAD83  = Lat)

pccf.data.ret.bc <- mutate(pccf.data.ret.bc,
                           Long_GCSNAD83 = Long,
                           Lat_GCSNAD83  = Lat)

# Turn BC postal code data into spatial point object using lat long variables

coordinates(pccf.data.bc)     <- c("Long", "Lat")
coordinates(pccf.data.ret.bc) <- c("Long", "Lat")

# Add original NAD83 geographic coordinate system to spatial objects

proj4string(pccf.data.bc)     <- CRS("+init=EPSG:4269")
proj4string(pccf.data.ret.bc) <- CRS("+init=EPSG:4269")

# Transform geographic coordinate system to match extent projection

pccf.data.bc     <- spTransform(pccf.data.bc,     proj4string(extent.van))
pccf.data.ret.bc <- spTransform(pccf.data.ret.bc, proj4string(extent.van))

# Map BC extent and postal codes to verify correct transformation

plot(extent.bc)
plot(pccf.data.bc,     add = T, pch = 1, cex = 0.25, col = "red")
plot(pccf.data.ret.bc, add = T, pch = 1, cex = 0.25, col = "blue")

# Subset BC postal codes within Metro Vancouver extent

pccf.data.van     <- pccf.data.bc[extent.van, ]
pccf.data.ret.van <- pccf.data.ret.bc[extent.van, ]

# Map BC extent and Van postal codes to verify clipping at provincial scale

plot(extent.bc)
plot(pccf.data.van,     add = T, pch = 1, cex = 0.25, col = "red")
plot(pccf.data.ret.van, add = T, pch = 1, cex = 0.25, col = "blue")

# Map Van extent and Van postal codes to verify clipping at regional scale

plot(extent.van)
plot(pccf.data.van,     add = T, pch = 1, cex = 0.25, col = "red")
plot(pccf.data.ret.van, add = T, pch = 1, cex = 0.25, col = "blue")

# Convert spatial objects back to data frames

pccf.data.van     <- as.data.frame(pccf.data.van)
pccf.data.ret.van <- as.data.frame(pccf.data.ret.van)

# Reorder columns and sort rows

pccf.data.van <- select(pccf.data.van,
                        Year,
                        PostalCode,
                        FSA,
                        PR,
                        Lat_GCSNAD83,
                        Long_GCSNAD83,
                        Lat_UTM10   = Lat,
                        Long_UTM10  = Long,
                        Birth,
                        Retired,
                        SLI) %>%
  rename(Lat  = Lat_GCSNAD83,
         Long = Long_GCSNAD83) %>%
  arrange(Year, PostalCode)

pccf.data.ret.van <- select(pccf.data.ret.van,
                            Year,
                            PostalCode,
                            FSA,
                            PR,
                            Lat_GCSNAD83,
                            Long_GCSNAD83,
                            Lat_UTM10   = Lat,
                            Long_UTM10  = Long,
                            Birth,
                            Retired,
                            SLI) %>%
  rename(Lat  = Lat_GCSNAD83,
         Long = Long_GCSNAD83) %>%
  arrange(Year, PostalCode)

# Save spatial subsets

write_csv(pccf.data.van,     "Data/Cleaned/pccf_data_van.csv")
write_csv(pccf.data.ret.van, "Data/Cleaned/pccf_data_ret_van.csv")

rm(list = ls())
