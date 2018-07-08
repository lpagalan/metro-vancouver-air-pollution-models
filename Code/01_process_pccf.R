#' ---
#' Import all PCCF files from 2003-2015, process them, and bind them to one
#' cleaned file containing all active postal codes in BC with their
#' corresponding lat-long.
#' ---

library(readr)
library(dplyr)

#' ---
#' Function loads PCCF files from 2003-2015. Then parses relevant variables
#' and subset data to active postal codes in BC
#'
#' path : char string of file location
#' year : num of source PCCF year
#' ---

pccf.parse <- function(path, year) {

  # Fixed width variables for 2003

  if(year == 2003) {
    fixedWidth <- c(6,3,8,2,9,11,1,2,4,3,70,3,3,3,1,7,2,4,5,4,1,1,1,
                    30,1,1,8,8)
    colIndex   <- c(1,2,5,6,7,8,27,28)
    colNames   <- c("PostalCode",
                    "FSA",
                    "Lat",
                    "Long",
                    "SLI",
                    "PR",
                    "Birth",
                    "Retired")

    # Fixed width variables for 2004-2006

  } else if(year >= 2004 && year <= 2006) {
    fixedWidth <- c(6,3,8,2,9,11,1,2,4,3,70,3,3,3,1,7,2,4,5,4,1,1,1,
                    30,1,1,8,8,5)
    colIndex   <- c(1,2,5,6,7,8,27,28)
    colNames   <- c("PostalCode",
                    "FSA",
                    "Lat",
                    "Long",
                    "SLI",
                    "PR",
                    "Birth",
                    "Retired")

    # Fixed width variables for 2007-2010

  } else if(year >= 2007 && year <= 2010) {
    fixedWidth <- c(6,3,2,4,7,70,3,3,3,1,7,2,4,5,4,1,8,2,1,11,13,1,1,
                    30,1,1,8,8,1,3,1)
    colIndex   <- c(1,2,3,20,21,22,27,28)
    colNames   <- c("PostalCode",
                    "FSA",
                    "PR",
                    "Lat",
                    "Long",
                    "SLI",
                    "Birth",
                    "Retired")

    # Fixed width variables for 2011-2015, except 2012, which has headers

  } else if(year >= 2011 && year <= 2015 && year != 2012) {
    fixedWidth <- c(6,3,2,4,7,70,3,3,3,1,7,2,4,5,4,1,8,2,1,11,13,1,1,
                    30,1,1,8,8,1,3,1,1)
    colIndex   <- c(1,2,3,20,21,22,27,28)
    colNames   <- c("PostalCode",
                    "FSA",
                    "PR",
                    "Lat",
                    "Long",
                    "SLI",
                    "Birth",
                    "Retired")
  }

  # Import PCCF files and parse by fixed width fields

  print(paste("Importing",year,"postal code conversion file"))

  if(year != 2012 ) {
    postalCodes <- read.fwf(path, widths = fixedWidth, fileEncoding = "latin1")
  } else if(year == 2012) {
    postalCodes <- read.csv(path, header = T)
  }

  # Select, rename, and add province and FSA columns

  print(paste("Selecting",year,"postal code variables"))

  if(year != 2012) {
    postalCodes <- postalCodes[ , colIndex]
    names(postalCodes) <- colNames
  } else if(year == 2012) {
    postalCodes <- select(postalCodes, SLI,
                          PostalCode = POSTALCODE,
                          PR         = PROV,
                          Birth      = BIRTH_DATE,
                          Retired    = RET_DATE,
                          Long       = LONGITUDE,
                          Lat        = LATITUDE) %>%
      mutate(PR = 59, FSA = substr(PostalCode, 1,3))
  }

  # Add columns for source year, and string split retired and birth years

  postalCodes$Year    <- year
  postalCodes$Retired <- as.numeric(substr(postalCodes$Retired, 1,4))
  postalCodes$Birth   <- as.numeric(substr(postalCodes$Birth,   1,4))

  # Subset data by province, SLI, active for current year

  print(paste("Filtering",year,"data by province and retired date"))

  postalCodes <- filter(postalCodes, PR == 59 & SLI == 1,
                        Retired == 1900 | Retired == year)

  # Reorder columns, sort by Year and Postal Code, and remove duplicates

  postalCodes <- select(postalCodes,
                        Year,
                        PostalCode,
                        FSA,
                        PR,
                        Lat,
                        Long,
                        Birth,
                        Retired,
                        SLI) %>%
    arrange(Year, PostalCode) %>%
    distinct(Year, PostalCode, .keep_all = T)

  print(paste(year, "PCCF succesfully processed"))
  print("---")

  return(postalCodes)
}

#' ---
#' Build data structure for PCCF parsing function. Create dataframe containing
#' path to PCCF and PCCF year.
#' ---

# Path prefix to original data folder

path.original <- "Data/Original/Postal Code Conversion Files/"

# Vector of file paths to parse

pccf.files <- c(
  paste(path.original,"2003/Data/pccf_59.txt",               sep=""),
  paste(path.original,"2004/Data/pccf59_JUL04_fccp59.txt",   sep=""),
  paste(path.original,"2005/Data/pccf59_oct05.txt",          sep=""),
  paste(path.original,"2006/Data/pccf59_SEP06_fccp59.txt",   sep=""),
  paste(path.original,"2007/Data/pccf59_SEPT07_fccp59.txt",  sep=""),
  paste(path.original,"2008/Data/pccf59_MAR08_fccp59.txt",   sep=""),
  paste(path.original,"2009/Data/pccf59_JUL09_fccp59.txt",   sep=""),
  paste(path.original,"2010/Data/pccf59_OCT10_fccp59.txt",   sep=""),
  paste(path.original,"2011/Data/pccf59_MAY11_fccp59.txt",   sep=""),
  paste(path.original,"2012/Data/CANmep_BC_merged.csv",      sep=""),
  paste(path.original,"2013/Data/pccfNat_JUN13_fccpNat.txt", sep=""),
  paste(path.original,"2014/Data/pccfNat_NOV14_fccpNat.txt", sep=""),
  paste(path.original,"2015/Data/pccfNat_AUG15_fccpNat.txt", sep=""))

# Column bind year and path vectors into a dataframe

pccf.files <- data.frame(cbind(pccf.files, c(2003:2015)))
colnames(pccf.files) <- c("Path", "Year")
pccf.files$Path <- as.character(pccf.files$Path)
pccf.files$Year <- as.numeric(levels(pccf.files$Year))[pccf.files$Year]

rm(path.original)

#' ---
#' Process all PCCF files from 2003-2015 in a loop by passing pccf.files
#' dataframe in pccf.parse function. Append results for each iteration.
#' ---

# Process first year: 2003

pccf.data.bc <- pccf.parse(pccf.files$Path[1], pccf.files$Year[1])

# Process remaining years and bind to pccf.data.bc

for(i in 2:nrow(pccf.files)) {
  pccf.data.bc <- rbind(pccf.data.bc,
                        pccf.parse(pccf.files$Path[i], pccf.files$Year[i]))
}; rm(i)

# Save files

write_csv(pccf.data.bc, "Data/Cleaned/pccf_data_bc.csv")

rm(list = ls())
