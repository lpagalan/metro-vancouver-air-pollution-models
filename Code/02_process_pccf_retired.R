#' ---
#' Import 2015 PCCF and retired postal code files, merge them, process them.
#' 2015 retired file contains all expired postal codes from previous years
#' ---

library(readr)
library(dplyr)

#' ---
#' Function loads 2015 PCCF and retired files. Then parses relevant variables
#' and subset data to all retired postal codes in BC
#'
#' path : char string of file location
#' year : num of source PCCF year
#' ---

pccf.parse.retired <- function(path, year) {

  # Fixed width variables for 2015

  if(year == 2015) {
    fixedWidth <- c(6,3,2,4,7,70,3,3,3,1,7,2,4,5,4,1,8,2,1,11,13,1,1,
                    30,1,1,8,8,1,3,1,1)
    colNames   <- c("PostalCode",   "FSA",
                    "PR",           "CDuid",
                    "CSDuid",       "CSDname",
                    "CSDtype",      "CCScode",
                    "SAC",          "SACtype",
                    "CTname",       "ER",
                    "DPL",          "FED13uid",
                    "POP_CNTR_RA",  "POP_CNTR_RA_type",
                    "DAuid",        "Dissemination block",
                    "Rep_Pt_T ype", "Lat",
                    "Long",         "SLI",
                    "PCtype",       "Comm_Name",
                    "DMT",          "H_DMT",
                    "Birth",        "Retired",
                    "PO",           "QI",
                    "Source",       "POP_CNTR_RA_SIZE_CLASS")
    colIndex   <- c(1,2,3,20,21,22,27,28)
  }

  # Import PCCF files, parse by fixed width fields, and name columns

  print(paste("Importing",year,"postal code conversion file"))
  postalCodes <- read.fwf(path, widths = fixedWidth,
                          fileEncoding = "latin1",
                          fill = T,
                          col.names = colNames)

  # Select, rename, and add province and FSA columns

  print(paste("Selecting",year,"postal code variables"))
  postalCodes <- postalCodes[ , colIndex]

  # Add columns for source year, and string split retired and birth years

  postalCodes$Year    <- year
  postalCodes$Retired <- as.numeric(substr(postalCodes$Retired, 1,4))
  postalCodes$Birth   <- as.numeric(substr(postalCodes$Birth,   1,4))

  # Subset data by province, SLI, and keep only retired postal codes

  print(paste("Filtering",year,"data by province and retired date"))

  postalCodes <- filter(postalCodes, PR == 59 & SLI == 1, Retired >= 2003)

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
    distinct(PostalCode, Retired, .keep_all = T)

  print(paste(year, "Retired PCCF succesfully processed"))
  print("---")

  return(postalCodes)
}

#' ---
#' Build data structure for retired PCCF parsing function. Create dataframe
#' containing path to both files.
#' ---

# Path prefix to original data folder

path.original <- "Data/Original/Postal Code Conversion Files/"

# Vector of file paths to parse

pccf.files <- c(
  paste(path.original,"2015/Data/pccfNat_AUG15_fccpNat.txt", sep=""),
  paste(path.original,"2015/Data/R2010.txt",                 sep=""))

# Column bind year and path vectors into a dataframe

pccf.files <- data.frame(cbind(pccf.files, c(2015, 2015)))
colnames(pccf.files) <- c("Path", "Year")
pccf.files$Path <- as.character(pccf.files$Path)
pccf.files$Year <- as.numeric(levels(pccf.files$Year))[pccf.files$Year]

rm(path.original)

#' ---
#' Process retired PCCF files from 2015 in a loop by passing pccf.files
#' dataframe in pccf.parse function. Append results for each iteration.
#' ---

# Process 2015 PCCF, containing expired postal codes after 2010

pccf.data.ret.bc <- pccf.parse.retired(pccf.files$Path[1], pccf.files$Year[1])

# Process pre-2010 retired file and bind to 2015 retired postal codes

pccf.data.ret.bc <- rbind(pccf.data.ret.bc,
                          pccf.parse.retired(pccf.files$Path[2],
                                             pccf.files$Year[2]))

# Save files

write_csv(pccf.data.ret.bc, "Data/Cleaned/pccf_data_ret_bc.csv")

rm(list = ls())
