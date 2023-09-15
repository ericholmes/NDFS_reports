## Database update script
library(tidyverse)
library(WDLimportr)

source("scripts/NDFS2023_Useful_functions.R")

site_name <- c("BL5 -Below Toe Drain", "Toe Drain@ I-80", "Liberty at S End", 
               "Toe Drain YB LISBON", "Ridge Cut Slough", "Toe Drain at Rd. 22",
               "Rominger Bridge", "	RVB - Rio Vista", "RYI - Cache Slough",
               "Toe Drain at STTD", "Sacramento River @ Sherwood Harbor")

site_code <- c("BL5", "I80", "LIB", 
               "LIS", "RCS", "RD22", 
               "RMB", "RVB", "RYI", 
               "STTD", "SHR")

discrete_number <- c("B9D81651399", "A0D83441350", "B9D81450411", 
                     "B9D82851352", NA, "A0D84061386",
                     "A0C85051515", "B9D80960412", "B9D81281402",
                     "A0D82120386", "A0200000")

continuous_number <- c("B9D81651399", "A0D83441350", NA, 
                       "B9156000", NA, "A0D84061386",
                       "A0C85051515", "B91212", NA,
                       "A0D82120386", NA)

downstream <- c(TRUE, FALSE, TRUE, 
                FALSE, FALSE, FALSE, 
                FALSE, TRUE, TRUE, 
                FALSE, FALSE)

NDFS_site_df <- data.frame(site_name, site_code, discrete_number, continuous_number, downstream)

# Download continuous WDL water quality -----------------------------------

wqcont <- data.frame()

for(StationNumber in na.omit(NDFS_site_df$continuous_number)){
  for(Parameter in c("Water_Temperature", 
                     "Electrical_Conductivity_at_25C",
                     "Dissolved_Oxygen",
                     "Dissolved_Oxygen_Percentage",
                     "pH",
                     "Turbidity",
                     "Chlorophyll",
                     "Fluorescent_Dissolved_Organic_Matter")){
    print(paste("Station:", StationNumber, "Parameter:", Parameter))
    try(temp <- read.csv(paste0("https://wdlstorageaccount.blob.core.windows.net/continuous/",
                            StationNumber, "/por/", StationNumber, "_", Parameter, "_Raw.csv"), skip = 2))
    temp <- temp[, 1:2]
    colnames(temp) <- c("Datetime", "Value")
    temp$Station <- StationNumber
    temp$Variable <- Parameter
    temp$Datetime <- as.POSIXct(temp$Datetime, format = "%m/%d/%Y %H:%M:%S")
    wqcont <- rbind(wqcont, temp[temp$Datetime >= as.POSIXct("2023-01-01 00:00") &
                                   temp$Datetime <= as.POSIXct("2023-12-31 23:59"),])
    
  }
}

wqcont <- merge(wqcont, NDFS_site_df[, c("site_code", "continuous_number")], 
                 by.x = "Station", by.y = "continuous_number", all.x = T)

save(wqcont, file = "data/NDFS2023_wqcont.Rdata")

# Download discrete WDL data ----------------------------------------------

wqpoint <- data.frame()

for(StationNumber in na.omit(NDFS_site_df$continuous_number)){
  for(Parameter in c("Water_Temperature", 
                     "Electrical_Conductivity_at_25C",
                     "Dissolved_Oxygen",
                     "Dissolved_Oxygen_Percentage",
                     "pH",
                     "Turbidity",
                     "Chlorophyll",
                     "Fluorescent_Dissolved_Organic_Matter")){
    print(paste("Station:", StationNumber, "Parameter:", Parameter))
    try(temp <- read.csv(paste0("https://wdlstorageaccount.blob.core.windows.net/continuous/",
                                StationNumber, "/por/", StationNumber, "_", Parameter, "_Raw.csv"), skip = 2))
    temp <- temp[, 1:2]
    colnames(temp) <- c("Datetime", "Value")
    temp$Station <- StationNumber
    temp$Variable <- Parameter
    temp$Datetime <- as.POSIXct(temp$Datetime, format = "%m/%d/%Y %H:%M:%S")
    wqpoint <- rbind(wqpoint, temp[temp$Datetime >= as.POSIXct("2023-01-01 00:00") &
                                   temp$Datetime <= as.POSIXct("2023-12-31 23:59"),])
    
  }
}

wqpoint <- merge(wqpoint, NDFS_site_df[, c("site_code", "continuous_number")], 
                by.x = "Station", by.y = "continuous_number", all.x = T)

save(wqpoint, file = "data/NDFS2023_wqpoint.Rdata")

# Download Lisbon weir cdec data ------------------------------------------

# ADD other CDEC parameters at LIS and RVB

LIS <- downloadCDEC(site_no = "LIS", parameterCd = 20, startDT = "2023-1-1", endDT = "2023-12-1")

save(LIS, file = "data/NDFS2023_Lisbon_weir_flow.Rdata")

# Download NWIS data at Toe -----------------------------------------------
# ADD Cache Slough a S Liberty Island NR Rio Vista CA - 11455315
# Cache Slough AB Ryer Island Ferry NR Rio Vista CA - 11455385

site_no = "11455140"
startDT = "2023-01-01" 
endDT = "2023-10-01"

paramsdf <- data.frame("parameterCd" = c("32316", "00060", "72137", "00400", 
                                         "00095", "00010", "63680", "00300"),
                       "parameterLabel" = c("Chlorophyll", "Discharge", "Discharge_tf", "pH", 
                                            "SPC", "Temp_C", "Turb_FNU", "DO_mgl"))

parameters <- c(
  "32316", #Chlorophyll fluorescence (fChl), water, micrograms per liter as chlorophyll
  "00060", #Discharge, cubic feet per second
  "72137", #Discharge, tidally filtered, cubic feet per second
  "00400", #pH, water, unfiltered, field, standard units
  "00095", #Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
  "00010", #Temperature, water, degrees Celsius
  "00300", #Dissolved oxygen, water, unfiltered, milligrams per liter
  "63680" #Turbidity, water, formazin nephelometric units (FNU)
)

##janky for-loop, replace with vectorized apply function?
datparams <- data.frame()

for(i in parameters){
  print(i)
  tempdat <- downloadNWIS(site_no, i, startDT, endDT)
  datparams <- rbind(datparams, tempdat)
}

datparams <- merge(datparams, paramsdf, by = "parameterCd", all.x = T)

datparams$Param_val <- as.numeric(datparams$Param_val)

datparams <- datparams[!(datparams$parameterLabel == "pH" & datparams$Param_val < 6),]

datparams <- datparams[is.na(datparams$parameterLabel) == F &
                         is.na(datparams$Datetime) == F &
                         is.na(datparams$Param_val) == F,]

TOE <- datparams

save(TOE, file = "data/NDFS2023_TOE_params.Rdata")
