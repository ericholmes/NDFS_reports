## Database update script
library(tidyverse)
library(WDLimportr)

source("scripts/NDFS2023_Useful_functions.R")

site_name <- c("BL5 -Below Toe Drain", "Toe Drain@ I-80", "Liberty at S End", 
               "Toe Drain YB LISBON", "Ridge Cut Slough", "Toe Drain at Rd. 22",
               "Rominger Bridge", "	RVB - Rio Vista", "RYI - Cache Slough",
               "Toe Drain at STTD", "Sacramento River @ Sherwood Harbor",
               "Woodland Wastewater", "Davis Wastewater", "Prospect Slough")

site_code <- c("BL5", "I80", "LIB", 
               "LIS", "RCS", "RD22", 
               "RMB", "RVB", "RYI", 
               "STTD", "SHR",
               "WWT", "DWT", "PRS")

discrete_number <- c("B9D81651399", "A0D83441350", "B9D81450411", 
                     "B9D82851352", "A0D84761435", "A0D84061386",
                     "A0C85051515", "B9D80960412", "B9D81281402",
                     "A0D82120386", "A0200000",
                     "A1030000", "A0033300", "B9B81541403")

continuous_number <- c("B9D81651399", "A0D83441350", NA, 
                       "B9156000", "A0D84761435", "A0D84061386",
                       "A0C85051515", "B91212", NA,
                       "A0D82120386", NA,
                       NA, NA, NA)

downstream <- c(TRUE, FALSE, TRUE, 
                FALSE, FALSE, FALSE, 
                FALSE, TRUE, TRUE, 
                FALSE, FALSE,
                FALSE, FALSE, TRUE)

NDFS_site_df <- data.frame(site_name, site_code, discrete_number, continuous_number, downstream)

# Download continuous WDL water quality -----------------------------------

wqcont <- data.frame()

for(StationNumber in na.omit(NDFS_site_df$continuous_number)){
  print(StationNumber)
  for(Parameter in c("Water_Temperature", 
                     "Electrical_Conductivity_at_25C",
                     "Dissolved_Oxygen",
                     "Dissolved_Oxygen_Percentage",
                     "pH",
                     "Turbidity",
                     "Chlorophyll",
                     "Fluorescent_Dissolved_Organic_Matter")){
    print(paste("Station:", StationNumber, "Parameter:", Parameter))
    try({temp <- read.csv(paste0("https://wdlstorageaccount.blob.core.windows.net/continuous/",
                            StationNumber, "/por/", StationNumber, "_", Parameter, "_Raw.csv"), skip = 2)
    temp <- temp[, 1:2]
    colnames(temp) <- c("Datetime", "Value")
    temp$Station <- StationNumber
    temp$Variable <- Parameter
    temp$Datetime <- as.POSIXct(temp$Datetime, format = "%m/%d/%Y %H:%M:%S")
    wqcont <- rbind(wqcont, temp[temp$Datetime >= as.POSIXct("2023-01-01 00:00") &
                                   temp$Datetime <= as.POSIXct("2023-12-31 23:59"),])
    })
    
  }
}

wqcont_wdl <- merge(wqcont, NDFS_site_df[, c("site_code", "continuous_number")], 
                 by.x = "Station", by.y = "continuous_number", all.x = T)

save(wqcont_wdl, file = "data/NDFS2023_wqcont_wdl.Rdata")

# Download discrete lab wq ------------------------------------------------

wqlab <- data.frame()
for(discrete in na.omit(NDFS_site_df$discrete_number)){
  print(paste(discrete, NDFS_site_df[NDFS_site_df$discrete_number %in% discrete,"site_code"]))
  wqlab <- rbind(wqlab, ImportDiscreteLab(discrete))
}
wqlab <- wqlab[wqlab$sample_date > as.POSIXct("2023-1-1"),]

save(wqlab, file = "data/NDFS2023_wqlab.Rdata")

# Download CDEC continuous WQ data ----------------------------------------

startDT = "2023-01-01" 
endDT = "2023-12-31"

cdec_sensors <- data.frame("parameterCd" = c(100, 221, 62, 61, 
                                             25, 28, 20, 27,
                                             100.1),
                           "parameterLabel" = c("Conductivity", "Turbidity", "pH", "Dissolved_Oxygen", 
                                                "Water_Temperature", "Chlorophyll", "Discharge", "Turbidity",
                                                "Electrical_Conductivity_at_25C"))

wqcont_cdec <- data.frame()
for(site in c("LIS", "RVB")){
  for(parm in cdec_sensors$parameterCd){
    print(paste(site, parm))
    try(wqcont_cdec <- rbind(wqcont_cdec, downloadCDEC(site_no = site, parameterCd = parm, startDT = startDT , endDT = endDT)))
  }
}

save(wqcont_cdec, file = "data/NDFS2023_wqcont_cdec.Rdata")

# Download NWIS continuous wq data ----------------------------------------

paramsdf <- data.frame("parameterCd" = c("32316", "00060", "72137", "00400", 
                                         "00095", "00010", "63680", "00300"),
                       "parameterLabel" = c("Chlorophyll", "Discharge", "Discharge_tf", "pH", 
                                            "Electrical_Conductivity_at_25C", "Water_Temperature", "Turbidity", "Dissolved_Oxygen"))

nwis_sitesdf <-data.frame(Site_no = c("11455140", "11455315", "11455385"),
                          site_code = c("TOE", "LIB", "RYI"))

##janky for-loop, replace with vectorized apply function?
datparams <- data.frame()
for(site in c("11455140", "11455315", "11455385")){
  for(i in paramsdf$parameterCd){
    print(paste(site, i))
    tempdat <- downloadNWIS(site, i, startDT, endDT)
    datparams <- rbind(datparams, tempdat)
  }
}

datparams <- merge(datparams, paramsdf, by = "parameterCd", all.x = T)

datparams$Param_val <- as.numeric(datparams$Param_val)

datparams <- datparams[!(datparams$parameterLabel == "pH" & datparams$Param_val < 6),]

datparams <- datparams[is.na(datparams$parameterLabel) == F &
                         is.na(datparams$Datetime) == F &
                         is.na(datparams$Param_val) == F,]

wqcont_nwis <- datparams

save(wqcont_nwis, file = "data/NDFS2023_wqcont_nwis.Rdata")

# Merge Continuous WQ datasets --------------------------------------------

##Prep CDEC data
str(wqcont_cdec)
wqcont_cdec$Param_val <- as.numeric(wqcont_cdec$Param_val)
##Convert temps from Fahrenheit to Celcius
wqcont_cdec$Param_val <- ifelse(wqcont_cdec$parameterCd == "25", 
                                      (wqcont_cdec$Param_val-32)*5/9, wqcont_cdec$Param_val)
##Pivot to wide format so temps can be used to calculate SPC
wqcont_cdec_spc <- pivot_wider(wqcont_cdec, id_cols = c("Site_no", "Datetime"),  
                              names_from = "parameterCd", values_from = "Param_val")
##Convert EC to SPC
wqcont_cdec_spc$Param_val <- wqcont_cdec_spc$"100" / (1 + 0.02*(wqcont_cdec_spc$"25"-25))
##Create SPC parameterCd
wqcont_cdec_spc$parameterCd <- 100.1
##Bind data
wqcont_cdec <- rbind(wqcont_cdec, wqcont_cdec_spc[, c("Site_no", "Datetime", "parameterCd", "Param_val")])
      
wqcont_cdec_merge <- merge(wqcont_cdec, cdec_sensors, by = "parameterCd", all.x = T)
wqcont_cdec_merge$site_code <- wqcont_cdec_merge$Site_no
wqcont_cdec_merge$source <- "cdec"

##Prep NWIS data
str(wqcont_nwis)
wqcont_nwis_merge <- merge(wqcont_nwis, nwis_sitesdf, by = "Site_no", all.x = T)
wqcont_nwis_merge$source <- "nwis"
dput(colnames(wqcont_nwis_merge))

##Prep WDL data
str(wqcont_wdl)
wqcont_wdl$source <- "wdl"
colnames(wqcont_wdl) <- c("Station", "Datetime", "Param_val", "parameterLabel",  "site_code", "source")
unique(wqcont_wdl$parameterLabel)

##Merge continuous data sources
wqcont_all <- rbind(wqcont_cdec_merge[,c("source", "site_code", "Datetime", "parameterLabel", "Param_val")],
                    wqcont_nwis_merge[,c("source", "site_code", "Datetime", "parameterLabel", "Param_val")],
                    wqcont_wdl[,c("source", "site_code", "Datetime", "parameterLabel", "Param_val")])
wqcont_all$Param_val <- as.numeric(wqcont_all$Param_val)

save(wqcont_all, file = "data/NDFS2023_wqcont_all.Rdata")
