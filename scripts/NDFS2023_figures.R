## NDFS 2023 analysis script

##Should output be saved?
saveOutput <- T

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggh4x)

# Load data ---------------------------------------------------------------

load("data/NDFS2023_wqlab.Rdata")
load("data/NDFS2023_wqcont_wdl.Rdata")
load("data/NDFS2023_wqcont_cdec.Rdata")
load("data/NDFS2023_wqcont_nwis.Rdata")
load("data/NDFS2023_wqcont_all.Rdata")

# Get column names from working WQ data
header = readxl::read_excel("data/YBFMP_WQ_Data_WORKING_20221006.xlsx", skip = 1) %>% colnames()

# Read in working water quality data
wq <- readxl::read_excel("data/YBFMP_WQ_Data_WORKING_20221006.xlsx", skip = 3, 
                         col_names = header[3:length(header)])

# Read in working zoop code data
zoopcode <- readxl::read_excel("data/Zoop_Code_Data.xlsx")

# Create station lookup data with longitudinal stream distance 
# NOTE: SHR was assigned 60 for visualization purposes even though it is actually upstream of RVB
stations <- data.frame(station_name = c("RCS", "WWT", "RD22", "DWT", "I80", 
                                        "LIS", "STTD","TOE","BL5", "PRS", 
                                        "LIB", "RYI", "RVB", "SHR"),
                       station_number = c(NA, NA, "A0D84061386", NA, "A0D83441350", 
                                      "B9D82851352", "A0D82120386", "B9D81651399", "B9D81651399", NA,
                                      "B9D81450411", "B9D81281402", "B9D80960412", "A0200000"),
                       discrete_station_number = c("A0D84761435","A1030000","A0D84061386","A0033300","A0D83441350",
                                                   "B9D82851352","A0D82120386",NA,"B9D81651399", "B9B81541403",
                                                   "B9D81450411", "B9D81281402", "B9D80960412", "A0200000"),
                       dist = c(78.71, 64, 62.60, 52, 49.80, 
                                38.74, 24.21, 24.21, 15.21, 13.00, 
                                10.67, 7.19, 0, -10))
# dput(NDFS_site_df$discrete_number)
# Tidally filtered flow at TOE --------------------------------------------
TOE <- wqcont_nwis[wqcont_nwis$Site_no == "11455140",]

if(saveOutput == T){png(paste("figures/NDFS2023_Fig2_Toeflow%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)}
unique(wqcont_all$parameterLabel)
ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1") & wqcont_all$parameterLabel == "Discharge_tf" &
                    wqcont_all$site_code == "TOE",],
  # TOE[TOE$parameterLabel == "Discharge_tf" & TOE$$Datetime > as.POSIXct("2023-6-1"),], 
       aes(x = Datetime)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = Param_val), show.legend = F, alpha = .5) +
  geom_line(aes(y = Param_val)) + theme_bw() + labs(x = NULL, y = "Tidally filtered discharge (cfs)") +
  scale_fill_manual(values = c("+" = "skyblue", "-" = "salmon2")) 

if(saveOutput == T){dev.off()}
  
# Point water quality plot ------------------------------------------------

# Change zoop code from character to integer format
zoopcode$Zoop_Code <- as.integer(zoopcode$Zoop_Code)

# Clean column names of the working wq data (temporary work around)
wq <- janitor::clean_names(wq)

# Merge wq and zoop score data
wq <- merge(wq, zoopcode[, c("Site", "Date", "Zoop_Code")], 
            by.x = c("station_name", "wdl_sam_collection_date"),
            by.y = c("Site", "Date"), all.x = T)

# Add year to wq data for future subsetting
wq$year <- format(wq$wdl_sam_collection_date, format = "%Y")

# Add week to wq data for plotting individual transects
wq$week <- format(wq$wdl_sam_collection_date, format = "%W")

# Subset to NDFS 2023 data
ndfs23 <- wq[!(wq$measuring_program_name %in% "YBFMP") & wq$year %in% 2023,]

# Convert week to transect number
ndfs23$transect <- as.character(as.integer(as.factor(ndfs23$week)))

# change format of WQ data from wide to long format and subset to wq parameters of interest
ndmelt <- reshape2::melt(ndfs23[, c("station_name",  "wdl_sam_collection_date", "transect",
                                    "secchi", "water_temp", "do_probe", "sp_cond", "ec", "p_h", "microcyst", "veg_rank", "turb", "Zoop_Code")],
                         id.vars = c("station_name",  "wdl_sam_collection_date", "transect"))

# merge longitudinal river mile data with long format wq data
ndmerge <- merge(ndmelt, stations, by = "station_name", all.x = T)
ndmerge$value <- as.numeric(ndmerge$value)

ndmerge$date <- ifelse(ndmerge$transect == 1, "06-26", 
                       ifelse(ndmerge$transect == 2, "07-10", 
                              ifelse(ndmerge$transect == 3, "07-25", 
                                     ifelse(ndmerge$transect == 4, "08-08", 
                                            ifelse(ndmerge$transect == 5, "08-23", 
                                                   ifelse(ndmerge$transect == 6, "09-05", 
                                                          ifelse(ndmerge$transect == 7, "09-19", "10-03")))))))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig3_wq%03d.png", sep = ""), 
    height = 6, width = 7.5, unit = "in", res = 1000)}

ggplot(ndmerge[ndmerge$variable != "Zoop_Code" & !(ndmerge$station_name %in% c("WWT", "DWT", "SHR")),], 
       aes(y = value, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + 
  geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("WWT", "DWT"),], shape = 4) +
  geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("SHR"),], shape = 1) +
  scale_x_reverse() + scale_color_viridis_d() + 
  facet_wrap(variable ~ ., scales = "free") + theme_bw() + labs(y = "Parameter value", x = "Distance from Rio Vista (km)")

ggplot(ndmerge[ndmerge$variable != "Zoop_Code",], 
       aes(y = value, x = date, color = dist, group = dist)) + 
  geom_line() + geom_point() + 
  # geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("WWT", "DWT"),], shape = 4) +
  # geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("SHR"),], shape = 1) +
  scale_color_viridis_c(option = "A", begin = .05, end = .91) + 
  facet_wrap(variable ~ ., scales = "free") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(y = "Parameter value", x = "Date")

if(saveOutput == T){dev.off()}

# Bryte water quality -----------------------------------------------------

wqlab <- wqlab[wqlab$sample_date > as.POSIXct("2023-6-25") & 
                 wqlab$sample_date < as.POSIXct("2023-10-5"), ]

# Add year to wqlab data for future subsetting
wqlab$year <- format(wqlab$sample_date, format = "%Y")

# Add week to wqlab data for plotting individual transects
wqlab$week <- format(wqlab$sample_date, format = "%W")

# Convert week to transect number
wqlab$transect <- as.character(as.integer(as.factor(wqlab$week)))

unique(wqlab$station_number)
head(wqlab)
stations$station_number

# merge longitudinal river mile data with long format wqlab data
wqlmerge <- merge(wqlab, stations[, c(1,3,4)], by.x = "station_number", 
                  by.y = "discrete_station_number",all.x = T)

# Convert result to numeric vlaues
wqlmerge$result <- as.numeric(wqlmerge$result)
wqlmerge$result2 <- ifelse(is.na(wqlmerge$result) == T, 0,wqlmerge$result)

wqlmerge$date <- ifelse(wqlmerge$transect == 1, "06-26", 
                       ifelse(wqlmerge$transect == 2, "07-10", 
                              ifelse(wqlmerge$transect == 3, "07-25", 
                                     ifelse(wqlmerge$transect == 4, "08-08", 
                                            ifelse(wqlmerge$transect == 5, "08-23", 
                                                   ifelse(wqlmerge$transect == 6, "09-05", 
                                                          ifelse(wqlmerge$transect == 7, "09-19", "10-03")))))))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig4_wq%03d.png", sep = ""), 
                        height = 6.5, width = 6.5, unit = "in", res = 1000)}
ggplot(wqlmerge[!(wqlmerge$station_name.y %in% c("WWT", "DWT", "SHR")),], 
       aes(y = result, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + 
  geom_point(data = wqlmerge[wqlmerge$station_name.y %in% c("WWT", "DWT"),], 
             shape = 4, show.legend = F) +
  geom_point(data = wqlmerge[wqlmerge$station_name.y %in% c("SHR"),], shape = 1) +
  scale_x_reverse() + scale_color_viridis_d() +
  facet_wrap(parameter ~ ., scales = "free") + theme_bw() + 
  labs(y = "Parameter value", x = "Distance from Rio Vista (km)", color = NULL) +
  theme(legend.position = "bottom")

if(saveOutput == T){dev.off()}

# Zooplankton score -------------------------------------------------------

if(saveOutput == T){png(paste("figures/NDFS2023_Fig5_zoop%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)}

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = dist, fill = date, color = date)) + 
  # geom_bar(stat = "identity", position = "dodge", width = 2) + 
  geom_jitter() + scale_x_reverse() + labs(y = "Zoop score", x = "Distance from Rio Vista (km)") +
  geom_line(stat = "smooth", se = F, method = "loess", span = .8) +
  scale_color_viridis_d() + scale_fill_viridis_d() + 
  # facet_grid(transect ~ .) + 
  theme_bw() #+ geom_text(aes(x = dist, y = value*.95, label = station_name), angle = 90, size = 2, show.legend = F)

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = date, fill = dist, color = dist, group = dist)) + 
  # geom_bar(stat = "identity", position = "dodge", width = 2) + 
  geom_point() + labs(y = "Zoop score", x = "Distance from Rio Vista (km)") +
  geom_line(stat = "smooth", se = F, span = .8) +
  scale_color_viridis_c() + scale_fill_viridis_c() + 
  # facet_grid(transect ~ .) + 
  theme_bw()

ndmerge[-order(ndmerge$dist), "station_name"]
ndmerge$stafac <- factor(ndmerge$station_name, 
                         levels = unique(ndmerge[order(ndmerge$dist, decreasing = T), "station_name"]))

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = date, fill = date, color = date)) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", show.legend = F) + labs(y = "Zoop score", x = "Distance from Rio Vista (km)") +
  # geom_line(stat = "smooth", se = F, method = "loess", span = .6) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  facet_wrap(stafac ~ .)

if(saveOutput == T){dev.off()}

if(saveOutput == T){png(paste("figures/NDFS2023_Fig4_zooptall%03d.png", sep = ""), 
                        height = 8, width = 3.5, unit = "in", res = 1000)}

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = transect, fill = date)) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", show.legend = F) + labs(y = "Zoop score", x = "Sample Date") +
  geom_line(stat = "smooth", se = F, method = "loess", span = .6) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  facet_grid(stafac ~ .)

if(saveOutput == T){dev.off()}

# Continuous data plotting ------------------------------------------------
load("data/NDFS2023_wqcont_all.Rdata")
wqcont_all <- merge(wqcont_all, stations[,c("station_name", "dist")], by.x = "site_code", by.y = "station_name", all.x = T)

# data cleaning
wqcont_all <- wqcont_all[is.na(wqcont_all$Param_val) == F,]
wqcont_all <- wqcont_all[is.na(wqcont_all$Datetime) == F,]
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel %in% "Water_Temperature" & wqcont_all$Param_val > 1000),]
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel %in% "Water_Temperature" & wqcont_all$Param_val < 3),] 
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel == "Chlorophyll" & wqcont_all$Param_val > 30),]
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel == "Dissolved_Oxygen" & wqcont_all$Param_val < 1),] 

wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel == "pH" & wqcont_all$Param_val < 1),] 
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel == "Electrical_Conductivity_at_25C" & wqcont_all$Param_val < 50),] 
wqcont_all <- wqcont_all[!(wqcont_all$parameterLabel == "Turbidity" & wqcont_all$Param_val > 200),] 

if(saveOutput == T){png(paste("figures/NDFS2023_Fig6_Continuous_temp%03d.png", sep = ""), 
                        height = 4, width = 6.5, unit = "in", res = 1000)}

##Find last point in ts to place site label
wqcont_all <- wqcont_all %>% group_by(site_code, parameterLabel) %>% mutate(maxdate = max(Datetime)) %>% data.frame()

## Water temp
ggplot(wqcont_all[wqcont_all$parameterLabel == "Water_Temperature" & wqcont_all$source %in% c("cdec") &
                    
                    wqcont_all$Datetime > as.POSIXct("2023-5-1"),], 
       aes(x = Datetime, y = Param_val, color = dist)) + geom_line(aes(group = site_code), show.legend = F, alpha = .7) + theme_bw() +
  labs(x = NULL, y = "Water temperature (C)") + scale_y_continuous(breaks = seq(0,30,2)) + 
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  geom_text(data = wqcont_all[wqcont_all$parameterLabel == "Water_Temperature" & wqcont_all$source %in% c("cdec") &
                                wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val + .5, label = site_code), 
            fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15) 
# scale_x_datetime(breaks = "2 weeks", date_labels = "%b-%d")

## DO mgl
ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Dissolved_Oxygen" & 
                    wqcont_all$source %in% c("cdec", "nwis") &
                     is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + geom_line(aes(group = site_code), show.legend = F, alpha = .7) + theme_bw() +
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  labs(x = NULL, y = "Dissolved Oxygen (mg/L)") + scale_y_continuous(breaks = seq(0,30,2)) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Dissolved_Oxygen" & 
                                               wqcont_all$source %in% c("cdec", "nwis") &
                                               wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15) 
## EC
ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Electrical_Conductivity_at_25C" & 
                    wqcont_all$source %in% c("cdec", "nwis") &
                     is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + geom_line(aes(group = site_code), show.legend = F, alpha = .7) + theme_bw() +
  labs(x = NULL, y = "Specific conductivity (uS/cm)") + scale_y_continuous(breaks = seq(0,1000,100)) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Electrical_Conductivity_at_25C" & 
                                wqcont_all$source %in% c("cdec", "nwis") &
                                wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15) 
unique(wqcont_all$parameterLabel)

ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Turbidity" & 
                    wqcont_all$source %in% c("cdec", "nwis") &
                    is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + geom_line(aes(group = site_code), show.legend = F, alpha = .7) + theme_bw() +
  labs(x = NULL, y = "Turbidity") + scale_y_continuous(breaks = seq(0,1000,100)) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Turbidity" & 
                                               wqcont_all$source %in% c("cdec", "nwis") &
                                               wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15) 

if(saveOutput == T){dev.off()}


# Dissolved oxygen daily range --------------------------------------------

# 
# # Prepare data fields
# wqcont$Datetime <- as.POSIXct(wqcont$sample_date, format = "%Y-%m-%d %H:%M:%S")
# wqcont$Date <- as.Date(wqcont$Datetime)
# wqcont$Year <- format(wqcont$Date, format = "%Y")
# 
# # Compute daily max, min, mean dissolved oxygen values for each site
# doply <- wqcont %>% filter(variable == "DO_conc") %>% group_by(Date, Year, cage_id, habitat_type) %>% 
#   summarize(maxDOmgl = max(value, na.rm = T), minDOmgl = min(value, na.rm = T), meanDOmgl = mean(value, na.rm = T)) %>% 
#   ungroup()
# 
# # Compute daily max, min, mean water temperature values for each site
# tempply <- wqcont %>% filter(variable == "TEMP") %>% group_by(Date, cage_id) %>% 
#   summarize(maxTemp = max(value, na.rm = T), minTemp = min(value, na.rm = T), meanTemp = mean(value, na.rm = T)) %>% 
#   ungroup()
# 
# # merge summarized DO and temp values
# doboply <- merge(doply, tempply, by = c("Date", "cage_id"), all = T)
# rm(doply, tempply)
# 
# # Compute daily do and temp range
# doboply$DOmglrange <- doboply$maxDOmgl - doboply$minDOmgl
# doboply$temprange <- doboply$maxTemp - doboply$minTemp
# 
# # Change date format to POSIXct for compatibility with the wqcont dataframe
# doboply$Datetime <- as.POSIXct(doboply$Date)
# 
# # Concatenate to create unique id year combinations
# doboply$IDyear <- paste(doboply$cage_id, doboply$Year)
# wqcont$IDyear <- paste(wqcont$cage_id, wqcont$Year)
# 
# # Create empty caegories to be filled with smoothed daily values
# wqcont$minDOmglmod <- NA; wqcont$maxDOmglmod <- NA; wqcont$meanDOmglmod <- NA
# wqcont$minTempmod <- NA; wqcont$maxTempmod <- NA; wqcont$meanTempmod <- NA
# 
# # Calculate LOESS smoothed min, max and mean values from daily data for each site and year combo
# for(i in unique(doboply$IDyear)){
#   
#   print(i)
#   
#   if(sum(doboply[doboply$IDyear %in% i, "maxDOmgl"], na.rm = T) > 0){
#     
#     minDOmglmod <- loess(minDOmgl ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#     maxDOmglmod <- loess(maxDOmgl ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#     meanDOmglmod <- loess(meanDOmgl ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#     
#     wqcont$minDOmglmod <- ifelse(wqcont$IDyear == i, predict(minDOmglmod, wqcont$Datetime), wqcont$minDOmglmod)
#     wqcont$maxDOmglmod <- ifelse(wqcont$IDyear == i, predict(maxDOmglmod, wqcont$Datetime), wqcont$maxDOmglmod)
#     wqcont$meanDOmglmod <- ifelse(wqcont$IDyear == i, predict(meanDOmglmod, wqcont$Datetime), wqcont$meanDOmglmod)
#   }
#   
#   minTempmod <- loess(minTemp ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#   maxTempmod <- loess(maxTemp ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#   meanTempmod <- loess(meanTemp ~ as.numeric(Datetime), doboply[doboply$IDyear %in% i, ], span = .25)
#   
#   wqcont$minTempmod <- ifelse(wqcont$IDyear == i, predict(minTempmod, wqcont$Datetime), wqcont$minTempmod)
#   wqcont$maxTempmod <- ifelse(wqcont$IDyear == i, predict(maxTempmod, wqcont$Datetime), wqcont$maxTempmod)
#   wqcont$meanTempmod <- ifelse(wqcont$IDyear == i, predict(meanTempmod, wqcont$Datetime), wqcont$meanTempmod)
#   
#   rm(minTempmod, maxTempmod, meanTempmod, minDOmglmod, maxDOmglmod, meanDOmglmod)
# }
# 
# # Continuous temp and DO plotting
# (dobodomglpan <- ggplot(wqcont[wqcont$variable == "DO_conc",], aes(x = Datetime, y = meanDOmglmod, fill = habitat_type, group = cage_id)) +
#     scale_fill_manual(values = c("Wetland" = "#E31A1C", "Agriculture" = "#FFAA00", "Canal channel" = "#33A02C", "River channel" = "#1F78B4")) +
#     scale_color_manual(values = c("Wetland" = "#E31A1C", "Agriculture" = "#FFAA00", "Canal channel" = "#33A02C", "River channel" = "#1F78B4")) +
#     geom_ribbon(aes(ymin = minDOmglmod, ymax = maxDOmglmod), alpha = .2,linetype = 0) + 
#     geom_line(data = wqcont[wqcont$variable == "DO_conc",], aes(x = Datetime, y = value, color = habitat_type), alpha = .5) + 
#     geom_line(color = "black", linetype = 2) +
#     theme_bw() +
#     labs(x = NULL, y = "Dissolved oxygen (mg/L)") + 
#     facet_grid(habitat_type ~ Year, scales = "free_x") + theme(legend.position = "none"))
# 
# (dobotemp <- ggplot(wqcont[wqcont$variable == "TEMP",], aes(x = Datetime, y = meanTempmod, fill = habitat_type, group = cage_id)) +
#     scale_fill_manual(values = c("Wetland" = "#E31A1C", "Agriculture" = "#FFAA00", "Canal channel" = "#33A02C", "River channel" = "#1F78B4")) +
#     scale_color_manual(values = c("Wetland" = "#E31A1C", "Agriculture" = "#FFAA00", "Canal channel" = "#33A02C", "River channel" = "#1F78B4")) +
#     geom_ribbon(aes(ymin = minTempmod, ymax = maxTempmod), alpha = .2,linetype = 0) + 
#     geom_line(data = wqcont[wqcont$variable == "TEMP",], aes(x = Datetime, y = value, color = habitat_type), alpha = .5) + 
#     geom_line(color = "black", linetype = 2) +theme_bw() +
#     labs(x = NULL, y = "Water temperature (°C)") + facet_grid(habitat_type ~ Year, scales = "free_x") + 
#     theme(legend.position = "none"))

# WDL continuous data plotting --------------------------------------------

ggplot(wqcont_wdl[wqcont_wdl$Datetime > as.POSIXct("2023-7-1") & 
                     is.na(wqcont_wdl$site_code) == F & is.na(wqcont_wdl$Datetime) == F,], 
       aes(x = Datetime, y = Value)) + geom_line() + theme_bw() +
  facet_grid(Variable ~ site_code, scales = "free_y") + 
  scale_x_datetime(breaks = "2 weeks", date_labels = "%b-%d")

if(saveOutput == T){dev.off()}
# Predictive model --------------------------------------------------------

# Zoop score as response variable, Explanatory variables: Do range, Turb, CHL, etc.
