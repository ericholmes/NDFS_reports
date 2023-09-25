## NDFS 2023 analysis script

##Should output be saved?
saveOutput <- TRUE

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggh4x)

# Load data ---------------------------------------------------------------

load("data/NDFS2023_wqlab.Rdata")
load("data/NDFS2023_wqcont.Rdata")
load("data/NDFS2023_wqcont_cdec.Rdata")
load("data/NDFS2023_wqcont_nwis.Rdata")
head(wqcont_cdec)
head(wqcont_nwis)

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
                                        "LIS", "STTD","BL5", "PRS", 
                                        "LIB", "RYI", "RVB", "SHR"),
                       station_number = c(NA, NA, "A0D84061386", NA, "A0D83441350", 
                                      "B9D82851352", "A0D82120386", "B9D81651399", NA,
                                      "B9D81450411", "B9D81281402", "B9D80960412", "A0200000"),
                       dist = c(78.71, 64, 62.60, 52, 49.80, 38.74, 24.21, 15.21, 13.00, 10.67, 7.19, 0, -10))

# Tidally filtered flow at TOE --------------------------------------------
TOE <- wqcont_nwis[wqcont_nwis$Site_no == "11455140",]

if(saveOutput == T){png(paste("figures/NDFS2023_Fig2_Toeflow%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)}

ggplot(TOE[TOE$parameterLabel == "Discharge_tf" & TOE$Datetime > as.POSIXct("2023-5-1"),], 
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

ndmerge$date <- ifelse(ndmerge$transect == 1, "6-26", 
                       ifelse(ndmerge$transect == 2, "7-10", 
                              ifelse(ndmerge$transect == 3, "7-25", 
                                     ifelse(ndmerge$transect == 4, "8-08", 
                                            ifelse(ndmerge$transect == 5, "8-23", 
                                                   ifelse(ndmerge$transect == 6, "9-05", "9-19"))))))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig3_wq%03d.png", sep = ""), 
    height = 5, width = 6.5, unit = "in", res = 1000)}

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
  scale_color_viridis_c() + 
  facet_wrap(variable ~ ., scales = "free") + theme_bw() + labs(y = "Parameter value", x = "Distance from Rio Vista (km)")

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

# merge longitudinal river mile data with long format wqlab data
wqlmerge <- merge(wqlab, stations, by = "station_number", all.x = T)

# Convert result to numeric vlaues
wqlmerge$result <- as.numeric(wqlmerge$result)
wqlmerge$result2 <- ifelse(is.na(wqlmerge$result) == T, 0,wqlmerge$result)

wqlmerge$date <- ifelse(wqlmerge$transect == 1, "6-26", 
                       ifelse(wqlmerge$transect == 2, "7-10", 
                              ifelse(wqlmerge$transect == 3, "7-25", 
                                     ifelse(wqlmerge$transect == 4, "8-08", 
                                            ifelse(wqlmerge$transect == 5, "8-23", 
                                                   ifelse(wqlmerge$transect == 6, "9-05", "9-19"))))))

ggplot(wqlmerge, aes(y = result2, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + scale_x_reverse() + scale_color_viridis_d() +
  facet_wrap(parameter ~ ., scales = "free") + theme_bw() + labs(y = "Parameter value", x = "Distance from Rio Vista (km)")

# Zooplankton score -------------------------------------------------------

if(saveOutput == T){png(paste("figures/NDFS2023_Fig4_zoop%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)}

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = dist, fill = date, color = date)) + 
  # geom_bar(stat = "identity", position = "dodge", width = 2) + 
  geom_jitter(height = 0) + scale_x_reverse() + labs(y = "Zoop score", x = "Distance from Rio Vista (km)") +
  geom_line(stat = "smooth", se = F, method = "loess", span = .6) +
  scale_color_viridis_d() + scale_fill_viridis_d() + 
  # facet_grid(transect ~ .) + 
  theme_bw() #+ geom_text(aes(x = dist, y = value*.95, label = station_name), angle = 90, size = 2, show.legend = F)

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

# Continuous data plotting ------------------------------------------------
wqcont_cdec$Param_val <- as.numeric(wqcont_cdec$Param_val)
##data cleaning
wqcont_cdec <- wqcont_cdec[!(wqcont_cdec$parameterCd == 25 & wqcont_cdec$Param_val > 1000) &
                              !(wqcont_cdec$parameterCd == 28 & wqcont_cdec$Param_val > 90) &
                              !(wqcont_cdec$parameterCd == 62 & wqcont_cdec$Param_val < 1) &
                              !(wqcont_cdec$parameterCd == 221 & wqcont_cdec$Param_val > 200),]

if(saveOutput == T){png(paste("figures/NDFS2023_Fig5_Continuous_wq%03d.png", sep = ""), 
    height = 6.5, width = 6.5, unit = "in", res = 1000)}

ggplot(wqcont_cdec[wqcont_cdec$Datetime > as.POSIXct("2023-7-1") & 
                is.na(wqcont_cdec$Site_no) == F & is.na(wqcont_cdec$Datetime) == F,], 
       aes(x = Datetime, y = Param_val)) + geom_line() + theme_bw() +
  facet_grid(parameterCd ~ Site_no, scales = "free_y") + 
  scale_x_datetime(breaks = "2 weeks", date_labels = "%b-%d")

if(saveOutput == T){dev.off()}


# WDL continuous data plotting --------------------------------------------

ggplot(wqcont_wdl[wqcont_wdl$Datetime > as.POSIXct("2023-7-1") & 
                     is.na(wqcont_wdl$site_code) == F & is.na(wqcont_wdl$Datetime) == F,], 
       aes(x = Datetime, y = Value)) + geom_line() + theme_bw() +
  facet_grid(Variable ~ site_code, scales = "free_y") + 
  scale_x_datetime(breaks = "2 weeks", date_labels = "%b-%d")


# Predictive model --------------------------------------------------------

# Zoop score as response variable, Explanatory variables: Do range, Turb, CHL, etc.
