## NDFS 2023 analysis script


# Load libraries ----------------------------------------------------------


library(tidyverse)
library(ggh4x)

# Load data ---------------------------------------------------------------

load("data/NDFS2023_TOE_params.Rdata")
load()

# Get column names from working WQ data
header = readxl::read_excel("data/YBFMP_WQ_Data_WORKING_20221006.xlsx", skip = 1) %>% colnames()

# Read in working water quality data
wq <- readxl::read_excel("data/YBFMP_WQ_Data_WORKING_20221006.xlsx", skip = 3, 
                         col_names = header[3:length(header)])

# Read in working zoop code data
zoopcode <- readxl::read_excel("data/Zoop_Code_Data.xlsx")

# Create station lookup data with longitudinal stream distance 
# NOTE: SHR was assigned 60 for visualization purposes even though it is actually upstream of RVB
stations <- data.frame(station_name = c("RCS", "WWT", "RD22", "DWT", "I80", "LIS", "STTD","BL5", "PRS", "LIB", "RYI", "RVB", "SHR"),
                       dist = c(78.71, 64, 62.60, 52, 49.80, 38.74, 24.21, 15.21, 13.00, 10.67, 7.19, 0, -10))

# Tidally filtered flow at TOE --------------------------------------------

png(paste("figures/NDFS2023_Fig2_Toeflow%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)

ggplot(TOE[TOE$parameterLabel == "Discharge_tf" & TOE$Datetime > as.POSIXct("2023-5-1"),], 
       aes(x = Datetime)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = Param_val), show.legend = F, alpha = .5) +
  geom_line(aes(y = Param_val)) + theme_bw() + labs(x = NULL, y = "Tidally filtered discharge (cfs)") +
  scale_fill_manual(values = c("+" = "skyblue", "-" = "salmon2")) 

dev.off()
  
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
                                            ifelse(ndmerge$transect == 5, "8-23", "9-5")))))

png(paste("figures/NDFS2023_Fig3_wq%03d.png", sep = ""), 
    height = 5, width = 6.5, unit = "in", res = 1000)

ggplot(ndmerge[ndmerge$variable != "Zoop_Code",], aes(y = value, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + scale_x_reverse() + scale_color_viridis_d() +
  facet_wrap(variable ~ ., scales = "free") + theme_bw() + labs(y = "Parameter value", x = "Distance from Rio Vista")

dev.off()

# Zooplankton score -------------------------------------------------------

png(paste("figures/NDFS2023_Fig4_zoop%03d.png", sep = ""), 
    height = 4, width = 6.5, unit = "in", res = 1000)

ggplot(ndmerge[ndmerge$variable == "Zoop_Code" & is.na(ndmerge$value) == F,], 
       aes(y = value, x = dist, fill = date, color = date)) + 
  # geom_bar(stat = "identity", position = "dodge", width = 2) + 
  geom_jitter(height = 0) + scale_x_reverse() + labs(y = "Zoop score", x = "Sample date") +
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
  geom_bar(stat = "identity", show.legend = F) + labs(y = "Zoop score", x = "Sample date") +
  # geom_line(stat = "smooth", se = F, method = "loess", span = .6) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  facet_wrap(stafac ~ .)

dev.off()

# Continuous data plotting ------------------------------------------------

ggplot()
