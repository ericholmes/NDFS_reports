## NDFS 2023 analysis script

##Should output be saved (TRUE or FALSE)?
saveOutput <- F

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggh4x)

# Load data ---------------------------------------------------------------

load("data/NDFS2023_wqlab.Rdata")
load("data/NDFS2023_wqcont_wdl.Rdata")
load("data/NDFS2023_wqcont_cdec.Rdata")
load("data/NDFS2023_wqcont_nwis.Rdata")
load("data/NDFS2023_wqcont_all.Rdata")
load("data/NDFS2023_zoop.Rdata")

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
ggplot(wqcont_all[wqcont_all$Datetime >= as.POSIXct("2023-5-1") & wqcont_all$Datetime <= as.POSIXct("2023-11-1") & 
                    wqcont_all$parameterLabel == "Discharge_tf" &
                    wqcont_all$site_code == "TOE",],
  # TOE[TOE$parameterLabel == "Discharge_tf" & TOE$$Datetime > as.POSIXct("2023-6-1"),], 
       aes(x = Datetime)) +
  scale_x_datetime(breaks = scales::date_breaks("1 month"), labels = scales::date_format("%b-%d")) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = Param_val), show.legend = F, alpha = .5) +
  geom_line(aes(y = Param_val)) + theme_bw() + labs(x = NULL, y = "Tidally filtered discharge (cfs)") +
  scale_fill_manual(values = c("+" = "skyblue", "-" = "salmon2")) 

if(saveOutput == T){dev.off()}
  
# Daily average flow at Lisbon --------------------------------------------
lisflow <- wqcont_all[wqcont_all$site_code == "LIS" & wqcont_all$parameterLabel == "Discharge",]
lisflow$Date <- as.Date(lisflow$Datetime)
lisflowply <- lisflow %>% group_by(Date) %>% summarize(meanflow = mean(Param_val, na.rm = T))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig2a_LISflow%03d.png", sep = ""), 
                        height = 4, width = 6.5, unit = "in", res = 1000)}
##Some missing data here during a crucial time
ggplot(lisflowply[lisflowply$Date > as.POSIXct("2023-5-1") & lisflowply$Date <= as.POSIXct("2023-11-1") ,],
       aes(x = Date)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = meanflow), show.legend = F, alpha = .5) +
  geom_line(aes(y = meanflow)) + theme_bw() + labs(x = NULL, y = "Tidally filtered discharge (cfs)") +
  scale_x_date(breaks = scales::date_breaks("1 month"), labels = scales::date_format("%b-%d")) +
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
# Change variable labels
varnamelookup <- data.frame(varname = c("secchi", "water_temp", "do_probe", 
                                        "sp_cond", "ec", "p_h", "microcyst", 
                                        "veg_rank", "turb", "Zoop_Code"),
                            labels = c("Secchi~(m)", "Water~temp.~(degree*C)", "DO~(mg~L^-1)",
                                       "SPC~(mu*S~cm^-1)", "EC~(mu*S~cm^-1)", "pH", "microcystis~rank",
                                       "vegetation~rank", "Turbidity~(NTU)", "Zoop~score"))
                           
ndmerge$varfac <- factor(ndmerge$variable, levels = ndmerge$variable,
                        labels = varnamelookup[match(ndmerge$variable, varnamelookup$varname), "labels"])

if(saveOutput == T){png(paste("figures/NDFS2023_Fig3_wq%03d.png", sep = ""), 
    height = 6.5, width = 7.5, unit = "in", res = 1000)}

ggplot(ndmerge[ndmerge$variable != "Zoop_Code" & !(ndmerge$station_name %in% c("WWT", "DWT", "SHR")),], 
       aes(y = value, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + 
  geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("WWT", "DWT"),], shape = 4) +
  geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("SHR"),], shape = 1) +
  scale_x_reverse() + scale_color_viridis_d() + 
  facet_wrap(varfac ~ ., scales = "free", labeller=label_parsed) + 
  theme_bw() + labs(y = "Parameter value", x = "Distance from Rio Vista (km)") +
  theme(legend.position = "bottom")

ggplot(ndmerge[!(ndmerge$variable %in% c("Zoop_Code", "water_temp", "do_probe", "microcyst", "ec", "veg_rank")) & 
                 !(ndmerge$station_name %in% c("WWT", "DWT", "SHR")),], 
       aes(y = value, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + 
  geom_point(data = ndmerge[!(ndmerge$variable %in% c("Zoop_Code", "water_temp", "do_probe", "microcyst", "ec", "veg_rank")) & 
                              ndmerge$station_name %in% c("WWT", "DWT"),], shape = 4) +
  geom_point(data = ndmerge[!(ndmerge$variable %in% c("Zoop_Code", "water_temp", "do_probe", "microcyst", "ec", "veg_rank")) & 
                              ndmerge$station_name %in% c("SHR"),], shape = 1) +
  scale_x_reverse() + scale_color_viridis_d() + 
  facet_wrap(varfac ~ ., scales = "free", labeller=label_parsed) + theme_bw() + 
  labs(y = "Parameter value", x = "Distance from Rio Vista (km)", color = "Date") +
  theme(legend.position = "bottom")

ggplot(ndmerge[ndmerge$variable != "Zoop_Code",], 
       aes(y = value, x = date, color = dist, group = dist)) + 
  geom_line() + geom_point() + 
  # geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("WWT", "DWT"),], shape = 4) +
  # geom_point(data = ndmerge[ndmerge$variable != "Zoop_Code" & ndmerge$station_name %in% c("SHR"),], shape = 1) +
  scale_color_viridis_c(option = "A", begin = .05, end = .91) + 
  facet_wrap(variable ~ ., scales = "free") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(y = "Parameter value", x = "Date") +
  theme(legend.position = "bottom")

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

# Change variable labels
wqlvarnamelookup <- data.frame(varname = c("Dissolved ortho-Phosphate", "Dissolved Ammonia", "Total Phosphorus", 
                                           "Dissolved Nitrate + Nitrite", "Dissolved Organic Nitrogen", 
                                           "Total Dissolved Solids", "Chlorophyll a", "Dissolved Silica (SiO2)", 
                                           "Total Organic Carbon", "Dissolved Chloride", "Dissolved Calcium", 
                                           "Total Suspended Solids", "Volatile Suspended Solids", "Dissolved Organic Carbon", 
                                           "Total Kjeldahl Nitrogen", "Pheophytin a"),
                            varfac = c("DOP~(mg~L^-1)", "NH[3]~(mg~L^-1)", "TP~(mg~L^-1)", 
                                       "DNN~(mg~L^-1)", "DON~(mg~L^-1)", 
                                       "TDS~(mg~L^-1)", "Chl.-alpha~(mu*g~L^-1)", "SiO[2]~(mg~L^-1)", 
                                       "TOC~(mg~L^-1)", "Cl^'-'~(mg~L^-1)", "Ca~(mg~L^-1)", 
                                       "TSS~(mg~L^-1)", "VSS~(mg~L^-1)", "DOC~(mg~L^-1)", 
                                       "TKN~(mg~L^-1)", "Pheo.-alpha~(mu*g~L^-1)"),
                               RL = c(0.05, 0.05, 0.01,
                                      0.05, 0.1, 
                                      2.5, 0.5, 0.1,
                                      0.5, 1, 1, 
                                      2.5, 2.5, 0.5, 
                                      0.1, 0.5))
# labels = c("Copepoda~log(orgs.~m^-3)", "Insecta~log(orgs.~m^-3)",
#            "Lg.~cladocera~log(orgs.~m^-3)", "Ostracoda~log(orgs.~m^-3)", "Rare~taxa~log(orgs.~m^-3)",
#            "Rotifera~log(orgs.~m^-3)", "Sm.~cladocera~log(orgs.~m^-3)",
#            "Chlorophyll-alpha~(mu*g~L^-1)", "Diss.~Organic~Carbon~(mu*g~L^-1)",
#            "Mean~daily~DO~range(mg~L^-1)","Mean~daily~wtemp~range~(degree*C)", "Spec.~conductivity~(mu*S~cm^-1)"))
dput(unique(wqlmerge$parameter))
wqlmerge$varfac <- factor(wqlmerge$parameter, levels = wqlmerge$parameter,
                         labels = wqlvarnamelookup[match(wqlmerge$parameter, wqlvarnamelookup$varname), "varfac"])

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
                        height = 7.5, width = 7.5, unit = "in", res = 1000)}
ggplot(wqlmerge[!(wqlmerge$station_name.y %in% c("WWT", "DWT", "SHR")),], 
       aes(y = result2, x = dist, color = date, group = date)) + 
  geom_line() + geom_point() + 
  geom_point(data = wqlmerge[wqlmerge$station_name.y %in% c("WWT", "DWT"),], 
             shape = 4, show.legend = F) +
  geom_point(data = wqlmerge[wqlmerge$station_name.y %in% c("SHR"),], shape = 1) +
  scale_x_reverse() + scale_color_viridis_d() +
  geom_hline(data = wqlvarnamelookup, aes(yintercept = RL), linetype = 2) + 
  facet_wrap(varfac ~ ., scales = "free", labeller=label_parsed) + theme_bw() + 
  labs(y = "Parameter value", x = "Distance from Rio Vista (km)", color = NULL) +
  theme(legend.position = "bottom")

if(saveOutput == T){dev.off()}

## Looping anovas?



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
  geom_point() + labs(y = "Zoop score", x = "Date") +
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
  geom_bar(stat = "identity", show.legend = F) + labs(y = "Zoop score", x = NULL) +
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


# Zooplankton community ----------------------------------------------------

# Zoop bar and area plots

zoopply <- zoop %>% group_by(station_id, Week, jday, sample_date, year, classification) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup()

zoopbarply <- zoop[zoop$classification != "Microzooplankton and nauplii",] %>% 
  group_by(station_id, Week, jday, sample_date, year, classification) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup() %>% group_by(station_id, Week, year) %>% mutate(perc = sumtot/sum(sumtot))
zoopbarply$station_id <- factor(zoopbarply$station_id, levels = 
                                  c("RCS", "WWT", "RD22", "I80", "LIS", "STTD","BL5", "PRS", "LIB", "RYI", "RVB", "SHR"))

(zoopbarperc <- ggplot(zoopbarply[zoopbarply$Week %in% 25:40 & is.na(zoopbarply$station_id) == F,], aes(x = sample_date, y = perc)) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + theme(legend.position = "bottom") + 
    labs(fill = NULL, y = "Proportion", x = NULL) +
    geom_bar(aes(fill = classification), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station_id, space = "free"))

(zoopbarsum <- ggplot(zoopbarply[zoopbarply$Week %in% 25:40 & is.na(zoopbarply$station_id) == F,], aes(x = sample_date, y = sumtot)) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(fill = NULL, y = "Density (ind*m^-3)",x = NULL) +
    geom_bar(aes(fill = classification), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station_id, space = "free"))

png(paste("figures/NDFS2023_Fig4_zoopbars%03d.png", sep = ""), 
                        height = 7.5, width = 8, unit = "in", res = 1000)
cowplot::plot_grid(zoopbarperc + theme(legend.position = "none", axis.text.x = element_blank()), 
                   zoopbarsum, 
        nrow = 2, rel_heights = c(9,10), align = "v")
dev.off()

# NMDS
library(vegan)
library(ggrepel)
zoopcast <- reshape2::dcast(zoopply[zoopply$Week %in% 25:40, ],
                          formula = station_id + jday + sample_date + year ~ classification, value.var = 'sumtot', fun.aggregate = sum, fill = 0)

zoopcast <- zoopcast[is.na(zoopcast$station_id) == F & is.na(zoopcast$year) == F & zoopcast$station_id != "SHR",]
# melt back to long form with the added zeros from the previous casting step
# zoopply <- melt(zoopcast, id.vars = c("station_id", "jday", "year"), value.name = 'total', variable.name = "classification")

com2016 <- decostand(zoopcast[, c(5:7,9)], method = "chi.square")
nmds2016 <- metaMDS(com2016, autotransform = F, na.rm = T)

data.scores16 <- as.data.frame(scores(nmds2016, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores16$station_id <- zoopcast[, "station_id"] #create a column of site names, from the rownames of data.scores
data.scores16$jday <- zoopcast[, "jday"]
data.scores16$sample_date <- zoopcast[, "sample_date"]
data.scores16 <- merge(data.scores16, stations, by.x = "station_id", by.y = "station_name", all.x = T)
data.scores16$distsite <- paste(sprintf("%02d",round(data.scores16$dist)), data.scores16$station_id, sep = "-")
species.scores16 <- as.data.frame(scores(nmds2016, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores16$species <- rownames(species.scores16)  # create a column of species, from the rownames of species.scores

png("figures/NDFS_zoop_NMDS_dist_%03d.png",
    family = "serif", height = 6, width = 7, units = "in", res = 1000)
range(data.scores16$NMDS1)
range(data.scores16$NMDS2)
ggplot() + 
  stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = distsite, group = station_id), geom = "polygon", alpha = .1) +
  stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, group = station_id), alpha = .9) +
  geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, shape = distsite)) +
  geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
  # coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) + 
  # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, group = station_id)) +
  geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = 1, bg.color = "white",
                  bg.r = 0.2) +
  scale_color_viridis_d(option = "A", begin = .1, end = .8)+
  scale_fill_viridis_d(option = "A", begin = .1, end = .8) + 
  scale_shape_manual(values = 1:10) +
  theme_bw(base_family = "serif") + theme(legend.title = element_blank()) 
  
dev.off()

## SPC and NMDS1

ndmerge$sample_date <- as.Date(paste0("2023-",ndmerge$date))

spcnmds <- merge(ndmerge[ndmerge$variable == "sp_cond",], 
      data.scores16[c("station_id", "sample_date", "NMDS1", "NMDS2")], 
      by.x = c("station_name", "sample_date"),
      by.y = c("station_id", "sample_date"))

ggplot(spcnmds, aes(x = value, y = NMDS1)) + geom_point()
ggplot(spcnmds[spcnmds$station_name == "STTD",], aes(x = value, y = NMDS1)) + geom_point()
               
# Continuous data plotting ------------------------------------------------
load("data/NDFS2023_wqcont_all.Rdata")
wqcont_all <- merge(wqcont_all, stations[,c("station_name", "dist")], by.x = "site_code", by.y = "station_name", all.x = T)
wqcont_all <- wqcont_all[!(wqcont_all$site_code == "LIS" & wqcont_all$source == "cdec"),]
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
wqcont_all <- wqcont_all %>% filter(Datetime >= as.POSIXct("2023-5-1") & Datetime <= as.POSIXct("2023-10-20")) %>% 
  group_by(site_code, parameterLabel) %>% mutate(maxdate = max(Datetime)) %>% data.frame()

dput(unique(wqcont_all$parameterLabel))
wcvarnamelookup <- data.frame(varname = c("Water_Temperature", "Dissolved_Oxygen", "Dissolved_Oxygen_Percentage",
                                          "Electrical_Conductivity_at_25C",  "Turbidity", 
                                          "Chlorophyll", "Fluorescent_Dissolved_Organic_Matter",  
                                           "pH", "Discharge", "Discharge_tf", "Conductivity"),
                            labels = c("Water~temp.~(degree*C)", "DO~(mg~L^-1)", "DO~(%~sat.)",
                                       "SPC~(mu*S~cm^-1)",  "Turbidity~(NTU)", 
                                       "Chl.-alpha~(mu*g~L^-1)", "FDOM~(mg~L^-1)",
                                       "pH", "Discharge~(cfs)", "Discharge_tf~(cfs)", "Conductivity~(mu*S~cm^-1)"))

wqcont_all$varfac <- factor(wqcont_all$parameterLabel, levels = wqcont_all$parameterLabel,
                         labels = wcvarnamelookup[match(wqcont_all$parameterLabel, wcvarnamelookup$varname), "labels"])

(cwq_wtemp2 <-ggplot(wqcont_all[wqcont_all$parameterLabel == "Water_Temperature" & 
                                  !(wqcont_all$site_code %in% c("I80", "STTD")),], 
       aes(x = Datetime, y = Param_val, color = dist)) + 
  geom_line(aes(group = site_code), show.legend = F, alpha = .3) + theme_bw() +
  geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
            stat = "smooth", method = "loess", span = .1) + theme_bw() +
  labs(x = NULL, y = bquote(Water~temp.~(degree*C))) + scale_y_continuous(breaks = seq(0,30,2)) + 
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Water_Temperature" & 
                                               !(wqcont_all$site_code %in% c("I80", "STTD")) &
                                wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val + .5, label = site_code), 
            fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15, size = 3) +
  scale_x_datetime(breaks=scales::date_breaks("1 month"), labels=scales::date_format("%b-%d"),
  limits = c(as.POSIXct("2023-5-1"), as.POSIXct("2023-10-20"))))

## DO mgl
(cwq_domgl <-ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Dissolved_Oxygen" & 
                                 !(wqcont_all$site_code %in% c("I80", "STTD")) &
                     is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + 
    geom_line(aes(group = site_code), show.legend = F, alpha = .3) + theme_bw() +
    geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
              stat = "smooth", method = "loess", span = .1) +
    theme_bw() +
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  labs(x = NULL, y = bquote(DO~(mg~L^-1))) + scale_y_continuous(breaks = seq(0,30,2)) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Dissolved_Oxygen" & 
                                               !(wqcont_all$site_code %in% c("I80", "STTD")) &
                                               wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15, size = 3) +
  scale_x_datetime(breaks=scales::date_breaks("1 month"), labels=scales::date_format("%b-%d"),
                   limits = c(as.POSIXct("2023-5-1"), as.POSIXct("2023-10-20"))))

## EC
(cwq_spc <-ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& 
                               wqcont_all$parameterLabel == "Electrical_Conductivity_at_25C" & 
                               !(wqcont_all$site_code %in% c("I80", "STTD")) &
                     is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + geom_line(aes(group = site_code), show.legend = F, alpha = .7) + theme_bw() +
  labs(x = NULL, y = bquote(SPC~(mu*S~cm^-1))) + scale_y_continuous(breaks = seq(0,1000,100)) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Electrical_Conductivity_at_25C" & 
                                               !(wqcont_all$site_code %in% c("I80", "STTD")) &
                                wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15, size = 3) +
  scale_x_datetime(breaks=scales::date_breaks("1 month"), labels=scales::date_format("%b-%d"),
                   limits = c(as.POSIXct("2023-5-1"), as.POSIXct("2023-10-20")))) 

## Turb 
(cwq_turb <-ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& 
                                wqcont_all$parameterLabel == "Turbidity" & 
                                !(wqcont_all$site_code %in% c("I80", "STTD")) &
                    is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + 
  geom_line(aes(group = site_code), show.legend = F, alpha = .3) + theme_bw() +
  geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
            stat = "smooth", method = "loess", span = .1) +
  labs(x = NULL, y = "Turbidity (NTU)") + 
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  coord_cartesian(ylim = c(0,80)) +
  scale_y_continuous(breaks = seq(0,100,20)) +
  scale_x_datetime(breaks=scales::date_breaks("1 month"), labels=scales::date_format("%b-%d"),
                   limits = c(as.POSIXct("2023-5-1"), as.POSIXct("2023-10-20"))) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Turbidity" & 
                                               !(wqcont_all$site_code %in% c("I80", "STTD")) &
                                               wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15, size = 3))


## Chlorophyll
(cwq_chl <- ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& 
                                wqcont_all$parameterLabel == "Chlorophyll" & 
                                !(wqcont_all$site_code %in% "TOE") &
                    is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Datetime, y = Param_val, color = dist)) + 
  geom_line(aes(group = site_code), show.legend = F, alpha = .2) + theme_bw() +
  geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
            stat = "smooth", method = "loess", span = .1) +
  labs(x = NULL, y = bquote(Chl.-alpha~(mu*g~L^-1))) + 
  scale_color_viridis_c(option = "A", begin = .1, end = .8) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,100,5)) +
  scale_x_datetime(breaks=scales::date_breaks("1 month"), labels=scales::date_format("%b-%d"),
                   limits = c(as.POSIXct("2023-5-1"), as.POSIXct("2023-10-20"))) +
  ggrepel::geom_text_repel(data = wqcont_all[wqcont_all$parameterLabel == "Chlorophyll" & 
                                               !(wqcont_all$site_code %in% "TOE") &
                                               wqcont_all$Datetime == wqcont_all$maxdate,], aes(y = Param_val, label = site_code), 
                           fontface = "bold", show.legend = F, bg.color = "white", bg.r = .15, size = 3))

if(saveOutput == T){dev.off()}

if(saveOutput == T){png(paste("figures/NDFS2023_Fig6_Continuous_temp_panel%03d.png", sep = ""), 
                        height = 7, width = 6.5, unit = "in", res = 1000, family = "serif")}

cowplot::plot_grid(cwq_wtemp2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(5.5,5.5,0,5.5), "pt")), 
                   cwq_domgl + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(5.5,5.5,0,5.5), "pt")), 
                   cwq_spc + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(5.5,5.5,0,5.5), "pt")), 
                   cwq_turb + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(5.5,5.5,0,5.5), "pt")), 
                   cwq_chl,
                   ncol = 1, align = "v", rel_heights = c(9,9,9,9,10))

if(saveOutput == T){dev.off()}

ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Chlorophyll" & 
             is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Param_val, fill = dist, color = dist)) + 
  geom_density(aes(group = site_code), show.legend = F, alpha = .2) + theme_bw() +
  # geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
            # stat = "smooth", method = "loess", span = .1) +
  labs(x = NULL, y = "Chlorophyll-a") + 
  scale_fill_viridis_c(option = "A", begin = .1, end = .8) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8)

chl <- wqcont_all[wqcont_all$parameterLabel== "Chlorophyll",]
chl$Date <- as.Date(chl$Datetime)
chlply <- chl %>% group_by(Date, dist, site_code) %>% summarize(medchl = median(Param_val))

ggplot(wqcont_all[wqcont_all$Datetime > as.POSIXct("2023-5-1")& wqcont_all$parameterLabel == "Chlorophyll" & 
                    is.na(wqcont_all$site_code) == F & is.na(wqcont_all$Datetime) == F,], 
       aes(x = Param_val, y = dist,fill = dist, color = dist)) + 
  geom_boxplot(aes(group = dist), show.legend = F, alpha = .2) + theme_bw() +
  # geom_line(aes(group = site_code), show.legend = F, alpha = .8, linewidth = 1,
  # stat = "smooth", method = "loess", span = .1) +
  labs(x = NULL, y = "Chlorophyll-a") + 
  scale_fill_viridis_c(option = "A", begin = .1, end = .8) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8)

ggplot(chlply[chlply$Date > as.POSIXct("2023-6-1"),], 
       aes(x = medchl, y = dist, fill = dist, color = dist)) + 
  geom_boxplot(aes(group = dist), show.legend = F, alpha = .2) + theme_bw() +
  labs(x = NULL, y = "Chlorophyll-a") + 
  scale_fill_viridis_c(option = "A", begin = .1, end = .8) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8)

wqcont_all$Date <- as.Date(wqcont_all$Datetime)
dput(unique(wqcont_all$parameterLabel))

wqcontply <- wqcont_all %>% 
  filter(parameterLabel %in% c("Electrical_Conductivity_at_25C", "pH", 
                               "Turbidity", "Dissolved_Oxygen", 
                               "Fluorescent_Dissolved_Organic_Matter", "Chlorophyll")) %>% 
  group_by(Date, dist, site_code, parameterLabel, varfac) %>% 
  summarize(medval = median(Param_val))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig6a_Continuous_vars%03d.png", sep = ""), 
                        height = 4, width = 6.5, unit = "in", res = 1000, family = "serif")}

ggplot(wqcontply[wqcontply$Date > as.POSIXct("2023-6-1"),], 
       aes(y = medval, x = dist, fill = dist, color = dist)) + 
  geom_boxplot(aes(group = dist), show.legend = F, alpha = .2) + theme_bw() +
  labs(x = "Distance from Rio Vista (km)", y = "Parameter value") + scale_x_reverse() +
  facet_wrap(varfac ~ ., scales = "free_y", labeller = label_parsed) +
  scale_fill_viridis_c(option = "A", begin = .1, end = .8) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8)

ggplot(wqcontply[wqcontply$Date > as.POSIXct("2023-6-1") & wqcontply$parameterLabel == "Chlorophyll",], 
       aes(y = medval, x = Date, color = dist, group = dist)) + 
  geom_line(show.legend = F, alpha = .6) + theme_bw() +
  labs(x = "Distance from Rio Vista (km)", y = "Parameter value") +
  facet_wrap(parameterLabel ~ ., scales = "free_y") +
  scale_fill_viridis_c(option = "A", begin = .1, end = .8) +
  scale_color_viridis_c(option = "A", begin = .1, end = .8)

dev.off()


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

ggplot(wqcont_wdl[wqcont_wdl$Datetime > as.POSIXct("2023-6-1") & 
                     is.na(wqcont_wdl$site_code) == F & is.na(wqcont_wdl$Datetime) == F,], 
       aes(x = Datetime, y = Value)) + geom_line() + theme_bw() +
  facet_grid(Variable ~ site_code, scales = "free_y") + 
  scale_x_datetime(breaks = "1 month", date_labels = "%b-%d")

if(saveOutput == T){dev.off()}


# Contaminants WQ ---------------------------------------------------------

contam <- readxl::read_excel("data/YoloWaterPesticideResults_2023_Preliminary_to_DWR.xlsx")
contam_lookup <- readxl::read_excel("data/OCRL_MDL_RL_AnalyteList_06062022.xlsx", skip = 4) %>% 
  janitor::clean_names() %>% select(c(compound, chemical_class, pesticide_type, pesticide_group))

contam <- contam[is.na(contam$Project) == F,]
contam$Date <- as.Date(contam$`Start date`)
contam_long <- contam %>% select(-c("Atrazine-13C3", "Fipronil-13C4,15N2", "Imidacloprid-d4", "Metolachlor-13C6", 
                                    "p,p'-DDE-13C12", "Permethrin-13C6", "Tebuconazole-13C3", "Trifluralin-d14")) %>% 
  pivot_longer(c("3,4-DCA", "3,5-DCA", "Acetamiprid", "Acetochlor", "Acibenzolar-S-Methyl", 
                                        "Allethrin", "Atrazine", "Atrazine, Desethyl", "Atrazine, Desisopropyl", 
                                        "Azoxystrobin", "Benefin", "Bentazon", "Benzobicyclon", "Benzovindiflupyr", 
                                        "Bifenthrin", "Boscalid", "Boscalid Metabolite - M510F01 Acetyl", 
                                        "Broflanilide", "Bromuconazole", "Butralin", "Carbaryl", "Carbendazim", 
                                        "Carbofuran", "Chlorantraniliprole", "Chlorfenapyr", "Chlorothalonil", 
                                        "Chlorpyrifos", "Chlorpyrifos Oxon", "Clomazone", "Clothianidin", 
                                        "Clothianidin Desmethyl", "Coumaphos", "Cyantraniliprole", "Cyazofamid", 
                                        "Cyclaniliprole", "Cycloate", "Cyfluthrin", "Cyhalofop-Butyl", 
                                        "Cyhalothrin", "Cymoxanil", "Cypermethrin", "Cyproconazole", 
                                        "Cyprodinil", "DCPA", "DCPMU", "DCPU", "Deltamethrin", "Desthio-Prothioconazole", 
                                        "Diazinon", "Diazinon Oxon", "Dichlorvos", "Difenoconazole", 
                                        "Dimethomorph", "Dinotefuran", "Dithiopyr", "Diuron", "EPTC", 
                                        "Esfenvalerate", "Ethaboxam", "Ethalfluralin", "Etofenprox", 
                                        "Etoxazole", "Famoxadone", "Fenamidone", "Fenbuconazole", "Fenhexamid", 
                                        "Fenpropathrin", "Fenpyroximate", "Fipronil", "Fipronil Desulfinyl", 
                                        "Fipronil Desulfinyl Amide", "Fipronil Sulfide", "Fipronil Sulfone", 
                                        "Flonicamid", "Florpyrauxifen-Benzyl", "Fluazinam", "Fludioxonil", 
                                        "Flufenacet", "Fluindapyr", "Flumetralin", "Fluopicolide", "Fluopyram", 
                                        "Fluoxastrobin", "Flupyradifurone", "Fluridone", "Flutolanil", 
                                        "Flutriafol", "Fluxapyroxad", "Halauxifen-methyl ester", "Hexazinone", 
                                        "Imazalil", "Imazosulfuron", "Imidacloprid", "Imidacloprid desnitro", 
                                        "Imidacloprid Olefin", "Imidacloprid Urea", "Imidacloprid, 5-Hydroxy", 
                                        "Indaziflam", "Indoxacarb", "Ipconazole", "Iprodione", "Isofetamid", 
                                        "Kresoxim-Methyl", "Malathion", "Malathion Oxon", "Mandestrobin", 
                                        "Mandipropamid", "Metalaxyl", "Metalaxyl Alanine Metabolite", 
                                        "Metconazole", "Methoprene", "Methoxyfenozide", "Metolachlor", 
                                        "Myclobutanil", "Naled (Dibrom)", "Napropamide", "Nitrapyrin", 
                                        "Novaluron", "Oryzalin", "Oxadiazon", "Oxathiapiprolin", "Oxyfluorfen", 
                                        "p,p'-DDD", "p,p'-DDE", "p,p-DDT", "Paclobutrazol", "Pendimethalin", 
                                        "Penoxsulam", "Pentachloroanisole (PCA)", "Pentachloronitrobenzene (PCNB)", 
                                        "Penthiopyrad", "Permethrin", "Phenothrin", "Phosmet", "Picarbutrazox", 
                                        "Picoxystrobin", "Piperonyl Butoxide", "Prodiamine", "Prometon", 
                                        "Prometryn", "Propanil", "Propargite", "Propiconazole", "Propyzamide", 
                                        "Pydiflumetofen", "Pyraclostrobin", "Pyridaben", "Pyrimethanil", 
                                        "Pyriproxyfen", "Quinoxyfen", "Sedaxane", "Simazine", "Sulfoxaflor", 
                                        "Tebuconazole", "Tebuconazole t-Butylhydroxy", "Tebufenozide", 
                                        "Tebupirimfos", "Tebupirimfos Oxon", "Tefluthrin", "Tetraconazole", 
                                        "Tetramethrin", "t-Fluvalinate", "Thiabendazole", "Thiacloprid", 
                                        "Thiamethoxam", "Thiamethoxam Degradate (CGA-355190)", "Thiamethoxam Degradate (NOA-407475)", 
                                        "Thiobencarb", "Tolfenpyrad", "Triadimefon", "Triadimenol", "Triallate", 
                                        "Tribufos", "Trifloxystrobin", "Triflumizole", "Trifluralin", 
                                        "Triticonazole", "Valifenalate", "Zoxamide"),
                                       names_to = "compound")
contam_long$compound2 <- tolower(contam_long$compound)
contam_lookup$compound2 <- tolower(contam_lookup$compound)
contam_merge <- merge(merge(contam_long, contam_lookup, by = "compound2", all.x = T),
                      stations[, c("station_name", "dist")], by.x = "Site", by.y = "station_name", all.x = T)
contam_merge <- contam_merge[is.na(contam_merge$value) == F,]

unique(contam_merge[is.na(contam_merge$chemical_class) == TRUE, "compound2"])

# Add week to contam_merge data for plotting individual transects
contam_merge$week <- format(contam_merge$Date, format = "%W")

# Convert week to transect number
contam_merge$transect <- as.character(as.integer(as.factor(contam_merge$week)))

contam_merge$date <- ifelse(contam_merge$transect == 1, "06-26", 
                        ifelse(contam_merge$transect == 2, "07-10", 
                               ifelse(contam_merge$transect == 3, "07-25", 
                                      ifelse(contam_merge$transect == 4, "08-08", 
                                             ifelse(contam_merge$transect == 5, "08-23", 
                                                    ifelse(contam_merge$transect == 6, "09-05", 
                                                           ifelse(contam_merge$transect == 7, "09-19", "10-03")))))))

contam_type <- contam_merge %>% group_by(Site, dist, date, pesticide_group) %>% 
  summarize(totconc = sum(value))
unique(contam_type$Site)
contam_type$site_fac <- factor(contam_type$Site, levels = c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR"))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig8_Contaminant_wq%03d.png", sep = ""), 
                        height = 6, width = 7.5, unit = "in", res = 1000, family = "serif")}
ggplot(contam_type, aes(x = site_fac, y = totconc), fill = "black") + 
  geom_bar(stat = "identity", position = "dodge", show.legend = F) + 
  facet_grid(pesticide_group ~ date, scales = "free") +
  # scale_fill_viridis_c(option = "C", begin = .05, end = .91) +
  labs(y = "Total concentration (ng/L)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1))

ggplot(contam_type, aes(x = date, y = totconc), fill = "black") + 
  geom_bar(stat = "identity", position = "dodge", show.legend = F) + 
  facet_grid(pesticide_group ~ site_fac, scales = "free") +
  # scale_fill_viridis_c(option = "C", begin = .05, end = .91) +
  labs(y = "Total concentration (ng/L)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1))
# if(saveOutput == T){dev.off()}

##Contaminants

contam_id <- contam_merge %>% group_by(Site, dist, compound2, pesticide_group) %>% 
  summarize(totconc = sum(value))
contam_id$site_fac <- factor(contam_id$Site, levels = c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR"))

# if(saveOutput == T){png(paste("figures/NDFS2023_Fig8a_Contaminant_wq%03d.png", sep = ""), 
#                         height = 6, width = 7, unit = "in", res = 1000, family = "serif")}

ggplot(contam_id, aes(x = reorder(compound2, -totconc), y = totconc)) + 
  geom_bar(stat = "identity") + scale_y_log10() + facet_grid(site_fac ~ pesticide_group, scales = "free") +
  labs(y = "Total concentration (ng/L)", x = "Compound") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(contam_id, aes(x = reorder(compound2, -totconc), y = totconc, fill = pesticide_group)) + 
  geom_bar(stat = "identity") + scale_y_log10() + facet_grid(site_fac ~ ., scales = "fixed") +
  labs(y = "Total concentration (ng/L)", x = "Compound", fill = "Pesticide class") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + theme(axis.text.x = element_text(angle = 55, hjust = 1),
                     legend.position = "bottom")

if(saveOutput == T){dev.off()}

contam_bool <- contam_merge %>% group_by(Site, compound2) %>% 
  summarize(sumconc =1) %>% group_by(Site) %>% summarize(tot = sum(sumconc))
contam_bool
contam_bool$sumconc <- 1

contam_datebool <- contam_merge %>% group_by(Site, pesticide_group, compound2, week, date) %>% 
  summarize(sumconc = 1, actual_res = value) %>% group_by(Site, week, date) %>% 
  summarize(tot = sum(sumconc), totconc = sum(actual_res))

contam_bool$site_fac <- factor(contam_bool$Site, levels = rev(c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR")))

contam_datebool$site_fac <- factor(contam_datebool$Site, levels = rev(c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR")))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig8_contaminant_wqbool%03d.png", sep = ""),
                        height = 5, width = 7, unit = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ggplot(contam_datebool, aes(x = date, y = site_fac, fill = tot)) + 
                     geom_tile(show.legend = F) +
                     # scale_fill_viridis_c(option = "C", begin = .2) + 
                     labs(x = NULL, y = NULL) +
                     scale_fill_gradient(low = "#8BD3E6", high = "#FF6D6A") + 
                     theme_bw() + geom_text(aes(label = tot)),
                   ggplot(contam_bool, aes(x = "all", y = site_fac, fill = tot)) + 
                     geom_tile(show.legend = F) +
                     # scale_fill_viridis_c(option = "C", begin = .2) + 
                     labs(x = NULL, y = NULL) +
                     scale_fill_gradient(low = "#8BD3E6", high = "#FF6D6A") + 
                     theme_bw() + geom_text(aes(label = tot)) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                   nrow = 1, rel_widths = c(9,1.5))

dev.off()

# Contaminants Zoop -------------------------------------------------------

zoopcontam <- readxl::read_excel("data/YoloWaterandZoopPesticideResults_2023_Preliminary_to_DWR.xlsx", sheet = "Zoop")
zoopcontam_lookup <- readxl::read_excel("data/OCRL_MDL_RL_AnalyteList_06062022.xlsx", skip = 4) %>% 
  janitor::clean_names() %>% select(c(compound, chemical_class, pesticide_type, pesticide_group))

# zoopcontam <- zoopcontam[is.na(contam$Project) == F,]
zoopcontam$Date <- as.Date(zoopcontam$`Start date`)
dput(colnames(zoopcontam))
zoopcontam_long <- zoopcontam %>% select(-c("Atrazine-13C3", "Fipronil-13C4,15N2", "Imidacloprid-d4", "Metolachlor-13C6", 
                                    "p,p'-DDE-13C12", "Permethrin-13C6", "Tebuconazole-13C3", "Trifluralin-d14")) %>% 
  reshape2::melt(id.vars = c("Lab ID", "Project", "Site", "USGS Site Name", "USGS Site Number", 
                             "Medium", "QC type", "Start date", "Start time", "Sample Mass (g)", "Date"), variable.name = "compound", value.name = "Result")
               
zoopcontam_long$compound2 <- tolower(zoopcontam_long$compound)
zoopcontam_lookup$compound2 <- tolower(zoopcontam_lookup$compound)

zoopcontam_merge <- merge(merge(zoopcontam_long, zoopcontam_lookup, by = "compound2", all.x = T),
                      stations[, c("station_name", "dist")], by.x = "Site", by.y = "station_name", all.x = T)
zoopcontam_merge[is.na(zoopcontam_merge$pesticide_type),]
zoopcontam_merge <- zoopcontam_merge[is.na(zoopcontam_merge$Result) == F,]

unique(zoopcontam_merge[is.na(zoopcontam_merge$chemical_class) == TRUE, "compound2"])

# Add week to zoopcontam_merge data for plotting individual transects
zoopcontam_merge$week <- format(zoopcontam_merge$Date, format = "%W")

# Convert week to transect number
zoopcontam_merge$transect <- as.character(as.integer(as.factor(zoopcontam_merge$week)))

zoopcontam_merge$date <- ifelse(zoopcontam_merge$transect == 1, "06-26", 
                            ifelse(zoopcontam_merge$transect == 2, "07-10", 
                                   ifelse(zoopcontam_merge$transect == 3, "07-25", 
                                          ifelse(zoopcontam_merge$transect == 4, "08-08", 
                                                 ifelse(zoopcontam_merge$transect == 5, "08-23", 
                                                        ifelse(zoopcontam_merge$transect == 6, "09-05", 
                                                               ifelse(zoopcontam_merge$transect == 7, "09-19", "10-03")))))))

zoopcontam_type <- zoopcontam_merge %>% group_by(Site, dist, date, pesticide_group) %>% 
  summarize(totconc = sum(Result))

unique(zoopcontam_type$Site)
zoopcontam_type$site_fac <- factor(zoopcontam_type$Site, levels = c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR"))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig9_Contaminant_zoop%03d.png", sep = ""), 
                        height = 6, width = 7.5, unit = "in", res = 1000, family = "serif")}
ggplot(zoopcontam_type, aes(x = site_fac, y = totconc), fill = "black") + 
  geom_bar(stat = "identity", position = "dodge", show.legend = F) + 
  facet_grid(pesticide_group ~ date, scales = "free") +
  # scale_fill_viridis_c(option = "C", begin = .05, end = .91) +
  labs(y = "Total concentration (ng/g)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1))

ggplot(zoopcontam_type, aes(x = site_fac, y = totconc, fill = pesticide_group)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(. ~ date, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Total concentration (ng/g)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1))

ggplot(zoopcontam_type, aes(x = date, y = totconc), fill = "black") + 
  geom_bar(stat = "identity", position = "dodge", show.legend = F) + 
  facet_grid(pesticide_group ~ site_fac, scales = "free") +
  # scale_fill_viridis_c(option = "C", begin = .05, end = .91) +
  labs(y = "Total concentration (ng/g)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1))

ggplot(zoopcontam_type, aes(x = date, y = totconc, fill = pesticide_group)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(. ~ site_fac, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Total concentration (ng/g)", x = NULL) +
  theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1), legend.position = "bottom")

# if(saveOutput == T){dev.off()}

##zoopcontaminants

zoopcontam_id <- zoopcontam_merge %>% group_by(Site, dist, compound2, pesticide_group) %>% 
  summarize(totconc = sum(Result))
zoopcontam_id$site_fac <- factor(zoopcontam_id$Site, levels = c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR"))

if(saveOutput == T){png(paste("figures/NDFS2023_Fig8a_zoopcontaminant_wq%03d.png", sep = ""),
                        height = 6, width = 7, unit = "in", res = 1000, family = "serif")}

ggplot(zoopcontam_id, aes(x = reorder(compound2, -totconc), y = totconc)) + 
  geom_bar(stat = "identity") + scale_y_log10() + facet_grid(site_fac ~ pesticide_group, scales = "free") +
  labs(y = "Total concentration (ng/g)", x = "Compound") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(zoopcontam_id, aes(x = reorder(compound2, -totconc), y = totconc, fill = pesticide_group)) + 
  geom_bar(stat = "identity") + scale_y_log10() + facet_grid(site_fac ~ ., scales = "fixed") +
  labs(y = "Total concentration (ng/g)", x = "Compound", fill = "Pesticide class") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + theme(axis.text.x = element_text(angle = 55, hjust = 1),
                     legend.position = "bottom")

if(saveOutput == T){dev.off()}

zoopcontam_bool <- zoopcontam_merge %>% group_by(Site, compound2) %>% 
  summarize(sumconc =1) %>% group_by(Site) %>% summarize(tot = sum(sumconc))
zoopcontam_bool
zoopcontam_bool$sumconc <- 1

zoopcontam_datebool <- zoopcontam_merge %>% group_by(Site, pesticide_group, compound2, week, date) %>% 
  summarize(sumconc = 1, actual_res = Result) %>% group_by(Site, week, date) %>% 
  summarize(tot = sum(sumconc), totconc = sum(actual_res))

zoopcontam_bool$site_fac <- factor(zoopcontam_bool$Site, levels = rev(c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR")))

zoopcontam_datebool$site_fac <- factor(zoopcontam_datebool$Site, levels = rev(c("RCS", "RD22", "LIS", "STTD", "BL5", "RYI","SHR")))

ggplot(zoopcontam_datebool, aes(x = week, y = tot)) + geom_bar(stat = "identity") + facet_wrap(Site ~ .)
ggplot(zoopcontam_datebool, aes(x = week, y = totconc)) + geom_bar(stat = "identity") + facet_wrap(Site ~ .)


if(saveOutput == T){png(paste("figures/NDFS2023_Fig10_zoopcontaminant_wq%03d.png", sep = ""),
                        height = 5, width = 7, unit = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ggplot(zoopcontam_datebool, aes(x = date, y = site_fac, fill = tot)) + 
  geom_tile(show.legend = F) +
  # scale_fill_viridis_c(option = "C", begin = .2) + 
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "#8BD3E6", high = "#FF6D6A") + 
  theme_bw() + geom_text(aes(label = tot)),
ggplot(zoopcontam_bool, aes(x = "all", y = site_fac, fill = tot)) + 
  geom_tile(show.legend = F) +
  # scale_fill_viridis_c(option = "C", begin = .2) + 
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "#8BD3E6", high = "#FF6D6A") + 
  theme_bw() + geom_text(aes(label = tot)) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
nrow = 1, rel_widths = c(9,1.5))

dev.off()
ggplot(zoopcontam_datebool, aes(x = date, y = site_fac, fill = totconc)) + geom_tile() +
  scale_fill_viridis_c(option = "C", begin = .2) + theme_bw() + geom_text(aes(label = round(totconc)))

ggplot(zoopcontam_datebool, aes(x = week, y = Site, fill = tot)) + geom_tile()

unique(wqlmerge$parameter)
silica <- wqlmerge[wqlmerge$parameter == "Dissolved Silica (SiO2)", c("station_name.y", "result", "week")]

silicamean <- silica %>% group_by(station_name.y, week) %>% summarize(meanres = mean(result, na.rm = T)) %>% data.frame()

zoopcontam_dateboolmergesi <- merge(zoopcontam_datebool, silicamean, 
                                  by.x = c("Site", "week"), by.y = c("station_name.y", "week"), all.x = T)
if(saveOutput == T){png(paste("figures/NDFS2023_Fig10_Contaminant_zoop_and_silica%03d.png", sep = ""), 
                        height = 4, width = 6, unit = "in", res = 1000, family = "serif")}
ggplot(zoopcontam_dateboolmergesi, aes(x = meanres, y = tot)) + geom_point(aes(color = Site)) + theme_bw() +
  stat_smooth(method = "lm", aes(color = Site), se = F) + stat_smooth(method = "lm", color = "black") +
  scale_color_brewer(palette = "Set1") + labs(title = "Silicate and contaminants relationship", 
                                              y = "Total contaminants detected", x = "Silicate concentration")
dev.off()
tss <- wqlmerge[wqlmerge$parameter == "Total Suspended Solids", c("station_name.y", "result", "week")]

tssmean <- tss %>% group_by(station_name.y, week) %>% summarize(meanres = mean(result, na.rm = T)) %>% data.frame()

zoopcontam_dateboolmergetss <- merge(zoopcontam_datebool, tssmean, 
                                  by.x = c("Site", "week"), by.y = c("station_name.y", "week"), all.x = T)
# good one: "Dissolved tss (SiO2)", 
ggplot(zoopcontam_dateboolmergesi, aes(x = meanres, y = tot)) + geom_point(aes(color = Site)) + theme_bw() +
  stat_smooth(method = "lm", aes(color = Site), se = F) + stat_smooth(method = "lm", color = "black", linewidth = 2) +
  scale_color_brewer(palette = "Set1")

summary(lm(tot ~ meanres * Site, data = zoopcontam_dateboolmergesi))

summary(lm(tot ~ meanres, data = zoopcontam_dateboolmergesi[zoopcontam_dateboolmergesi$Site == "RD22",]))
# Predictive model --------------------------------------------------------

# Zoop score as response variable, Explanatory variables: Do range, Turb, CHL, etc.
