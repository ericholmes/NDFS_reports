## Zoop scraper
require(readxl)
library(tidyverse)

# Load and compile data ---------------------------------------------------

files <- list.files("data/zoopraw", pattern = ".csv")

zoopdat <- data.frame()

for(file in files){
  print(file)
  
  temp <- read.csv(paste0("data/zoopraw/", file))
  print(temp$date[1])
  colnames(temp)[1] <- "project" #temp fix since first column name in file 3 did not match previous columns
  if(file == files[3]){
    temp$date <- as.Date(temp$date, format = "%m/%d/%Y")
  }
  else{temp$date <- as.Date(temp$date)}
  zoopdat <- rbind(zoopdat, temp)
  rm(temp)
}

range(zoopdat$date)

# NOTE: SHR was assigned -10 for visualization purposes even though it is actually upstream of RVB
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

# Add rotations data ------------------------------------------------------

ltdnames <- colnames(janitor::clean_names(readxl::read_excel("data/YBFMP_LowerTrophic_Data_WORKING_20231102.xlsx", skip = 1)))

ltd <- readxl::read_excel("data/YBFMP_LowerTrophic_Data_WORKING_20231102.xlsx", skip = 3, col_names = ltdnames)

ltd$year <- format(ltd$sampling_event_date, format = "%Y")
ltd$rotations <- abs(ltd$flow_meter_end_150 - ltd$flow_meter_start_150)

ltd <- ltd[is.na(ltd$flow_meter_end_150) == F & ltd$year == 2023, c("measuring_program_short_name", "sampling_event_date", "sampling_event_time", 
                                                                    "sampling_area_number", "sampling_event_number", "condition_code", 
                                                                    "sampling_altered", "net_type", "start_time_150", "stop_time_150", 
                                                                    "set_time_150","flow_meter_start_150", "flow_meter_end_150", "flow_meter_speed", 
                                                                     "field_comments",  "year", "rotations")]


ltd$rotations <- ifelse(ltd$rotations > 990000, 1000000 - ltd$flow_meter_end_150 + ltd$flow_meter_start_150, ltd$rotations)
ltd$sampling_event_date <- as.Date(ltd$sampling_event_date)

ltd <- data.frame(ltd)

zoop <- merge(zoopdat[zoopdat$net_size_um == "150",], as.data.frame(ltd), 
              by.x = c("station", "date"), by.y = c("sampling_area_number", "sampling_event_date"), all.x = T)
zoop$subsample <- ifelse(zoop$subsample == 0, 10, zoop$subsample) # missing subsamples, verified to be 10
# CPUE calculation --------------------------------------------------------

zoop$NetAreaZoop = pi*.25^2
zoop$volume <- ifelse(zoop$flow_meter_speed == "Regular",
                      zoop$rotations * 26873 * zoop$NetAreaZoop/999999,
                      ifelse(zoop$flow_meter_speed == "Low",
                             zoop$rotations * 57560 * zoop$NetAreaZoop/999999, NA))

zoop$sampfraction <- ifelse(zoop$category == "MICROZOOPLANKTON & NAUPLII",
                            zoop$subsample*zoop$sub2_ml/zoop$v2_ml,
                            zoop$subsample*zoop$sub1_ml/zoop$v1_ml)

zoop$CPUE <-  round((zoop$count/zoop$sampfraction)/zoop$volume,3)

ggplot(zoop, aes(x = CPUE)) + geom_histogram() + scale_x_log10()
ggplot(zoop, aes(x = CPUE)) + geom_histogram() + scale_x_log10() + facet_wrap(station ~ .)



# save(zoop, file = "data/NDFS2023_zoop_precleaned.Rdata")

# Analysis ----------------------------------------------------------------
zoop$jday <- as.integer(format(zoop$date, format = "%j"))
zoop$Week <- as.integer(format(zoop$date, format = "%W"))
zoop$year <- as.integer(format(zoop$date, format = "%Y"))

zoop <- zoop[zoop$count>0,]

##Individual taxa counts
zoopindply <- zoop %>% filter(CPUE > 0) %>% group_by(station, Week, jday, date, year, taxon, category) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup()

# Zoop bar and area plots
unique(zoop$category)
zoopply <- zoop %>% filter(CPUE > 0) %>% group_by(station, Week, jday, date, year, category) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup()

zoopbarply <- zoop[zoop$category != "MICROZOOPLANKTON & NAUPLII",] %>% 
  group_by(station, Week, jday, date, year, category) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup() %>% group_by(station, Week, year) %>% mutate(perc = sumtot/sum(sumtot))
zoopbarply$station <- factor(zoopbarply$station, levels = 
                                  c("RCS", "WWT", "RD22", "I80", "LIS", "STTD","BL5", "PRS", "LIB", "RYI", "RVB", "SHR"))

(zoopbarperc <- ggplot(zoopbarply[zoopbarply$Week %in% 25:40 & is.na(zoopbarply$station) == F,], aes(x = date, y = perc)) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)) + 
    labs(fill = NULL, y = "Proportion", x = NULL) +
    geom_bar(aes(fill = category), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station, space = "free"))

(zoopbarsum <- ggplot(zoopbarply[zoopbarply$Week %in% 25:40 & is.na(zoopbarply$station) == F,], aes(x = date, y = sumtot)) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(fill = NULL, y = "Density (ind*m^-3)",x = NULL) +
    geom_bar(aes(fill = category), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station, space = "free"))
zoop$station <- factor(zoop$station, levels = 
                               c("RCS", "WWT", "RD22", "I80", "LIS", "STTD","BL5", "PRS", "LIB", "RYI", "RVB", "SHR"))
(zoopbarraw <- ggplot(zoop[zoop$Week %in% 25:40 & is.na(zoopbarply$station) == F &
                             zoop$category != "MICROZOOPLANKTON & NAUPLII",], aes(x = date, y = CPUE)) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(fill = NULL, y = "Density (ind*m^-3)",x = NULL) +
    geom_bar(aes(fill = category), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station, space = "free"))

# (zoopbarsum <- ggplot(zoopbarply[zoopbarply$Week %in% 25:40 & is.na(zoopbarply$station) == F,], aes(x = date, y = sumtot)) + 
#     scale_fill_brewer(palette = "Set1") + theme_bw() + 
#     theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)) +
#     scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#     scale_y_log10() +
#     labs(fill = NULL, y = "Density (ind*m^-3)",x = NULL) +
#     geom_bar(aes(fill = category), stat = "identity", position = "stack", width = 15) + facet_grid(. ~ station, space = "free"))

png(paste("figures/NDFS2023_Fig4_zoopbars_preclean%03d.png", sep = ""), 
    height = 7.5, width = 8, unit = "in", res = 1000)

cowplot::plot_grid(zoopbarperc + theme(legend.position = "none", axis.text.x = element_blank()), 
                   zoopbarsum, nrow = 2, rel_heights = c(9,10), 
                   align = "v", labels = c("A", "B"))

dev.off()

# NMDS
library(vegan)
library(ggrepel)
zoopcast <- reshape2::dcast(zoopply[zoopply$Week %in% 25:40, ],
                            formula = station + jday + date + year ~ category, value.var = 'sumtot', fun.aggregate = sum, fill = 0)

zoopcast <- zoopcast[is.na(zoopcast$station) == F & is.na(zoopcast$year) == F & zoopcast$station != "SHR",]
# melt back to long form with the added zeros from the previous casting step
# zoopply <- melt(zoopcast, id.vars = c("station", "jday", "year"), value.name = 'total', variable.name = "category")

com2016 <- decostand(zoopcast[, c(5:9)], method = "hellinger")
nmds2016 <- metaMDS(com2016, autotransform = F, na.rm = T)

data.scores16 <- as.data.frame(scores(
  nmds2016, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores16$station <- zoopcast[, "station"] #create a column of site names, from the rownames of data.scores
data.scores16$jday <- zoopcast[, "jday"]
data.scores16$date <- zoopcast[, "date"]
data.scores16 <- merge(data.scores16, stations, by.x = "station", by.y = "station_name", all.x = T)
data.scores16$distsite <- paste(sprintf("%02d",round(data.scores16$dist)), data.scores16$station, sep = "-")
species.scores16 <- as.data.frame(scores(nmds2016, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores16$species <- rownames(species.scores16)  # create a column of species, from the rownames of species.scores

png("figures/NDFS_zoop_NMDS_dist_preclean_%03d.png",
    family = "serif", height = 5, width = 6, units = "in", res = 1000)
range(data.scores16$NMDS1)
range(data.scores16$NMDS2)
(nmdsplot <- ggplot() + 
  stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = distsite, group = station), geom = "polygon", alpha = .1, type = "t") +
  stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, group = station), alpha = .9) +
  geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, shape = distsite)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = 1.3, color = "white") +
  geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) + 
    coord_cartesian(xlim = range(data.scores16$NMDS1)*1.2,
                    ylim = range(data.scores16$NMDS2)*1.3) +
  # coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) + 
  # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distsite, group = station)) +
  geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = 1, bg.color = "white",
                  bg.r = 0.2) +
  scale_color_viridis_d(option = "A", begin = .1, end = .8)+
  scale_fill_viridis_d(option = "A", begin = .1, end = .8) + 
  scale_shape_manual(values = 1:10) +
  theme_bw(base_family = "serif") + theme(legend.title = element_blank()) )

(nmdsboxplot <- ggplot(data.scores16, aes(x = NMDS1, y = station)) + geom_boxplot(aes(fill = distsite)) + theme_bw() +
  scale_fill_viridis_d(option = "A", begin = .1, end = .8) + coord_cartesian(xlim = range(data.scores16$NMDS1)*1.2))
library(ggridges)
(nmdsridgeplot <- ggplot(data.scores16, aes(x = NMDS1, y = distsite)) + geom_density_ridges(aes(fill = distsite), show.legend = F) + theme_bw() +
    scale_fill_viridis_d(option = "A", begin = .1, end = .8) + coord_cartesian(xlim = range(data.scores16$NMDS1)*1.2) + labs(y = NULL))

cowplot::plot_grid(nmdsplot, 
                   nmdsridgeplot, nrow = 2, align = "v", axis = "lr", rel_heights = c(7,4))

dev.off()

(nmdsridgeplot_2 <- ggplot(data.scores16, aes(x = NMDS2, y = distsite)) + geom_density_ridges(aes(fill = distsite), show.legend = F) + theme_bw() +
    scale_fill_viridis_d(option = "A", begin = .1, end = .8) + coord_cartesian(xlim = range(data.scores16$NMDS2)*1.3) + labs(y = NULL) + coord_flip())

cowplot::plot_grid(nmdsridgeplot_2, nmdsplot, 
                   NULL, nmdsridgeplot,
                   ncol = 2, byrow = T,
                   nrow = 2, align = "hv", axis = "lr", rel_widths = c(9,10,9,10))

# Seasonal trend mixed effects model --------------------------------------

library(lme4)
library(lmerTest)

zooptotply <- zoop[zoop$category != "MICROZOOPLANKTON & NAUPLII",] %>% filter(CPUE > 0 & Week %in% 26:40) %>% 
  group_by(station, Week, jday, date, year) %>% 
  summarize(sumtot = sum(CPUE, na.rm = T)) %>% ungroup()

zooptotply$logtot <- log(zooptotply$sumtot + 1)

lme_seas_int <- lmer(formula = "logtot ~ jday + (1|station)", data = zooptotply, REML = F)
lme_seas_slope <- lmer(formula = "logtot ~ 1 + (jday|station)", data = zooptotply, REML = F)

summary(lme_seas_int)
coef(lme_seas_int)

summary(lme_seas_slope)
coef(lme_seas_slope)

AIC(lme_seas_int, lme_seas_slope)

ggplot(zooptotply, aes(x = jday, y = logtot, color = station)) + 
  theme_bw() +
  geom_point() + stat_smooth(method = "lm", se = F)

newdat <- data.frame(jday = rep(seq(from = range(zooptotply$jday)[1],
                                   to = range(zooptotply$jday)[2], length.out = 100),10),
                     station = rep(unique(zooptotply$station)[1:10], each = 100))

newdat$stationfac = as.integer(newdat$station)
zooptotply$stationfac = as.integer(zooptotply$station)

# pal <- RColorBrewer::brewer.pal(n=10,"Set3")
pal <- viridis::viridis(10, option = "A", begin = .1, end = .8)
zooptotply <- merge(zooptotply, data.frame(station = unique(newdat$station), stationpal = pal, by = "station", all.x = T))
newdat <- merge(newdat, data.frame(station = unique(newdat$station), stationpal = pal, by = "station", all.x = T))
plot(logtot ~ jday, data = zooptotply, cex = 0, las = 1, bty = "n")

for(i in unique(newdat$station)){
  points(newdat[newdat$station == i,"jday"],
         predict(lme_seas_int, newdata = newdat[newdat$station == i,], type = "response"),
         pch = newdat[newdat$station == i,"stationfac"], cex = .2, col = newdat[newdat$station == i,"stationpal"], type = "l")
}

points(logtot ~ jday, data = zooptotply, col = stationpal, pch = stationfac)
# legend("topright", legend = 1:9, pch = 1:9, col = pal, bty = "n", ncol = 3)

plot(logtot ~ jday, data = zooptotply, cex = 0, las = 1, bty = "n")

for(i in unique(newdat$station)){
  points(newdat[newdat$station == i,"jday"],
         predict(lme_seas_slope, newdata = newdat[newdat$station == i,], type = "response"),
         pch = newdat[newdat$station == i,"stationfac"], cex = .2, col = newdat[newdat$station == i,"stationpal"], type = "l")
}

points(logtot ~ jday, data = zooptotply, col = stationpal, pch = stationfac)
