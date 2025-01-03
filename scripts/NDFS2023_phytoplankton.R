## NDFS 2023 Phyto data analysis
# Prepare working environment ----

library(tidyverse)
library(viridis)
library(vegan)
library(readxl)

## Should output be saved?
save_output <- T

## Set color palette for major algal groups ----
algalpal = c("Chrysophytes" = "#E41A1C", 
             "Cryptophytes" =  "#FF7F00",
             "Dinoflagellates" = "#A65628", 
             "Euglenoids" =  "#FFFF33",
             "Cyanobacteria" = "#377EB8", 
             "Green Algae" = "#4DAF4A", 
             "Pennate Diatoms" = "#984EA3", 
             "Centric Diatoms" = "#F781BF", 
             "other" = "#999999")

## Create functions ----

## Create Shannon-Weiner Diversity Index function
swi <- function(pop){-sum((pop/sum(pop)*log(pop/sum(pop))))}
## Create unit density function
unit_density <- function(){unit_abundance_number_of_natural_units*factor}
## Create phytoplankton biovol density function
biovol_density <- function(){total_number_of_cells*biovolume_1*factor}

# Load datasets -----------------------------------------------------------

phyto <- read_excel("data/20240524 DWR YOLO BYPASS PHYTOS - OUTPUT.xlsx")
# calculations: unit_density = unit_abundance_number_of_natural_units*factor,
#              biovol_density = total_number_of_cells*biovolume_1*factor
sites <- read.csv("data/NDFS_longitudinal_dist.csv")
phyto <- janitor::clean_names(phyto)
# Remove duplicate samples (these are used for methodology QC in the lab)
phyto <- phyto[is.na(phyto$sample_date) == F & 
                 !grepl(phyto$station_code, pattern = "DUP") &
                 !grepl(phyto$station_code, pattern = "Dup"),]

phyto$sample_date <- as.Date(phyto$sample_date)

# phyto_lookup <- read.csv("Nutrient-Phyto Manuscript/data/phyto_classification.csv")
phyto_lookup <- readxl::read_excel("data/PESP_taxalookup_eholmes.xlsx")

phyto_lookup_simple <- phyto_lookup[duplicated(phyto_lookup$taxalower) == F, 
                                    c("taxalower","Kingdom", "Phylum", "Class", "AlgalGroup", "Genus", 
                                      "CurrentGenus", "CurrentName",
                                      "Growth.Habit", "Habitat.Type", "Salinity.Range")]

# Prepare data -------------------------------------------------------------

phyto$taxalower <- gsub(tolower(phyto$taxon), pattern = c("cf. |cf.|cf,"), replacement = "")
phyto$taxalower <- ifelse(phyto$taxalower == "chroomonas nordstedtii ", "chroomonas nordstedtii",phyto$taxalower)
## QC check: are all taxa in the lookup table? ----
dbcheck <- data.frame(taxalower = unique(phyto$taxalower))
dbcheck$found <- unique(dbcheck$taxalower) %in% unique(phyto_lookup$taxalower)

dput(dbcheck[dbcheck$found == F,"taxalower"])

## join phyto lookup ----
dbcheck <- data.frame(taxalower = unique(phyto$taxalower))
dbcheck$found <- unique(dbcheck$taxalower) %in% unique(phyto_lookup$taxalower)

dput(dbcheck[dbcheck$found == F,"taxalower"])

phytomerge <- merge(phyto, phyto_lookup_simple, by.x = "taxalower", by.y = "taxalower", all.x = T)
phytomerge <- merge(phytomerge, sites, by.x = "station_code", by.y = "Sitename", all.x = T) 
phytomerge$sitefac <- as.factor(phytomerge$station_code)
phytomerge$sitefac <- factor(phytomerge$sitefac, 
                             levels = rev(sites[sites$Sitename %in% phytomerge$station_code, "Sitename"]))

phytomerge <- phytomerge[is.na(phytomerge$AlgalGroup) == F,]
phytomerge$year <- format(phytomerge$sample_date, format = "%Y")
phytomerge$jday <- as.numeric(format(phytomerge$sample_date, format = "%j"))

phytomerge$AlgalGroup <- ifelse(phytomerge$AlgalGroup %in% c("Ciliates", "Unknown Flagellates", 
                                                             "Synurophytes", "Haptophytes", "Xanthophytes"),
                                "other", phytomerge$AlgalGroup)

## Join phyto data at taxa level
phytotaxamerge_lookup <- merge(phyto_lookup, phyto[duplicated(phyto$taxalower) == F, c("taxalower", "taxon")], by = "taxalower", all = T)

# write.csv(phytotaxamerge_lookup, 
# "C:/Users/eholmes/Documents/R/Projects/NDFS-Projects/Nutrient-Phyto Manuscript/data/PESP_taxalookup_eholmes.csv", row.names = F)

unique(phyto[!(phyto$taxalower %in% phyto_lookup$taxalower), "taxalower"])

# manuscript stations
stations <- c('RCS', 'RD22', 'I80', 'LIS', 'STTD', 'BL5', 'LIB', 'RYI', 'RVB')

phytomerge$station_code <- ifelse(phytomerge$station_code %in% c("BL-5", "BL5"), "BL5",
                                  ifelse(phytomerge$station_code %in% c("SHER"), "SHR",
                                         phytomerge$station_code))

phytomerge_clean <- phytomerge[phytomerge$station_code %in% stations &
                                 is.na(phytomerge$sample_date) == F & 
                                 phytomerge$jday %in% 150:330 & 
                                 !(phytomerge$depth_m %in% 3), ]

phytomerge_clean$factorcalc <- phytomerge_clean$slide_chamber_area_mm2/(phytomerge_clean$volume_analyzed_m_l * 
                                                                          phytomerge_clean$field_of_view_mm2 * 
                                                                          phytomerge_clean$number_of_fields_counted)
dput(colnames(phytomerge_clean))
phytomerge_clean <- phytomerge_clean %>% 
  mutate(biov_avg = apply(phytomerge_clean[,c("biovolume_1", "biovolume_2", "biovolume_3", 
                                              "biovolume_4", "biovolume_5", "biovolume_6", "biovolume_7", "biovolume_8", 
                                              "biovolume_9", "biovolume_10")], 1, mean, na.rm = TRUE)) %>% # Average all 10 biovolume measurements for each taxon (BioV.Avg)
  mutate(cell_density = total_number_of_cells * factorcalc) %>% # Calculate cell abundance per mL (Density)
  mutate(unit_density = unit_abundance_number_of_natural_units * factorcalc) %>% # Calculate unit abundance per mL (Density)
  mutate(biov_per_mL = total_number_of_cells * biov_avg * factorcalc)  # Calculate Biovolume density (per mL) per Flynn_Brown memo

hist(log(phytomerge_clean$biov_per_mL))

phytoply <- phytomerge_clean %>% group_by(sample_date, jday, year, sitefac, AlgalGroup) %>% 
  filter(year != 2016 & is.na(sitefac) == F, jday %in% 175:330) %>% 
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density)) %>% 
  group_by(sitefac, jday, year) %>% 
  mutate(perc = sumbiovol/sum(sumbiovol))

## QC plotting ----
###investigate if devil is in the denominator ----
eucap <- phytomerge_clean %>% group_by(sample_date, sitefac, CurrentGenus, AlgalGroup) %>% 
  summarize(tot_units = sum(unit_abundance_number_of_natural_units))
eucap$sitedate <- paste(eucap$sitefac, eucap$sample_date)

length(unique(paste(phytomerge_clean$sample_date, phytomerge_clean$sitefac)))
nrow(eucap[eucap$CurrentGenus == "Eucapsis",])

eucap[eucap$CurrentGenus == "Eucapsis" & eucap$tot_units < 200, c("sitedate")]

ggplot(eucap[eucap$AlgalGroup %in% "Cyanobacteria",], aes(x = sample_date, y = tot_units)) + geom_point() + #stat_smooth() +
  facet_grid(CurrentGenus ~ .)

ggplot(eucap[eucap$AlgalGroup %in% "Cyanobacteria",], aes(x = sample_date, y = tot_units)) + geom_point() + #stat_smooth() +
  facet_grid(CurrentGenus ~ .)

phytobvqc <- phytomerge_clean %>% filter(year != 2016) %>% group_by(taxalower, year, taxonomist, CurrentGenus) %>% 
  summarize(meanbiov_avg = mean(biov_avg), sdbiov_avg = sd(biov_avg))
dput(colnames(phytomerge_clean))

eucap <- phytomerge_clean %>% group_by(sample_date, sitefac, year, CurrentGenus, AlgalGroup) %>% 
  summarize(tot_units = sum(unit_abundance_number_of_natural_units),
            tot_unitden = sum(unit_density),
            tot_biov= sum(biov_per_mL)) %>% group_by(sample_date, sitefac) %>%  
  mutate(perc = 100*tot_units/sum(tot_units),
         percbiov = 100*tot_biov/sum(tot_biov)) %>% data.frame()
mean(eucap[eucap$CurrentGenus == "Eucapsis","perc"], na.rm = T)
range(eucap[eucap$CurrentGenus == "Eucapsis","perc"], na.rm = T)

range(eucap[eucap$CurrentGenus != "Eucapsis","perc"], na.rm = T)

ggplot(eucap[eucap$CurrentGenus == "Eucapsis",], aes(x = year, y = perc)) + 
  geom_violin(fill = "black") + ylim(0,100) + theme_bw() + labs(y = "Percent Eucapsis units")

ggplot(eucap[eucap$CurrentGenus != "Eucapsis",], aes(x = year, y = perc)) + 
  geom_violin(fill = "black") + ylim(0,100) + theme_bw() + labs(y = "Percent NOT Eucapsis units")

ggplot(eucap[eucap$CurrentGenus == "Eucapsis",], aes(x = year, y = percbiov)) + 
  geom_violin(fill = "black") + ylim(0,100) + theme_bw() + labs(y = "Percent Eucapsis biovolume")

ggplot(eucap, aes(x = tot_unitden)) + geom_histogram() + scale_x_log10() + facet_grid(sitefac ~ .) +
  geom_histogram(data = eucap[eucap$CurrentGenus == "Eucapsis",], fill = "red")

ggplot(eucap, aes(x = tot_units)) + geom_histogram() + scale_x_log10() + facet_grid(sitefac ~ .) +
  geom_histogram(data = eucap[eucap$CurrentGenus == "Eucapsis",], fill = "red")

ulnaria <- phytomerge_clean[phytomerge_clean$CurrentGenus == "Ulnaria", c("taxon", "year", "sitefac", "genus", "biov_avg", "biov_per_mL",
                                                                          "biovolume_1", 
                                                                          "biovolume_2", "biovolume_3", "biovolume_4", "biovolume_5", "biovolume_6", 
                                                                          "biovolume_7", "biovolume_8", "biovolume_9", "biovolume_10")]

Aulacoseira <- phytomerge_clean[phytomerge_clean$genus == "Aulacoseira", c("taxon", "year", "sitefac", "genus", "biov_avg", "biov_per_mL",
                                                                           "biovolume_1", 
                                                                           "biovolume_2", "biovolume_3", "biovolume_4", "biovolume_5", "biovolume_6", 
                                                                           "biovolume_7", "biovolume_8", "biovolume_9", "biovolume_10")]

ggplot(phytobvqc, aes(x = taxalower, y = meanbiov_avg)) + geom_point()

ggplot(phytobvqc, aes(x = reorder(taxalower, -sdbiov_avg), y = sdbiov_avg)) + geom_bar(stat = "identity") +
  facet_grid(year ~ .) + scale_y_log10()

ggplot(phytobvqc[phytobvqc$CurrentGenus == "Sellaphora",], aes(x = year, y = meanbiov_avg)) + geom_point()
ggplot(phytobvqc[phytobvqc$CurrentGenus == "Eucapsis",], aes(x = year, y = meanbiov_avg)) + geom_point() +
  geom_errorbar(aes(ymin = meanbiov_avg - sdbiov_avg, ymax = meanbiov_avg + sdbiov_avg), width = .1)

ggplot(phytobvqc[phytobvqc$CurrentGenus == "Plagioselmis",], 
       aes(x = year, y = meanbiov_avg)) + 
  geom_point() + facet_wrap(taxalower ~ .) + theme_bw() +
  geom_errorbar(aes(ymin = meanbiov_avg - sdbiov_avg, ymax = meanbiov_avg + sdbiov_avg), width = .1)

ggplot(phytobvqc[phytobvqc$CurrentGenus == "Plagioselmis",], 
       aes(x = taxonomist, y = meanbiov_avg)) + 
  geom_point() + facet_wrap(taxalower ~ .) + theme_bw() +
  geom_errorbar(aes(ymin = meanbiov_avg - sdbiov_avg, ymax = meanbiov_avg + sdbiov_avg), width = .1)

ggplot(phytobvqc, 
       aes(x = taxonomist)) + 
  geom_bar() + facet_wrap(year ~ .) + theme_bw()

ggplot(phytobvqc, 
       aes(x = year)) + 
  geom_bar() + facet_grid(taxonomist ~ .) + theme_bw()

# Takes a while to run

# for(i in unique(phyto$genus)){
#   print(i)
#   if(nrow(phytobvqc[phytobvqc$genus == i,])>0){
#     png(paste0("figures/Phyto_figs/Phyto_biov_qc/",i,"_biov_qc.png"),
#         height = 8, width = 8, units = "in", res = 500, family = "serif")
#     
#     print(ggplot(phytobvqc[phytobvqc$genus == i,], 
#                  aes(x = year, y = meanbiov_avg)) + 
#             geom_point() + facet_wrap(taxon ~ .) + theme_bw() +
#             geom_errorbar(aes(ymin = meanbiov_avg - sdbiov_avg, ymax = meanbiov_avg + sdbiov_avg), width = .1))
#     
#     dev.off()}
# }

# Biomass conversion ------------------------------------------------------

# Biomass equations from Menden-Deuer and Lessard (1999)
## non diatom pgC cell-1 = 0.760 *biovolume0.819
## diatom pgC cell-1 = 0.288 *biovolume0.811

phytomerge_clean$pgc_per_mL <- ifelse(phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms"),
                                      0.288 * phytomerge_clean$biov_per_mL^0.811,
                                      0.760 * phytomerge_clean$biov_per_mL^0.819)

phytomerge_clean$pgc_per_cell <- ifelse(phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms"),
                                        0.288 * phytomerge_clean$biov_avg^0.811,
                                        0.760 * phytomerge_clean$biov_avg^0.819)

range(phytomerge_clean[phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms"),"biov_avg"], na.rm = T)
range(phytomerge_clean[!(phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms")),"biov_avg"], na.rm = T)

range(phytomerge_clean[phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms"),"biov_per_mL"], na.rm = T)
range(phytomerge_clean[!(phytomerge_clean$AlgalGroup %in% c("Centric Diatoms", "Pennate Diatoms")),"biov_per_mL"], na.rm = T)

phytomerge_clean[phytomerge_clean$biov_per_mL == max(phytomerge_clean$biov_per_mL, na.rm = T), "taxon"]

df <- data.frame(biov_avg = seq(from = 0, to = 135000, length.out = 100),
                 biov_per_ml = seq(from = 0, to = 76015560, length.out = 100))
df$diatom_pgc <- 0.288 * df$biov_avg^0.811
df$nondiatom_pgc <- 0.760 * df$biov_avg^0.819

df$diatom_pgc_ml <- 0.288 * df$biov_per_ml^0.811
df$nondiatom_pgc_ml <- 0.760 * df$biov_per_ml^0.819

ggplot(df, aes(x = biov_avg)) + 
  geom_line(aes(y = diatom_pgc), color = "brown") +
  geom_line(aes(y = nondiatom_pgc), color = "forestgreen") +
  theme_bw()

ggplot(df, aes(x = biov_per_ml)) + 
  geom_line(aes(y = diatom_pgc_ml), color = "brown") +
  geom_line(aes(y = nondiatom_pgc_ml), color = "forestgreen") +
  theme_bw()

# Plotting ;) -------------------------------------------------

if(save_output == T){png("figures/Phyto_figs/Fig_6_Phytoplankton_taxa_boxplots_%02da.png",
                         height = 8, width = 8, units = "in", res = 1000, family = "serif")}

ggplot(phytomerge_clean, aes(x = year, y = log(biov_per_mL), fill = station_code)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot() +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  labs(x = element_blank(),
       y = expression(paste("Log Phytoplankton Biovolume (","  ",mu, m^3, "/", mL,")", sep=""))) +
  theme_bw() 

(ggplot(phytomerge_clean[phytomerge_clean$year != 2016,], aes(x = sitefac, y = log(biov_per_mL), fill = distance_km)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot() +
    scale_fill_viridis(option = "A", discrete = F) +
    #scale_x_discrete(limits = sampling_time) +
    labs(x = "",
         y = expression(paste("Log Phytoplankton Biovolume ("," " ,mu, m^3, "/", mL,")", sep="")),
         fill = "Station") +
    theme_bw() + facet_grid(AlgalGroup ~ year, scales = "free") + theme(axis.text.x = element_text(angle = 90))
)

if(save_output == T){dev.off()}

# unit count research -----------------------------------------------------

unit_trend <- phytomerge_clean %>% group_by(AlgalGroup, year, sitefac) %>% 
  summarize(unit_tot = mean(unit_abundance_number_of_natural_units, na.rm = T),
            unit_den_tot = mean(unit_density, na.rm = T))

ggplot(unit_trend[unit_trend$AlgalGroup != "Cyanobacteria",], aes(x = year, y = unit_tot, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(sitefac ~ .)

ggplot(unit_trend[unit_trend$AlgalGroup != "Cyanobacteria",], aes(x = unit_den_tot)) + 
  geom_histogram() + facet_grid(sitefac ~ .)

cyano_unit_trend <- phytomerge_clean[phytomerge_clean$AlgalGroup == "Cyanobacteria",] %>% 
  group_by(CurrentGenus, year, sitefac) %>% 
  summarize(unit_tot = mean(unit_abundance_number_of_natural_units, na.rm = T),
            unit_den_tot = mean(unit_density, na.rm = T))

ggplot(unit_trend[unit_trend$AlgalGroup == "Cyanobacteria",], aes(x = year, y = unit_den_tot, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(sitefac ~ .)

ggplot(cyano_unit_trend, aes(x = year, y = unit_den_tot, fill = CurrentGenus)) + 
  geom_bar(stat = "identity") + facet_grid(sitefac ~ .)

ggplot(cyano_unit_trend, aes(x = reorder(CurrentGenus, -unit_den_tot), y = unit_den_tot)) + 
  geom_bar(stat = "identity") + facet_grid(sitefac ~ .) + scale_y_log10()

ggplot(phytomerge_clean, aes(x = year, y = unit_density, fill = station_code)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot() +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  labs(x = element_blank(),
       y = expression(paste("Log Phytoplankton Biovolume (","  ",mu, m^3, "/", mL,")", sep=""))) +
  theme_bw() 

ggplot(phytomerge_clean, aes(x = unit_abundance_number_of_natural_units)) +
  geom_histogram() +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  labs(x = element_blank(),
       y = expression(paste("Log unit abundance (","  ",mu, m^3, "/", mL,")", sep=""))) +
  theme_bw() + facet_grid(AlgalGroup ~ year, scales = "free")

ggplot(phytomerge_clean[phytomerge_clean$AlgalGroup != "Cyanobacteria",], aes(x = unit_abundance_number_of_natural_units)) +
  geom_histogram() + scale_x_log10() +
  geom_histogram(data = phytomerge_clean[phytomerge_clean$AlgalGroup == "Cyanobacteria",], fill = "green") +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  labs(x = element_blank(),
       y = expression(paste("Log unit abundance (","  ",mu, m^3, "/", mL,")", sep=""))) +
  theme_bw() + facet_grid(year ~ ., scales = "free")

ggplot(phytomerge_clean[phytomerge_clean$AlgalGroup != "Cyanobacteria",], aes(x = unit_abundance_number_of_natural_units)) +
  geom_density(aes(color = year)) + scale_x_log10() +
  # geom_histogram(data = phytomerge_clean[phytomerge_clean$AlgalGroup == "Cyanobacteria",], fill = "green") +
  # scale_fill_viridis(option = "A", discrete = TRUE) +
  labs(x = element_blank(),
       y = expression(paste("Log unit abundance (","  ",mu, m^3, "/", mL,")", sep=""))) +
  theme_bw() + facet_grid(sitefac ~ ., scales = "free")

# Area plot ---------------------------------------------------------------

phytobiovcast <- phytoply %>% select(-c(perc, swi_biov, swi_unit, sum_unit_den)) %>%
  pivot_wider(names_from = "AlgalGroup", values_from = "sumbiovol", values_fill = 0, )

phytocast <- phytoply %>% select(-c(sumbiovol,swi_biov, swi_unit, sum_unit_den)) %>%
  pivot_wider(names_from = "AlgalGroup", values_from = "perc", values_fill = 0, )

phytoarea <- phytocast %>% 
  pivot_longer(cols = c("Centric Diatoms", "Chrysophytes", "Cryptophytes", 
                        "Cyanobacteria", "Green Algae", "Pennate Diatoms", "Dinoflagellates", 
                        "Euglenoids", "other"), 
               names_to = "AlgalGroup",
               values_to = "perc") %>% data.frame()

# phytoarea$AlgalGroup <-  sub(pattern = "\\.", replacement = " ", x = phytoarea$AlgalGroup)
phytoareasum <- phytoply %>% select(-c(perc,swi_biov, swi_unit, sum_unit_den)) %>% #filter(is.na(sitefac) == F) %>% 
  pivot_wider(names_from = "AlgalGroup", values_from = "sumbiovol", values_fill = 0) %>% 
  pivot_longer(cols = c("Centric Diatoms", "Chrysophytes", "Cryptophytes", 
                        "Cyanobacteria", "Green Algae", "Pennate Diatoms", "Dinoflagellates", 
                        "Euglenoids", "other"), values_to = "sumbiovol") %>% 
  data.frame()
unique(phytoarea$AlgalGroup)

if(save_output == T){png("figures/Phyto_figs/Fig_6_Phytoplankton_taxa_%02da.png",
                         height = 6, width = 8, units = "in", res = 1000, family = "serif")}
phytoarea$AlgalGroup <- factor(phytoarea$AlgalGroup, levels = c("Centric Diatoms", "Pennate Diatoms", 
                                                                "Chrysophytes", "Cryptophytes", 
                                                                "Cyanobacteria", "Green Algae", "Dinoflagellates", 
                                                                "Euglenoids", "other"))
ggplot(phytoarea, aes(x = jday, y = perc,
                      fill = AlgalGroup)) +  
  geom_bar(stat = "identity", aes(group = jday), width = 7) +
  theme_bw() + 
  facet_grid(sitefac ~ year, scale = "free_x") +
  labs(x = "Month", y = "Percent biovolume", fill = NULL) +
  scale_fill_manual(values = algalpal) +
  scale_x_continuous(limits = c(200,325),
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = substr(x = c(month.abb, month.abb[1]), 1, 1))

ggplot(phytoarea, aes(x = jday, y = perc, 
                      fill = AlgalGroup)) + 
  geom_area() +
  theme_bw() + 
  facet_grid(sitefac ~ year, scale = "free_x") +
  labs(x = "Month", y = "Percent biovolume", fill = NULL) +
  scale_fill_manual(values = algalpal) +
  scale_x_continuous(limits = c(200,280),
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(phytoareasum, aes(x = jday, y = sumbiovol, 
                         fill = factor(name, levels=c("Centric Diatoms", "Pennate Diatoms", 
                                                      "Chrysophytes", "Cryptophytes", 
                                                      "Cyanobacteria", "Green Algae", "Dinoflagellates", 
                                                      "Euglenoids", "other")))) + 
  geom_bar(stat = "identity", aes(group = jday), width = 10) +
  theme_bw() + labs(x= "Month", y = expression(paste("Phytoplankton Biovolume (","  ",mu, m^3, "/", mL,")", sep="")),
                    fill = NULL) +
  facet_grid(sitefac ~ year, scale = "fixed") +
  scale_fill_manual(values = algalpal) +
  scale_x_continuous(limits = c(200,325),
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = substr(x = c(month.abb, month.abb[1]), 1, 1))

ggplot(phytoareasum, aes(x = jday, y = sumbiovol, 
                         fill = factor(name, levels=c("Centric Diatoms", "Pennate Diatoms", 
                                                      "Chrysophytes", "Cryptophytes", 
                                                      "Cyanobacteria", "Green Algae", "Dinoflagellates", 
                                                      "Euglenoids", "other")))) + 
  geom_bar(stat = "identity", aes(group = jday), width = 10) +
  theme_bw() + labs(x= "Month", y = expression(paste("Phytoplankton Biovolume (","  ",mu, m^3, "/", mL,")", sep="")),
                    fill = NULL) +
  facet_grid(sitefac ~ year, scale = "fixed") +
  scale_fill_manual(values = algalpal) +
  scale_x_continuous(limits = c(200,325),
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = substr(x = c(month.abb, month.abb[1]), 1, 1))

if(save_output == T){dev.off()}

# Average community by site -----------------------------------------------

siteply <- phytomerge_clean %>% 
  filter(year != 2016 & is.na(sitefac) == F &
           AlgalGroup != "NA" & jday %in% 175:330) %>% 
  group_by(sample_date, jday, year, sitefac, AlgalGroup) %>% 
  
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            sumpgc = sum(pgc_per_mL),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density),) %>% 
  group_by(sample_date, year, sitefac, AlgalGroup) %>% 
  summarize(meanbiovol = mean(sumbiovol), 
            mean_unit_den = mean(sum_unit_den),
            meanpgc = mean(sumpgc),
            mean_swi_biov = swi(swi_biov), 
            mean_swi_unit = swi(swi_unit))

siteply$AlgalGroup <- factor(siteply$AlgalGroup, levels = c("Centric Diatoms", "Pennate Diatoms", 
                                                            "Chrysophytes", "Cryptophytes", 
                                                            "Cyanobacteria", "Green Algae", "Dinoflagellates", 
                                                            "Euglenoids", "other"))

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = meanbiovol, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL) +
  scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = meanpgc, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Mean Carbon (pg)") +
  scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = meanpgc/1000, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Mean Carbon (ng)") +
  scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = meanpgc/1000, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Mean Carbon (ng)") +
  scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = meanpgc/1000, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(AlgalGroup ~ year, scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Mean Carbon (ng)") #+
# scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = year, y = meanpgc/1000, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(AlgalGroup ~ sitefac, scales = "fixed") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Mean Carbon (ng)") #+
scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F,], 
       aes(x = sitefac, y = mean_unit_den, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "free") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL) +
  scale_fill_manual(values = algalpal)

ggplot(siteply[is.na(siteply$AlgalGroup) == F & siteply$AlgalGroup != "Cyanobacteria",], 
       aes(x = sitefac, y = mean_unit_den, fill = AlgalGroup)) + 
  geom_bar(stat = "identity") + facet_grid(year ~ ., scales = "free") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL) +
  scale_fill_manual(values = algalpal)

lis2022 <- phytomerge_clean[phytomerge_clean$station_code == "LIS" & phytomerge_clean$year == 2022, 
                            c("sample_date", "taxon", "AlgalGroup","unit_abundance_number_of_natural_units", "biov_avg", "biov_per_mL")]
# Shannon Weiner Diversity index ------------------------------------------

phytoswiply <- phytomerge_clean %>% group_by(sample_date, jday, year, sitefac) %>% 
  filter(year != 2016 & is.na(sitefac) == F) %>% 
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density)) %>% 
  group_by(sitefac, jday, year) %>% 
  mutate(perc = sumbiovol/sum(sumbiovol))

if(save_output == T){png("figures/Phyto_figs/Phytoplankton_diversity_index_%02da.png",
                         height = 6, width = 8, units = "in", res = 1000, family = "serif")}

# By year
cowplot::plot_grid(
  ggplot(phytoswiply, aes(x = sitefac, y = swi_biov, fill = sitefac)) + geom_boxplot(show.legend = F) + 
    facet_grid(. ~ year) + theme_bw() + scale_fill_viridis_d() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)),
  ggplot(phytoswiply, aes(x = sitefac, y = swi_unit, fill = sitefac)) + geom_boxplot(show.legend = F) + 
    facet_grid(. ~ year) + theme_bw() + scale_fill_viridis_d() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)),
  align  = "v", nrow = 2)

# By site
cowplot::plot_grid(
  ggplot(phytoswiply, aes(x = year, y = swi_biov, fill = year)) + geom_boxplot(show.legend = F) + 
    facet_grid(. ~ sitefac) + theme_bw() + scale_fill_viridis_d() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)),
  
  ggplot(phytoswiply, aes(x = year, y = swi_unit, fill = year)) + geom_boxplot(show.legend = F) + 
    facet_grid(. ~ sitefac) + theme_bw() + scale_fill_viridis_d() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)),
  align  = "v", nrow = 2)

if(save_output == T){dev.off()}

# genus richness --------------------------------------------------------

phytogenus <- phytomerge_clean %>% group_by(sample_date, jday, year, taxonomist, field_of_view_mm2, sitefac, taxon) %>% 
  filter(year != 2016 & is.na(sitefac) == F) %>% 
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density)) %>% 
  group_by(sitefac, jday, year) %>% 
  mutate(perc = sumbiovol/sum(sumbiovol))

phytogenus <- phytogenus[!grepl(pattern = "cf. | cff. ", x = phytogenus$taxon),]

phytobiovgenus <- phytogenus %>% filter(jday %in% 200:325) %>% select(-c(perc, swi_biov, swi_unit, sum_unit_den)) %>%
  pivot_wider(names_from = "taxon", values_from = "sumbiovol", values_fill = 0) %>% data.frame()

phyto_pa <- decostand(phytobiovgenus[,c(7:ncol(phytobiovgenus))], method = "pa")

phytorich <- cbind(phytobiovgenus[,1:6])
phytorich$richness <- rowSums(phyto_pa)

ggplot(phytorich, aes(x = sitefac, y = richness, fill = sitefac)) + facet_grid(. ~ year) +
  geom_boxplot() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(phytorich, aes(x = year, y = richness, fill = year)) + facet_grid(. ~ sitefac) +
  geom_boxplot() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(phytorich, aes(x = jday, y = richness)) + geom_line() + facet_grid(sitefac ~ year)

ggplot(phytorich, aes(x = richness)) + geom_histogram() + facet_grid(year~.)

phytobiov <- phytobiovgenus[,c(7:ncol(phytobiovgenus))]
phytosums <- colSums(phytobiov)
colnames(phyto_pa)[colSums(phyto_pa)>4]

ggplot(reshape2::melt(phytobiov), aes(x = reorder(variable,-value), y = value)) + geom_bar(stat = "identity")+ 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(reshape2::melt(phytobiov), aes(x = reorder(variable,-value), y = value)) + geom_bar(stat = "identity")+ 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# avg Biov distribution over time -----------------------------------------


# Hill numbers ------------------------------------------------------------
library(iNEXT)
data("spider")
(iout <- iNEXT(spider, q=c(0,1,2), datatype="abundance"))

iout$DataInfo
iout$iNextEst
iout$AsyEst

ggiNEXT(iout, type=1, se=TRUE, grey=FALSE, facet.var = "Assemblage")

phytocomm <- data.frame(phytobiovgenus[,c(7:ncol(phytobiovgenus))])
row.names(phytocomm) <- paste(phytobiovgenus$sitefac, phytobiovgenus$sample_date, sep = "_")
phytocommlist <- list(unlist(phytocomm[1,], use.names = FALSE), unlist(phytocomm[2,], use.names = FALSE))
names(phytocommlist) <- c("STTD", "RYI")

##Careful: this takes a long time to run!
# irare <- iNEXT(list(phytocommlist[1]), q=c(0), datatype="abundance")
# ggiNEXT(irare, type=1, se=TRUE, grey=FALSE, facet.var = "Assemblage")

# Investigate patterns in factor scaling ----------------------------------

ggplot(phytomerge_clean, aes(x = factor)) + geom_histogram() + scale_x_log10()

ggplot(phytomerge_clean[phytomerge_clean$jday %in% 150:325,], aes(x = factor)) + geom_histogram() + #scale_x_log10() +
  facet_wrap(year ~ .)

ggplot(phytomerge_clean[phytomerge_clean$jday %in% 150:325,], aes(x = factor, fill = taxonomist)) + 
  geom_density() +
  facet_grid(year ~ .) + scale_x_log10()

ggplot(phytomerge_clean[phytomerge_clean$jday %in% 150:325,], aes(y = factor, x = taxonomist, fill = taxonomist)) + 
  geom_boxplot() + geom_jitter() +
  facet_grid(. ~ year) + scale_y_log10()

ggplot(phytomerge_clean[phytomerge_clean$jday %in% 150:325,], aes(y = biov_per_mL, x = taxonomist, fill = taxonomist)) + 
  geom_boxplot() +
  facet_grid(AlgalGroup ~ year)# + scale_y_log10()

ggplot(phytomerge_clean[phytomerge_clean$jday %in% 150:325,], aes(y = unit_density, x = taxonomist, fill = taxonomist)) + 
  geom_boxplot() +
  facet_grid(AlgalGroup ~ year) #+ scale_y_log10()

# NMDS --------------------------------------------------------------------



com2016 <- decostand(phytobiovgenus[,c(7:ncol(phytobiovgenus))], method = "range")
nmds2016 <- metaMDS(com2016, autotransform = F, na.rm = T)

data.scores16 <- as.data.frame(scores(nmds2016, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores16$sitefac <- phytobiovgenus$sitefac #create a column of site names, from the rownames of data.scores
data.scores16$jday <- phytobiovgenus$jday
data.scores16$year <- phytobiovgenus$year
data.scores16$taxonomist <- phytobiovgenus$taxonomist
data.scores16$field_of_view_mm2 <- phytobiovgenus$field_of_view_mm2
data.scores16 <- merge(data.scores16, sites, by.x = "sitefac", by.y = "Sitename", all.x = T)
species.scores16 <- as.data.frame(scores(nmds2016, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores16$species <- rownames(species.scores16)  # create a column of species, from the rownames of species.scores
data.scores16$fieldfactor <- factor(round(data.scores16$field_of_view_mm2, 3))
data.scores16$fieldyear <- paste(data.scores16$year,data.scores16$fieldfactor)

if(save_output == T){png("figures/Phyto_figs/NDFS_phytoCurrentGenus_NMDS_dist_%03d.png",
                         family = "serif", height = 6, width = 6, units = "in", res = 1000)}

(phytonmds <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km)) +
    # geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    # ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_c() + scale_fill_viridis_c() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = distance_km, group = sitefac), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac), alpha = .5))

(phytonmdsyear <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, shape = year)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = year, group = year), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, group = year), alpha = .5))

(phytonmdsfield <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = fieldfactor)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = fieldfactor, group = fieldfactor), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = fieldfactor, group = fieldfactor), alpha = .5))

(phytonmdsfield <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, shape = fieldfactor)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    # scale_color_viridis_d() + scale_fill_viridis_d() + 
    scale_color_brewer(palette = "Set1") +scale_fill_brewer(palette = "Set1") +
    labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = year, group = fieldyear), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, group = fieldyear), alpha = .5))


(phytonmdsfield <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = fieldfactor, shape = fieldfactor)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    # scale_color_viridis_d() + scale_fill_viridis_d() + 
    scale_color_brewer(palette = "Set1") +scale_fill_brewer(palette = "Set1") +
    labs(title = "NMDS - Phytoplankton") +
    facet_wrap(year ~.) +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = fieldfactor, group = fieldfactor), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = fieldfactor, group = fieldfactor), alpha = .5))

(phytonmdstaxono <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = taxonomist)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    facet_wrap(year~.) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = taxonomist, group = taxonomist), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = taxonomist, group = taxonomist), alpha = .5))

(phytonmdstaxono <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = sitefac, shape = sitefac)) +
    # geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    # ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    facet_wrap(year~.)) 

(phytonmdstaxono <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = jday, shape = sitefac)) +
    # geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    # ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_c() + scale_fill_viridis_c() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + 
    scale_shape_manual(values = 1:10) +#theme(legend.position = "none") +
    facet_wrap(year~.)) 
# +
#   stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = sitefac, group = sitefac), geom = "polygon", alpha = .1) +
#   stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = sitefac, group = sitefac), alpha = .5))

if(save_output == T){dev.off()}

library(vegan)
colnames(phytobiovcast)
##Algal class aggregation
com2016 <- decostand(phytobiovcast[,c(5:12)], method = "hellinger")
nmds2016 <- metaMDS(com2016, autotransform = F, na.rm = T)

data.scores16 <- as.data.frame(scores(nmds2016, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores16$sitefac <- phytobiovcast$sitefac #create a column of site names, from the rownames of data.scores
data.scores16$jday <- phytobiovcast$jday
data.scores16$year <- phytobiovcast$year
data.scores16 <- merge(data.scores16, sites, by.x = "sitefac", by.y = "Sitename", all.x = T)
species.scores16 <- as.data.frame(scores(nmds2016, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores16$species <- rownames(species.scores16)  # create a column of species, from the rownames of species.scores

if(save_output == T){png("figures/Phyto_figs/NDFS_phytoggroup_NMDS_dist_%03d.png",
                         family = "serif", height = 6, width = 6, units = "in", res = 1000)}

(phytonmds <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_c() + scale_fill_viridis_c() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = distance_km, group = sitefac), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac), alpha = .5))

(phytonmds <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + #theme(legend.position = "none") +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = year, group = year), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, group = year), alpha = .5))


(phytonmds <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_c() + scale_fill_viridis_c() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + facet_wrap(year~.) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = distance_km, group = sitefac), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac), alpha = .5))

(phytonmds <- ggplot() + 
    geom_point(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year)) +
    geom_point(data = species.scores16, aes(x = NMDS1, y=NMDS2), size = .8) +
    # geom_path(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = distance_km, group = sitefac)) +
    ggrepel::geom_text_repel(data=species.scores16,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.9, size = 3, force = .1) +
    scale_color_viridis_d() + scale_fill_viridis_d() + labs(title = "NMDS - Phytoplankton") +
    theme_bw(base_family = "serif") + facet_wrap(sitefac ~ .) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, fill = year, group = year), geom = "polygon", alpha = .1) +
    stat_ellipse(data = data.scores16, aes(x = NMDS1, y = NMDS2, color = year, group = year), alpha = .5))


if(save_output == T){dev.off()}
# Indicator species analysis ----------------------------------------------

library(indicspecies)

indval <- multipatt(phytobiovgenus[,c(6:ncol(phytobiovgenus))],  phytobiovgenus$sitefac, 
                    control = how(nperm=100)) 

summary(indval)

indval.year <- multipatt(phytobiovgenus[,c(6:ncol(phytobiovgenus))],  phytobiovgenus$year, 
                         control = how(nperm=500)) 

summary(indval.year)

if(save_output == T){png("figures/Phyto_figs/NDFS_phytogenus_indicator_%03d.png",
                         family = "serif", height = 8, width = 6, units = "in", res = 1000)}

cowplot::plot_grid(
  # Group 2017+2018+2019+2020+2021  #sps.  1 
  ggplot(phytobiovgenus, aes(x = sitefac, y = Plagioselmis)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  ggplot(phytobiovgenus, aes(x = sitefac, y = Snowella)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  #2020
  ggplot(phytobiovgenus, aes(x = sitefac, y = Aulacoseira)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  ggplot(phytobiovgenus, aes(x = sitefac, y = Aphanizomenon)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  ggplot(phytobiovgenus, aes(x = sitefac, y = Chrysosaccus)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  #2021
  ggplot(phytobiovgenus, aes(x = sitefac, y = Sellaphora)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  #2022
  ggplot(phytobiovgenus, aes(x = sitefac, y = Tetraedron)) + 
    geom_bar(stat = "identity") + theme_bw() + facet_grid(. ~ year),
  ncol = 1, align = "v")

if(save_output == T){dev.off()}

# Multi-panel plot: could include 
# 1) SWI by sites or years
# 2) stacked area/perc abund plot
# 3) NMDS of phyto community



# Relationships between CHL and Phyto biovolume and carbon ----------------

sfsu_nuts <- readxl::read_excel("Nutrient-Phyto Manuscript/data/NDFS Mastersheet 2017to2023_14Nov23_updated.xlsx", skip = 7, col_names = F, sheet = "All stations & upta")
colnames(sfsu_nuts) <- colnames(janitor::clean_names(readxl::read_excel("Nutrient-Phyto Manuscript/data/NDFS Mastersheet 2017to2023_14Nov23_updated.xlsx", skip = 4, sheet = "All stations & upta")))
sfsu_nuts$station <- ifelse(sfsu_nuts$station %in% c("I80TD", "I80-TD"), "I80", sfsu_nuts$station)
sfsu_nuts$date <- as.Date(sfsu_nuts$date)

sitedateply <- phytomerge_clean %>% 
  filter(year != 2016 & is.na(sitefac) == F &
           AlgalGroup != "NA" & jday %in% 175:330) %>% 
  group_by(sample_date, jday, year, sitefac, AlgalGroup) %>% 
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            sumpgc = sum(pgc_per_mL),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density),)

phytosfsu <- merge(sitedateply, sfsu_nuts, by.x = c("sample_date", "sitefac"), by.y = c("date", "station"))

ggplot(phytosfsu, aes(x = chl_t, y = sumbiovol, color = AlgalGroup)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + 
  scale_x_log10() + scale_y_log10() + theme_bw()

ggplot(phytosfsu, aes(x = chl_t, y = sumpgc, color = AlgalGroup)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + 
  scale_x_log10() + scale_y_log10() + theme_bw() +
  scale_color_manual(values = algalpal)

ggplot(phytosfsu, aes(x = chl_t, y = sumpgc)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + theme_bw() +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(AlgalGroup ~ .)

ggplot(phytosfsu, aes(x = chl_t, y = sumbiovol)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + theme_bw() +
  scale_x_sqrt() + scale_y_sqrt() +
  facet_wrap(AlgalGroup ~ .)

ggplot(phytosfsu, aes(x = chl_t, y = sum_unit_den)) + 
  geom_point() + stat_smooth(method = "lm", se = T, color = "red") + theme_bw() +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(AlgalGroup ~ .)

phytosfsu$logchl <- log
phytolm <- lm(log(chl_t) ~ log(sumpgc)*AlgalGroup, data = phytosfsu)

summary(phytolm)
AIC(phytolm)
sfsu_nutsub <- sfsu_nuts[sfsu_nuts$station %in% sites$Sitename,] 
sfsu_nutsub$year <- format(sfsu_nutsub$date, format = "%Y")
sfsu_nutsub$sitefac <- factor(sfsu_nutsub$station, 
                              levels = rev(sites[sites$Sitename %in% sfsu_nutsub$station, "Sitename"]))

ggplot(sfsu_nutsub[is.na(sfsu_nutsub$year) == F,], aes(x = sitefac, y = chl_t)) + geom_boxplot() + 
  facet_grid(year ~ .) + scale_y_log10()

## noalgalgroup relationship -----------------------------------------------------

sitedatechlply <- phytomerge_clean %>% 
  filter(year != 2016 & is.na(sitefac) == F &
           AlgalGroup != "NA" & jday %in% 175:330) %>% 
  group_by(sample_date, jday, year, sitefac) %>% 
  summarize(sumbiovol = sum(biov_per_mL), 
            sum_unit_den = sum(unit_density),
            sumpgc = sum(pgc_per_mL),
            swi_biov = swi(biov_per_mL), 
            swi_unit = swi(unit_density),)

phytosfsuchl <- merge(sitedatechlply, sfsu_nuts, by.x = c("sample_date", "sitefac"), by.y = c("date", "station"))

ggplot(phytosfsuchl, aes(x = chl_t, y = sumpgc)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + theme_bw() +
  scale_x_log10() + scale_y_log10()

summary(lm(sumpgc ~ chl_t, data = phytosfsuchl))

summary(lm(sumbiovol ~ chl_t, data = phytosfsuchl))

ggplot(phytosfsuchl, aes(x = chl_t, y = sumbiovol)) + 
  geom_point() + geom_line(stat = "smooth", method = "lm") + theme_bw() +
  scale_x_log10() + scale_y_log10()


