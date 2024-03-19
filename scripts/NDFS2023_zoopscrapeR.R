## Zoop scraper
require(readxl)

# Load and compile data ---------------------------------------------------

files <- list.files("data/zoopraw/ICF")

zoopdat_icf <- data.frame()

for(file in files){
  sheets <- readxl::excel_sheets(paste0("data/zoopraw/ICF/", file))
  sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC")]
  
  for(i in sheets){
    print(paste(file, i))
    tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/ICF/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
    
    tdf <- data.frame(taxon = c(tempdat[21:45, 1], #Micro & nauplii
                                tempdat[48:69, 1], #Cyclopoids
                                tempdat[72:96, 1], #Calanoids
                                tempdat[21:47, 17], #Cladocera
                                tempdat[59:59, 17], #Harpacticoids
                                tempdat[63:82, 17]), #Macrozoop
                      
                      classification = c(rep("Microzooplankton and nauplii", 25),
                                         rep("Cyclopoid", 22),
                                         rep("Calanoid", 25),
                                         rep("Cladocera", 27),
                                         rep("Harpacticoid", 1),
                                         rep("Macrozooplankton", 20)),
                      
                      count = as.numeric(c(tempdat[21:45, 9], #Micro & nauplii
                                tempdat[48:69, 9], #Cyclopoids
                                tempdat[72:96, 9], #Calanoids
                                tempdat[21:47, 25], #Cladocera
                                tempdat[59:59, 25], #Harpacticoids
                                tempdat[63:82, 25])), #Macrozoop
                      
                      subsample = as.numeric(c(tempdat[21:45, 11], #Micro & nauplii
                                    tempdat[48:69, 11], #Cyclopoids
                                    tempdat[72:96, 11], #Calanoids
                                    tempdat[21:47, 27], #Cladocera
                                    tempdat[59:59, 27], #Harpacticoids
                                    tempdat[63:82, 27])) #Macrozoop
                      ) 
    
    tdf$project <- tempdat[6, 1]
    tdf$sample_id <- tempdat[9, 1]
    tdf$station_id <- tempdat[6, 22]
    tdf$sheet_id <- i
    tdf$sample_date <- as.character(substr(tdf$sample_id, 1, 8))
    tdf$sample_time <- as.character(substr(tdf$sample_id, 10, 13))
    tdf$vol_meso <- as.numeric(tempdat[9, 13])
    tdf$vol_micro <- as.numeric(tempdat[12, 3])
    tdf$sub_meso <- as.numeric(tempdat[9, 22])
    tdf$sub_micro <- as.numeric(tempdat[12, 9])
    
    zoopdat_icf <- rbind(zoopdat_icf, tdf)
    
    rm(tempdat, tdf, i)
  } 
  rm(sheets, file)
}

zoopdat_icf$sample_date <- as.Date(zoopdat_icf$sample_date, format = "%Y%m%d")

# # BSA ---------------------------------------------------------------------
# 
# files <- list.files("data/zoopraw/BSA")
# 
# zoopdat_bsa <- data.frame()
# 
# for(file in files){
#   sheets <- readxl::excel_sheets(paste0("data/zoopraw/BSA/", file))
#   sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC")]
#   
#   for(i in sheets){
#     print(paste(file, i))
#     tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/BSA/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
#     tempdat
#     tdf <- data.frame(taxon = c(tempdat[19:22, 1], #Micro & nauplii
#                                 tempdat[27:45, 1], #Cyclopoids
#                                 tempdat[47:64, 1], #Calanoids
#                                 tempdat[19:39, 17], #Cladocera
#                                 tempdat[43:43, 17], #Harpacticoids
#                                 tempdat[47:62, 17]), #Macrozoop
#                       
#                       classification = c(rep("Microzooplankton and nauplii", 4),
#                                          rep("Cyclopoid", 19),
#                                          rep("Calanoid", 18),
#                                          rep("Cladocera", 21),
#                                          rep("Harpacticoid", 1),
#                                          rep("Macrozooplankton", 16)),
#                       
#                       count = c(tempdat[19:22, 9], #Micro & nauplii
#                                 tempdat[27:45, 9], #Cyclopoids
#                                 tempdat[47:64, 9], #Calanoids
#                                 tempdat[19:39, 25], #Cladocera
#                                 tempdat[43:43, 25], #Harpacticoids
#                                 tempdat[47:62, 25]), #Macrozoop
#                       
#                       subsample = as.numeric(c(tempdat[19:22, 11], #Micro & nauplii
#                                     tempdat[27:45, 11], #Cyclopoids
#                                     tempdat[47:64, 11], #Calanoids
#                                     tempdat[19:39, 27], #Cladocera
#                                     tempdat[43:43, 27], #Harpacticoids
#                                     tempdat[47:62, 27])) #Macrozoop
#     ) 
#     
#     tdf$project <- tempdat[6, 1]
#     tdf$sample_id <- tempdat[9, 1]
#     tdf$station_id <- tempdat[6, 22]
#     tdf$sheet_id <- i
#     tdf$sample_date <- paste0(tempdat[6 , c(10:13,6:9)], collapse = "")
#     tdf$sample_time <- NA
#     tdf$vol_meso <- as.numeric(tempdat[9, 13])
#     tdf$vol_micro <- as.numeric(tempdat[12, 3])
#     tdf$sub_meso <- as.numeric(tempdat[9, 22])
#     tdf$sub_micro <- as.numeric(tempdat[12, 9])
#     
#     zoopdat_bsa <- rbind(zoopdat_bsa, tdf)
#     
#     rm(tempdat, tdf, i)
#   } 
#   rm(sheets, file)
# }

# Bind bsa and icf zoop data ---------------------------------------------

zoopdat <- rbind(zoopdat_bsa, zoopdat_icf)
zoopdat <- zoopdat[is.na(zoopdat$count) == F, ]

critters <- zoopdat[duplicated(zoopdat$taxon) == F,c("taxon", "classification")]
write.csv(critters, "data/critters.csv", row.names = F)

zooplookup <- read.csv("C:/Users/eholmes/Documents/R/Projects/NDFS-Projects/IEP_Poster_2024/data/Zoop/YB_TaxonomyTable.csv")

unique(zoopdat$taxon) %in% zooplookup$Organism


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
ggplot(ltd, aes(x = rotations)) + geom_histogram()


dput(colnames(ltd))

zoop <- merge(zoopdat_icf, ltd, by.x = c("station_id", "sample_date"), by.y = c("sampling_area_number", "sampling_event_date"), all.x = T)

# CPUE calculation --------------------------------------------------------

zoop$NetAreaZoop = pi*.25^2
zoop$volume <- ifelse(zoop$flow_meter_speed == "Regular",
                      zoop$rotations * 26873 * zoop$NetAreaZoop/999999,
                      ifelse(zoop$flow_meter_speed == "Low",
                             zoop$rotations * 57560 * zoop$NetAreaZoop/999999, NA))

zoop$sampfraction <- ifelse(zoop$classification == "Microzooplankton and nauplii",
                            zoop$subsample*zoop$sub_micro/zoop$vol_micro,
                            zoop$subsample*zoop$sub_meso/zoop$vol_meso)


zoop$CPUE <-  round((zoop$count/zoop$sampfraction)/zoop$volume,3)
ggplot(zoop, aes(x = CPUE)) + geom_histogram() + scale_x_log10()
