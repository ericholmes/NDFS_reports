## Zoop scraper
library(readxl)
library(tidyverse)

# Load and compile data ---------------------------------------------------

files <- list.files("data/zoopraw/ICF")

zoopdat_icf <- data.frame()

for(file in files){
  sheets <- readxl::excel_sheets(paste0("data/zoopraw/ICF/", file))
  sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC")]
  
  for(i in sheets){
    print(paste(file, i))
    tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/ICF/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
    
    tdf <- data.frame(taxon = c(tempdat[21:46, 1], #Micro & nauplii
                                tempdat[48:70, 1], #Cyclopoids
                                tempdat[72:96, 1], #Calanoids
                                tempdat[21:47, 17], #Cladocera
                                tempdat[59:59, 17], #Harpacticoids
                                tempdat[63:82, 17]), #Macrozoop
                      
                      classification = c(rep("Microzooplankton and nauplii", 26),
                                         rep("Cyclopoid", 23),
                                         rep("Calanoid", 25),
                                         rep("Cladocera", 27),
                                         rep("Harpacticoid", 1),
                                         rep("Macrozooplankton", 20)),
                      
                      count = as.numeric(c(tempdat[21:46, 9], #Micro & nauplii
                                tempdat[48:70, 9], #Cyclopoids
                                tempdat[72:96, 9], #Calanoids
                                tempdat[21:47, 25], #Cladocera
                                tempdat[59:59, 25], #Harpacticoids
                                tempdat[63:82, 25])), #Macrozoop
                      
                      subsample = as.numeric(c(tempdat[21:46, 11], #Micro & nauplii
                                    tempdat[48:70, 11], #Cyclopoids
                                    tempdat[72:96, 11], #Calanoids
                                    tempdat[21:47, 27], #Cladocera
                                    tempdat[59:59, 27], #Harpacticoids
                                    tempdat[63:82, 27])) #Macrozoop
                      ) 
    tdf <- tdf[is.na(tdf$count) == F, ]
    tdf[is.na(tdf$subsample),] <- mean(tdf$subsample, na.rm = T)
    tdf$file <- file
    tdf$sheet_id <- i
    tdf$project <- tempdat[6, 1]
    tdf$sample_id <- tempdat[9, 1]
    tdf$station_id <- tempdat[6, 22]
    
    tdf$sample_date <- as.character(substr(tdf$sample_id, 1, 8))
    tdf$sample_time <- as.character(substr(tdf$sample_id, 10, 13))
    tdf$vol_meso <- as.numeric(tempdat[9, 13])
    tdf$vol_micro <- as.numeric(tempdat[12, 3])
    tdf$sub_meso <- as.numeric(tempdat[9, 22])
    tdf$sub_micro <- as.numeric(tempdat[12, 9])
    tdf$net_mesh <- ifelse(!is.na(tempdat[101,10]),
                           ifelse(tolower(tempdat[101,9]) == "x", "150", "50"), "Farfegnugan")
    
    zoopdat_icf <- rbind(zoopdat_icf, tdf)
    print(as.character(substr(tdf$sample_id, 1, 8))[1])
    rm(tempdat, tdf, i)
  } 
  rm(sheets, file)
}

zoopdat_icf$sample_date <- as.Date(zoopdat_icf$sample_date, format = "%Y%m%d")

# # BSA ---------------------------------------------------------------------

files <- list.files("data/zoopraw/BSA")

index_bsa <-data.frame()

for(file in files){
  sheets <- readxl::excel_sheets(paste0("data/zoopraw/BSA/", file))
  sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC") & 
                     !grepl(sheets, pattern = "RPD") & !grepl(sheets, pattern = "QAQC") &
                              !grepl(sheets, pattern = "DUP")]
  for(i in sheets){
    print(paste(file, i))
    
    tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/BSA/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
    
    ##develop sheet index
    vec_1 <- tempdat[,1]
    vec_17 <- tempdat[,17]
    mn_start_index <- grep("MICROZOOPLANKTON & NAUPLII", tempdat[,1])
    cyc_start_index <- grep("CYCLOPOIDS", tempdat[,1])
    cal_start_index <- grep("CALANOIDS", tempdat[,1])
    clad_start_index <- grep("CLADOCERA", tempdat[,17])
    harp_start_index <- grep("HARPACTICOIDS", tempdat[,17])
    macro_start_index <- grep("MACROZOOPLANKTON", tempdat[,17])
    grep("Unable", tempdat[,18])
    
    index_bsa <- rbind(index_bsa, data.frame(
      file = file,
      sheet = i,
      sample_id = tempdat[9, 1], 
      station_id = tempdat[grep("Station", tempdat[,23]) - 1, 22],
      sample_date = paste0(tempdat[6 , c(10:13,6:9)], collapse = ""),
      mn_start = mn_start_index,
      mn_end = cyc_start_index - 1,
      cyc_start = cyc_start_index,
      cyc_end = cal_start_index - 1,
      cal_start = cal_start_index,
      cal_end = which(is.na(vec_1[cal_start_index:length(vec_1)]))[1] + cal_start_index - 2,
      clad_start = clad_start_index,
      clad_end = harp_start_index - 1,
      harp_start = harp_start_index, harp_end = harp_start_index,
      macro_start = macro_start_index,
      macro_end = grep("unable", tolower(tempdat[,18])) - 1)
      )
    
    rm(tempdat, tdf, i)
  }
  rm(sheets, file)
}

zoopdat_bsa <- data.frame()

for(file in files){
  sheets <- readxl::excel_sheets(paste0("data/zoopraw/BSA/", file))
  sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC") & 
                     !grepl(sheets, pattern = "RPD") & !grepl(sheets, pattern = "QAQC") &
                     !grepl(sheets, pattern = "DUP")]
  for(i in sheets){
    
    
    print(paste(file, i))
    tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/BSA/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
    tempindex <- index_bsa[index_bsa$file == file & index_bsa$sheet == i,]
    
    tdf <- data.frame(taxon = c(tempdat[tempindex$mn_start:tempindex$mn_end, 1], #Micro & nauplii
                                tempdat[tempindex$cyc_start:tempindex$cyc_end, 1], #Cyclopoids
                                tempdat[tempindex$cal_start:tempindex$cal_end, 1], #Calanoids
                                tempdat[tempindex$clad_start:tempindex$clad_end, 17], #Cladocera
                                tempdat[tempindex$harp_start:tempindex$harp_end, 17], #Harpacticoids
                                tempdat[tempindex$macro_start:tempindex$macro_end, 17]), #Macrozoop
                      
                      classification = c(rep("Microzooplankton and nauplii", length(tempindex$mn_start:tempindex$mn_end)),
                                         rep("Cyclopoid", length(tempindex$cyc_start:tempindex$cyc_end)),
                                         rep("Calanoid", length(tempindex$cal_start:tempindex$cal_end)),
                                         rep("Cladocera", length(tempindex$clad_start:tempindex$clad_end)),
                                         rep("Harpacticoid", length(tempindex$harp_start:tempindex$harp_end)),
                                         rep("Macrozooplankton", length(tempindex$macro_start:tempindex$macro_end))),
                      
                      count = as.integer(c(tempdat[tempindex$mn_start:tempindex$mn_end, 9], #Micro & nauplii
                                tempdat[tempindex$cyc_start:tempindex$cyc_end, 9], #Cyclopoids
                                tempdat[tempindex$cal_start:tempindex$cal_end, 9], #Calanoids
                                tempdat[tempindex$clad_start:tempindex$clad_end, 25], #Cladocera
                                tempdat[tempindex$harp_start:tempindex$harp_end, 25], #Harpacticoids
                                tempdat[tempindex$macro_start:tempindex$macro_end, 25])), #Macrozoop
                      
                      subsample = as.integer(c(tempdat[tempindex$mn_start:tempindex$mn_end, 11], #Micro & nauplii
                                               tempdat[tempindex$cyc_start:tempindex$cyc_end, 11], #Cyclopoids
                                               tempdat[tempindex$cal_start:tempindex$cal_end, 11], #Calanoids
                                               tempdat[tempindex$clad_start:tempindex$clad_end, 27], #Cladocera
                                               tempdat[tempindex$harp_start:tempindex$harp_end, 27], #Harpacticoids
                                               tempdat[tempindex$macro_start:tempindex$macro_end, 27])) #Macrozoop
    )
    
    tdf <- tdf[is.na(tdf$count) == F, ]
    tdf[is.na(tdf$subsample),] <- mean(tdf$subsample, na.rm = T)
    if(nrow(tdf)>0){
      tdf$file <- file
      tdf$sheet_id <- i
      tdf$project <- tempdat[grep("Project", tempdat[,1]) - 1, 1]
      tdf$sample_id <- ifelse(length(tempdat[grep("Sample ID", tempdat[,1]) - 1, 1]) > 0,
                              tempdat[grep("Sample ID", tempdat[,1]) - 1, 1], "NULL")
      tdf$station_id <- tempdat[grep("Station", tempdat[,23]) - 1, 22]
      tdf$sample_date <- paste0(tempdat[grep("month", tempdat[,6]) - 1, c(10:13,6:9)], collapse = "")
      tdf$sample_time <- tempdat[grep("Time", tempdat[,6]) - 1, 6]
      tdf$vol_meso <- sum(as.numeric(tempdat[grep("V1", tempdat[,11]), 13:17]), na.rm = T)
      tdf$vol_micro <- sum(as.numeric(tempdat[grep("V2", tempdat[,1]), c(3,4)]), na.rm = T)
      tdf$sub_meso <- sum(as.numeric(tempdat[grep("Sub1", tempdat[,19]), 21:23]), na.rm = T)
      
      tdf$sub_micro <- ifelse(length(sum(as.numeric(tempdat[grep("Sub2", tempdat[,6]), c(8,9)]), na.rm = T)) > 0,
                              sum(as.numeric(tempdat[grep("Sub2", tempdat[,6]),  c(8,9)]), na.rm = T), NA)
      #Solve ICF artifact
      tdf$vol_micro <- ifelse(tdf$vol_micro == 0, tdf$vol_meso, tdf$vol_micro)
      tdf$sub_micro <- ifelse(tdf$sub_micro == 0, tdf$sub_meso, tdf$sub_micro)
      
      tdf$net_mesh <- ifelse(tolower(tempdat[grep("150um", tempdat[,10]),9]) == "x", "150", "50")
      
      zoopdat_bsa <- rbind(zoopdat_bsa, tdf[is.na(tdf$count) == F, ])
    }
  }
}

zoopdat_bsa$sample_date <- as.Date(zoopdat_bsa$sample_date, format = "%Y%m%d")

# Bind bsa and icf zoop data ---------------------------------------------

# zoopdat <- rbind(zoopdat_bsa, zoopdat_icf)
zoopdat <- zoopdat_bsa
zoopdat <- zoopdat[is.na(zoopdat$count) == F, ]

critters <- zoopdat[duplicated(zoopdat$taxon) == F,c("taxon", "classification")]
# write.csv(critters, "data/critters.csv", row.names = F)

zooplookup <- read.csv("C:/Users/eholmes/Documents/R/Projects/NDFS-Projects/IEP_Poster_2024/data/Zoop/YB_TaxonomyTable.csv")

unique(zoopdat$taxon) %in% zooplookup$Organism
glimpse(zoopdat)
unique(paste(zoopdat[is.na(zoopdat$net_mesh), "sample_date"], zoopdat[is.na(zoopdat$net_mesh), "station_id"]))

# Add rotations data ------------------------------------------------------

ltdnames <- colnames(janitor::clean_names(readxl::read_excel("data/YBFMP_LowerTrophic_Data_WORKING_20231102.xlsx", skip = 1)))

ltd <- readxl::read_excel("data/YBFMP_LowerTrophic_Data_WORKING_20231102.xlsx", skip = 3, col_names = ltdnames)

ltd$year <- format(ltd$sampling_event_date, format = "%Y")
ltd$rotations <- abs(ltd$flow_meter_end_150 - ltd$flow_meter_start_150)

ltd <- ltd[is.na(ltd$flow_meter_end_150) == F, c("measuring_program_short_name", "sampling_event_date", "sampling_event_time", 
                                                                    "sampling_area_number", "sampling_event_number", "condition_code", 
                                                                    "sampling_altered", "net_type", "start_time_150", "stop_time_150", 
                                                                    "set_time_150","flow_meter_start_150", "flow_meter_end_150", "flow_meter_speed", 
                                                                     "field_comments",  "year", "rotations")]


ltd$rotations <- ifelse(ltd$rotations > 990000, 1000000 - ltd$flow_meter_end_150 + ltd$flow_meter_start_150, ltd$rotations)
ltd$sampling_event_date <- as.Date(ltd$sampling_event_date)
ggplot(ltd, aes(x = rotations)) + geom_histogram()

ltd <- data.frame(ltd)

histrots <- read.csv("data/zoopraw/Zoop_rotations_historical.csv")

colnames(histrots) <-c("PhysicalDataID", "sampling_event_date", "sampling_area_number", "flow_meter_start_150", 
                       "flow_meter_end_150", "X50_ZoopStartMeter", "X50_ZoopEndMeter", 
                       "flow_meter_speed", "field_comments")

histrots$sampling_event_date <- as.Date(histrots$sampling_event_date, format = "%m/%d/%Y")

ltdfull <- rbind(ltd[,c("sampling_event_date", "sampling_area_number", "flow_meter_start_150", 
                        "flow_meter_end_150", "flow_meter_speed", "field_comments")], 
                 histrots[,c("sampling_event_date", "sampling_area_number", "flow_meter_start_150", 
                             "flow_meter_end_150", "flow_meter_speed", "field_comments")])

zoop <- merge(zoopdat[zoopdat$net_mesh == "150",], ltdfull, 
              by.x = c("station_id", "sample_date"), by.y = c("sampling_area_number", "sampling_event_date"), all.x = T)

ltdrvb <- ltd[ltd$sampling_area_number == "RVB" & ltd$sampling_event_date == as.Date("2023-09-06"),]

zoop <- zoop[is.na(zoop$count) == F & 
               zoop$station_id %in% c("BL5", "I-80", "I80", "LIB", "LIS",  
                                      "PRS", "RCs", "RCS", "RD22", "RIO VISTA", "RMB", 
                                      "RV", "RVB", "RYI", "SDI", "SHR", "STTD"),]
# CPUE calculation --------------------------------------------------------

zoop$rotations <- abs(zoop$flow_meter_end_150 - zoop$flow_meter_start_150)
zoop$rotations <- ifelse(zoop$rotations > 990000, 1000000 - zoop$flow_meter_end_150 + zoop$flow_meter_start_150, zoop$rotations)
zoop$NetAreaZoop = pi*.25^2
zoop$volume <- ifelse(zoop$flow_meter_speed %in% c("Regular", "Reg"),
                      zoop$rotations * 26873 * zoop$NetAreaZoop/999999,
                      ifelse(zoop$flow_meter_speed == "Low",
                             zoop$rotations * 57560 * zoop$NetAreaZoop/999999, NA))

zoop$sampfraction <- ifelse(zoop$classification == "Microzooplankton and nauplii",
                            zoop$subsample*zoop$sub_micro/zoop$vol_micro,
                            zoop$subsample*zoop$sub_meso/zoop$vol_meso)

zoopsamp <- zoop[is.na(zoop$sampfraction),]

zoop$CPUE <-  round((zoop$count/zoop$sampfraction)/zoop$volume,3)

ggplot(zoop, aes(x = CPUE)) + geom_histogram() + scale_x_log10()
ggplot(zoop, aes(x = CPUE)) + geom_histogram() + scale_x_log10() + facet_wrap(station_id ~ .)

zoop$jday <- as.integer(format(zoop$sample_date, format = "%j"))
zoop$Week <- as.integer(format(zoop$sample_date, format = "%W"))
zoop <- zoop[is.na(zoop$count) == F,]
save(zoop, file = "data/NDFS2017-2023_zoop.Rdata")
