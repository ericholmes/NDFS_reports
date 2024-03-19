## Zoop scraper
require(readxl)

files <- list.files("data/zoopraw/")

zoopdat <- data.frame()

for(file in files){
  sheets <- readxl::excel_sheets(paste0("data/zoopraw/", file))
  sheets <- sheets[grepl(sheets, pattern = " ") & !grepl(sheets, pattern = "COC")]
  
  for(i in sheets){
    print(paste(file, i))
    tempdat <- data.frame(readxl::read_excel(paste0("data/zoopraw/", file), sheet = i, col_names = NA, .name_repair = "minimal"))
    
    tdf <- data.frame(taxon = c(tempdat[21:45, 1], #Micro & nauplii
                                tempdat[48:69, 1], #Cyclopoids
                                tempdat[72:96, 1], #Calanoids
                                tempdat[21:47, 17], #Cladocera
                                tempdat[59:59, 17], #Harpacticoids
                                tempdat[63:82, 17]), #Macrozoop
                      count = c(tempdat[21:45, 9], #Micro & nauplii
                                tempdat[48:69, 9], #Cyclopoids
                                tempdat[72:96, 9], #Calanoids
                                tempdat[21:47, 25], #Cladocera
                                tempdat[59:59, 25], #Harpacticoids
                                tempdat[63:82, 25]), #Macrozoop
                      subsample = c(tempdat[21:45, 11], #Micro & nauplii
                                    tempdat[48:69, 11], #Cyclopoids
                                    tempdat[72:96, 11], #Calanoids
                                    tempdat[21:47, 27], #Cladocera
                                    tempdat[59:59, 27], #Harpacticoids
                                    tempdat[63:82, 27])) #Macrozoop
    
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
    
    zoopdat <- rbind(zoopdat, tdf)
    
    rm(tempdat, tdf)
  } 
  rm(sheets)
}




