# Title: Transferring Excel Metadata to code-friendly Date Times & Values ------
# Author: Jessica Kendall-Bar
# Date: 6/26/2021

# 0 Setup ----
library(ggplot2)
library(lubridate)
library(here)
library(tidyverse)
library(scales)
library(dplyr)
library(ggnewscale)
library(cowplot)
library(ggeasy)
library(conflicted)
library(devtools)

conflict_prefer("here","here")
conflict_prefer("filter","dplyr")
conflict_prefer("mutate","dplyr")

# * 0A Save color schemes ------------------------------------------------------
sleep.col = c("Certain REM Sleep" = "#FCBE46", "Putative REM Sleep" = "#FCBE46",
              "HV Slow Wave Sleep" = "#41b6c4", "LV Slow Wave Sleep" = "#c7e9b4", "Drowsiness"="#BBA9CF",
              "Quiet Waking" = "#225ea8","Active Waking"= "#0c2c84","Unscorable"="#D7D7D7")

sleep.col2 = c("Active Waking"= "#FF7F7F", "Quiet Waking" = "#ACD7CA",
               "HV Slow Wave Sleep" = "#A3CEED", "LV Slow Wave Sleep" = "#A3CEED",
               "Certain REM Sleep" = "#FFC000", "Putative REM Sleep" = "#FFC000")

simple.sleep.col = c("Unscorable"="#D7D7D7", "Active Waking"= "#0c2c84","Quiet Waking" = "#225ea8",
                     "Drowsiness"="#BBA9CF","REM" = "#FCBE46", "SWS" = "#41b6c4")

resp.col = c("Eupnea"= "#CDEBE6", "transition to Eupnea"="#80cdc1",
             "Apnea"="#018571", "transition to Apnea" = "#80cdc1", "Unscorable"="#D7D7D7")
resp.col2 = c("Eupnea"= "#a6611a", "transition to Eupnea"="#dfc27d",
              "Apnea"="#018571", "transition to Apnea" = "#80cdc1")

location.col = c("LAND"="#DBBFA2","SHALLOW WATER"="#39CCCA", "DEEP WATER"="#225ea8","OPEN OCEAN"="#0c2c84")


# 1 Load seal metadata ----
## Load all metadata, make valid names, pivot longer, save as CSV.
metadata <- read_csv(here::here('analysis/data/raw_data/00_Sleep_Study_Metadata.csv'),
                     na = c("NA", "n/a", ""),
                     skip = 1) %>% #skipping other names
  t()
colnames(metadata) <- make.names(metadata[1, ], unique = TRUE)
metadata <- metadata[-1, ]
metadata <- as.data.frame(metadata)
metadata <- metadata %>% rownames_to_column("Nickname")

#For more efficient storage in long format
metadata <- metadata %>% pivot_longer(cols = 2:length(metadata), names_to = "description", names_repair = "unique" )
colnames(metadata) <- c("TestID", "description", "value")
metadata <- metadata[, c(2,3,1)]
metadata <- na.omit(metadata) # remove any rows with NAs
metadata$TestID <- as.factor(metadata$TestID)
metadata$R.Time <- mdy_hms(metadata$value)
metadata$R.Time <- as.POSIXct(metadata$R.Time, format = "%Y-%m-%d %H:%M:%OS")
metadata$Matlab.Time <- as.POSIXct(metadata$R.Time, origin = '0000-01-01')
# Optional: save metadata in long format
# write.csv(metadata,here::here('analysis/data/derived_data/01_Sleep_Study_Metadata.csv'), row.names = FALSE)

# Get list of all seals
SealIDs <- unique(metadata$TestID)
print(SealIDs) #All animal nicknames

# 2 Load scored sleep data ----
# Data scored by
scorer = "JKB" # replace with scorer initials
# Read CSV made from copy/pasted Windows>Comments in LabChart;
raw_events <- read.csv(here(paste("analysis/data/raw_data/06_Sleep_Scoring_Comments_",scorer,".csv", sep="")))

# 3 Load scored location data ----
raw_metadata <- read.csv(here("analysis/data/raw_data/00_Raw_Scoring_Metadata.csv"))
raw_metadata$R.Time <- mdy_hms(raw_metadata$Corrected.Date.Time)

# 3 Process scored data ----

for (i in 1:length(SealIDs)){

  # * A Filter seal-specific metadata ----
  SealID <- SealIDs[i]                        # cycle through all seals
  info <- filter(metadata, TestID==SealID)    # filter metadata seal of choice
  description <- unique(info$description)     # save description for column titles
  info <- data.frame(t(info$value))           # transpose to wide format
  colnames(info) <- description               # use description for column titles

  paste("Retrieving data for", SealID,"who was", info$Age, "and", info$Mass.animal._kg,"kilograms.")
  paste("Logger was started at", info$Logger.Start)
  paste("The logger restarted", info$Number.of.Restarts, "times")
  paste("The logger was deployed at", info$Deploy.Latitude, info$Deploy.Longitude)
  if (str_detect(info$Device.Failure,"Yes") == "TRUE"){
    paste("The device was blinking and not recording upon retrieval.")
  } else{
    paste("The device did not fail.")
  }

  # Read in important metadata including time of instrument attachment/removal and time of device failure (if applicable)
  ON.ANIMAL <- mdy_hms(info$ON.ANIMAL)
  OFF.ANIMAL <- mdy_hms(info$OFF.ANIMAL)
  FAILED.END.RECORDING<-as.numeric(info$Recording.Duration_s) + mdy_hms(info$Logger.Start)

  if (str_detect(info$Device.Failure,"Yes") == "TRUE"){
    paste("The device was blinking and not recording upon retrieval.")
    DURATION <- difftime(FAILED.END.RECORDING,ON.ANIMAL,units="hours")
    paste(SealID, "Recording duration:",DURATION)
  } else{
    paste("The device did not stop.")
    DURATION <- difftime(OFF.ANIMAL,ON.ANIMAL,units="hours")
    paste(SealID, "Recording duration:",DURATION, "hours")
  }

  # * B Filter seal-specific scoring data ----
  events <- raw_events %>%
    filter(Seal_ID==SealID)

  if (nrow(events)==0){
    print("Scoring Data DOES NOT EXIST")
    next
  } else {
    print("Scoring Data Exists")
  }

  # Get date time from seconds column and instrument attachment time
  events$Onset_sec <- round(events$Seconds)
  events$R.Time <- events$Onset_sec + ON.ANIMAL

  # Check out the data
  unique(factor(events$Comment))

  # * C Process Location Data ----
  # Separate times related to animal location (land, shallow water, continental shelf, open ocean)
  WaterData <- raw_metadata %>%
    filter(Seal_ID==SealID) %>%
    filter(Comment=="Animal Enters Water"|
             Comment=="Animal Exits Water"|
             Comment=="Animal Leaves Shallow Water"|
             Comment=="Animal Returns to Shallow Water"|
             Comment=="Animal Leaves Continental Shelf Water"|
             Comment=="Animal Returns to Continental Shelf Water"|
             Comment=="Instrument ON Animal") %>%
    select('Seconds','Comment','R.Time')
  WaterData <- WaterData[order(WaterData$Seconds),]

  # Calculate duration of each state using either off-animal time or device failure time.
  for (j in 1:nrow(WaterData)){
    if (j<nrow(WaterData)){
      WaterData$duration[j] <- as.double(as.duration(interval(WaterData$R.Time[j],WaterData$R.Time[j+1])))
    }else if (j==nrow(WaterData)){
      if (str_detect(info$Device.Failure,"Yes") == "TRUE"){
        print("The device was blinking and not recording upon retrieval.")
        WaterData$duration[j] <- as.double(as.duration(interval(WaterData$R.Time[j],FAILED.END.RECORDING)))
      } else{
        WaterData$duration[j] <- as.double(as.duration(interval(WaterData$R.Time[j],OFF.ANIMAL)))
        print("The device did not fail.")
      }
    }else {
      print("Error")
    }
  }

  # Replace comments with desired code name & value
  for (j in 1:nrow(WaterData)){
    if (WaterData$Comment[j] == "Instrument ON Animal" |
        WaterData$Comment[j] =="Animal Exits Water"){
      WaterData$Code[j] <- "LAND"
      WaterData$Num[j] <- 0
    }else if (WaterData$Comment[j] == "Animal Enters Water"|
              WaterData$Comment[j] == "Animal Returns to Shallow Water"){
      WaterData$Code[j] <- "SHALLOW WATER"
      WaterData$Num[j] <- 1
    }else if (WaterData$Comment[j] == "Animal Leaves Shallow Water"|
              WaterData$Comment[j] == "Animal Returns to Continental Shelf Water"){
      WaterData$Code[j] <- "DEEP WATER"
      WaterData$Num[j] <- 2
    }else if (WaterData$Comment[j] == "Animal Leaves Continental Shelf Water"){
      WaterData$Code[j] <- "OPEN OCEAN"
      WaterData$Num[j] <- 3
    }else{
      print("undefined event present")
      print(resp_events$Comment[j])
    }
  }

  # FUNCTION THAT DOES THE SAME THING AS ABOVE
  WaterData$WaterCode <- make_waterlabels(WaterData$Comment)

  # * D Process Respiration Data ----

  # Get respiratory comments only
  resp_events <- events %>%
    filter(Comment=="APNEA"|
             Comment=="First Breath"|
             Comment=="Last Breath"|
             Comment=="Anticipatory HR Increase"|
             Comment=="Heart Patterns Unscorable"|
             Comment=="Heart Patterns Scorable") %>%
    select('Seconds','Comment','R.Time')

  # Calculate duration of each state using either off-animal time or device failure time.
  for (j in 1:nrow(resp_events)){
    if (j<nrow(resp_events)){
      resp_events$duration[j] <- as.double(as.duration(interval(resp_events$R.Time[j],resp_events$R.Time[j+1])))
    }else if (j==nrow(resp_events)){
      if (str_detect(info$Device.Failure,"Yes") == "TRUE"){
        print("The device was blinking and not recording upon retrieval.")
        resp_events$duration[j] <- as.double(as.duration(interval(resp_events$R.Time[j],FAILED.END.RECORDING)))
      } else{
        resp_events$duration[j] <- as.double(as.duration(interval(resp_events$R.Time[j],OFF.ANIMAL)))
        print("The device did not fail.")
      }
    }else {
      print("Error")
    }
  }

  # Replace comments with desired code name & value
  for (j in 1:nrow(resp_events)){
    if (resp_events$Comment[j] == "APNEA"){
      resp_events$Code[j] <- "Apnea"
      resp_events$Num[j] <- -2
    }else if (resp_events$Comment[j] == "Anticipatory HR Increase"){
      resp_events$Code[j] <- "transition to Eupnea"
      resp_events$Num[j] <- 1
    }else if (resp_events$Comment[j] == "First Breath"){
      resp_events$Code[j] <- "Eupnea"
      resp_events$Num[j] <- 2
    }else if (resp_events$Comment[j] == "Last Breath"){
      resp_events$Code[j] <- "transition to Apnea"
      resp_events$Num[j] <- -1
    }else if (resp_events$Comment[j] == "Heart Patterns Scorable"){
      resp_events$Code[j] <- "Eupnea"
      resp_events$Num[j] <- 2
    }else if (resp_events$Comment[j] == "Heart Patterns Unscorable"){
      resp_events$Code[j] <- "Unscorable"
      resp_events$Num[j] <- 0
    }else{
      print("undefined event present")
      print(resp_events$Comment[j])
    }
  }

  # Initializing variables to store device restart information
  Restart_Start <- numeric()
  Restart_End <- numeric()

  # Check sequence for any deviation from Apnea > Breath > First Breath > Last Breath > Apnea etc.
  for (j in 2:nrow(resp_events)){
    if (resp_events$Code[j] == "Apnea"){
      if (resp_events$Code[j-1] != "Last Breath"){
        paste("Missing 'Breath' end bradycardia comment, check timestamp",resp_events$R.Time[j])
      }
    }else if (resp_events$Code[j] == "transition to Eupnea"){
      if (resp_events$Code[j-1] != "Apnea"){
        paste("Missing 'Apnea' start bradycardia comment, check timestamp",resp_events$R.Time[j])
      }
    }else if (resp_events$Code[j] == "Eupnea"){
      if (resp_events$Code[j-1] != "transition to Eupnea"){
        paste("Missing 'Breath' end bradycardia comment, check timestamp",resp_events$R.Time[j])
      }
    }else if (resp_events$Code[j] == "transition to Apnea"){
      if (resp_events$Code[j-1] != "Eupnea"){
        paste("Missing 'Last Breath' end breathing comment, check timestamp",resp_events$R.Time[j])
      }
    }else if (resp_events$Code[j] == "Unscorable"){
      print("Restart from")
      print(resp_events$R.Time[j])
      print("to")
      print(resp_events$R.Time[j+1])
      Restart_Start[j] <- resp_events$R.Time[j]
      Restart_End[j+1] <- resp_events$R.Time[j+1]
    }else{
      print("undefined event present")
    }
  }
  Restart_Start <- Restart_Start[!is.na(Restart_Start)]
  Restart_End <- Restart_End[!is.na(Restart_End)]

  Restarts <- data.frame(Restart_Start,Restart_End)
  colnames(Restarts) <- c('Restart_Start','Restart_End')

  # * E Process Sleep Data ----

  sleep_events <- events %>%
    filter(Comment == 'Instrument ON Animal'|
             Comment == 'Instrument OFF Animal'|
             Comment == 'MVMT (from calm)'|
             Comment == 'JOLT (from sleep)'|
             Comment == 'CALM (from motion)'|
             Comment == 'WAKE (from sleep)'|
             Comment == 'SWS1'|
             Comment == 'SWS2'|
             Comment == 'LS (light sleep)'|
             Comment == 'REM1'|
             Comment == 'REM2'|
             Comment == 'Sleep State Unscorable') %>%
    select('Seconds','Comment','R.Time')

  # Calculate duration of each state using either off-animal time or device failure time.
  for (j in 1:nrow(sleep_events)){
    if (j<nrow(sleep_events)){
      sleep_events$duration[j] <- as.double(as.duration(interval(sleep_events$R.Time[j],sleep_events$R.Time[j+1])))
    }else if (j==nrow(sleep_events)){
      if (str_detect(info$Device.Failure,"Yes") == "TRUE"){
        print("The device was blinking and not recording upon retrieval.")
        sleep_events$duration[j] <- as.double(as.duration(interval(sleep_events$R.Time[j],FAILED.END.RECORDING)))
        last_record = FAILED.END.RECORDING
      } else{
        sleep_events$duration[j] <- as.double(as.duration(interval(sleep_events$R.Time[j],OFF.ANIMAL)))
        print("The device did not fail.")
        last_record = OFF.ANIMAL
      }
    }else {
      print("Error")
    }
  }

  unique(sleep_events$Comment)

  # Replace comments with desired code name & value
  for (j in 1:nrow(sleep_events)){
    if (sleep_events$Comment[j] == "MVMT (from calm)" ||
        sleep_events$Comment[j] == "JOLT (from sleep)"){
      sleep_events$Code[j] <- "Active Waking"
      sleep_events$Num[j] <- 1
    }else if (sleep_events$Comment[j] == "CALM (from motion)" ||
              sleep_events$Comment[j] == "WAKE (from sleep)" ||
              sleep_events$Comment[j] == "Instrument ON Animal"){
      sleep_events$Code[j] <- "Quiet Waking"
      sleep_events$Num[j] <- 2
    }else if (sleep_events$Comment[j] == "SWS1"){
      sleep_events$Code[j] <- "LV Slow Wave Sleep"
      sleep_events$Num[j] <- 4
    }else if (sleep_events$Comment[j] == "SWS2"){
      sleep_events$Code[j] <- "HV Slow Wave Sleep"
      sleep_events$Num[j] <- 5
    }else if (sleep_events$Comment[j] == "LS (light sleep)"){
      sleep_events$Code[j] <- "Drowsiness"
      sleep_events$Num[j] <- 3
    }else if (sleep_events$Comment[j] == "REM2"){
      sleep_events$Code[j] <- "Certain REM Sleep"
      sleep_events$Num[j] <- 7
    }else if (sleep_events$Comment[j] == "REM1"){
      sleep_events$Code[j] <- "Putative REM Sleep"
      sleep_events$Num[j] <- 6
    }else if (sleep_events$Comment[j] == "Sleep State Unscorable"){
      sleep_events$Code[j] <- "Unscorable"
      sleep_events$Num[j] <- 0
    }else{
      print("undefined event present")
      print(sleep_events$Comment[j])
    }
  }

  sleep_events$SealID <- SealID
  sleep_events$Recording.ID = info$Recording.ID
  sleep_events$ID = sub("_[^_]+$", "", info$Recording.ID)
  resp_events$SealID <- SealID
  resp_events$Recording.ID = info$Recording.ID
  resp_events$ID = sub("_[^_]+$", "", info$Recording.ID)
  WaterData$SealID <- SealID
  WaterData$Recording.ID = info$Recording.ID
  WaterData$ID = sub("_[^_]+$", "", info$Recording.ID)

  if (i==1){
    # ONLY RUN FOR FIRST ANIMAL
    summary_sleep_events <- sleep_events
    summary_resp_events <- resp_events
    summary_WaterData <- WaterData
  }
  if (i>1){
    #RUN FOR SUBSEQUENT ANIMALS
    summary_sleep_events <- rbind(summary_sleep_events, sleep_events)
    summary_resp_events <- rbind(summary_resp_events, resp_events)
    summary_WaterData <- rbind(summary_WaterData, WaterData)
  }

  print("Scored events processed.")
  # write.csv(sleep_events,here(paste("analysis/data/derived_data/",SealID,"_06_Sleep-Events_",scorer,".csv",sep="")), row.names = FALSE)
  # write.csv(resp_events,here(paste("analysis/data/derived_data/",SealID,"_06_Resp-Events_",scorer,".csv",sep="")), row.names = FALSE)
  # write.csv(WaterData,here(paste("analysis/data/derived_data/",SealID,"_06_Water-Data_",scorer,".csv",sep="")), row.names = FALSE)
}

saveRDS(summary_sleep_events,
        file = here(paste("analysis/data/derived_data/06_Summary_Water-Data.rds",sep="")),
        ascii = FALSE, version = NULL,compress = TRUE)
saveRDS(summary_resp_events,
        file = here(paste("analysis/data/derived_data/06_Summary_Resp-Events.rds",sep="")),
        ascii = FALSE, version = NULL,compress = TRUE)
saveRDS(summary_sleep_events,
        file = here(paste("analysis/data/derived_data/06_Summary_Sleep-Events.rds",sep="")),
        ascii = FALSE, version = NULL,compress = TRUE)
saveRDS(metadata,
        file = here(paste("analysis/data/derived_data/06_Metadata.rds",sep="")),
        ascii = FALSE, version = NULL,compress = TRUE)

