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
events <- read.csv(here(paste("analysis/data/raw_data/06_Sleep_Scoring_Comments_",scorer,".csv", sep="")))

# 3 Load scored location data ----
raw_metadata <- read.csv(here("analysis/data/raw_data/00_Raw_Scoring_Metadata.csv"))
raw_metadata$R.Time <- mdy_hms(raw_metadata$Corrected.Date.Time)

# 3 Process scored sleep data ----

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
  events <- events %>%
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

}



