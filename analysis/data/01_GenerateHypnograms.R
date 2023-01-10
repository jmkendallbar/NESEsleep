# 1 Load seal metadata ----
metadata <- readRDS(file = here(paste("analysis/data/derived_data/06_Metadata.rds",sep="")))
WaterData <- readRDS(file = here(paste("analysis/data/derived_data/06_Summary_Water-Data.rds",sep="")))
resp_events <- readRDS(file = here(paste("analysis/data/derived_data/06_Summary_Resp-Events.rds",sep="")))
sleep_events <- readRDS(file = here(paste("analysis/data/derived_data/06_Summary_Sleep-Events.rds",sep="")))

# Get list of all seals
SealIDs <- unique(metadata$TestID)
print(SealIDs) #All animal nicknames

  # Creating hypnograms ----
  # 1Hz histogram first:
  hypno_freq = "1Hz"

  # Add respiratory patterns
  respno_1Hz <- data.frame(rep(resp_events$Code, resp_events$duration))
  respno_1Hz$New <- rep(resp_events$Num, resp_events$duration)
  respno_1Hz$Seconds <- resp_events$Seconds[1]+0:(nrow(respno_1Hz)-1) #add column for seconds elapsed
  respno_1Hz$R.Time <- round_date(resp_events$R.Time[1]+0:(nrow(respno_1Hz)-1),unit="seconds") #add column for R.Time
  colnames(respno_1Hz) <- c("Resp.Code","Resp.Num", "Seconds","Time")

  sleepno_1Hz <- data.frame(rep(sleep_events$Code, sleep_events$duration))
  sleepno_1Hz$New <- rep(sleep_events$Num, sleep_events$duration)
  sleepno_1Hz$Seconds <- sleep_events$Seconds[1]+0:(nrow(sleepno_1Hz)-1)
  sleepno_1Hz$R.Time <- round_date(sleep_events$R.Time[1]+0:(nrow(sleepno_1Hz)-1),unit="seconds")
  colnames(sleepno_1Hz) <- c("Sleep.Code","Sleep.Num","Seconds","Time")

  waterno_1Hz <- data.frame(rep(WaterData$Code, WaterData$duration))
  waterno_1Hz$New <- rep(WaterData$Num, WaterData$duration)
  waterno_1Hz$Seconds <- 0:(nrow(waterno_1Hz)-1)
  waterno_1Hz$R.Time <- round_date(WaterData$R.Time[1]+0:(nrow(waterno_1Hz)-1),unit="seconds")
  waterno_1Hz$SealID <- rep(WaterData$SealID, WaterData$duration)
  waterno_1Hz$Recording.ID <- rep(WaterData$Recording.ID, WaterData$duration)
  waterno_1Hz$ID <- rep(WaterData$ID, WaterData$duration)
  colnames(waterno_1Hz) <- c("Water.Code","Water.Num","Seconds","Time","SealID","Recording.ID","ID")

  hypnogram <- full_join(sleepno_1Hz,respno_1Hz) # joins datasets by times
  hypnogram <- full_join(hypnogram,waterno_1Hz, by="Time") # joins datasets by times
  hypnogram <- na.omit(hypnogram) # deletes end or beginning where there is no breathing or sleep scoring data
  colnames(hypnogram) <- c("Sleep.Code","Sleep.Num","Seconds","R.Time",
                           "Resp.Code","Resp.Num",
                           "Water.Code","Water.Num","Water.sec", "SealID", "Recording.ID", "ID")

  print("Hypnogram generated")

  hypnogram$Date <- as_date(hypnogram$R.Time, tz='UTC') # Can also use: floor_date(R.Time, unit = "day") to get date
  hypnogram$Time <- ymd_hms(paste("2000-01-01",strftime(hypnogram$R.Time,format="%H:%M:%S", tz='UTC')))
  hypnogram$Hour = as.double(format(hypnogram$R.Time, format='%H', tz='UTC'))

  # Simplifying different types of REM/SWS
  hypnogram <- hypnogram %>%
    mutate(Simple.Sleep.Code = replace(Sleep.Code, Sleep.Code=="Certain REM Sleep"|
                                         Sleep.Code=="Putative REM Sleep","REM")) %>%
    mutate(Simple.Sleep.Code = replace(Simple.Sleep.Code, Simple.Sleep.Code=="HV Slow Wave Sleep"|
                                         Simple.Sleep.Code=="LV Slow Wave Sleep","SWS")) %>%
    mutate(Simple.Sleep.Num = replace(Sleep.Num, Sleep.Code=="Certain REM Sleep", 6))


  hypnogram$Simple.Sleep.Code <- factor(hypnogram$Simple.Sleep.Code,
                                        levels = c("Unscorable", "Active Waking", "Quiet Waking",
                                                   "Drowsiness","SWS","REM"))
  hypnogram$Water.Code <- factor(hypnogram$Water.Code,
                                 levels = c("LAND", "SHALLOW WATER", "DEEP WATER",
                                            "OPEN OCEAN"))

  # MAKE 30s HYPNOGRAM -----
  hypnogram$timebins <- cut(hypnogram$R.Time, breaks='30 sec')
  hypnogram_30s <- hypnogram %>%
    group_by(timebins, SealID, Recording.ID, ID) %>%
    dplyr::summarise(Sleep.Code = calculate_mode(Sleep.Code),
                     Simple.Sleep.Code = calculate_mode(Simple.Sleep.Code),
                     Sleep.Num = calculate_mode(Sleep.Num),
                     Simple.Sleep.Num = calculate_mode(Sleep.Num),
                     Resp.Code = calculate_mode(Resp.Code),
                     Resp.Num = calculate_mode(Resp.Num),
                     Water.Code = calculate_mode(Water.Code),
                     Water.Num = calculate_mode(Water.Num),
                     R.Time = as.POSIXct(calculate_mode(timebins)))
  hypnogram_30s$Time_s_per_day = period_to_seconds(hms(format(hypnogram_30s$R.Time, format='%H:%M:%S')))
  hypnogram_30s$Time = ymd_hms(paste("2000-01-01",strftime(hypnogram_30s$R.Time,format="%H:%M:%S")))
  hypnogram_30s$Date = floor_date(hypnogram_30s$R.Time, unit = "day")
  hypnogram_30s$Day = floor_date(hypnogram_30s$R.Time, unit = "day")-floor_date(hypnogram_30s$R.Time[1], unit = "day")

  saveRDS(hypnogram,
          file = here(paste("analysis/data/derived_data/07_Hypnogram_1Hz.rds",sep="")),
          ascii = FALSE, version = NULL,compress = TRUE)

  saveRDS(hypnogram_30s,
          file = here(paste("analysis/data/derived_data/07_Hypnogram_30s.rds",sep="")),
          ascii = FALSE, version = NULL,compress = TRUE)

  # CALCULATE SUMMARY SLEEP STATISTICS ----
  stats <- hypnogram %>%
    group_by(Date) %>%
    mutate(obs_per_day=n()) %>%
    group_by(Hour,Date) %>%
    mutate(obs_per_hour=n()) %>%
    group_by(Hour,Date,Simple.Sleep.Code) %>%
    mutate(sleep_per_hour=n())

  stats <- stats %>%
    group_by(Date,Simple.Sleep.Code) %>%
    mutate(sleep_per_day=n())

  summarised_stats <- stats %>%
    group_by(Hour,Date,Simple.Sleep.Code) %>%
    summarise(Percentage=mean(sleep_per_hour/obs_per_hour))

  stage_per_day <- hypnogram %>%
    group_by(Date,Simple.Sleep.Code,.drop = FALSE) %>%
    summarise(count=n())
  stage_per_hour <- hypnogram %>%
    group_by(Hour,Date,Simple.Sleep.Code,.drop = FALSE) %>%
    summarise(count=n())
  stage_overall <- hypnogram %>%
    group_by(Simple.Sleep.Code,.drop = FALSE) %>%
    summarise(count=n())
  stage_per_loc <- hypnogram %>%
    group_by(Water.Code,Simple.Sleep.Code,.drop=FALSE) %>%
    summarise(count=n())
  obs_per_day <- hypnogram %>%
    group_by(Date) %>%
    summarise(total=n())
  obs_per_hour <- hypnogram %>%
    group_by(Hour,Date) %>%
    summarise(total=n())
  obs_per_loc <- hypnogram %>%
    group_by(Water.Code) %>%
    summarise(total=n())

  per_hour_stats <- full_join(stage_per_hour,obs_per_hour) %>%
    mutate(Percentage = count/total,
           SealID = SealID,
           Recording.ID = info$Recording.ID,
           ID = sub("_[^_]+$", "", info$Recording.ID))
  per_day_stats <- full_join(stage_per_day,obs_per_day) %>%
    mutate(Percentage = count/total,
           SealID = SealID,
           Recording.ID = info$Recording.ID,
           ID = sub("_[^_]+$", "", info$Recording.ID))
  per_loc_stats <- full_join(stage_per_loc,obs_per_loc) %>%
    mutate(Percentage = count/total,
           SealID = SealID,
           Recording.ID = info$Recording.ID,
           ID = sub("_[^_]+$", "", info$Recording.ID))
  stage_overall <- stage_overall %>%
    mutate(Percentage = count/nrow(hypnogram),
           SealID = SealID,
           Recording.ID = info$Recording.ID,
           ID = sub("_[^_]+$", "", info$Recording.ID))

  full_days_only <- hypnogram %>%
    group_by(Date) %>%
    mutate(nObservation = n(),
           SealID = SealID,
           Recording.ID = info$Recording.ID,
           ID = sub("_[^_]+$", "", info$Recording.ID)) %>%
    filter(nObservation == 86400)

  if (i==1){
    # ONLY RUN FOR FIRST ANIMAL
    summary_per_hour_stats <- per_hour_stats
    summary_per_day_stats <- per_day_stats
    summary_per_loc_stats <- per_loc_stats
    summary_stage_overall <- stage_overall
    summary_full_days_only <- full_days_only
    summary_hypnogram_30s <- hypnogram_30s
  }
  if (i>1){
    #RUN FOR SUBSEQUENT ANIMALS
    summary_per_hour_stats <- rbind(summary_per_hour_stats, per_hour_stats)
    summary_per_day_stats <- rbind(summary_per_day_stats, per_day_stats)
    summary_per_loc_stats <- rbind(summary_per_loc_stats, per_loc_stats)
    summary_stage_overall <- rbind(summary_stage_overall, stage_overall)
    summary_full_days_only <- rbind(summary_full_days_only, full_days_only)
    summary_hypnogram_30s <- rbind(summary_hypnogram_30s,hypnogram_30s)
}

write.csv(hypnogram,here("Data",paste(SealID,"_06_Hypnogram_",scorer,"_",hypno_freq,".csv",sep="")), row.names = FALSE)
write.csv(hypnogram_30s,here("Data",paste(SealID,"_06_Hypnogram_",scorer,"_30s.csv",sep="")), row.names = FALSE)
print("PROCESSED SUCCESSFULLY")

# SINGLE ANIMAL PLOTS ------------------------------------------------------------------------

# Hypnogram plot
hypno_plot <- ggplot()+
  geom_rect(data=hypnogram_30s, aes(xmin=Time,xmax=Time+30,ymin=-7,ymax=-5.5, fill=Water.Code))+
  scale_fill_manual(values=location.col)+
  new_scale_fill()+
  geom_line(data=hypnogram_30s,aes(x=Time, y=Sleep.Num), color='grey')+
  geom_point(data=hypnogram_30s, aes(x= Time, y=Sleep.Num, color=Sleep.Code))+
  geom_rect(data=hypnogram_30s, aes(xmin=Time,xmax=Time+30,ymin=-3,ymax=1, fill=Sleep.Code),
            alpha=1, size=0)+
  theme_classic()+
  annotate('text', x = floor_date(hypnogram_30s$Time, unit = "day")-1200, y = 4, label = "Sleep", hjust=1)+
  annotate('text', x = floor_date(hypnogram_30s$Time, unit = "day")-1200, y = 1.5, label = "Wake", hjust=1)+
  annotate('text', x = floor_date(hypnogram_30s$Time, unit = "day")-1200, y = -2, label = "Sleep State", hjust=1)+
  annotate('text', x = floor_date(hypnogram_30s$Time, unit = "day")-1200, y = -4, label = "Respiration", hjust=1)+
  annotate('text', x = floor_date(hypnogram_30s$Time, unit = "day")-1200, y = -6, label = "Location", hjust=1)+
  easy_remove_y_axis()+
  scale_fill_manual(values=sleep.col)+
  scale_color_manual(values=sleep.col)+
  new_scale_fill()+
  #scale_color_manual(values=sleep.col)+
  geom_rect(data=hypnogram_30s, aes(xmin=Time,xmax=Time+30,ymin=-5,ymax=-3, fill=Resp.Code))+
  scale_fill_manual(values=resp.col)+
  geom_hline(yintercept=2.5,color='grey',linetype='dotdash')+
  scale_x_datetime(labels=date_format('%H:%M',tz='PST8PDT'),
                   breaks=date_breaks("2 hours"),
                   limits=c(min(hypnogram_30s$Time)-3600*4,max(hypnogram_30s$Time)+20))+
  labs(y = "", x="Time of Day", fill='Respiratory Patterns',
       title=paste(SealID,"Hypnogram -",info$Recording.ID,"Age",info$Age))+
  facet_grid(rows=vars(Date),scales='free_x')+
  theme(legend.position='right')
ggsave(here("Figures",paste(SealIDs[i],"_06_Scored_Hypnogram.pdf",sep="")),hypno_plot, width= 10,height = 6,units="in",dpi=300)
ggsave(here("Figures",paste(SealIDs[i],"_06_Scored_Hypnogram.png",sep="")),hypno_plot, width= 10,height = 6,units="in",dpi=300)

# STACKED AREA CHART
stacked_sleep_stats_area <- ggplot()+
  geom_point(data=hypnogram_30s, aes(x=Time_s_per_day/3600-0.5,
                                     y=-0.2,color=Water.Code),size=3)+
  scale_color_manual(values=location.col)+
  labs(fill='Location')+
  geom_area(data=per_hour_stats,aes(x=Hour, y=Percentage, fill=Simple.Sleep.Code))+
  scale_fill_manual(values=simple.sleep.col)+
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Hour of Day", y = "Percentage of Hour", paste(SealID,"Percent of Sleep per hour"))+
  facet_grid(rows=vars(as.Date(Date)))
stacked_sleep_stats_area

sleeptime_per_day <- ggplot()+
  geom_col(data=per_day_stats, aes(x=Simple.Sleep.Code,y=Percentage, fill=Simple.Sleep.Code))+
  scale_fill_manual(values=simple.sleep.col)+
  geom_text(data=per_day_stats, aes(x=Simple.Sleep.Code,y=Percentage+0.05, label = paste(round(Percentage*24,digits=1),"h")), size=3, hjust=0.5)+
  facet_wrap(~Date)+
  theme_classic()
sleeptime_per_day

sleeptime_per_loc <- ggplot()+
  geom_col(data=per_loc_stats, aes(x=Simple.Sleep.Code,y=Percentage, fill=Simple.Sleep.Code))+
  scale_fill_manual(values=simple.sleep.col)+
  geom_text(data=per_loc_stats, aes(x=Simple.Sleep.Code,y=Percentage+0.05, label = paste(round(Percentage*24,digits=1),"h")), size=3, hjust=0.5)+
  facet_grid(cols=vars(Water.Code))+
  theme_classic()
sleeptime_per_loc

per_day_stats$Simple.Sleep.Code <- factor(per_day_stats$Simple.Sleep.Code,
                                          levels = c("Unscorable","Active Waking", "Quiet Waking",
                                                     "Drowsiness","SWS","REM"))
per_loc_stats$Simple.Sleep.Code <- factor(per_loc_stats$Simple.Sleep.Code,
                                          levels = c("Active Waking", "Quiet Waking",
                                                     "Drowsiness","SWS","REM","Unscorable"))
per_loc_stats$Water.Code <- factor(per_loc_stats$Water.Code,
                                   levels = c("LAND", "SHALLOW WATER", "DEEP WATER",
                                              "OPEN OCEAN"))

# PROPORTION OF SLEEP STAGE across days
sleeptime_day <- ggplot()+
  geom_bar(data=per_day_stats, aes(x=as.Date(Date), y=Percentage,
                                   fill=Simple.Sleep.Code), position="stack", stat="identity", width=0.6)+
  scale_fill_manual(values=simple.sleep.col)+
  new_scale_fill()+
  geom_text(data=per_day_stats,aes(x=as.Date(Date)-0.35, y=Percentage, label=paste(round(100*Percentage, 1), "%", sep="")),
            size = 3, position = position_stack(vjust = 0.5),hjust=1)+
  geom_rect(data=hypnogram_30s, aes(xmin=as.Date(Date)-0.5,xmax=as.Date(Date)+0.5,
                                    ymin=-0.1,ymax=-0.05,fill=Water.Code))+
  scale_fill_manual(values=location.col)+
  scale_y_continuous(labels=scales::percent)+
  labs(x="Day", y="Percent Time", title="Sleep Summary across Days")+
  #geom_text(data=stage_overall, aes(y=Percentage+0.05, label = paste(round(Percentage*24,digits=1),"h")), size=3, hjust=0.5)+
  theme_classic()
sleeptime_day

sleeptime_loc <- ggplot()+
  geom_bar(data=per_loc_stats, aes(x=Water.Code, y=Percentage,
                                   fill=Simple.Sleep.Code), position="stack", stat="identity", width=0.6)+
  scale_fill_manual(values=simple.sleep.col)+
  new_scale_fill()+
  geom_text(data=per_loc_stats,aes(x=Water.Code, y=Percentage, label=paste(round(100*Percentage, 1), "%", sep="")),
            size = 3, position = position_stack(vjust = 0.5),hjust=0.5)+
  geom_rect(data=hypnogram_30s, aes(xmin=Water.Code,xmax=Water.Code,
                                    ymin=-0.1,ymax=-0.05,fill=Water.Code))+
  scale_fill_manual(values=location.col)+
  scale_y_continuous(labels=scales::percent)+
  labs(x="Location", y="Percent Time", title="Sleep Summary across Locations")+
  #geom_text(data=stage_overall, aes(y=Percentage+0.05, label = paste(round(Percentage*24,digits=1),"h")), size=3, hjust=0.5)+
  theme_classic()
sleeptime_loc

overall <- hypnogram %>%
  group_by(Date, Simple.Sleep.Code)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%

  ggplot(aes(x=1,y = count/sum(count), fill = Simple.Sleep.Code))+
  geom_bar(position="stack",stat="identity", width=0.3, show.legend = FALSE)+
  scale_fill_manual(values=simple.sleep.col)+
  geom_text(data=stage_overall,aes(x=1.35,y=Percentage,label = paste(round(Percentage*24,digits=1),"h",sep="")),
            position = position_stack(vjust = 0.5),
            size=3.5, hjust=1)+
  labs(x="", y="% of Time", title="Total Sleep Time")+
  theme(legend.position = 'none')+
  easy_remove_x_axis()+
  scale_y_continuous(labels=scales::percent)+
  theme_classic()
overall

overall_ordered <- hypnogram %>%
  group_by(Date, Simple.Sleep.Code)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%

  ggplot(aes(x = reorder(Simple.Sleep.Code, desc(count)), y = count/sum(count), fill = Simple.Sleep.Code))+
  scale_fill_manual(values=simple.sleep.col)+
  geom_text(data=stage_overall, aes(x=reorder(Simple.Sleep.Code, desc(count)),y=Percentage+0.02, label = paste(round(Percentage*24,digits=1),"h")), size=4, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Sleep State", y="% of Time")+
  scale_y_continuous(labels=scales::percent)+
  #facet_wrap(~Date)+
  theme_classic()+
  geom_col(show.legend = FALSE)
overall_ordered

# SLEEP BREAKDOWN SUMMARY
top_summary <- plot_grid(nrow=1,overall_ordered,overall,sleeptime_day,labels="AUTO",rel_widths=c(1.5,1,3))
summary <- plot_grid(nrow=2,top_summary,hypno_plot,rel_heights = c(1,2))

ggsave(here("Figures",paste(SealIDs[i],"_06_sleep_time_summary_plot.pdf",sep="")),summary, width= 12,height = 10,units="in",dpi=300)
ggsave(here("Figures",paste(SealIDs[i],"_06_sleep_time_summary_plot.png",sep="")),summary, width= 12,height = 10,units="in",dpi=300)

#CIRCULAR PLOT
circle_activity_plot <- ggplot()+
  coord_polar(theta="x", start=-0.13)+
  geom_histogram(data=full_days_only, stat="bin", aes(x=Hour, fill=Simple.Sleep.Code), binwidth=1,show.legend = FALSE)+
  scale_fill_manual(values=simple.sleep.col)+
  scale_x_continuous(breaks=0:24, expand = c(0,0))+
  labs(x = "Hour", y = "Seconds of Sleep", paste(SealID,"Sleep per hour"))+
  theme_classic()
circle_activity_plot

# FLAT PLOT
sleep_full_days_only <- full_days_only %>%
  filter(Simple.Sleep.Code=="SWS" | Simple.Sleep.Code=="REM" | Simple.Sleep.Code=="LS")
flat_sleep_density_plot <- ggplot()+
  geom_density(data=sleep_full_days_only,aes(x=Hour, y=..density.., fill=Simple.Sleep.Code, color=Simple.Sleep.Code),alpha=0.8, show.legend = FALSE)+
  geom_histogram(data=sleep_full_days_only,aes(x=Hour, y=..density.., fill=Simple.Sleep.Code), position="identity", alpha=0.2, binwidth=1, show.legend = FALSE)+
  scale_color_manual(values=simple.sleep.col)+
  scale_fill_manual(values=simple.sleep.col)+
  facet_wrap(~Simple.Sleep.Code)+
  #geom_hline(yintercept = seq(0,50, by=10), color='grey', size = 0.3)+
  labs(x = "Hour", y = "Density of Observations", title = paste(SealID,"Sleep per hour"))+
  theme_classic()
flat_sleep_density_plot

# CIRCULAR PLOT
circle_sleep_density_plot <- ggplot()+
  coord_polar(theta="x", start=-0.13)+
  geom_density(data=sleep_full_days_only,aes(x=Hour, y=..density.., fill=Simple.Sleep.Code, color=Simple.Sleep.Code),alpha=0.8, show.legend = FALSE)+
  geom_histogram(data=sleep_full_days_only,aes(x=Hour, y=..density.., fill=Simple.Sleep.Code), position="identity", alpha=0.2, binwidth=1, show.legend = FALSE)+
  scale_color_manual(values=simple.sleep.col)+
  scale_fill_manual(values=simple.sleep.col)+
  facet_wrap(~Simple.Sleep.Code)+
  #geom_hline(yintercept = seq(0,50, by=10), color='grey', size = 0.3)+
  scale_x_continuous(breaks=0:24, expand = c(0,0))+
  labs(x = "Hour", y = "Density of Observations", title = paste(SealID,"Sleep per hour"))+
  theme_classic()
circle_sleep_density_plot

sleep_summary_plot<- plot_grid(circle_activity_plot,
                               stacked_sleep_stats_area,
                               flat_sleep_density_plot,
                               circle_sleep_density_plot, labels='AUTO', rel_widths = c(1,2))

ggsave(here("Figures",paste(SealIDs[i],"_06_sleep_summary_plot.pdf",sep="")),sleep_summary_plot, width= 10,height = 7,units="in",dpi=300)
ggsave(here("Figures",paste(SealIDs[i],"_06_sleep_summary_plot.png",sep="")),sleep_summary_plot, width= 10,height = 7,units="in",dpi=300)

print("PLOTS CREATED AND SAVED SUCCESSFULLY")
}

summary_per_hour_stats[is.na(summary_per_hour_stats)] <- 0
summary_per_day_stats[is.na(summary_per_day_stats)] <- 0
summary_per_loc_stats[is.na(summary_per_loc_stats)] <- 0
summary_stage_overall[is.na(summary_stage_overall)] <- 0
summary_hypnogram_30s[is.na(summary_hypnogram_30s)] <- 0

write.csv(summary_hypnogram_30s,here("Data",paste("06_summary_hypnogram_30s_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
write.csv(summary_per_hour_stats,here("Data",paste("06_summary_per_hour_stats_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
write.csv(summary_per_day_stats,here("Data",paste("06_summary_per_day_stats_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
write.csv(summary_per_loc_stats,here("Data",paste("06_summary_per_loc_stats_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
write.csv(summary_stage_overall,here("Data",paste("06_summary_stage_overall_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
write.csv(summary_full_days_only,here("Data",paste("06_summary_full_days_only_ALL_ANIMALS.csv",sep="")), row.names = FALSE)
