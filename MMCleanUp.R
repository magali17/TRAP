#script to: 1) summarize high temporal resolution mobile monitoring data into stop averages (reduce file size); 2) clean up overnight data from AQS sites

##########################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load packages 
pacman::p_load(dplyr, tidyverse, chron, knitr)  #chronn: is.holiday, is.weekend

# source global variables & functions
source("A2-3_Var&Fns.R")

##########################################################################################
############################# 1. mobile monitoring #######################################
##########################################################################################

############################ read in data ################################################

#mobile monitoring 
mm_full <- readRDS("Data/MobileMonitoring/all_data_190806.rda") %>%
  #drop unwanted variable values to reduce data size
  filter(!variable %in% c("gps_lat",
                          "gps_long",
                          #"ufp_pt_screen_ct_cm3",
                          "ufp_disc_med_size_nm",
                          "bc_uv375nm_ng_m3" #this is not total BC
                          )
         )

############################ clean up ###################################################

#give each site unique number ID, based on time when stop was first sampled
site_no <- mm_full %>%
  arrange(route, time) %>%
  select(site_id) %>% unique() %>%
  mutate(site_no = seq(1:length(site_id)))

mm_full <- mm_full %>% left_join(site_no)

#relabel instruments that were incorrectly labeled
mm_full <- mm_full %>%
  mutate(instrument_id = ifelse(instrument_id == "BC_0063", "BC_63",
                                ifelse(instrument_id == "CO_2", "CO_3",
                                       ifelse(instrument_id == "PMSCAN_1", "PMSCAN_3",
                                              instrument_id)))
  )

mm_full <- mm_full %>%
  group_by(variable, instrument_id) %>%
  #remove top 1% of values for each pollutant and instrument before taking avg
  mutate(value = ifelse(value < quantile(value, myquantile, na.rm = T), value, NA)) %>%
  #drop rows w/ NA "values"
  drop_na(value) %>%
  ungroup()

#take avg of each stop to REDUCE FILE SIZE
mm <- mm_full %>%
  group_by(runname, site_id) %>%  #, instrument_id, variable
  #set time to arrival time, in case a stop elapsed e.g., 2 diff hours
  mutate(arrival_time = min(time)) %>%
  select(-time) %>%
  
  #take avg of each unique site stop for each instrument
  group_by(runname, route, site_id, aqs_id, aqs_location, site_long, site_lat, duration_sec, arrival_time, instrument_id, variable, site_no) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup()

#   #? ALTERNATIVE: FOR NOW, CALCULATE weighted 3/4 YR AVG, weigh by season (see BeaconHill2001.Rmd)
# group_by(runname, route, site_id, aqs_id, aqs_location, site_long, site_lat, duration_sec, arrival_time, instrument_id, variable, site_no               ) %>%
#   summarize(value = mean(value, na.rm = T)) %>%
#   ungroup()





#give each driving day a unique ID
run_no <- mm %>%
  arrange(runname) %>%
  select(runname) %>%
  unique() %>%
  mutate(run_no = seq(1:length(runname)))

mm <- mm %>% left_join(run_no)

#number of times each site has been visited
site_id_visit_no <- mm %>%
  group_by(site_id) %>%
  select(site_id, runname) %>%
  unique() %>%
  #group_by(site_id) %>%
  mutate(
    site_id_visit_no = seq(1:n())
  )

mm <- mm %>% left_join(site_id_visit_no)

#ID if stops are AQS sites
mm <- mm %>%
  mutate(aqs_site = ifelse(is.na(aqs_id), 0, 1))

#add temporal variables
mm <- mm %>% mutate(
  #arrival_time = as.POSIXct(arrival_time),
  date = as.Date(substr(arrival_time, 1,10)),
  hour = as.numeric(format(arrival_time, "%H")),
  time_of_day = factor(ifelse(hour %in% early_am, "early_am",
                              ifelse(hour %in% am, "am",
                                     ifelse(hour %in% noon, "noon",
                                            ifelse(hour %in% evening, "evening", "night")))),
                       levels= c("early_am", "am", "noon", "evening", "night")
  ),
  day = factor(format(arrival_time, "%a"), levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
  weekend = factor(ifelse(day =="Sat" | day == "Sun", "weekend", "weekday")),
  month = factor(format(arrival_time, "%b"), levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  season = factor(ifelse((date >= winter1 & date < spring) | date >= winter2, "winter",
                         ifelse(date >= spring & date < summer, "spring",
                                ifelse(date >= summer & date < fall, "summer", "fall"))),
                  levels = c("winter", "spring", "summer", "fall"))
)

############################ make wide format ############################################

#note: dataframe does NOT include instrument_ID - takes avg reading if 2 instruments were collocated
mm.wide <- mm %>% 
  #?? don't care whate instrument took measurement, as long as we have a measurement?
  select(-instrument_id) %>%
  # ??? HOW do you account for completely missed stops for which we have no data?
  #group_by arrival_time first to order by time
  group_by(arrival_time, runname, site_id, variable, 
           #variables not immediately necessary
           route, aqs_id, aqs_location, site_long, site_lat, duration_sec, site_no, run_no, site_id_visit_no, aqs_site, date, hour, time_of_day, day, weekend, month, season) %>% 
  #spread() fn has issues when have dupliate instruments taking readings at same time - so take avg of both the readings
  summarize(value = mean(value)) %>%
  ungroup() %>%
  spread(variable, value) 

############################ save data for quicker access ################################

# saveRDS(mm, file.path("Data", "MobileMonitoring", "mm_190806.rda"))
# saveRDS(mm.wide, file.path("Data", "MobileMonitoring", "mm.wide_190806.rda"))  

##########################################################################################
########################### 2. overnight collocations ####################################
##########################################################################################

#################################### read in data ####################################  
#attributes()
#R recognizes that they are in both PST and PDT
Ptrak.10W <- read.csv("Data/MobileMonitoring/Overnight Collocations/Ptrak 10W.csv") %>% 
  mutate(Location = as.factor("10W"))  

Ptrak.BH <- read.csv("Data/MobileMonitoring/Overnight Collocations/Ptrak BH.csv") %>% 
  mutate(Location = as.factor("BH"))  

#################################### clean up ####################################  

ptrak <- rbind(Ptrak.10W, Ptrak.BH) %>% 
  mutate(
    date_time_local = as.POSIXct(paste(Date, Time), format="%m/%d/%y %H:%M:%S", tz = "America/Los_Angeles"),
    #convert to PST (this creates a character vector), convert back to date-time format
    date_time_pst =as.POSIXct(format(date_time_local, tz="Etc/GMT+8")), 
    date = as.Date(date_time_pst),
    hour = as.numeric(format(date_time_pst, format="%H")),
    time_of_day = factor(ifelse(hour %in% early_am, "early_am",
                                ifelse(hour %in% am, "am",
                                       ifelse(hour %in% noon, "noon",
                                              ifelse(hour %in% evening, "evening", "night")))),
                         levels= c("early_am", "am", "noon", "evening", "night")),
    day = factor(format(date_time_pst, "%a"), 
                 levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    weekend = factor(ifelse(day =="Sat" | day == "Sun", "weekend", "weekday")),
    #month = as.numeric(format(date_time_pst, format="%m")),
    month = factor(format(date_time_pst, "%b"), 
                   levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    season = factor(ifelse((date >= winter1 & date < spring) | date >= winter2, "winter",
                           ifelse(date >= spring & date < summer, "spring",
                                  ifelse(date >= summer & date < fall, "summer", "fall"))),
                    levels = c("winter", "spring", "summer", "fall"))
  ) %>%
  select(date_time_pst, date, hour, time_of_day, day, weekend, month, season, Conc_pt_cm3, Location)


# ??? take median (since not excluding extreme values) or use raw 10-sec values?
#calculate median/avg concentration per day-hour
ptrak <- ptrak %>%
  group_by(date, hour, time_of_day, day, weekend, month, season, Location) %>%
  summarize(Conc_pt_cm3 = round(median(Conc_pt_cm3)))

############################ save data for quicker access ################################
#saveRDS(ptrak, file.path("Data", "MobileMonitoring", "ptrak_190628.rda"))
