#script to: 
##1) summarize high temporal resolution mobile monitoring data into stop MEDIANS (reduce file size); data are not trimmed as of  9/19/19

##2) clean up overnight data from AQS sites, calculate median hourly concentrations

##########################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load packages 
pacman::p_load(dplyr, tidyverse, chron, lubridate, knitr)   

# source global variables & functions
source("A2.0.1_Var&Fns.R")

##########################################################################################
############################# 1. mobile monitoring #######################################
##########################################################################################

############################ read in data ################################################

#mobile monitoring 
mm_full <- readRDS("Data/MobileMonitoring/all_data_190917.rda") %>%
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

quant_limit_upper <- as.numeric(quantile(mm_full$value, myquantile_upper, na.rm = T))
quant_limit_lower <- as.numeric(quantile(mm_full$value, myquantile_lower, na.rm = T))

mm_full <- mm_full %>%
  group_by(variable, instrument_id) %>%
  #remove top 1% of values for each pollutant and instrument before taking avg
  filter(value > as.numeric(quantile(value, myquantile_lower, na.rm = T)) & 
           value < as.numeric(quantile(value, myquantile_upper, na.rm = T))) %>%
     # mutate(value = ifelse(value < quantile(value, myquantile_upper, na.rm = T), value, NA)) %>%
  # #drop rows w/ NA "values"
#   drop_na(value) %>%
  ungroup()

mm_full2 %>%
  ggplot(aes(x=value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales="free")

#take avg of each stop to REDUCE FILE SIZE
mm <- mm_full %>%
  group_by(runname, site_id) %>%  #, instrument_id, variable
  #set time to arrival time, in case a stop elapsed e.g., 2 diff hours
  mutate(arrival_time = min(time)) %>%
  select(-time) %>%
  
  #take avg/median of each unique site stop for each instrument
  group_by(runname, route, site_id, aqs_id, aqs_location, site_long, site_lat, duration_sec, arrival_time, instrument_id, variable, site_no) %>%
  summarize(value = median(value, na.rm = T)) %>%
  ungroup()


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
mm <- mm %>% 
  mutate(
   date = as.Date(substr(arrival_time, 1,10))) %>%
  add.temporal.variables(data = .,
                         date.var = "arrival_time")
  

############################ make wide format ############################################

#note: dataframe does NOT include instrument_ID - takes avg reading if 2 instruments were collocated
mm.wide <- mm %>% 
  #?? don't care whate instrument took measurement, as long as we have a measurement?
  select(-instrument_id) %>%
  # ??? HOW do you account for completely missed stops for which we have no data?
  #group_by arrival_time first to order by time
  group_by(arrival_time, runname, site_id, variable, 
           #variables not immediately necessary
           route, aqs_id, aqs_location, site_long, site_lat, duration_sec, site_no, run_no, site_id_visit_no, aqs_site, date, hour, time_of_day, day, time_of_week, month, season) %>% 
  #spread() fn has issues when have dupliate instruments taking readings at same time - so take avg of both the readings
  summarize(value = mean(value)) %>%
  ungroup() %>%
  spread(variable, value) 

############################ save data for quicker access ################################

# saveRDS(mm, file.path("Data", "MobileMonitoring", "mm_190917.rda"))
# saveRDS(mm.wide, file.path("Data", "MobileMonitoring", "mm.wide_190917.rda"))

##########################################################################################
########################### 2. overnight collocations ####################################
##########################################################################################

#################################### read in data ####################################  

# folders with files to be uploaded. folder should only include ptraks w/o screens 
path_10w <- file.path("Data", "Aim 2", "Overnight Collocations", "10W_raw", "ptrak_noscreen")
path_bh <- file.path("Data", "Aim 2", "Overnight Collocations", "BH_raw", "ptrak_noscreen")

  
ptrak_10w <- ptrak.bind.fn(folder_path = path_10w)

ptrak_bh <- ptrak.bind.fn(folder_path = path_bh)

 


 
#################################### clean up ####################################  

ptrak <- rbind(Ptrak.10W, Ptrak.BH) %>% 
  mutate(
    date_time_local = as.POSIXct(paste(Date, Time), format="%m/%d/%y %H:%M:%S", tz = "America/Los_Angeles"),
    #convert to PST (this creates a character vector), convert back to date-time format
    #date_time_pst =as.POSIXct(format(date_time_local, tz="Etc/GMT+8")), 
    date = as.Date(date_time_local),
    hour = as.numeric(format(date_time_local, format="%H")),
    #hour_local = as.numeric(format(date_time_local, format="%H")),
    time_of_day = factor(ifelse(hour %in% early_am, "early_am",
                                ifelse(hour %in% am, "am",
                                       ifelse(hour %in% noon, "noon",
                                              ifelse(hour %in% evening, "evening", "night")))),
                         levels= c("early_am", "am", "noon", "evening", "night")),
    day = factor(format(date_time_local, "%a"), 
                 levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    time_of_week = factor(ifelse(day =="Sat" | day == "Sun", "weekend", "weekday")),
    #month = as.numeric(format(date_time_local, format="%m")),
    month = factor(format(date_time_local, "%b"), 
                   levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    season = factor(ifelse((date >= winter1 & date < spring) | date >= winter2, "winter",
                           ifelse(date >= spring & date < summer, "spring",
                                  ifelse(date >= summer & date < fall, "summer", "fall"))),
                    levels = c("spring", "summer", "fall", "winter"))
  ) %>%
  # take out extreme low and high values
  filter(Conc_pt_cm3 > as.numeric(quantile(Conc_pt_cm3, myquantile_lower, na.rm = T)) &
           Conc_pt_cm3 < as.numeric(quantile(Conc_pt_cm3, myquantile_upper, na.rm = T))) %>%
  select(date_time_local, date, hour, time_of_day, day, time_of_week, month, season, Conc_pt_cm3, Location) #hour_local


# take median (since not excluding extreme values) concentration of 10-sec day-hour values
ptrak <- ptrak %>%
  group_by(date, hour, time_of_day, day, time_of_week, month, season, Location) %>% #hour_local
  summarize(Conc_pt_cm3 = round(median(Conc_pt_cm3)))

# #before/after daylight savings? 
# ptrak <- ptrak %>%
#   mutate(DaylightSavings = date > "2019-03-10" & date < "2019-11-03")

############################ save data for quicker access ################################
#saveRDS(ptrak, file.path("Data", "MobileMonitoring", "ptrak_190628.rda"))

 
