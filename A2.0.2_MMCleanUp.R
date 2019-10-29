#script to: 
##1) summarize high temporal resolution mobile monitoring data into stop MEDIANS (reduce file size); data are not trimmed as of  9/19/19

##2) clean up overnight data from AQS sites, calculate median hourly concentrations

############### to do 

# clean up geocovariates:
## take out AP predictions (drop all "em_..." & "no2_..."? e.g., em_NOx_s03000)
## take out variables: w/ little variation, those w/ lot variation (see: https://github.com/kaufman-lab/STmodel/blob/master/functions_covariate_preprocess_regional.R)
 
#geocovariates 
mm.geo <- read.csv("~/Everything/School/PhD_UW/Dissertation/TRAP R Project/Data/Aim 2/Geocovariates/dr0311_mobile_locations.txt")

 








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

mm_full <- readRDS(file.path("Data", "Aim 2", "Mobile Monitoring", "all_data_191021.rda")) %>%
  #drop unwanted variable values to reduce data size
  filter(!variable %in% c("gps_lat",
                          "gps_long",
                          #"ufp_pt_screen_ct_cm3",
                          "ufp_disc_med_size_nm",
                          "bc_uv375nm_ng_m3", #this is not total BC
                          "ufp_scan_20_5_ct_cm3" #don't need this bin
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

# quant_limit_upper <- as.numeric(quantile(mm_full$value, myquantile_upper, na.rm = T))
# quant_limit_lower <- as.numeric(quantile(mm_full$value, myquantile_lower, na.rm = T))

# mm_full <- mm_full %>%
#   group_by(variable, instrument_id) %>%
#   #remove top 1% of values for each pollutant and instrument before taking avg
#   filter(value > as.numeric(quantile(value, myquantile_lower, na.rm = T)) &
#            value < as.numeric(quantile(value, myquantile_upper, na.rm = T))) %>%
#      # mutate(value = ifelse(value < quantile(value, myquantile_upper, na.rm = T), value, NA)) %>%
#   # #drop rows w/ NA "values"
# #   drop_na(value) %>%
#   ungroup()

# mm_full2 %>%
#   ggplot(aes(x=value)) + 
#   geom_histogram() + 
#   facet_wrap(~variable, scales="free")

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
   date = as.Date(substr(arrival_time, 1,10))
   ) %>%
  add.temporal.variables(data = .,
                         date.var = "arrival_time")

 
#create NanoScan counts (20-420 nm) (ptraks: 20-1000 nm)
temp <- mm %>% 
  #look at variables from NanoScan
  filter(grepl("scan", variable)) %>%
  #make wide format to calculate the difference
  spread(variable, value) %>%
  mutate(
    ufp_scan_20_421_nm_ct_cm3 = ufp_scan_ct_cm3 - ufp_scan_11_5_ct_cm3 - ufp_scan_15_4_ct_cm3
  ) %>% 
  rename(
    ufp_scan_10_421_nm_ct_cm3 = ufp_scan_ct_cm3
  ) %>%
  select( - ufp_scan_11_5_ct_cm3, 
          -ufp_scan_15_4_ct_cm3) %>%
  #make back to long forma
  gather("variable", "value", ufp_scan_10_421_nm_ct_cm3:ufp_scan_20_421_nm_ct_cm3)

#replace old nanoscan bin values with new ones 
mm <- mm %>%
  filter(!grepl("scan", variable)) %>%
  rbind(temp) %>%
  arrange(arrival_time)

###################### merge mm w/ geocovariates ######################




  

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

# saveRDS(mm, file.path("Data", "Aim 2", "Mobile Monitoring", "mm_191021.rda"))
# saveRDS(mm.wide, file.path("Data", "Aim 2", "Mobile Monitoring", "mm.wide_191021.rda"))

##########################################################################################
########################### 2. overnight collocations ####################################
##########################################################################################

# folders with files to be uploaded. folder should only include ptraks w/o screens. new files added to folder will automatically be uploaded
path_10w <- file.path("Data", "Aim 2", "Overnight Collocations", "10W_raw", "ptrak_noscreen")
path_bh <- file.path("Data", "Aim 2", "Overnight Collocations", "BH_raw", "ptrak_noscreen")

#all files for each AQS site in the ptrak folder are uploaded and combined   
ptrak_10w <- ptrak.bind.fn(folder_path = path_10w)
ptrak_bh <- ptrak.bind.fn(folder_path = path_bh)
ptrak <- rbind(ptrak_10w, ptrak_bh)

ptrak <- ptrak %>%
  mutate(
    #get rid of minutes & seconds
    datetime = format(ptrak$datetime, "%Y-%m-%d %H:%M")
  ) %>%
  group_by(datetime, location) %>%
  summarize(
    #estimate hourly medians
    Conc_pt_cm3 = round(median(Conc_pt_cm3))
  ) %>%
  ungroup() %>%
  mutate(
    #make datetime into POSIXct() format
    datetime = ymd_hm(datetime)
  ) %>%
  #add temporal variables
  add.temporal.variables(data = ., date.var = "datetime") #%>%
  # # take out extreme low and high values
  # filter(Conc_pt_cm3 > as.numeric(quantile(Conc_pt_cm3, myquantile_lower, na.rm = T)) &
  #        Conc_pt_cm3 < as.numeric(quantile(Conc_pt_cm3, myquantile_upper, na.rm = T))) %>%
  
  
# #before/after daylight savings? 
# ptrak <- ptrak %>%
#   mutate(DaylightSavings = date > "2019-03-10" & date < "2019-11-03")

############################ save data for quicker access ################################
#saveRDS(ptrak, file.path("Data", "Aim 2", "Overnight Collocations", "ptrak.rda"))

 
