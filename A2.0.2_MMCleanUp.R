#script to: 
##1) summarize high temporal resolution mobile monitoring data into stop MEDIANS (reduce file size)
##2) clean up geocovariates. drop: a) AP predictions, which are based on other covariates & other models, b) variables w/ too little or too much variation 
##3) clean up overnight data from AQS sites, calculate median hourly concentrations

############### to do 


 


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
mm_full <- readRDS(file.path("Data", "Aim 2", "Mobile Monitoring", "all_data_191112.rda")) %>%
  #drop unwanted variable values to reduce data size
  filter(
    # !variable %in% c("gps_lat",
    #                       "gps_long",
    #                       #"ufp_pt_screen_ct_cm3",
    #                       "ufp_disc_med_size_nm",
    #                       "bc_uv375nm_ng_m3", #this is not total BC
    #                       "ufp_scan_20_5_ct_cm3" #don't need this bin
    #                       )
    variable %in% c("bc_ir880nm_ng_m3", "ufp_pt_noscreen_ct_cm3", 
                    "ufp_scan_11_5_ct_cm3", "ufp_scan_15_4_ct_cm3", "ufp_scan_ct_cm3")
    )  %>%
  mutate(
    variable = recode_factor(factor(variable), 
                              "bc_ir880nm_ng_m3" = "bc_880_ng_m3",
                              "ufp_pt_noscreen_ct_cm3" = "ptrak_ct_cm3"
                             )
    )

#read in current locations
site_ids <- read.csv(file.path("Data", "Aim 2", "Mobile Monitoring", "locations_190715.csv")) %>%
  select(site_id) %>%
  #drop Roosevelt garage stop
  filter(site_id != "MS0000") %>%
  unique()

#### --> update file later: there are 310 site_ids and in mm_full. There should be 309.           
# We stopped sampling MS0398  and switched to MS0601 which is fairly close




############################ clean up ###################################################
#drop old stops 
mm_full <- left_join(site_ids, mm_full)

#give each site unique number ID, based on time when stop was first sampled
site_no <- mm_full %>%
  arrange(route, time) %>%
  select(site_id) %>% unique() %>%
  mutate(site_no = seq(1:length(site_id)))

mm_full <- mm_full %>% left_join(site_no)

#relabel instruments that were incorrectly labeled
mm_full <- mm_full %>%
  mutate(instrument_id = ifelse(instrument_id == "BC_0063", "BC_63",
                                #ifelse(instrument_id == "CO_2", "CO_3",
                                       ifelse(instrument_id == "PMSCAN_1", "PMSCAN_3",
                                              instrument_id)) #)
  )

#take avg of each stop to REDUCE FILE SIZE
mm <- mm_full %>%
  group_by(runname, site_id) %>%  #, instrument_id, variable
  #set time to arrival time, in case a stop elapsed e.g., 2 diff hours
  mutate(arrival_time = min(time)) %>%
  select(-time) %>%
  
#take median of each unique site stop for each instrument
  ## don't include duration_sec b/c for some reason, some stops have 2 (or more?) entries @ same time, but diff durations (? GPS error??) and this creates multiple rows per sample
  group_by(runname, route, site_id, aqs_id, aqs_location, site_long, site_lat, arrival_time, instrument_id, variable, site_no #duration_sec
           ) %>%
  summarize(median_value = median(value, na.rm = T),
            mean_value = mean(value, na.rm = T)
            ) %>%
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
  mutate(site_id_visit_no = seq(1:n()))

mm <- mm %>% left_join(site_id_visit_no)

#ID if stops are AQS sites
mm <- mm %>%
  mutate(aqs_site = ifelse(is.na(aqs_id), 0, 1))

#add temporal variables
mm <- mm %>% 
  mutate(
   date = as.Date(substr(arrival_time, 1,10))) %>%
  add.temporal.variables(data = ., date.var = "arrival_time")

 
#create NanoScan counts (20-420 nm) (~similar to ptraks: 20-1000 nm)
ns_mean_values <- mm %>% 
  #look at variables from NanoScan
  filter(grepl("scan", variable)) %>%
  # drop median_value for now since spread fn is more complex if both mean_value and median_value
  select(-median_value) %>%
  #make wide format to calculate the difference
  spread(variable, mean_value) %>%
  mutate(ufp_scan_20_421_nm_ct_cm3 = ufp_scan_ct_cm3 - ufp_scan_11_5_ct_cm3 - ufp_scan_15_4_ct_cm3) %>% 
  rename(ufp_scan_10_421_nm_ct_cm3 = ufp_scan_ct_cm3) %>%
  select( - ufp_scan_11_5_ct_cm3, 
          -ufp_scan_15_4_ct_cm3) %>%
  #make back to long forma
  gather("variable", "mean_value", ufp_scan_10_421_nm_ct_cm3:ufp_scan_20_421_nm_ct_cm3)

# same as above but with medians
ns_median_values <- mm %>% 
  filter(grepl("scan", variable)) %>%
  select(-mean_value) %>%
  spread(variable, median_value) %>%
  mutate(ufp_scan_20_421_nm_ct_cm3 = ufp_scan_ct_cm3 - ufp_scan_11_5_ct_cm3 - ufp_scan_15_4_ct_cm3) %>% 
  rename(ufp_scan_10_421_nm_ct_cm3 = ufp_scan_ct_cm3) %>%
  select( - ufp_scan_11_5_ct_cm3, 
          -ufp_scan_15_4_ct_cm3) %>%
  gather("variable", "median_value", ufp_scan_10_421_nm_ct_cm3:ufp_scan_20_421_nm_ct_cm3)

ns_values <- left_join(ns_mean_values, ns_median_values) 

#replace old nanoscan bin values with new ones 
mm <- mm %>%
  filter(!grepl("scan", variable)) %>%
  rbind(ns_values) %>%
  arrange(arrival_time)  

###################### 2. clean up geocovariates ######################
mm.geo0 <- read.csv(file.path("Data", "Aim 2", "Geocovariates", "dr0311_mobile_locations.txt")) 

mm.geo <- mm.geo0 %>% 
  #drop AP predictions, which are based on other covariates & other models
  select(-contains("em_"),
         -contains("no2_"),
         -location_id,
         site_id = native_id
         )


## -->  take out variables: w/ little variation, those w/ lot variation (see: https://github.com/kaufman-lab/STmodel/blob/master/functions_covariate_preprocess_regional.R)

## --> drop AP predictions from other models


#?? var(mm.geo)




############################ make wide format ############################################

#note: dataframe does NOT include instrument_ID - takes avg reading if 2 instruments were collocated
mm.wide_means <- mm %>% 
  #?? don't care whate instrument took measurement, as long as we have a measurement?
  select(-instrument_id,
         -median_value
         ) %>%
  # ??? HOW do you account for completely missed stops for which we have no data?
  #group_by arrival_time first to order by time
  group_by(arrival_time, runname, site_id, variable, 
           route, aqs_id, aqs_location, site_long, site_lat, site_no, run_no, site_id_visit_no, aqs_site, date, hour, time_of_day, day, time_of_week, month, season, #duration_sec
           ) %>% 
  #spread() fn has issues when have dupliate instruments taking readings at same time - so take avg of both the readings
  summarize(mean = mean(mean_value)) %>%
  ungroup() %>%
  spread(variable, mean) %>%
  rename(
    bc_880_ng_m3_mean = bc_880_ng_m3,
    ptrak_ct_cm3_mean = ptrak_ct_cm3,
    scan_10_421_nm_ct_cm3_mean = ufp_scan_10_421_nm_ct_cm3,
    scan_20_421_nm_ct_cm3_mean = ufp_scan_20_421_nm_ct_cm3
  )

mm.wide_medians <- mm %>% 
  #?? don't care whate instrument took measurement, as long as we have a measurement?
  select(-instrument_id,
         -mean_value) %>%
  group_by(arrival_time, runname, site_id, variable, 
           route, aqs_id, aqs_location, site_long, site_lat, site_no, run_no, site_id_visit_no, aqs_site, date, hour, time_of_day, day, time_of_week, month, season) %>% 
  #spread() fn has issues when have dupliate instruments taking readings at same time - so take avg of both the readings
  summarize(one_median = mean(median_value)) %>%
  ungroup() %>%
  spread(variable, one_median) %>%
  rename(
    bc_880_ng_m3_median = bc_880_ng_m3,
    ptrak_ct_cm3_median = ptrak_ct_cm3,
    scan_10_421_nm_ct_cm3_median = ufp_scan_10_421_nm_ct_cm3,
    scan_20_421_nm_ct_cm3_median = ufp_scan_20_421_nm_ct_cm3
  )

mm.wide <- left_join(mm.wide_means, mm.wide_medians)



###################### merge mm w/ geocovariates ######################

mm <- left_join(mm, mm.geo)
mm.wide <- left_join(mm.wide, mm.geo)

############################ save data for quicker access ################################

# saveRDS(mm, file.path("Data", "Aim 2", "Mobile Monitoring", "mm_191112.rda"))
# saveRDS(mm.wide, file.path("Data", "Aim 2", "Mobile Monitoring", "mm.wide_191112.rda"))

##########################################################################################
########################### 3. overnight collocations ####################################
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
  add.temporal.variables(data = ., date.var = "datetime")  
     
############################ save data for quicker access ################################
#saveRDS(ptrak, file.path("Data", "Aim 2", "Overnight Collocations", "ptrak.rda"))

 
