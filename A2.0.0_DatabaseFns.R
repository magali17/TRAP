######################################################################
#notes
# SCRIPT ONLY WORKS FROM H DRIVE IN OUR TRAP PROJECT FOLDER 
#useful files: TRAPdata.R, data_proc_lib.R, ajg_functions.R

## 1. setwd("../../../projects/trap/data_management/code")
## 2. Go To Path Folder viewer:  /projects/trap/data_management/code
## 3. open R project to set wd to projects/data_management/code 

######################################################################

library(RMariaDB) 

##reimport_and_report() fn
source(file.path("..", "code", "reimport_and_report.R"))
##get_config() fn that sets local time zone
source(file.path("..", "code", "data_proc_lib.R"))

reimport_and_report()

## Returns results of a generic mysql query
get_query <- function(ajg_query){
  channel <- connect_to_db()
  ajg_query <- paste0(ajg_query)
  result <- dbGetQuery(channel,ajg_query)
  suppressWarnings(res <- dbDisconnect(channel))
  result
}

##returns pollutant values, converts to wide format, appropriately formats columns
results_fn <- function(StartDate = "2019-04-03", EndDate = "2019-04-03", tz = NA) {
  
  # Set configuration options (set local time zone since db is in UTC)
  if (is.na(tz)) {
    if(!exists("config")) config <- get_config()
    tz <- config$tz   
  }
  
  #extract data from database 
  myresults <- get_query(paste("SELECT imports.runname, imports.site, imports.instrument_id, results.timeint, results.variable, results.value ", 
                               "FROM results ",
                               "INNER JOIN imports ",
                               "ON results.import_id = imports.id ",
                               "WHERE imports.flags = '' AND imports.success = 1 ",
                               "AND results.variable IN ('altitude_m', 'latitude', 'longitude', 'trh_temp_f', 'trh_rh', 'neph_bscat', 'neph_coef', 'ma200_ir_bc1', 'ma200_uv_bc1', 'ns_total_conc', 'pmdisc_size', 'pmdisc_number', 'pnc_noscreen', 'pnc_screen', 'co_ppm', 'co2_umol_mol', 'no2') ",
                               "AND CAST(SUBSTRING(imports.runname, 1, 10) AS DATE) >= '", StartDate, "' ",
                               "AND CAST(SUBSTRING(imports.runname, 1, 10) AS DATE) <= '", EndDate, "' " #,
                               #"LIMIT 100"
  )
  )
  
  myresults <- data.table(myresults)
  #ensure the results are returned in local time (database is in UTC)
  setattr(myresults$timeint, 'tzone', tz)
  
  # ? make back to 
  myresults <- as.data.frame(myresults) %>% 
    
    # #make wide format (data.table is converted to tibble) 
    # myresults.wide <- myresults %>% 
    #     dplyr::mutate(row_id = 1:n()) %>% #each row needs unique ID for spread to work when there are many records...?
    #     tidyr::spread(variable, value) %>%
    #     dplyr::select(-row_id) %>% #delete row ID after spread is complete
    
    #organize by time
    dplyr::arrange(timeint) %>%
    
    # #format columns
    #     dplyr::mutate(site = as.factor(site)) #%>%
    
    #rename columns & add units 
    rename(time = timeint) %>%  #, gps_altitude_m = altitude_m, gps_lat = latitude, gps_long = longitude, bc_ir880nm_ng_m3 = ma200_ir_bc1, bc_uv375nm_ng_m3 = ma200_uv_bc1, no2_ppb = no2, ufp_scan_ct_cm3 = ns_total_conc, ufp_pt_ct_cm3 = pnc_noscreen, ufp_pt_screen_ct_cm3 = pnc_screen, ufp_disc_med_size_nm = pmdisc_size, ufp_disc_ct_cm3 = pmdisc_number, neph_bscat_per_m = neph_bscat, rh_pct = trh_rh, temp_f = trh_temp_f) %>%
    
    mutate(variable = recode(variable, altitude_m = "gps_altitude_m", 
                             latitude = "gps_lat", 
                             longitude = "gps_long", 
                             ma200_ir_bc1 = "bc_ir880nm_ng_m3",
                             ma200_uv_bc1 = "bc_uv375nm_ng_m3", 
                             no2 = "no2_ppb", 
                             ns_total_conc = "ufp_scan_ct_cm3", 
                             pnc_noscreen = "ufp_pt_noscreen_ct_cm3", 
                             pnc_screen = "ufp_pt_screen_ct_cm3", 
                             pmdisc_size = "ufp_disc_med_size_nm", 
                             pmdisc_number = "ufp_disc_ct_cm3", 
                             neph_bscat = "neph_bscat_per_m", 
                             trh_rh = "rh_pct", 
                             trh_temp_f = "temp_f"
    )
    )
  
  #unique(myresults[c("variable", "variable2")])
  
  # #calc avg results by the second 
  #       dplyr::group_by(time, runname, site, instrument_id) %>%  #
  #       dplyr::summarise_if(is.numeric, funs(mean(., na.rm = TRUE)))
  #  
  #  #remove NaNs created when R tries to take mean of all NAs 
  #  myresults.wide[is.na(myresults.wide)] <- NA
  
  # return(myresults.wide) 
  return(myresults) 
}

#test <- results_fn(StartDate = "2019-04-03", EndDate = "2019-04-03")


##returns time at designated stops
stops_fn <- function(StartDate = "2019-04-03", EndDate = "2019-04-03", tz = NA) {
  
  # Set configuration options (set local time zone)
  if (is.na(tz)) {
    if(!exists("config")) config <- get_config()
    tz <- config$tz
  }
  
  #extract data from database. data will be in wide format 
  myresults <- get_query(paste("SELECT runname, timeint, endtime, duration, flags, location ", 
                               "FROM stops ",
                               "WHERE location <> '<NA>' ",
                               "AND CAST(SUBSTRING(runname, 1, 10) AS DATE) >= '", StartDate, "' ",
                               "AND CAST(SUBSTRING(runname, 1, 10) AS DATE) <= '", EndDate, "' " #,
                               #"LIMIT 100"
  )
  )
  
  myresults <- data.table(myresults)
  #return results in local time (database is in UTC)
  setattr(myresults$timeint, 'tzone', tz)
  
  #convert to long format (1 record per second)
  myresults.long <- myresults %>% 
    rowwise() %>%
    mutate(time = list(seq(timeint, endtime, 1))) %>%
    ungroup() %>%
    select(-timeint, -endtime) %>%
    unnest( ) %>%
    
    dplyr::arrange(time) %>%   
    
    #rename columns
    rename(site_id = location, 
           duration_sec = duration)  
  
  return(myresults.long) 
}

#s1 <- stops_fn(StartDate = "2019-04-03", EndDate = "2019-04-03") 

##returns pollutant values from stop locations (drops driving data)
stops_results_fn <- function(StartDate = "2019-04-03", EndDate = "2019-04-03"){
  
  #lat long of designated stops
  locations <- read.csv("/projects/trap/data_management/locations/locations.csv") %>%
    mutate(site = paste0(substr(route,1,1), 0, substr(route,6,6))) %>%
    select(site, site_id, longitude, latitude) %>%
    rename(site_long = longitude, site_lat = latitude)
  
  #AQS site indicator
  aqs_sites <- locations %>% 
    filter(substr(locations$site_id, 1, 2) == "MC") %>%
    unique() %>%
    mutate(aqs_id = recode(site_id, 
                           MC0120 = "BK",
                           MC0003 = "BW",
                           MC0126 = "CE",
                           MC0406 = "CW",
                           MC0002 = "BL"),
           aqs_location = recode(aqs_id, 
                                 BK = "10th & Weller",
                                 BW = "Beacon Hill",
                                 CE = "Duwamish",
                                 CW = "Kent",
                                 BL = "Tukwila Allentown")
    )
  
  #add aqs site info to locations dataset
  locations <- left_join(locations, aqs_sites)
  
  stops_data <- stops_fn(StartDate = StartDate, EndDate = EndDate)
  results_data <- results_fn(StartDate = StartDate, EndDate = EndDate)
  mydata <- left_join(stops_data, results_data)  %>% 
    left_join(locations) %>% 
    
    #organize columns; drop "flags" column
    select(runname, site, site_id, aqs_id, aqs_location, site_long, site_lat, duration_sec, time, instrument_id, variable, value) %>%
    
    # #organize columns
    # select(runname, site, site_id, site_long, site_lat, duration_sec, time, instrument_id, co_ppm, co2_umol_mol, no2_ppb, bc_ir880nm_ng_m3, bc_uv375nm_ng_m3, neph_bscat_per_m, ufp_scan_ct_cm3, ufp_disc_ct_cm3, ufp_disc_med_size_nm, ufp_pt_ct_cm3, ufp_pt_screen_ct_cm3, temp_f, rh_pct, gps_long, gps_lat, gps_altitude_m) %>% 
    rename(route = site) %>%
    
    #drop garage "stop" data (MS0000)
    filter(site_id != "MS0000")  
  
  return(mydata)
}

#test.all <- stops_results_fn()

#####################################################################################
#save data  
# all_data_1909017 <- stops_results_fn(StartDate = "2019-02-22", EndDate = "2019-09-17")
# saveRDS(all_data_1909017, file = file.path("..", "data_exports_mb", "all_data_190918.rda"))

#####################################################################################
