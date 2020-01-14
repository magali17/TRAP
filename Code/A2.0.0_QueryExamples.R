##################### NOTES ##################### 
#imports variables of interest: runname, site

#results variables of interests: 'altitude_m', 'latitude', 'longitude', 'trh_temp_f', 'trh_rh', 'neph_bscat', 'neph_coef', 'ma200_ir_bc1', 'ma200_uv_bc1', 'ns_total_conc', 'pmdisc_size', 'pmdisc_number', 'pnc_noscreen', 'pnc_screen', 'co_ppm', 'co2_umol_mol', 'no2' 

## ? 11_5, 15_4, 20_5, 27_4, 36_5, 48_7, 64_9, 86_6, 115_5, , 154_0, 205_4, 273_8, 365_2

###BC: 880 nm (IR), 625 nm (Red), 528 nm (Green), 470 nm (Blue), and 375 nm (UV); Tim G: “BC = IR 880 nm”; PSCAA: “UV minus BC correlates w/ OC”

#stops variables of interest: timeint, endtime, duration, flags, location

################################################################################### 



#Source Amanda's function get_query() fn
source(file.path("", "projects", "trap", "data_management", "code", "ajg_functions.R"))
#source(file.path("ajg_functions.R"))

#Source Brian's get_all_data() fn
source(file.path("", "projects", "trap", "data_management", "code", "data_proc_lib.R"))
#source(file.path( "data_proc_lib.R"))


##################### AMANDA'S FN ##################### 

#name of tables
get_query("SELECT table_name FROM information_schema.tables WHERE TABLE_SCHEMA='trap'")

#see table column names
get_query("SELECT * FROM fieldnotes limit 3;")
get_query("SELECT * FROM imports limit 3;")
get_query("SELECT * FROM locations limit 3;")
get_query("SELECT * FROM results limit 3;")
get_query("SELECT * FROM stops limit 3;")

##DOE data
get_query("SELECT * FROM doe_stations;")
get_query("SELECT * FROM doe_day;")
get_query("SELECT * FROM doe_hour limit 30;")
get_query("SELECT * FROM doe_minute limit 30;")


#see unique variable names
get_query("SELECT DISTINCT variable FROM results;")





get_query("desc imports")

## ...first few records
get_query("SELECT * FROM results WHERE variable = 'ma200_ir_bc1' LIMIT 5;")

##field notes btween 2 dates
get_query("SELECT * FROM fieldnotes WHERE timeint >= '2019-04-02 01:00:00' AND endtime <= '2019-04-02 23:59:59' ")

##get column names of table in descending order
get_query("desc results")


## ...limit rows starting not at row 1
get_query("SELECT * FROM results LIMIT 5 OFFSET 10000;")

## ...descending order, ordered by "id" column
get_query("SELECT * FROM results ORDER BY id DESC LIMIT 5;")

##rows 5-10
get_query("SELECT * FROM results LIMIT 5, 10;")


##DISTINCT variable values from a results   #takes long time to process
#get_query("SELECT DISTINCT variable FROM results")


##STOPS
get_query("SELECT * from stops limit 5")










##################### BRIAN'S FN ##################### 
#ERROR - not working?? takes too long
##Brian's fn to get data, transforms long to wide; converts to correct data type; returns "output.wide" df

##gathers All data. takes Long time to process
#get_all_data()

##import data from 1 day
x <- get_all_data(runname = "2019-04-03_R01")



#### TEST TO CREATE FN to....????
## Should: select only desired columns, between 2 dates, merge ??imports to ??results

##variables in results table to import:
###location (degrees unless otherwise stated): altitude_m, latitude, longitude  

###temp/RH: trh_temp_f, trh_rh

###---> use bscat?? units of each?? is there a traditional way of converting to ug/m3 or does it depend on air particle assumptions?
###PM2.5  (____): neph_bscat, neph_coef

### -->focus on UV BC1?
###BC (ng/m3): ma200_uv_bc1, #could also do: ma200_blue_bc1, ma200_green_bc1, ma200_red_bc1, ma200_ir_bc1 
###nanoscan total conc (#/cm3): ns_total_conc  #could also do bin counts 15.4-365.2. column names are mid-points in nm of each of the bins: 15_4, 20_5, 27_4, 36_5, 48_7, 64_9, 86_6, 115_5, 154_0, 205_4, 273_8, 365_2
###discmini median particle size (nm), PNC (#/cm3): pmdisc_size, pmdisc_number
###PTRAK screened, unscreened (#/cm3): pnc_noscreen, pnc_screen
###CO (ppm): co_ppm
###CO2 (umol/mol): co2_umol_mol
###NO2  (ppb): no2

#(results.vars <- paste("co_ppm", " ", sep = ", "))

### --> switch imports & results table

##only select variables we're interestedin from the "results" table
y <- get_query("SELECT * FROM results WHERE variable = 'altitude_m' OR variable = 'latitude' OR variable = 'longitude' OR variable = 'trh_temp_f' OR variable = 'trh_rh' OR variable = 'neph_bscat' OR variable = 'neph_coef' OR variable = 'ma200_uv_bc1' OR variable = 'ns_total_conc' OR variable = 'pmdisc_size' OR variable = 'pmdisc_number' OR variable = 'pnc_noscreen' OR variable = 'pnc_screen' OR variable = 'co_ppm' OR variable = 'co2_umol_mol' OR variable = 'no2' ")


######ERRROR 

#### ~Brian's function
mb_get_all_data <- function(runname = NA, tz = Sys.timezone(), vars) {
  vars = c("")
  # Get all TRAP monitoring data from database
  channel <- connect_to_db()
  query_select <- paste('SELECT * FROM results WHERE variable = "altitude_m" OR variable = "latitude" OR variable = "longitude" OR variable = "trh_temp_f" OR variable = "trh_rh" OR variable = "neph_bscat" OR variable = "neph_coef" OR variable = "ma200_uv_bc1" OR variable = "ns_total_conc" OR variable = "pmdisc_size" OR variable = "pmdisc_number" OR variable = "pnc_noscreen" OR variable = "pnc_screen" OR variable = "co_ppm" OR variable = "co2_umol_mol" OR variable = "no2" ',
                        'INNER JOIN results ', 
                        'ON imports.id = results.import_id ', 
                        'WHERE imports.flags = "" AND imports.success = 1')
  
  # query_select <- paste('SELECT * FROM imports INNER JOIN results ', 
  #                       'ON imports.id = results.import_id ', 
  #                       'WHERE imports.flags = "" AND imports.success = 1')
  if (!is.na(runname)) {
    query_select <- paste0(query_select, ' AND ( imports.runname = ') 
    runnames <- paste0(gsub('(.*)', '"\\1"', runname), 
                       collapse = ' OR imports.runname = ')
    query_select <- paste0(query_select, ' ', runnames, ' );')
  }
  
  suppressWarnings(output <- dbGetQuery(channel, query_select))
  suppressWarnings(res <- dbDisconnect(channel))
  output[, c(1, 2, 7, 8, 9, 10, 11, 12)] <- NULL
  output <- data.table(output)
  output[, timeint := as.POSIXct(timeint)]
  setattr(output$timeint, 'tzone', tz)
  
  status <-  output[grep("status", variable), 
                    c('runname', 'site', 'timeint', 'variable', 'status')]
  status <- unique(status)
  status[variable == 'ns_status', ns_status := status]
  status[variable == 'nsgl_status', nsgl_status := status]
  status[variable == 'ma200_status', ma200_status := status]
  status[variable == 'no2_status', no2_status := status]
  status[, status := NULL]
  status[, variable := NULL]
  setattr(status$timeint, "tzone", tz)
  setkeyv(status, c('runname', 'timeint'))
  
  if (nrow(output) > 0) {
    output.wide <-output %>% 
      dplyr::select(runname, timeint, variable, value) %>%
      dplyr::filter(!grepl('status', variable)) %>% unique() %>%
      dplyr::group_by_at(vars(-value)) %>% 
      dplyr::mutate(row_id = 1:n()) %>% 
      ungroup() %>%
      tidyr::spread(variable, value) %>% 
      dplyr::select(-row_id) %>%
      dplyr::arrange(runname, timeint) %>% 
      as.data.table()
    setkeyv(output.wide, c('runname', 'timeint'))
    
    if (length(grep('status', colnames(output))) > 0) {
      output.wide <- status[output.wide, nomatch=0]
    }
    
    if ("datetime_nano_sc" %in% colnames(output.wide)) {
      output.wide[, datetime_nano_sc := base::as.POSIXct(datetime_nano_sc,
                                                         origin = "1970-01-01 UTC")]
      setattr(output.wide$datetime_nano_sc, "tzone", tz)
    }
    
    #if("missing" %in% input$dataoptions) output.wide <- handle_missing(output.wide)
    
    return(output.wide)
  } else {
    return(NULL)
  }
}


#############################################################################################
dt <- get_all_data(startdate = "2019-04-01", enddate = "2019-04-03")



