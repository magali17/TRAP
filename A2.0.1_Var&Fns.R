#scrit of values & functions used in various RMarkdown scripts.


####################################################################################
############################### global variables ####################################
####################################################################################
images_path0 <- file.path("Output", "Aim 2", "Images")
tables_path0 <- file.path("Output", "Aim 2", "Tables")

#data to use  # use all data for now
myquantile_lower <- 0.00 
myquantile_upper <- 1.00 

#trim
trim_quantile <- 0.05 #0.05

#sampling dates
start.date = "2019-02-22"

##time of day
early_am <- seq(5,8)
am <- seq(9,11)
noon <- seq(12,15)
evening <- seq(16,20)
night <- c(seq(21,23), seq(0,4)) #? 0-4 in "night"?

####################################################################################
#################################### functions ####################################
####################################################################################

###################### geocovariate selection functions ######################

# no.missing.values <- function(var){
#   result <- sum(!is.na(var)) == length(var)
#   return(result)
# }
# 
# varies.enough <- function(var, threshold = 0.2){
#   #don't use '<=' or '>=' b/c will result in "TRUE" for a constant (e.g., all 0s)
#   result <- min(var) < mean(var)*(1 - threshold) |
#     max(var) > mean(var)*(1 + threshold)
#   return(result)
# }
# 
# ### --> ? what to use as "outlier" definition 
# few.outliers <- function(var, 
#                          z_outlier = 5, 
#                          max_prop_outliers = 0.02
# ){
#   
#   z_score <- (var - mean(var))/sd(var)
#   num_outliers <- sum(abs(z_score) > z_outlier)
#   prop_outliers <-  num_outliers/ length(var)
#   result <- prop_outliers <= max_prop_outliers
#   
#   return(result)
# }
# ## --> ? increase lower max_prop_outliers so fewer variables dropped? 
# few.outliers.iqr <- function(var,
#                              max_prop_outliers = 0.02){
#   
#   normal.range <- iqr(var)*1.5
#   outliers <- var > (quantile(var, 0.75) + normal.range) | 
#     var < (quantile(var, 0.25) - normal.range) 
#   
#   result <- sum(outliers) /length(var) <= max_prop_outliers
#   
#   return(result)
# }

################################# BC ONA Correction #################################
#code modified from Elena Austin's. 
#Elena: The tape position is to make sure measured values within the same attenuation threshold but different tape positions are not averaged. If you have measurements at the same position over multiple days, then I would make sure to add a term in the 'by' specification. Something like: by = c("ma200_tape_posit", "day")

#takes a dataset for BC in wide format and returns ONA corrected estimates for each MA200 wavelength.

# makenum = function(x) {as.numeric(as.character(x))}
# 
# ONA <- function(MAdata, 
#                        deltaATN=data.table(UV=0.05, 
#                                            Blue = 0.05, 
#                                            Green = 0.05, 
#                                            Red = 0.05, 
#                                            IR = 0.05)) {
#   pacman::p_load("data.table", "zoo", "plyr")
#   
#   MAdata <- MAdata %>% as.data.table()
#   
#   MAdata[ , UVthresh := round_any((`ma200_uv_atn1`)-max(`ma200_uv_atn1`)-deltaATN$UV, (deltaATN$UV+0.01), ceiling) + max(`ma200_uv_atn1`), by = c("ma200_tape_posit", "runname")]
#   MAdata[ , ona_uv := mean(makenum(`ma200_uv_bc1`)), by = c("ma200_tape_posit", "UVthresh", "runname")] 
#   
#   MAdata[ , Bluethresh := round_any((`ma200_blue_atn1`)-max(`ma200_blue_atn1`)-deltaATN$Blue, (deltaATN$Blue+0.01), ceiling) + max(`ma200_blue_atn1`), by = c("ma200_tape_posit", "runname")]    
#   MAdata[ , ona_blue := mean(makenum(`ma200_blue_bc1`)), by = c("ma200_tape_posit", "Bluethresh", "runname")] 
#   
#   MAdata[ , Greenthresh := round_any((`ma200_green_atn1`)-max(`ma200_green_atn1`)-deltaATN$Green, (deltaATN$Green+0.01), ceiling) + max(`ma200_green_atn1`), by = c("ma200_tape_posit", "runname")]    
#   MAdata[ , ona_green := mean(makenum(`ma200_green_bc1`)), by = c("ma200_tape_posit", "Greenthresh", "runname")] 
#   
#   MAdata[ , Redthresh := round_any((`ma200_red_atn1`)-max(`ma200_red_atn1`)-deltaATN$Red, (deltaATN$Red+0.01), ceiling) + max(`ma200_red_atn1`), by = c("ma200_tape_posit", "runname")]    
#   MAdata[ , ona_red := mean(makenum(`ma200_red_bc1`)), by = c("ma200_tape_posit", "Redthresh", "runname")] 
#   
#   MAdata[ , IRthresh := round_any((`ma200_ir_atn1`)-max(`ma200_ir_atn1`)-deltaATN$IR, (deltaATN$IR+0.01), ceiling) + max(`ma200_ir_atn1`), by = c("ma200_tape_posit", "runname")]    
#   MAdata[ , ona_ir := mean(makenum(`ma200_ir_bc1`)), by = c("ma200_tape_posit", "IRthresh", "runname")] 
#   
#   MAdata <- MAdata %>% as.data.frame()
#   
#   #creates issues later if you leave loaded
#   #pacman::p_unload("data.table")
#   
#   return(MAdata)
#   
#   }


############################## separate buffers from covariate names ################################
#dt=geo_cor
#cov = "cov"
split_cov_name <- function(dt, cov) {
  dt <- suppressWarnings(dt %>%
                           rename(cov_full_name = cov) %>%
                           mutate(
                             buffer = substr(cov_full_name, nchar(cov_full_name)-4, nchar(cov_full_name)),
                             buffer = as.numeric(ifelse(!is.na(as.integer(buffer)), buffer, NA)),
                             cov = ifelse(is.na(buffer), cov_full_name, substr(cov_full_name, 1, nchar(cov_full_name)-5) )
                           )
  ) %>%
    select(contains("cov"), buffer, everything())
  
    # elevation
    dt$cov[grepl("^elev_.+_above$", dt$cov_full_name)] <- "elev_above"
    dt$cov[grepl("^elev_.+_below$", dt$cov_full_name)] <- "elev_below"
    dt$cov[grepl("^elev_.+_stdev$", dt$cov_full_name)] <- "elev_stdev"
    dt$cov[grepl("^elev_.+_at_elev$", dt$cov_full_name)] <- "elev_at_elev"
    
    dt$buffer[grepl("_1k_", dt$cov_full_name)] <- 1000
    dt$buffer[grepl("_5k_", dt$cov_full_name)] <- 5000
    
  return(dt)
}


#################################### temporal variables ####################################

# function to add temporal variables to any dataset: time of day, day, time of week, month, season

##date.var should be in posixct() format
add.temporal.variables <- function(data,
                                   date.var = "time",
                                   early_am. = early_am, am.=am, noon.=noon, evening.=evening, night.=night
){
  library(lubridate)
  
  #month and day when seasons typically start. will be used later to paste onto the current year 
  ##http://www.glib.com/season_dates.html 
  winter <- "-12-21" #usually winter starts on 21st, sometimes on 22nd 
  spring <- "-03-20"
  summer <- "-06-21" #usually summer starts on 21st, sometimes on 22nd 
  fall <- "-09-23" #usually fall starts on 22nd, sometimes on 23nd. Using 23rd for 2019 mobile monitoring campaign 
 
  mydata <- data %>% 
    dplyr::rename(
      mydate = date.var
    ) %>%
    dplyr::mutate(
      hour = hour(mydate),
      #group night hours together 
      hour_night = ifelse(hour < min(early_am.), hour + 24, hour),
      time_of_day = factor(ifelse(hour %in% early_am., "early_am",
                                ifelse(hour %in% am., "am",
                                       ifelse(hour %in% noon., "noon",
                                              ifelse(hour %in% evening., "evening", "night")))),
                         levels= c("early_am", "am", "noon", "evening", "night")),
    day = lubridate::wday(mydate, label = T, abbr = T),
    time_of_week = factor(ifelse(wday(mydate) %in% c(1, 7), "weekend", "weekday")),
    month = lubridate::month(mydate, label = T, abbr = T),
    season = factor(ifelse((mydate >= ymd(paste0((year(mydate)-1), winter)) & mydate < ymd(paste0(year(mydate), spring))) |
                       mydate >= ymd(paste0(year(mydate), winter)), "winter",
                     ifelse(mydate >= ymd(paste0(year(mydate), spring)) &
                              mydate < ymd(paste0(year(mydate), summer)), "spring",
                            ifelse(mydate >= ymd(paste0(year(mydate), summer)) &
                                     mydate < ymd(paste0(year(mydate), fall)), "summer", 
                                   ifelse( mydate >= ymd(paste0(year(mydate), fall)) &
                                             mydate < ymd(paste0(year(mydate), winter)), "fall", 
                                           NA)))), 
                    levels = c("spring", "summer", "fall", "winter"))
    )  
  
  #change time variable back to what it was originally
  names(mydata)[names(mydata) %in% "mydate"] <- date.var

  return(mydata)

}

############################## combines multiple ptrak or aethalometer MA200 files ##############################
# ptrak.bind.fn # OLD NAME

#returns single file of ptrak data
read_directory_files <- function(folder_path, 
                                 instrument = "ptrak"
) {
  
  #names of files in folder
  files_list <- list.files(folder_path)
  
  #create empty df w/ correct number of columns for specific files
  df <- data.frame()
  
  #rbind files
  for (i in 1:length(files_list)) {
    
    if(instrument == "ptrak") {df1 <- read.ptrak(myfilepath = file.path(folder_path, files_list[i]))} 
    
    if(instrument == "bc") {df1 <- read_bc(myfilepath = file.path(folder_path, files_list[i]))}
    
    df <- rbind(df, df1)
  } 
  
  tz(df$time) <- "America/Los_Angeles"
  
  return(df)
  
}

################################ reads raw ptrak files ################################
#returns clean ptrak files from raw files 

#data.table allows us to look for text in a line, otherwise use a row #

#myfilepath <- file.path(my_path, "2019-04-18_PMPT_93_LAB_a.txt")

read.ptrak <- function(myfilepath) {
  
  #library(stringi)
  
  dt <- data.table::fread(myfilepath,
                           #skip lines until see this text & make it column headers
                           skip = "MM/dd/yyyy", 
                           #keeps reading after blank row
                           fill=T) %>%
 dplyr::rename(
    date = "MM/dd/yyyy", 
    time = "hh:mm:ss", 
    Conc_pt_cm3 = "pt/cc"
  ) %>%
  #only keep rows that start with numbers (the data)
  filter(substr(date, 1,1) %in% c(0:9)) %>%
  mutate(
    Conc_pt_cm3 = as.numeric(Conc_pt_cm3),
    datetime = mdy_hms(paste(date, time), tz = "America/Los_Angeles")
  ) %>%
  select(
    time = datetime,
    Conc_pt_cm3
   ) 
  
  #location ID
  dt$location <- NA
  dt$location[grepl("10W", toupper(myfilepath))] <- "10W"
  dt$location[grepl("BH", toupper(myfilepath))] <- "BH"
  dt$location[grepl("LAB", toupper(myfilepath))] <- "Lab"
  dt$location[grepl("RM114", toupper(myfilepath))] <- "Rm 114"
  dt$location[grepl("_R0", toupper(myfilepath))] <- "End of route"
  
  # instrument ID
  dt$instrument_id <- NA
                                    # (?i) # ignore lower case
  dt$instrument_id <- str_extract(myfilepath, "(?i)(PMPT_)[0-9]*")  
  
  return(dt)
  
  }

#   str_extract(myfilepath, "(?i)(?<=PMPT)[0-9]")


################################ reads raw BC files ################################

read_bc <- function(myfilepath) {
  
  dt <- read.csv(myfilepath) %>%
    select(date = Date.local..yyyy.MM.dd., 
           time = Time.local..hh.mm.ss.,
           ir_atten = IR.ATN1,
           ir_bc =  IR.BC1
    ) %>%
    mutate(
      time = ymd_hms(paste(date, time), tz = "America/Los_Angeles")
    ) %>%
    select(-date)
  
  #location ID
  dt$location <- NA
  dt$location[grepl("10W", toupper(myfilepath))] <- "10W"
  dt$location[grepl("BH", toupper(myfilepath))] <- "BH"
  dt$location[grepl("LAB", toupper(myfilepath))] <- "Lab"
  dt$location[grepl("RM114", toupper(myfilepath))] <- "Rm 114"
  dt$location[grepl("_R0", toupper(myfilepath))] <- "End of route"
  
  #instrument ID
  dt$instrument_id <- NA
  # (?i) # ignore lower case
  dt$instrument_id <- str_extract(myfilepath, "(?i)(bc_)[0-9]*")  
  
  return(dt)
  
}


 
###################### Time series plots #####################################################
# returns time series plots of all values in the dataset, facetted by run

# dt <-  mm_full %>%
#   mutate(value = ifelse(variable == "bc_ng_m3", value*bc_factor, value)) %>%
#   filter(runname %in% high_bc$runname)
# 
# hline_value = ptrak_lim

time_series_plots <- function(dt, 
                              plot_bc_factor = 10, 
                              mytitle = "",
                              hline_value = NA,
                              ymax=NA,
                              facet_runname = TRUE
                          
                              ) {
  
 if(is.na(ymax)) {ymax <- max(dt$value)}
  
   myplot <- dt %>%
    ggplot(aes(x = time, y=value, col=variable, 
               #shape=instrument_id
               )) + 
    geom_point(alpha=0.4) +
    scale_y_continuous(sec.axis = sec_axis(~./plot_bc_factor, name = "BC (ng/m3)",
                                           labels = function(x) format(x, scientific = T)),
                       labels = function(x) format(x, scientific = T),
                       limits = c(NA, ymax)
                       ) +
    labs(title= mytitle,
         y = "UFP (pt/cm3)",
         shape = "Instrument ID",
         col = ""
    ) +
    theme(legend.position = "bottom") 
    
  # add horizontal line
  if(!is.na(hline_value)) {
    myplot <- myplot +
      geom_hline(aes(yintercept=hline_value))
  }
  
  # facet plot
  if(facet_runname == TRUE) {
    myplot <- myplot +
      facet_wrap(~runname, scales = "free") 
  }
  
  myplot
  
}



########################################################################################################################
########################################################################################################################
# returns unique times each site was sampled based on different time combinations & the site IDs. default is by site and pollutant variable

# dt = stops_l
# min_samples_required = min.t 
# temporal_vars = c("season", "time_of_week", "tod5")

unique_times_sampled <- function(dt, 
                                 min_samples_required,
                                 temporal_vars,
                                 return_site_ids = FALSE
                                 #grouping_vars = c("Pollutant", "site_id")
) {
  
  times_sampled <- dt %>%
    select(Pollutant, site_id, temporal_vars) %>%
    unique() %>%
    group_by(Pollutant, site_id) %>%
    dplyr::summarize(unique_times =n()) 
  
  sites_sampled_min_amount <- times_sampled %>%
    filter(unique_times >= min_samples_required) %>%
    select(-unique_times)
  
  n_sampled_min_amount <- times_sampled %>%
    group_by(Pollutant) %>%
    # number of sites sampled each combination of times
    dplyr::summarize(sites_sampled_all_times = sum(unique_times >= min_samples_required)) 
  
  if(return_site_ids == FALSE) {
    result <- n_sampled_min_amount
    } else {
      result <- list(n_sampled_min_amount = n_sampled_min_amount, 
                   sites_sampled_min_amount = sites_sampled_min_amount)
  }
  
  return(result)
  
}

##################################################################################################################
#############################################################################################
#returns boxplots of UFP estimates for selected sites by method
ufp_by_method <- function(dt, 
                          dt.range,
                          .months.sampled = months.sampled,
                          add.to.title = ""
                          ) {
  
  n <- length(unique(dt$site_id))
  
  p <- dt %>%
    ggplot(aes(x=factor(site_id), y= ufp,
               col=method, group=method)) +  #fill=method #had fill instead of col
    geom_pointrange(data=dt.range, 
                    position=position_dodge(width=0.75), 
                    aes(y=mean, ymin = min, ymax = max,
                        fill=""
                        ),
                    shape= 0, #3, #95,
                    alpha=0.5
                    ) + 
    geom_point(position=position_dodge(width=0.75), aes(shape=weight)) +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(title = paste0("Site mean UFP for ", .months.sampled[1], " - ", .months.sampled[length(.months.sampled)], " (Spring - Fall), n = ", n, " sites ", add.to.title),
         y = "UFP (pt/cm3)",
         col = "method",
         fill = "method avg"
         #caption = "mean = circle"
    ) 
  
  #p
  
  return(p)
  
}
 
# ufp_by_method (dt = annual.l.tod2, 
#                dt.range = annual.l.tod2.rage) 


####################################################################################################
#returns dataset with yhat (predictions) column

# dt = annual
# y_name = "mean_s_tow2_tod2"
# x_names = reg_predictors

save.pred.fn <- function(dt, 
                         y_name,
                         x_names) {
  
  lm1 <- lm(formula(paste0(y_name, "~", 
                           # --> used to say "sep ="  ?
                           paste(x_names, collapse = " + ")
                           )), data = dt)
  
  #needed when there are NAs in a df & want to save predictions in correct index
  not_na_index <- !is.na(dt[[y_name]])
  dt$yhat <- NA
  dt$resid_yhat <- NA

  #save predictions 
  dt$yhat[not_na_index] = predict(lm1)
  dt$resid_yhat[not_na_index] = residuals(lm1)
  
  #rename columns
  names(dt)[names(dt) == "yhat"] <- paste0("LUR_", y_name)
  names(dt)[names(dt) == "resid_yhat"] <- paste0("resid_", y_name)
  
  return(dt)
  
}


################################## map data using Stament maps ##################################################

# dt = uw_ufp_and_cov
# y_name = "ufp"
# y_units = "pt/cm3"
# latitude_name = "latitude"
# longitude_name = "longitude"
# map_title = NULL
# include_monitoring_area = TRUE
# include_study_area = T
# maptype. = "terrain"
# zoom_lvl = 11


# https://www.r-bloggers.com/getting-started-stamen-maps-with-ggmap/ 

pacman::p_load(ggmap)

# returns a base map (2D)
map_base <- function(dt,
                   latitude_name = "latitude", longitude_name = "longitude",
                   map_title = NULL,
                   include_monitoring_area = FALSE, monitoring_area_alpha = 0.3,
                   include_study_area = FALSE, study_area_alpha = 0.4,
                   include_st_area = FALSE, st_area_alpha = 0.4,
                   maptype. = "terrain", #, "toner", "toner-background", "watercolor"
                   zoom_lvl = 11
) {
  
  dt <- rename(dt, 
               lat = latitude_name,
               long = longitude_name) 
  
  # dimensions of data 
  height <- max(dt$lat) - min(dt$lat)
  width <- max(dt$long) - min(dt$long)
  
  # make map boundaries little larger than data dimensions
  bbox <- c(
    min(dt$long) - 0.5*width,
    min(dt$lat) - 0.1*height,
    max(dt$long) + 0.5*width,
    max(dt$lat) + 0.1*height
  )
  
  names(bbox) <- c("left", "bottom", "right", "top")
  
  # Make a map base layer of "Stamen" tiles
  map <- suppressMessages(get_stamenmap(bbox = bbox, zoom = zoom_lvl,
                                        maptype = maptype.))
  
  # Make basic map image from the tiles
  mymap <- ggmap(ggmap = map, darken = c(0.5, "white")) + theme_void()
  
  # add spatiotemporal model area
  if(include_st_area) {
    st_area <- readRDS(file.path("Data", "GIS", "st_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = st_area, 
                   aes(x = long, y = lat, group = group,
                   ),
                   alpha = st_area_alpha,
                   size = 0.3)
  }
  
  # add study area
  if(include_study_area) {
    study_area <- readRDS(file.path("Data", "GIS", "study_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = study_area, 
                   aes(x = long, y = lat, group = group, 
                       #col = "Study" #used to be fill = , but has issues w/ stat_contour() which also uses fill
                   ),
                   #fill = "plum2",
                   #col = "plum2",
                   alpha = study_area_alpha,
                   size = 0.5 )
  }
  # add monitoring area
  if(include_monitoring_area) {
    monitoring_area <- readRDS(file.path("Data", "GIS", "monitoring_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = monitoring_area, 
                   aes(x = long, y = lat, group = group,
                       #col = "Monitoring"
                   ),
                   #color = 'peru',  
                   #fill = 'peru', 
                   alpha = monitoring_area_alpha,
                   size = 0.3)
  }
  
  mymap <- mymap +
    labs(title = map_title,
         col = "Area"
         ) + 
    theme(legend.position = c(0.98, 0.01), legend.justification = c(1,0)) 
  
  return(mymap)
  
}

################

# dt <- annual_maps
# color_map = TRUE
# color_by = annual_ufp_names[i]
# color_units = "pt/cm3"
# outline_points = TRUE
# latitude_name = "latitude"
# longitude_name = "longitude"
# map_title = annual_ufp_names[i]
# include_monitoring_area = FALSE
# monitoring_area_alpha = 0.2
# include_study_area = FALSE
# study_area_alpha = 0.2
# include_st_area = FALSE
# st_area_alpha = 0.2
# maptype. = "terrain"
# zoom_lvl = 11
# low_conc_col = "yellow"
# high_conc_col = "red"

#################
# combines map_base() and map_color() fns. but fills by study and monitoring regions (basemap does not - works better for e.g. stat_contour() which uses fill)
map_fn <- function(dt, 
                   color_map = TRUE,
                   color_by = "ufp", color_units = "pt/cm3",
                   outline_points = TRUE,
                   dot_size = 2,
                   latitude_name = "latitude", longitude_name = "longitude",
                   map_title = NULL,
                   include_monitoring_area = FALSE, monitoring_area_alpha = 0.2,
                   include_study_area = FALSE, study_area_alpha = 0.2,
                   include_st_area = FALSE, st_area_alpha = 0.2,
                   maptype. = "terrain", #, "toner", "toner-background", "watercolor"
                   zoom_lvl = 11,
                   low_conc_col = "yellow",
                   high_conc_col = "red"
) {
  
  
  #label by variable being mapped if no title is provided
  if(is.null(map_title)) {map_title <- color_by}
  
  dt <- rename(dt,
               y = color_by,
               lat = latitude_name,
               long = longitude_name
  ) 
  
  # dimensions of data 
  height <- max(dt$lat) - min(dt$lat)
  width <- max(dt$long) - min(dt$long)
  
  # make map boundaries little larger than data dimensions
  bbox <- c(
    min(dt$long) - 0.5*width,
    min(dt$lat) - 0.1*height,
    max(dt$long) + 0.5*width,
    max(dt$lat) + 0.1*height
  )
  
  names(bbox) <- c("left", "bottom", "right", "top")
  
  # Make a map base layer of "Stamen" tiles
  map <- suppressMessages(get_stamenmap(bbox = bbox, zoom = zoom_lvl,
                                        maptype = maptype.))
  
  # Make basic map image from the tiles
  mymap <- ggmap(ggmap = map, darken = c(0.5, "white")) + theme_void()
  
  # add spatiotemporal model area
  if(include_st_area) {
    st_area <- readRDS(file.path("Data", "GIS", "st_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = st_area, 
                   aes(x = long, y = lat, group = group,
                       fill = "ST model"),
                   alpha = st_area_alpha,
                   size = 0.3)
  }
  
  # add study area
  if(include_study_area) {
    study_area <- readRDS(file.path("Data", "GIS", "study_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = study_area, 
                aes(x = long, y = lat, group = group, 
                    fill = "Study"),
                alpha = study_area_alpha,
                size = 0.5 
                )
  }
  # add monitoring area
  if(include_monitoring_area) {
    monitoring_area <- readRDS(file.path("Data", "GIS", "monitoring_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = monitoring_area, 
                aes(x = long, y = lat, group = group,
                    fill = "Monitoring"
                    ),
                #color = 'peru',  
                #fill = 'peru', 
                alpha = monitoring_area_alpha,
                size = 0.3)
    }
  
  # color map
  if(color_map) {
    if(outline_points) {
    mymap <- mymap +
      geom_point(data = dt,
                 aes(x = long, y = lat), col= "black",
                 size = dot_size+0.5)
    }
    mymap <- mymap +
      geom_point(data = dt,
                 aes(x = long, y = lat, col = y),
                 alpha = 0.8, size = dot_size,
      ) +
      scale_color_gradient(name = color_units, low = low_conc_col, high = high_conc_col) 
  }
  
  mymap <- mymap +
    labs(title = map_title,
         fill = "Area"
         ) + 
    theme(legend.position = c(0.98, 0.01), legend.justification = c(1,0)) 
  
  return(mymap)
  
}


#################################################################################################################
#################################################################################################################

# returns annual average site estimates for "ptrak" variable from repeated stop-level observations. Also returns summary tables and figures. 

# dt <- stops
# var <- "ufp_pt_cm3"
# lm_x_names = c("site_id", "season", "time_of_week", "tod5")

estimate_annual_avg <- function(dt,
                                var,
                                # model used to estimate annual averages
                                lm_x_names = c("site_id", "season", "time_of_week", "tod5"),
                                estimate_label = ""
                                ) {
  # #rename variables
  # dt <- dt %>%
  #   rename(var = var)

  ## grid to save predictions
  lm_pred <- expand.grid(
    #include all possible variables
    site_id = unique(dt$site_id), 
    season = unique(dt$season),
    time_of_week = unique(dt$time_of_week),
    tod2 = unique(dt$tod2),
    tod5 = unique(dt$tod5)) %>%
    mutate(tod5_wt = recode(tod5, 
                     "3_8" = 6,
                     "9_11" = 3,
                     "12_15" = 4,
                     "16_20" = 5,
                     "21_2" = 6
                     )/24,
           tod2_wt = recode(tod2, 
                         "9_17" = 9,
                         "18_08" = 15
                         )/24,
          season_wt = 1/length(unique(dt$season)),
          time_of_week_wt = ifelse(time_of_week == "weekday", 5/7, 2/7)) %>%
    # only keep variable names used to estimate annual avg
    select(matches(paste0(lm_x_names, collapse = "|"))) %>%
    #may have multiple rows per combination
    unique()
  
    lm_fit <- lm(formula(paste0(var, "~", paste(lm_x_names, collapse = " + "))), data = dt)
    
    lm_pred$yhat <- predict(lm_fit, lm_pred) 
    
    # weigh temporally-specific predictions
    if("season" %in% lm_x_names) {lm_pred$yhat <- lm_pred$yhat*lm_pred$season_wt}
    
    if("time_of_week" %in% lm_x_names) {lm_pred$yhat <- lm_pred$yhat*lm_pred$time_of_week_wt}
    
    if("tod2" %in% lm_x_names) {lm_pred$yhat <- lm_pred$yhat*lm_pred$tod2_wt}
    
    if("tod5" %in% lm_x_names) {lm_pred$yhat <- lm_pred$yhat*lm_pred$tod5_wt}
    
    lm_pred <- lm_pred %>%
    group_by(site_id) %>%
    dplyr::summarize(yhat = sum(yhat))
    
    names(lm_pred)[names(lm_pred) == "yhat"] <- estimate_label
  
    # return results 
    results <- lm_pred
    
    return(results)
}


####################################################
# returns df w/ relabeled pollutant names in the "variable" column

label_pollutant <- function(dt, 
                           var = "variable",
                           label = "instrument"
                           ) {
  dt <- dt %>%
    dplyr::rename(var = var) 
  
  if(label == "instrument") {
  
    dt <- dt %>%
      mutate(var = recode_factor(var,
                          "ptrak_pt_cm3" = "P-TRAK UFP (20-1,000 nm)",
                          "scan_20_420_pt_cm3" = "NanoScan UFP (20-420 nm)",
                          "ona_bc" = "ONA-corrected BC",
                          "ma200_bc" = "MA 200 BC",
                          "ma200_ir_bc1" = "MA 200 BC",
                          "bc_ng_m3" = "MA 200 BC"
                          )) 
  }
  
  if(label == "pollutant") {
    
    dt <- dt %>%
      mutate(var = recode_factor(var,
                                 #Aim 2
                                 "ptrak_pt_cm3" = "UFP (pt/cm3)",
                                 "ufp_pt_cm3" = "UFP (pt/cm3)",
                                 #"scan_20_420_pt_cm3" = "UFP (20-420 nm)",
                                 "ona_bc" = "ONA-corrected BC (ng/m3)",
                                 "ma200_bc" = "BC (ng/m3)",
                                 "bc_ng_m3" = "BC (ng/m3)",
                                 
                                 #Aim 3
                                 "bc" = "BC (ng/m3)",
                                 "ufp" = "UFP (pt/cm3)"
                                 )) 
  }
  
  
  
  #change variable name back
  names(dt)[names(dt) == "var"] <- var

  return(dt)
  
}

##############################################################################################################################
# returns dataframe with analysis name (e.g., ufp_primary) broken up by pollutant (UFP, BC) and a description label

# dt <- cv_results
# var <- "Analysis"
# end_character = 0

label_analysis <- function(dt, 
                           var,
                           end_character = 0
                           ) {
  dt <- dt %>%
    rename(var = var) %>%
    mutate(
      Pollutant = ifelse(grepl("ufp", var), "UFP (pt/cm3)", "BC (ng/m3)" ),
      Pollutant = factor(Pollutant, levels = c("UFP (pt/cm3)", "BC (ng/m3)")),
      var = as.character(var),
      var = ifelse(grepl("ufp", var), substr(var, 5, nchar(var)-end_character), substr(var, 4, nchar(var)-end_character)),
      var = factor(var, levels = c("primary", "stop_means", "trim10", "windsorize", "uw", "native_scale", "monitoring_area")),
      var = recode_factor(var, 
                          "primary" = "Primary",
                          "stop_means" = "Stop means",
                          "trim10" = "10% trimmed mean",
                          "windsorize" = "Windsorized mean",
                          "uw" = "Unweighted mean",
                          "native_scale" = "Native scale modeling",
                          "monitoring_area" = "Predict within monitoring area"
                          )
      ) %>%
    select(Pollutant, Analysis = var, everything())
  
  return(dt)
  
}

#################################################################################################################
################################################# UK ########################################################

#1. loops over pls_uk_cv_predictions() fn below to return CV RMSE and R2 for a series of PLS components and maximum variogram plotting distances. 
pls_uk_cv_eval <- function(dt2 = annual_train_test,
                           # no. of PLS components to loop over
                           pls_comps = c(1:6),
                           # for UK model variogram - fraction
                           dist_fract = c(0.05, seq(0.1, 0.5, by=0.1)),
                           # to be passed to pls_uk_cv_predictions()
                           y_name.. = "log_ufp",
                           # predictors
                           cov_names.. = cov_names,
                           #CV folds
                           k. = 10,
                           # whether values should be exponentiated (e.g., if on log scale) before calculating CV RMSE and R2
                           exponentiate_obs_and_pred = TRUE #,
                           #site_locations.. = site_locations
                           ) {
  
  #df to save CV model evaluation
  cv_eval <- data.frame(
    expand.grid(pls_comp = pls_comps,
                dist_fract = dist_fract),
    RMSE = NA,
    R2 = NA,
    #for UK variogram
    max_plot_dist = NA,
    max_dist = NA
    )
  
  ################## CV loops ##################################
  # estimate CV RMSE and r2 for diff number of PLS components
  for(i in pls_comps) {
    #i=1

    for(j in dist_fract) {
      #j = 0.05
      #10FCV for each PLS-distance fraction combination
        cv_prediction <- pls_uk_cv_predictions(dt = dt2, 
                              dist_fract. = j,
                               use_n_scores. = i, 
                               
                               y_name = y_name.., 
                               cov_names. = cov_names.., 
                               k = k. #, 
                              # site_locations. = site_locations..
                              )
      
      # calculate performance statistics on native scale 
        ## exponeniate before calculating RMSE, R2 (e.g., if on log scale)
        dt_obs <- cv_prediction$dt$y_name
        uk_pred <- cv_prediction$dt$cv_prediction
        
        if(exponentiate_obs_and_pred == TRUE) {
        dt_obs <- exp(dt_obs)
        uk_pred <- exp(uk_pred)
        } 
      
        # CV RMSE & R2 
        cv_eval$RMSE[cv_eval$pls_comp== i & cv_eval$dist_fract==j] <- rmse(obs = dt_obs, pred = uk_pred)
        cv_eval$R2[cv_eval$pls_comp== i & cv_eval$dist_fract==j] <-  r2_mse_based(obs = dt_obs, pred = uk_pred)
        
        # variogram plotting paramters
        cv_eval$max_plot_dist[cv_eval$pls_comp== i & cv_eval$dist_fract==j] <- cv_prediction$max_plot_dist
        
        cv_eval$max_dist[cv_eval$pls_comp== i & cv_eval$dist_fract==j] <- cv_prediction$max_dist
        
    }
  }
  
  ######################### select CV parameters #########################
  cv_rmse <- min(cv_eval$RMSE)  
  rmse_inx <- cv_eval$RMSE == cv_rmse
    
  cv_r2 <- cv_eval$R2[rmse_inx] 
  cv_comp <- cv_eval$pls_comp[rmse_inx]
  cv_dist_fract <- cv_eval$dist_fract[rmse_inx]
  
  cv_plot_dist <- cv_eval$max_plot_dist[rmse_inx] 
  #cv_max_dist <- cv_eval$max_dist[rmse_inx]  
  
  cv_table <- data.frame(PLS_Components = cv_comp,
                         Variogram_Distance_Fraction = cv_dist_fract,
                         Variogram_Plotting_Dist_m = round(cv_plot_dist),
                         #Max_Dist_m = round(cv_max_dist),
                         RMSE = round(cv_rmse),
                         R2 = round(cv_r2, 2)
                         )
  #return(cv_table)
  
  eval_results <- list(cv_eval = cv_eval,
                        cv_table = cv_table)

  return(eval_results)
  
  
}


# [fn used above in pls_uk_cv_eval()]
# returns CV predictions for a selected number of PLS components and maximum variogram plotting distance. "dt" includes y outcome (UFP), site location (lambert_x, lambert_y) and covariates 

# dt = annual
# y_name = analysis_names[1]
# cov_names. = cov_names_log
# k=2
# use_n_scores. = 2
# dist_fract. = 0.1

pls_uk_cv_predictions <- function(
  dt = annual_train_test,
                                  y_name = "log_ufp",
                                  cov_names. = cov_names,
                                  #CV folds
                                  k = 10,
                                  # max PLS components to evaluate
                                  use_n_scores. = 3, 
                                  # variogram maximum distance fraction to model
                                  dist_fract. = 0.1 
                                  
) {  
  
  score_n_names <- paste0("Comp", 1:use_n_scores.)
  
  dt <- dt %>% rename(y_name = y_name) %>%
    drop_na() %>%
    #create folds for test/train set
    mutate(set = sample(c(1:k), size = nrow(.), replace = T),
           # to save predictions
           cv_prediction = NA)
 
  for(f in seq_len(k)) {
    #f=1
    train_grp <- dt$set != f
    
    dt_train <- dt %>% filter(train_grp)  
    
    dt_test <- dt %>% filter(!train_grp)   
    
    #fit PLS to training data
    pls_train <- plsr(y_name ~.,
                      data=dt_train[,c("y_name", cov_names.)], 
                      ncomp = use_n_scores.,
                      scale=T)
    
    # extract scores ("new covariate") for UK
    scores_train <- scores(pls_train)[,c(1:use_n_scores.)] %>% 
      as.data.frame()
    scores_test <- predict(object = pls_train,
                           newdata = dt_test,
                           ncomp = 1:use_n_scores.,
                           type = "score") %>%
      as.data.frame()
    
    # take out spaces in names
    names(scores_train) <- score_n_names
    names(scores_test) <- score_n_names
    
    # dataset w/ UFP measurements, geocovariates & location info
    pls_df_train <- cbind(dt_train, scores_train)  
    
    pls_df_test <- cbind(dt_test, scores_test)  
    
    ################################ UK ################################
    geo_train <- as.geodata(pls_df_train, 
                            coords.col = c("lambert_x", "lambert_y"), 
                            data.col = "y_name", 
                            covar.col = score_n_names)
    geo_test <- as.geodata(pls_df_test, 
                           coords.col = c("lambert_x", "lambert_y"), 
                           data.col = "y_name", 
                           covar.col = score_n_names)
    
    ##trend
    cov_trend <-  as.formula(paste0("~ ", paste0(score_n_names,  collapse = " + " )))
    
    max.dist <- summary(geo_train)$distances.summary[["max"]]
    
    # --> ? select this variogram parameter through CV??
    max.plot.dist <- max.dist*dist_fract. #[dist_fract_index] 
    
    ############################ model residuals ###################################### 
    ##Empirical Variogram
    brk_pt <- 1000
    by1_pt <- 300
    by2_pt <- 1000
    
    variog_train <- variog(geo_train,
                           #plotting breakpoints 
                           uvec=c(seq(0, brk_pt, by = by1_pt), seq((brk_pt + by2_pt), max.plot.dist, by= by2_pt)),
                           #UK
                           trend = cov_trend, 
                           messages = F)
    
    #use geoR try to estimate intitial range & sill values. using WLS and an exponential fit
    wls_ests_train <- variofit(variog_train, cov.model = "exp", 
                               messages = F)
    
    # --> ? select this variogram parameter through CV??
    #don't need initial values above since estimates seem to be the same w/ or w/o ini = wls_ests_train (based on small sample)?
    resid_model_train <- variofit(vario = variog_train, 
                                  ini = wls_ests_train, 
                                  cov.model = "exp",
                                  #weights = "equal", #ols
                                  weights = "npairs",#wls
                                  #messages = F
                                  ) 
    
    #trend
    train_trend <- trend.spatial(trend = cov_trend, geo_train)
    test_trend <- trend.spatial(trend = cov_trend, geo_test)
    
    ############################# Predict #############################
    kc_cv <- krige.conv(geo_train,
                        # where you want to predict
                        locations = geo_test$coords,
                        krige = krige.control(type = "ok",
                                                          # range, nugget, partial sill
                                              obj.model = resid_model_train, 
                                              trend.d = train_trend,
                                              trend.l = test_trend))
    
    #save CV predictions
    dt$cv_prediction[!train_grp] <- kc_cv$predict
  }
  
  result <- list(dt = dt,
                 max_plot_dist = max.plot.dist,
                 max_dist = max.dist)
  
  return(result)
  #return(dt)
}


# 2. returns UK predictions for new locations. Fits PLS to a modeling dataset using a selected number of PLS components, variogram distance fraction; creates geodatasets; predicts at new locations using UK. 
# unlike pls_uk_cv_predictions() fn above, does not create trainin/test sets over which it loops over to estimate CV predictions

# dt = annual[!validation_idx,]
# cov_loc_new = annual[validation_idx,]
# y_name = pls_cv_names[i]
# cov_names. = cov_names_log
# pls_comp = pls_components 
# variogram_dist_fract = pls_variogram_dist


uk_predictions <- function ( 
  #data used to build PLS model and for kriging
  dt = annual_train_test,
  #df w/ location & covariate info for new location where predictions should be made
  cov_loc_new = cov_act,
  # for PLS model/kriging
  y_name = "log_ufp",
  cov_names. = cov_names,
  pls_comp, 
  variogram_dist_fract # = t$cv_table$Variogram_Distance_Fraction
) {
  
  dt <- rename(dt, y_name = y_name)
  
  # #trends
  pls_comp_names <- paste0("Comp", 1:pls_comp)
  
  ##################################### fit PLS & extract scores #####################################
  # fit model to all data
  pls_model <- plsr(y_name ~.,
                    data=dt[,c("y_name", cov_names.)], 
                    ncomp = pls_comp,
                    scale=T)
  
  # extract scores for UK
  scores_dt <- scores(pls_model)[,c(1:pls_comp)] %>%
    as.data.frame()
  
  scores_new <- predict(object = pls_model,
                        newdata = cov_loc_new,
                        ncomp = 1:pls_comp,
                        type = "score") %>%
    as.data.frame()
  
  # take out spaces/dots in names
  names(scores_dt) <- pls_comp_names
  names(scores_new) <- pls_comp_names
  
  ##################################### create geodatasets & other UK inputs #####################################
  
  # add site_id, ufp & lat/long
  scores_loc_dt <- cbind(dt, scores_dt)  
  
  scores_loc_new <- cbind(cov_loc_new, scores_new)
  
  # create geodatasets
  geo_dt <- as.geodata(scores_loc_dt, 
                       coords.col = c("lambert_x", "lambert_y"), 
                       data.col = "y_name", 
                       covar.col = pls_comp_names)
  
  geo_new <- as.geodata(scores_loc_new,
                        coords.col = c("lambert_x", "lambert_y"),
                        covar.col = pls_comp_names)
  
  # trend
  cv_cov_trend <- as.formula(paste0("~ ", paste0(pls_comp_names, collapse = " + " )))
  
  trend_dt <- trend.spatial(trend = cv_cov_trend,
                            geodata = geo_dt)
  
  trend_new <- trend.spatial(trend = cv_cov_trend,
                             geodata = geo_new)
  
  # residual model
  max.dist <- summary(geo_dt)$distances.summary[["max"]]
  max.plot.dist <- max.dist*variogram_dist_fract
  
  brk_pt <- 1000
  by1_pt <- 300
  by2_pt <- 1000
  
  ##Empirical Variogram
  variog_dt <- variog(geo_dt,
                      uvec=c(seq(0, brk_pt, by = by1_pt), seq((brk_pt + by2_pt), max.plot.dist, by= by2_pt)),
                      #UK
                      trend = cv_cov_trend,
                      #messages = F
                      )
  
  #estimate range & sill values. using WLS and an exponential fit
  wls_ests <- variofit(vario = variog_dt, cov.model = "exp", messages = F)
  
  # ols: equal; wls: npairs
  resid_model <- variofit(vario = variog_dt, 
                          ini=wls_ests, 
                          cov.model = "exp",
                          weights = "npairs", 
                          messages = F)  
  
  
  # partial sill: sigma sq
  # range: phi, 
  # nugget: tau sq
  resid_model.s <- summary(resid_model)
  
  ## residual model parameters
  residual_model_table <- data.frame(
    #Method = c("OLS"),
    Partial_Sill = resid_model.s$estimated.pars[["sigmasq"]],
    Range_m = resid_model.s$estimated.pars[["phi"]],
    Nugget = resid_model.s$estimated.pars[["tausq"]]
  )  
  
  ############################################# predict at participant homes #############################################
              # could also use likfit(here)
  uk_new <- krige.conv(geo_dt, #coords = geo_dt$coords, data = geo_dt$data,
                       locations = geo_new$coords,
                       krige=krige.control(type = "ok",
                                           obj.model = resid_model,
                                           trend.d= trend_dt,
                                           trend.l= trend_new))
  
  #save predictions
  scores_loc_new$uk_pred  <- uk_new$predict #%>% exp()
  
  results <- list(
    #results for prediction locations
    dt = scores_loc_new,
    # modeling locations w/ PLS scores
    pls_model = pls_model,
    #residual model
    residual_model_table = residual_model_table,
    empirical_variogram = variog_dt,
    residual_model = resid_model,
    # betas
    betas = uk_new$beta.est
    # ? need?
    #geo_dataset = geo_dt
    )
  
  
  return(results)
  
}

############################################## PLS Score distribution ##############################################

compare_pls_scores <- function(my_pls_model = pls_models[["primary_uk"]],
                                        new_prediction_sites = cov_act,
                                        new_site_label = "ACT Cohort Locations",
                                        my_title = "Distribution of PLS component scores for the ACT cohort and mobile monitoring stops"
) {
  
  scores_mm <- my_pls_model$scores[] %>% as.data.frame()  
  
  score_names <- names(scores_mm) 
  score_names <- gsub(" ", "", score_names)
  
  names(scores_mm) <- score_names
  scores_mm$Comp1
  scores_mm$site_type <- "Mobile Monitoring Stops"
  
  scores_new_locs <- predict(object = my_pls_model,
                             newdata = new_prediction_sites,
                             #ncomp = cv_pls_comp,
                             type = "score") %>%
    as.data.frame() 
  
  names(scores_new_locs) <- score_names
  scores_new_locs$site_type <- new_site_label
  
  scores_mm_new <- rbind(scores_mm, scores_new_locs)
  
  myplot <- scores_mm_new %>%
    gather("Component", "Score", contains("Comp")) %>%
    # only keep # in component name
    mutate(Component = gsub("\\D", "", Component)) %>%
    ggplot(aes(x=Score, fill = site_type)) + 
    geom_density(alpha = 0.4) + 
    facet_wrap(~Component, labeller = "label_both") + 
    labs(fill = "",
         title = my_title) + 
    theme(legend.position = "bottom")
  
  results <- list(plot = myplot,
                  scores_mm = scores_mm,
                  scores_new_locs = scores_new_locs
                  )
  
  return(results)
  
}

######################################################################################################################



