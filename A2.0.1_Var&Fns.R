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

makenum = function(x) {as.numeric(as.character(x))}

ONA <- function(MAdata, 
                       deltaATN=data.table(UV=0.05, 
                                           Blue = 0.05, 
                                           Green = 0.05, 
                                           Red = 0.05, 
                                           IR = 0.05)) {
  pacman::p_load("data.table", "zoo", "plyr")
  
  MAdata <- MAdata %>% as.data.table()
  
  MAdata[ , UVthresh := round_any((`ma200_uv_atn1`)-max(`ma200_uv_atn1`)-deltaATN$UV, (deltaATN$UV+0.01), ceiling) + max(`ma200_uv_atn1`), by = c("ma200_tape_posit", "runname")]
  MAdata[ , ona_uv := mean(makenum(`ma200_uv_bc1`)), by = c("ma200_tape_posit", "UVthresh", "runname")] 
  
  MAdata[ , Bluethresh := round_any((`ma200_blue_atn1`)-max(`ma200_blue_atn1`)-deltaATN$Blue, (deltaATN$Blue+0.01), ceiling) + max(`ma200_blue_atn1`), by = c("ma200_tape_posit", "runname")]    
  MAdata[ , ona_blue := mean(makenum(`ma200_blue_bc1`)), by = c("ma200_tape_posit", "Bluethresh", "runname")] 
  
  MAdata[ , Greenthresh := round_any((`ma200_green_atn1`)-max(`ma200_green_atn1`)-deltaATN$Green, (deltaATN$Green+0.01), ceiling) + max(`ma200_green_atn1`), by = c("ma200_tape_posit", "runname")]    
  MAdata[ , ona_green := mean(makenum(`ma200_green_bc1`)), by = c("ma200_tape_posit", "Greenthresh", "runname")] 
  
  MAdata[ , Redthresh := round_any((`ma200_red_atn1`)-max(`ma200_red_atn1`)-deltaATN$Red, (deltaATN$Red+0.01), ceiling) + max(`ma200_red_atn1`), by = c("ma200_tape_posit", "runname")]    
  MAdata[ , ona_red := mean(makenum(`ma200_red_bc1`)), by = c("ma200_tape_posit", "Redthresh", "runname")] 
  
  MAdata[ , IRthresh := round_any((`ma200_ir_atn1`)-max(`ma200_ir_atn1`)-deltaATN$IR, (deltaATN$IR+0.01), ceiling) + max(`ma200_ir_atn1`), by = c("ma200_tape_posit", "runname")]    
  MAdata[ , ona_ir := mean(makenum(`ma200_ir_bc1`)), by = c("ma200_tape_posit", "IRthresh", "runname")] 
  
  MAdata <- MAdata %>% as.data.frame()
  
  #creates issues later if you leave loaded
  #pacman::p_unload("data.table")
  
  return(MAdata)
  
  }


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


################################ reads raw ptrak files ################################
#returns clean ptrak files from raw files 

#data.table allows us to look for text in a line, otherwise use a row #
read.ptrak <- function(myfilepath) {
  
  library(stringi)
  
  dt <- data.table::fread(myfilepath,
                           #skip lines until see this text & make it column headers
                           skip = "MM/dd/yyyy", 
                           #keeps reading after blank row
                           fill=T) %>%
  rename(
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
    datetime,
    Conc_pt_cm3
  ) %>% 
    mutate(#look at file path to ID AQS location
           location = ifelse(stri_detect_fixed(myfilepath, "10W"), "10W", "BH"
                             )
           )
  
  return(dt)
  
  }

############################## combines multiple ptrak files ##############################

#returns single file of ptrak data
ptrak.bind.fn <- function(folder_path) {
  
  #names of files in folder
  files_list <- list.files(folder_path)
  
  #create empty df
  df <- as.data.frame(matrix(ncol=3,nrow=0))
  
  #rbind files
  for (i in 1:length(files_list)) {
    df1 <- read.ptrak(myfilepath = file.path(folder_path, files_list[i]))
    
    df <- rbind(df, df1)
  } 
  
  return(df)
  
  }
 

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
                   include_monitoring_area = FALSE, monitoring_area_alpha = 0.25,
                   include_study_area = FALSE, study_area_alpha = 0.1,
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

#################

# dt = grid_dens_expand
# color_by = "z"
# latitude_name. = "y"
# longitude_name. = "x"
# map_title = NULL

# colors the base map by a variable (3D)
map_color <- function(dt, 
                   color_by = "ufp", color_units = "pt/cm3",
                   latitude_name = "latitude", longitude_name = "longitude",
                   map_title = NULL
                   ) {
  
  #label by variable being mapped if no title is provided
  if(is.null(map_title)) {map_title <- color_by}
  
  dt <- rename(dt,
               y = color_by,
               lat = latitude_name,
               long = longitude_name) 
  
  mymap <- dt %>%
    map_base(latitude_name = "lat", longitude_name = "long") +
    # color map
    geom_point(data = dt,
                 aes(x = long, y = lat), col= "black",
                 size = 2.5) +
      geom_point(data = dt,
                 aes(x = long, y = lat, col = y),
                 alpha = 0.8, size = 2) +
      scale_color_gradient(name = color_units, low = "yellow", high = "red") +
    labs(title = map_title
         #fill = "Area"
         )
  
  #+ theme(legend.position = c(0.98, 0.01), legend.justification = c(1,0)) 
  
  return(mymap)
  
}


#################
# combines map_base() and map_color() fns. but fills by study and monitoring regions (basemap does not - works better for e.g. stat_contour() which uses fill)
map_fn <- function(dt, 
                   color_map = TRUE,
                   color_by = "ufp", color_units = "pt/cm3",
                   latitude_name = "latitude", longitude_name = "longitude",
                   map_title = NULL,
                   include_monitoring_area = TRUE, monitoring_area_alpha = 0.25,
                   include_study_area = FALSE, study_area_alpha = 0.1,
                   maptype. = "terrain", #, "toner", "toner-background", "watercolor"
                   zoom_lvl = 11
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
  
  # add study area
  if(include_study_area) {
    study_area <- readRDS(file.path("Data", "GIS", "study_area_df.rda"))
    
    mymap <- mymap + 
      geom_polygon(data = study_area, 
                aes(x = long, y = lat, group = group, 
                    fill = "Study"
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
                    fill = "Monitoring"
                    ),
                #color = 'peru',  
                #fill = 'peru', 
                alpha = monitoring_area_alpha,
                size = 0.3)
    }
  
  # color map
  if(color_map) {
    mymap <- mymap +
      geom_point(data = dt,
                 aes(x = long, y = lat), col= "black",
                 size = 2.5) +
      geom_point(data = dt,
                 aes(x = long, y = lat, col = y),
                 alpha = 0.8, size = 2,
      ) +
      scale_color_gradient(name = color_units, low = "yellow", high = "red") 
  
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

estimate_annual_avg <- function(dt,
                                var = "ptrak",
                                # model used to estimate annual averages
                                lm_x_names = c("site_id", "season", "time_of_week", "tod5")
                                ) {
  #rename variables
  dt <- dt %>%
    rename(var = var)
  
  ##########################################################################################
  #################### characterize stop-level concentrations at sites ####################
  ##########################################################################################
  
  # table of distribution
  t_stops_distribution <- dt %>% ungroup() %>%
    distribution.table(var.string = "var") %>%
    mutate(time = "Overall") %>%
    select(time, everything()) %>%
    kable(caption = "Distribution of stop-level UFP concentrations") %>%
    kable_styling()
  
  # density plot
  p_stops_density <- dt %>%
    ggplot(aes(x=var)) + 
    geom_density() + 
    labs(title = "Distribution of stop-level UFP concentrations",
         x = "UFP (pt/cm3)")
  
  # plots over time
  #by time
  p0_hour <- dt %>% 
    mutate(hour = factor(hour)) %>%
    ggplot(aes(x=hour, y=var)) + 
    geom_boxplot() +
    labs(x = "hour") + 
    labs(y="")
  
  p0_tod5 <- dt %>% 
    #mutate(hour = factor(hour)) %>%
    ggplot(aes(x=tod5, y=var)) + 
    geom_boxplot() +
    labs(x = "Time of Day (hour)",
         y = "",
         fill = "Time of day") 
  
  p0_day <- dt %>%
    ggplot(aes(x=day, y=var)) + 
    geom_boxplot() + 
    labs(y="")
  
  p0_season <- dt %>%
    ggplot(aes(x=season, y=var)) + 
    geom_boxplot() + 
    labs(y="")
  
  p_stop_concs_over_time <- ggarrange(p0_hour, p0_tod5, p0_day, p0_season, 
                                      common.legend = T, legend = "bottom") %>%
    annotate_figure(left = "UFP (pt/cm3)", 
                    fig.lab = "Stop-level UFP concentrations")
  
  ##########################################################################################
  #################################### stop sample size ####################################
  ##########################################################################################
  
  ## table of stop counts/site overall & stratified
  t_stop_count <- dt %>%
    dplyr::group_by(site_id) %>%
    # no. samples/site
    dplyr::summarize(N = n()) %>%
    # distribution of no. samples
    distribution.table(var.string = "N") %>%
    mutate(Time = "Overall") %>%
    select(Time, everything()) %>%
    kable(caption = "Number of stop samples per site (after trimming; N = No. sites sampled)") %>%
    kable_styling()
  
  ##########################################################################################
  #################################### annual averages ####################################
  ##########################################################################################
  # lm_x_names = c("site_id", "season", "time_of_week", "tod5")
  # lm_x_names <- c("site_id", "season")
  
  ## grid to save predictions
  lm_pred0 <- expand.grid(
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
  
    lm_fit0 <- lm(formula(paste0("var ~", paste(lm_x_names, collapse = " + "))), data = dt)
    
    lm_pred0$yhat <- predict(lm_fit0, lm_pred0) 
    
    # weigh temporally-specific predictions
    if("season" %in% lm_x_names) {
    lm_pred0 <- lm_pred0 %>%
      mutate(yhat = yhat*season_wt)
    }
    
    if("time_of_week" %in% lm_x_names) {
      lm_pred0 <- lm_pred0 %>%
        mutate(yhat = yhat*time_of_week_wt)
    }
    
    if("tod2" %in% lm_x_names) {
      lm_pred0 <- lm_pred0 %>%
        mutate(yhat = yhat*tod2_wt)
    }
    
    if("tod5" %in% lm_x_names) {
      lm_pred0 <- lm_pred0 %>%
        mutate(yhat = yhat*tod5_wt)
    }
    
    lm_pred <- lm_pred0 %>%
    group_by(site_id) %>%
    dplyr::summarize(yhat = sum(yhat))
  
  # table of UFP distribution
  t_annual_distribution <- lm_pred %>%
    distribution.table(var.string = "yhat") %>%
    kable(caption = "Distribution of annual average estimates") %>%
    kable_styling()
  
  ## plot of UFP distribution
  p_annual_density <- lm_pred %>%
    ggplot(aes(x=yhat)) + 
    geom_density() + 
    labs(x = "UPF (pt/cm3)",
         title = "Distribution of annual average estimates")
  
  ## --> need to add lat/long if want to plot here
  ## map of annual estimates
  # annual_map <- dt %>% map_fn(map_title = "Annual average estimates")
  
  ##########################################################################################
  #################################### results ############################################
  ##########################################################################################
  
  # list to return
  results <- list( 
    #stop-level concentrations
    t_stops_distribution = t_stops_distribution,
    p_stops_density = p_stops_density,
    p_stop_concs_over_time =  p_stop_concs_over_time,
    
    # stop sample size
    t_stop_count = t_stop_count,
    
    # annual averages
    annual_estimates = lm_pred,
    t_annual_distribution = t_annual_distribution,
    p_annual_density = p_annual_density
    
    # map estimates
    # #annual_map
  )
  
  return(results)
  
}

#################################################################################################################
#################################################################################################################


