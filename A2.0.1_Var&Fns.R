#scrit of values & functions used in various RMarkdown scripts.


### --> create fn to add temporal variables to any dataset? 

####################################################################################
############################### global variables ####################################
####################################################################################

#data to use  # use all data for now
myquantile_lower <- 0.00 
myquantile_upper <- 1.00 

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

#################################### correlation plot ####################################
#fn takes mm dataset (in long format) and creates a scatterplot comparing readings from 2 collocated instruments 

colo.plot <- function(mydata=mm, 
                      primary.instrument, secondary.instrument, 
                      int.digits = 0, 
                      r2.digits = 2, 
                      rmse.digits = 0) {
  
  
  data.wide <- mydata %>% 
    #select only values from desired instruments
    filter(instrument_id %in% c(primary.instrument, secondary.instrument)) %>%
             #make wide format
             spread(instrument_id, value) %>% 
             #only look at rows where have observations for both instruments
             drop_na(primary.instrument, secondary.instrument) 
           
           data.wide <- data.wide %>%
             #calculate percent difference in estimates
             mutate(pct.diff = (data.wide[[secondary.instrument]] - data.wide[[primary.instrument]])/data.wide[[primary.instrument]]*100)
           
           
           lm1 <- lm(formula(paste(secondary.instrument, "~", primary.instrument)), 
                     data = data.wide)
           
           #percent difference between instruments
           pct.diff.median <- round(median(data.wide$pct.diff, na.rm = T), 1)
           
           #rmse
           rmse <- (data.wide[[secondary.instrument]] - data.wide[[primary.instrument]])^2 %>%
             mean() %>%
             sqrt() %>%
             round(digits = rmse.digits)
           
           #compare primary & secondary instrument agreement 
           data.wide %>%
             ggplot(aes(x= data.wide[[primary.instrument]], y= data.wide[[secondary.instrument]])) + 
             geom_point(alpha=0.3, aes(colour = route)) + 
             geom_abline(intercept = 0, slope = 1) +
             geom_smooth(method = "lm", aes(fill="lm")) + 
             labs(fill="", 
                  title = paste0(data.wide$variable[1]),
                  subtitle = paste0("y = ", round(coef(lm1)[1], int.digits), " + ", round(coef(lm1)[2], 2), 
                                    "x \nR2 = ", round(summary(lm1)$r.squared, r2.digits), 
                                    "\nRMSE = ", rmse,
                                    #"\nmedian percent diff = ", pct.diff.median, "%",
                                    "\nno. pairs = ", nrow(data.wide)
                  ),
                  x=primary.instrument,
                  y=secondary.instrument
             )
  
  
}

#################################### correlation plot Wide format ####################################

# same but for data in wide format 
colo.plot.wide.data <- function(data.wide=mm.wide, 
                      x.variable, y.variable, 
                      int.digits = 0, 
                      r2.digits = 2, 
                      rmse.digits = 0) {
  
  data.wide <- data.wide %>% 
             #only look at rows where have observations for both instruments
             drop_na(x.variable, y.variable) 
           
           data.wide <- data.wide %>%
             #as.data.frame() %>%
             #calculate percent difference in estimates
             mutate(pct.diff = (data.wide[[y.variable]] - data.wide[[x.variable]])/data.wide[[x.variable]]*100)
           
           
           lm1 <- lm(formula(paste(y.variable, "~", x.variable)), 
                     data = data.wide)
           
           #percent difference between instruments
           pct.diff.median <- round(median(data.wide$pct.diff, na.rm = T), 1)
           
           #rmse
           rmse <- (data.wide[[y.variable]] - data.wide[[x.variable]])^2 %>%
             mean() %>%
             sqrt() %>%
             round(digits = rmse.digits)
           
           #compare primary & secondary instrument agreement 
           data.wide %>%
             ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
             geom_point(alpha=0.3, aes(colour = route)) + 
             geom_abline(intercept = 0, slope = 1) +
             geom_smooth(method = "lm", aes(fill="lm")) + 
             labs(fill="", 
                  title = paste0(data.wide$variable[1]),
                  subtitle = paste0("y = ", round(coef(lm1)[1], int.digits), " + ", round(coef(lm1)[2], 2), 
                                    "x \nR2 = ", round(summary(lm1)$r.squared, r2.digits), 
                                    "\nRMSE = ", rmse,
                                    #"\nmedian percent diff = ", pct.diff.median, "%",
                                    "\nno. pairs = ", nrow(data.wide)
                  ),
                  x=x.variable,
                  y=y.variable
             )
           
           
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
  fall <- "-09-22" #usually fall starts on 22nd, sometimes on 23nd 
 
  mydata <- data %>% 
    rename(
      mydate = date.var
    ) %>%
    mutate(
      hour = hour(mydate),
      time_of_day = factor(ifelse(hour %in% early_am., "early_am",
                                ifelse(hour %in% am., "am",
                                       ifelse(hour %in% noon., "noon",
                                              ifelse(hour %in% evening., "evening", "night")))),
                         levels= c("early_am", "am", "noon", "evening", "night")),
    day = wday(mydate, label = T, abbr = T),
    time_of_week = factor(ifelse(wday(mydate) %in% c(1, 7), "weekend", "weekday")),
    month = month(mydate, label = T, abbr = T),
    season = ifelse((mydate >= ymd(paste0((year(mydate)-1), winter)) & mydate < ymd(paste0(year(mydate), spring))) |
                       mydate >= ymd(paste0(year(mydate), winter)), "winter",
                     ifelse(mydate >= ymd(paste0(year(mydate), spring)) &
                              mydate < ymd(paste0(year(mydate), summer)), "spring",
                            ifelse(mydate >= ymd(paste0(year(mydate), summer)) &
                                     mydate < ymd(paste0(year(mydate), fall)), "summer", 
                                   ifelse( mydate >= ymd(paste0(year(mydate), fall)) &
                                             mydate < ymd(paste0(year(mydate), winter)), "fall", 
                                           NA))))
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


