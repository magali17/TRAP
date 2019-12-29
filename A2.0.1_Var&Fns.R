#scrit of values & functions used in various RMarkdown scripts.


####################################################################################
############################### global variables ####################################
####################################################################################

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

no.missing.values <- function(var){
  result <- sum(!is.na(var)) == length(var)
  return(result)
}

varies.enough <- function(var, threshold = 0.2){
  #don't use '<=' or '>=' b/c will result in "TRUE" for a constant (e.g., all 0s)
  result <- min(var) < mean(var)*(1 - threshold) |
    max(var) > mean(var)*(1 + threshold)
  return(result)
}

### --> ? what to use as "outlier" definition 
few.outliers <- function(var, 
                         z_outlier = 5, 
                         max_prop_outliers = 0.02
){
  
  z_score <- (var - mean(var))/sd(var)
  num_outliers <- sum(abs(z_score) > z_outlier)
  prop_outliers <-  num_outliers/ length(var)
  result <- prop_outliers <= max_prop_outliers
  
  return(result)
}
## --> ? increase lower max_prop_outliers so fewer variables dropped? 
few.outliers.iqr <- function(var,
                             max_prop_outliers = 0.02){
  
  normal.range <- iqr(var)*1.5
  outliers <- var > (quantile(var, 0.75) + normal.range) | 
    var < (quantile(var, 0.25) - normal.range) 
  
  result <- sum(outliers) /length(var) <= max_prop_outliers
  
  return(result)
}

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



###################### table of distribution ######################
#returns table of distribution of a variable

distribution.table <- function(dt,
                               var.string = "ptrak_pt_cm3",
                               round.int = 0
                               ) {
  #dt=ufp
  #round.int = 1
  # dt <- dt %>% group_by(season)
  
  t <- dt %>%
    dplyr::rename(var = var.string) %>%
    dplyr::summarize(
      N = n(),
      #N = sum(var),
      mean_sd =  qwraps2::mean_sd (var, digits = round.int, na_rm = T, denote_sd = "paren"),
      median_iqr =  qwraps2::median_iqr(var, digits = round.int, na_rm = T, ),
      min = round(min(var), round.int),
      max = round(max(var)), round.int) %>%
    select(-round.int)
  
  return(t)
}



################################# correlation plot Wide format ####################################

#fn takes dataset (in wide format) and creates a scatterplot comparing readings from 2 collocated instruments 
colo.plot <- function(data.wide=mm.wide, 
                      x.variable, x.label = "",
                      y.variable, y.label = "",
                      col.by = "",
                      mytitle = "",
                      int.digits = 0, 
                      r2.digits = 2, 
                      rmse.digits = 0) {
  # x.variable = "mean_s_tow2" 
  # y.variable = "LUR_mean_s_tow2"
  # x.label = ""
  # y.label = ""
  # col.by = ""
  # mytitle = ""
  # int.digits = 0
  # r2.digits = 2 
  # rmse.digits = 0
  
  #if label is left blank, use variable name
  if(x.label == ""){x.label <- x.variable}
  if(y.label == ""){y.label <- y.variable}
  
  #data.wide <- data.wide %>%drop_na(x.variable, y.variable)  
           
           lm1 <- lm(formula(paste(y.variable, "~", x.variable)), data = data.wide)
           
           #rmse
           rmse <- (data.wide[[y.variable]] - data.wide[[x.variable]])^2 %>%
             mean() %>% sqrt() %>%
             round(digits = rmse.digits)
           
           fit.info <- paste0("y = ", round(coef(lm1)[1], int.digits), " + ", round(coef(lm1)[2], 2), 
                              "x \nR2 = ", round(summary(lm1)$r.squared, r2.digits), 
                              "\nRMSE = ", rmse,
                              "\nno. pairs = ", nrow(data.wide))
           #compare  
           p <- data.wide %>%
             ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
             geom_point(alpha=0.3, aes(col = data.wide[[col.by]]
                                       )) + 
             geom_abline(intercept = 0, slope = 1) +
             geom_smooth(aes(fill="loess")) + 
             geom_smooth(method = "lm", aes(fill="LS")) + 
             labs(title = mytitle,
                  x = x.label,
                  y = y.label,
                  col = col.by,
                  fill = "fit"
                  ) +
             annotate("text", -Inf, Inf, label = fit.info, hjust = 0, vjust = 1)
           
           return(p)
           
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
  #methods.to.compare = c("mean_uw", "mean_s_tow2_tod2", "mean_s_tow2", "mean_s", "yhat_uw", "yhat_s", "yhat_s_tow2", "yhat_s_tow2_tod2", "yhat_m_day_hr")
  
  # dt <- annual.l %>%
  #   filter(site_no %in% s_tow2_tod2$site_no,
  #          !weight %in% c("m_day_hr")) %>%
  #   mutate(site_no = factor(site_no))
  # 
  # dt.range <-  s_tow2_tod2.rage 
  
  n <- length(unique(dt$site_no))
  
  p <- dt %>%
    ggplot(aes(x=factor(site_no), y= ufp,
               col=method, group=method)) +  #fill=method #had fill instead of col
    geom_pointrange(data=dt.range, 
                    position=position_dodge(width=0.75), 
                    aes(y=mean, ymin = min, ymax = max,
                        #col=method
                        ),
                    #shape= 0, #3, #95,
                    alpha=0.5
                    ) + 
    geom_point(position=position_dodge(width=0.75), aes(shape=weight)) +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(title = paste0("Site mean UFP for ", .months.sampled[1], " - ", .months.sampled[length(.months.sampled)], " (Spring - Fall), n = ", n, " sites ", add.to.title),
         y = "UFP (pt/cm3)",
         col = "Method",
         fill = "Weight"
    ) 
  
  #p
  
  return(p)
  
}
  
    # dt <- dt %>%
  #   drop_na() %>%
  #   gather(key = "method_weight", value = "ufp", methods.to.compare) %>%
  #   mutate(method = ifelse(grepl(method_weight , pattern = "yhat"), "regression", "site mean")) %>%
  #   separate(method_weight, into=c("method", "weight"), sep = "_" , 
  #            remove = F, extra = "merge") 
  # 
  # n <- length(unique(dt$site_no))
  # 
  # ufp_by_site <- dt %>%
  #   #mutate(site_no = factor(site_id)) %>%
  #   ggplot(aes(x=factor(site_no), y= ufp)) + 
  #   #geom_boxplot(aes(), alpha=0.6) + #position="dodge"
  #   geom_point(position=position_dodge(width=0.75), aes(fill=method, group=method, col=weight)) +
  #   theme(axis.text.x = element_text(angle = 90)) + 
  #   labs(title = paste0("Site mean UFP for ", .months.sampled[1], " - ", .months.sampled[length(.months.sampled)], " (Spring - Winter), n = ", n, " sites ", add.to.title),
  #        y = "UFP (pt/cm3)",
  #        col = "weight"
  #        )
  #  
  #ufp_by_site
  
  #return(ufp_by_site)
  
#}

 
 

####################################################################################################
#returns dataset with yhat (predictions) column

save.pred.fn <- function(dt, 
                         y_string,
                         x_string = "ll_a1_s01500 + m_to_airp  + elev_elevation + pop10_s15000"
                         ) {
  
  lm1 <- lm(formula(paste0(y_string, "~", x_string)), data = dt)
  
  #save predictions (yhat)
  dt$yhat = predict(lm1)
  dt$resid_yhat = residuals(lm1)
  
  names(dt)[names(dt) == "yhat"] <- paste0("LUR_", y_string)
  names(dt)[names(dt) == "resid_yhat"] <- paste0("resid_", y_string)
  
  return(dt)
  
}


####################################################################################################



