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


######################## basic calculations ######################## 
#wraps text to fit ggplot
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
  }

#returns MSE
mse <- function(obs, pred){
  mean((obs - pred)^2)
}


rmse <- function(obs, pred){
  sqrt(mean((obs - pred)^2))
  
}
  
#returns MSE-based R2
r2_mse_based <- function(obs, pred) {
  mse.est <- mse(obs, pred)
  r2 <- 1- mse.est/mean((obs - mean(obs))^2)
  max(0, r2)
}  

#returns table of distribution of a variable
distribution.table <- function(dt,
                               var.string = "ptrak_pt_cm3",
                               round.int = 0) {
  #dt=ufp %>% ungroup()
  #round.int = 1
  
  t <- dt %>%
    dplyr::rename(var = var.string) %>%
    dplyr::summarize(
      N = n(),
      mean_sd =  qwraps2::mean_sd (var, digits = round.int, na_rm = T, denote_sd = "paren"),
      median_iqr =  qwraps2::median_iqr(var, digits = round.int, na_rm = T, ),
      min = round(min(var), round.int),
      max = round(max(var), round.int)
      ) #%>%
    #select(-round.int)
  
  return(t)
}

############################################ lasso ############################################ 

# dt = annual_ufp_and_cov
# y_name = "log_ufp"
# x_names = cov_names
# family. = "gaussian"
# lambda. = ""

# dt = df
# x = cov_names
# y_name = "high_variability"
# family. = "binomial"


#load library for lasso
pacman::p_load(glmnet)

lasso_fn <- function(dt, x_names, y_name, family. = "gaussian", lambda. = "") {
  
  
  x <- model.matrix(as.formula(paste(y_name, "~", 
                                     paste(x_names, collapse = " + "))
                               ), dt)[, -1]
  
  #replace y "name" w/ actual data
  y <- dt[[y_name]]   
  
  #select lambda through CV if not supplied
  if(lambda. == ""){
    cv.out <- cv.glmnet(x = x,
                        y = y, 
                        alpha=1, 
                        family= family., 
                        standardize=T)
    
    lambda. <- cv.out$lambda.min
  }
  
  # run Lasso
  lasso.m <- glmnet(x = x,
                    y = y, 
                    alpha = 1, 
                    family= family.,  
                    standardize = T)
  
  #save coefficient estimates
  lasso_coef <- predict(lasso.m, 
                        type= "coefficients",  
                        s= lambda.)[1:(ncol(x)+1),] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(cov = rowname,
           coef = ".") %>%
    #keep coefficients that are not 0 or intercept values
    filter(coef != 0,
           cov != "(Intercept)")

  
  results <- list(results = lasso_coef,
                  lambda = lambda.
                  )
  
  return(results)
  
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
  
  return(dt)
}

################################# correlation plot Wide format ####################################

#fn takes dataset (in wide format) and creates a scatterplot comparing readings from 2 collocated instruments 
colo.plot <- function(data.wide=mm.wide, 
                      x.variable, x.label = "",
                      y.variable, y.label = "",
                      col.by = "",
                      mytitle = "", title_width = 60,
                      mysubtitle = NULL,
                      mycaption = NULL,
                      coef_digits = 0, 
                      r2.digits = 2, 
                      rmse.digits = 0) {
  # x.variable = "mean_s_tow2" 
  # y.variable = "LUR_mean_s_tow2"
  # x.label = ""
  # y.label = ""
  # col.by = ""
  # mytitle = ""
  # coef_digits = 0
  # r2.digits = 2
  # rmse.digits = 0
  
  #if label is left blank, use variable name
  if(x.label == ""){x.label <- x.variable}
  if(y.label == ""){y.label <- y.variable}
  
  data.wide <- data.wide %>% drop_na(x.variable, y.variable)  
           
           lm1 <- lm(formula(paste(y.variable, "~", x.variable)), data = data.wide)
           
           #rmse
           rmse <- rmse(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>% 
             round(digits = rmse.digits)
           
           r2 <- r2_mse_based(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>%
             round(r2.digits)
           
           fit.info <- paste0("y = ", round(coef(lm1)[1], coef_digits), " + ", round(coef(lm1)[2], coef_digits), 
                              "x \nR2 = ", r2, #round(summary(lm1)$r.squared, r2.digits), 
                              "\nRMSE = ", rmse,
                              "\nno. pairs = ", nrow(data.wide))
           #compare  
           p <- data.wide %>%
             ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
             geom_point(alpha=0.3, aes(col = data.wide[[col.by]]
                                       )) + 
             geom_abline(intercept = 0, slope = 1) +
             #geom_smooth(aes(fill="loess")) + 
             geom_smooth(method = "lm", aes(fill="LS")) + 
             labs(title = wrapper(mytitle, width = title_width),
                  subtitle = mysubtitle,
                  caption = mycaption,
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
                           paste(x_names, sep = " + ")
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


####################################################################################################


