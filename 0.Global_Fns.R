############################################################################################## 
############################################ basic fns ############################################ 
############################################################################################## 
# When estimating R2 and RMSE parameters: 
# a) use estimates calculated from the best-fit line if we are interested in seeing how well our independent variables (X) predicted our outcome (Y)
# In this case a single X (or a combination of X's) is not necessarily even the same units as Y. 
# Sometimes we also look at fit around the best-fit line for things we think are more or less the same, 
# but this tells us something different because it corrects for the systematic difference between the two variables (as characterized by the intercept and slope). 
#
# b) use estimates around the 1-1 line if we would like to see how similar two things are (we think they should be more/less the same).
# We also use this to evaluate cross-validated predictions, which again we think should be more or less the same thing.


# These functions calculate RMSE and R2 estimates in my colocation plots based on the 1-1 line. 
# “obs” and “pred” are the variables being plotted on the x and y axes and not necessarily what was used to fit a line. 


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

############################################################################################## 
#returns table of distribution of a variable

# dt <- dem1 %>%   drop_na(m2_vars) 
# var.string = "exp_avg10_yr_SP"

# note var.string should not have NAs, otherwise can return wrong results 
distribution.table <- function(dt,
                               var.string = "ptrak_pt_cm3",
                               round.int = 0) {
  
  t <- dt %>%
    dplyr::rename(var = var.string) %>%
    dplyr::summarize(
      N = n(),
      Min = min(var, na.rm = T),
      Q05 = quantile(var, 0.05, na.rm = T),
      #"Mean (SD)" =  qwraps2::mean_sd (var, digits = round.int, na_rm = T, denote_sd = "paren"),
      Mean = mean(var, na.rm = T),
      SD = sd(var, na.rm = T),
      #"Median (IQR)" =  qwraps2::median_iqr(var, digits = round.int, na_rm = T, ),
      Median = median(var, na.rm = T), 
      IQR = IQR(var, na.rm = T),
      #IQR = round(IQR(var), round.int),
      Q95 = quantile(var, 0.95, na.rm = T),
      Max = max(var, na.rm = T)
    ) %>%
    # round values
    mutate_if(is.numeric, ~round(., round.int)) %>%
    # add commas separating thousands 
    mutate_if(is.numeric, ~prettyNum(., big.mark = ",")) #%>%
  
  return(t)
}

############################################################################################## 
#wraps text to fit ggplot
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

############################################################################################## 
#returns the minimum and maximum value of two vectors. for plotting purposes when wanting a perfectly diagonal 1-1 line (square plotting area).
plot_range <- function(vector1 = uk_decompose_l$primary_uk, 
                       vector2 = uk_decompose_l$regression_prediction){
  min <- min(min(vector1), min(vector2))
  max <- max(max(vector1), max(vector2))
  
  result <- data.frame(min = min, 
                       max = max)
  
  return(result)
  
}

################################# correlation plot Wide format ####################################

# data.wide <- uk_decompose %>% filter(study_area) 
# y.variable = my_analysis
# x.variable = "regression_prediction"
# x.label = ""
# y.label = ""
# col.by = ""
# mytitle = ""
# coef_digits = 0
# r2.digits = 2
# rmse.digits = 2
# convert_rmse_r2_to_native_scale=T

colo.plot <- function(data.wide=mm.wide, 
                      x.variable, x.label = "",
                      y.variable, y.label = "",
                      col.by = "",
                      alpha_value = 0.3,
                      mytitle = "", title_width = 60,
                      mysubtitle = NULL,
                      mycaption = NULL,
                      int_digits = 0,
                      slope_digits = 2,
                      r2.digits = 2, 
                      rmse.digits = 0,
                      convert_rmse_r2_to_native_scale=F
                      ) {
  
  #if label is left blank, use variable name
  if(x.label == ""){x.label <- x.variable}
  if(y.label == ""){y.label <- y.variable}
  
  data.wide <- data.wide %>% drop_na(x.variable, y.variable)  
  
  lm1 <- lm(formula(paste(y.variable, "~", x.variable)), data = data.wide)
  #summary(lm1)
  
  
  ################################################
  ## ?? need this fns inside this fn???
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
  
  ################################################ 
  
  
  #rmse
  if (convert_rmse_r2_to_native_scale==T) {
    rmse <- rmse(obs = exp(data.wide[[x.variable]]), pred = exp(data.wide[[y.variable]])) %>% 
      round(digits = rmse.digits)
    
    r2 <- r2_mse_based(obs = exp(data.wide[[x.variable]]), pred = exp(data.wide[[y.variable]])) %>%
      round(r2.digits)
    } 
  
  else {
    rmse <- rmse(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>% 
    round(digits = rmse.digits)
    
    r2 <- r2_mse_based(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>%
      round(r2.digits)
  }
  
  
  
  fit.info <- paste0("y = ", round(coef(lm1)[1], int_digits), " + ", round(coef(lm1)[2], slope_digits), 
                     "x \nR2 = ", r2,  
                     "\nRMSE = ", rmse,
                     "\nNo. Pairs = ", nrow(data.wide))
  
  max_plot <- max(max(data.wide[[x.variable]]), max(data.wide[[y.variable]]) )
  min_plot <- min(min(data.wide[[x.variable]]), min(data.wide[[y.variable]]) )
    
  #compare  
  p <- data.wide %>%
    ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
    geom_point(alpha=alpha_value, aes(col = data.wide[[col.by]]
    )) + 
    coord_fixed() +
    geom_abline(intercept = 0, slope = 1) +
    #geom_smooth(aes(fill="loess")) +
    geom_smooth(method = "lm", aes(fill="LS")) +
    xlim(min_plot, max_plot) +
    ylim(min_plot, max_plot) +
    labs(title = wrapper(mytitle, width = title_width),
         subtitle = mysubtitle,
         caption = mycaption,
         x = x.label,
         y = y.label,
         col = col.by,
         fill = "fit") +
    annotate("text", -Inf, Inf, label = fit.info, hjust = 0, vjust = 1)

  return(p)
  
}

# issues/notes: 
# coord_fixed() and theme(aspect.ration =1) don't do anything??


############################################################################################## 
############################################ Add Seasons ############################################ 
# adds sesason to a given dataset with a date variable. Uses typical equinox/solstice dates

add_season <- function(dt, .date_var) {
  
  pacman::p_load(lubridate)
  
  # dt <- aqs_daily
  # .date_var <- "Date.Local"
  
  winter <- "-12-21" #usually winter starts on 21st, sometimes on 22nd 
  spring <- "-03-20"
  summer <- "-06-21" #usually summer starts on 21st, sometimes on 22nd 
  fall <- "-09-23" #usually fall starts on 22nd, sometimes on 23nd. Using 23rd for 2019 mobile monitoring campaign 
  
  dt <- dt %>%
    rename(date_var = .date_var) %>%
    #make sure variable is in date format
    mutate(date_var = as.Date(date_var),
           season = factor(ifelse((date_var >= ymd(paste0((year(date_var)-1), winter)) & date_var < ymd(paste0(year(date_var), spring))) |
                                    date_var >= ymd(paste0(year(date_var), winter)), "winter",
                                  ifelse(date_var >= ymd(paste0(year(date_var), spring)) &
                                           date_var < ymd(paste0(year(date_var), summer)), "spring",
                                         ifelse(date_var >= ymd(paste0(year(date_var), summer)) &
                                                  date_var < ymd(paste0(year(date_var), fall)), "summer", 
                                                ifelse( date_var >= ymd(paste0(year(date_var), fall)) &
                                                          date_var < ymd(paste0(year(date_var), winter)), "fall", 
                                                        NA)))), 
                           levels = c("spring", "summer", "fall", "winter"))
    )
  
  #change time variable back to what it was originally
  names(dt)[names(dt) %in% "date_var"] <- .date_var
  
  return(dt)
  
}


############################################################################################## 
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

############################################################################################## 
