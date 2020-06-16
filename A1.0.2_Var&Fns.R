#library(tidyverse)

###################################################################################
################################ VARIABLES ######################################## 
###################################################################################
images_path0 <- file.path("Output", "Aim 1", "Images")
tables_path0 <- file.path("Output", "Aim 1", "Tables")

#minimum percent of months required for us to consider a prediction "valid"
min.pct <- 0.95

my.no2.units <- 10  #ppb
my.pm25.units <- 1 #ug/m3 

model.digits <- 2 

###################################################################################
################################ FUNCTIONS ######################################## 
###################################################################################
### NOT USING??
# function calculates summary statististics for a table

# dt <- dem.w %>%
#   # only keep individuals included in Model 2
#   drop_na(m2_variables) %>%
#   #1st year of enrollment (baseline)
#   filter(enrollment_yr == 1)


summarize_variable <- function(dt, var, var_label, 
                               #group_var = "anydementia",
                               calc_summary,
                               round.var = 2
                               ) {
  # dt = data
  # var = "birth_cohort"
  # var_label = "Birth Cohort"
  # calc_summary = "n_perc"  # "mean_sd", "median_iqr
  
    if(calc_summary == "n_perc") {
      
    dt %>%
        #group_by(anydementia) %>%
        rename(var = var,
               #group_var = group_var
               ) %>%
        drop_na(var) %>%
        # add grouping var to other already existing grouping vars (e.g., anydementia)
        group_by(var, add=TRUE) %>%
        dplyr::summarize(
          N = nrow(.),
          n = n(),
          prop = round(n/N, round.var)
          )  %>%
        mutate(
          summary = paste0(n, " (", prop*100, "%)")
          ) %>%
        select(var, 
               #group_var, 
               summary) %>%
        #spread(group_var, summary) %>%
         ungroup() #%>%
         #add_row(var = var_label, .before = 1)

      }
    
    ## --> add mean_sd, median_iqr
    
    
  
  #return(df)
  
  
}


# dem.w %>%
#   # only keep individuals included in Model 2
#   drop_na(m2_variables) %>%
#   #1st year of enrollment (baseline)
#   filter(enrollment_yr == 1) %>%
#   group_by(anydementia) %>%
#   summarize_variable(dt=., var = "birth_cohort", var_label = "Birth Cohort", calc_summary =  "n_perc") %>%
#   spread(anydementia, summary) %>%
#   add_row(var = var_label, .before = 1)
# 
# 
# unique(dem.w$birth_cohort)







#function returns Table 1 summary statistics for a given dataset, with a column name describing group 


t1.fn <- function(data, 
                  #description variables
                  column.name = "") {  
 
  #pacman::p_load(kableExtra)
  
  round.var <- 0
  

  t1 <- data %>%
    dplyr::summarize(
      N = n(),
      N_perc = round(N/nrow(.)*100, round.var),
      follow_up_yrs_mean = round(mean(fu_yrs), round.var), 
      follow_up_yrs_sd = round(sd(fu_yrs), round.var),
      
      birth_cohort1_n = sum(birth_cohort==1895, na.rm=T),
      birth_cohort1_pct = round(birth_cohort1_n/N*100, round.var),
      birth_cohort2_n = sum(birth_cohort==1910, na.rm=T),
      birth_cohort2_pct = round(birth_cohort2_n/N*100, round.var),
      birth_cohort3_n = sum(birth_cohort==1915, na.rm=T),
      birth_cohort3_pct = round(birth_cohort3_n/N*100, round.var),
      birth_cohort4_n = sum(birth_cohort==1920, na.rm=T),
      birth_cohort4_pct = round(birth_cohort4_n/N*100, round.var),
      birth_cohort5_n = sum(birth_cohort==1925, na.rm=T),
      birth_cohort5_pct = round(birth_cohort5_n/N*100, round.var),
      birth_cohort6_n = sum(birth_cohort==1930, na.rm=T),
      birth_cohort6_pct = round(birth_cohort6_n/N*100, round.var),
      birth_cohort7_n = sum(birth_cohort==1935, na.rm=T),
      birth_cohort7_pct = round(birth_cohort7_n/N*100, round.var),

      age_entry_median = round(median(age_intake), round.var),
      age_entry_iqr = round(IQR(age_intake), round.var),
      female_n = sum(!male),
      female_pct = round(female_n/N*100, round.var),
      race_known_n = sum(!is.na(race_white)),
      race_white_n = sum(race_white ==1, na.rm = T),
      race_white_pct = round(race_white_n/race_known_n*100, round.var),
      edu_known_n = sum(!is.na(degree)),
      degree_known_n = sum(!is.na(degree)),
      degree0_n = sum(degree ==0, na.rm = T),
      degree0_pct = round(degree0_n/degree_known_n*100, round.var),
      #degree==1 is for GED or HS
      degree1_n = sum(degree ==1, na.rm = T),
      degree1_pct = round(degree1_n/degree_known_n*100, round.var),
      # degree2_n = sum(degree ==2, na.rm = T),
      # degree2_pct = round(degree2_n/degree_known_n*100, 1),
      degree3_n = sum(degree ==3, na.rm = T),
      degree3_pct = round(degree3_n/degree_known_n*100, round.var),
      degree4_n = sum(degree ==4, na.rm = T),
      degree4_pct = round(degree4_n/degree_known_n*100, round.var),
      degree5_n = sum(degree ==5, na.rm = T),
      degree5_pct = round(degree5_n/degree_known_n*100, round.var),
      degree6_n = sum(degree ==6, na.rm = T),
      degree6_pct = round(degree6_n/degree_known_n*100, round.var),
      income_cat_known_n = sum(!is.na(income_cat)),
      income1_n = sum(income_cat==1, na.rm=T),
      income1_pct = round(income1_n/income_cat_known_n*100, round.var),
      income2_n = sum(income_cat==2, na.rm=T),
      income2_pct = round(income2_n/income_cat_known_n*100, round.var),
      income3_n = sum(income_cat==3, na.rm=T),
      income3_pct = round(income3_n/income_cat_known_n*100, round.var),
      income4_n = sum(income_cat==4, na.rm=T),
      income4_pct = round(income4_n/income_cat_known_n*100, round.var),
      apoe_known_n = sum(!is.na(apoe)),
      apoe_carrier_n = sum(apoe, na.rm=T),
      apoe_carrier_pct = round(apoe_carrier_n/apoe_known_n*100, round.var),
      smoke_known_n = sum(!is.na(smoke)),
      smoke_never_n = sum(smoke==0, na.rm = T),
      smoke_never_pct = round(smoke_never_n/smoke_known_n*100, round.var),
      smoke_former_n = sum(smoke==1, na.rm = T),
      smoke_former_pct = round(smoke_former_n/smoke_known_n*100, round.var),
      smoke_current_n = sum(smoke==2, na.rm = T),
      smoke_current_pct = round(smoke_current_n/smoke_known_n*100, round.var),
      exercise_regular_known_n = sum(!is.na(exercise_reg)),
      exercise_regular_n = sum(exercise_reg==1, na.rm = T),
      exercise_regular_pct = round(exercise_regular_n/exercise_regular_known_n*100, round.var),
      bmi_known_n = sum(!is.na(bmi)),
      bmi_under_n = sum(bmi==0, na.rm = T),
      bmi_under_pct = round(bmi_under_n/bmi_known_n*100, round.var),
      bmi_normal_n = sum(bmi==1, na.rm = T),
      bmi_normal_pct = round(bmi_normal_n/bmi_known_n*100, round.var),
      bmi_over_n = sum(bmi==2, na.rm = T),
      bmi_over_pct = round(bmi_over_n/bmi_known_n*100, round.var),
      bmi_obese_n = sum(bmi==3, na.rm = T),
      bmi_obese_pct = round(bmi_obese_n/bmi_known_n*100, round.var),
      hypertension_known_n = sum(!is.na(Hypertension)),
      hypertension_n = sum(Hypertension == 1, na.rm=T),
      hypertension_pct = round(hypertension_n/hypertension_known_n*100, round.var),
      diabetes_known_n = sum(!is.na(Diabetes)),
      diabetes_n = sum(Diabetes == 1, na.rm=T),
      diabetes_pct = round(diabetes_n/diabetes_known_n*100, round.var),
      cv_dis_known_n = sum(!is.na(CV_DIS)),
      cv_dis_n = sum(CV_DIS == 1, na.rm=T),
      cv_dis_pct = round(cv_dis_n/cv_dis_known_n*100, round.var),
      heart_dis_known_n = sum(!is.na(Heart_Dis)),
      heart_dis_n = sum(Heart_Dis == 1, na.rm=T),
      heart_dis_pct = round(heart_dis_n/heart_dis_known_n*100, round.var),
      
      act_cohort_known = sum(!is.na(cohort)),
      act_cohort1_n = sum(cohort == 1, na.rm=T),
      act_cohort1_pct = round(act_cohort1_n/act_cohort_known*100, round.var),
      act_cohort2_n = sum(cohort == 2, na.rm=T),
      act_cohort2_pct = round(act_cohort2_n/act_cohort_known*100, round.var),
      act_cohort3_n = sum(cohort == 3, na.rm=T),
      act_cohort3_pct = round(act_cohort3_n/act_cohort_known*100, round.var),
      
      casi_irt_mean = round(mean(casi_irt, na.rm=T), 2), 
      casi_irt_sd = round(sd(casi_irt, na.rm=T), 2),
      
    ) 
  
  t1 <- t1 %>%
    mutate(
      "Participants (n, %)" = paste0(N, " (", N_perc, "%)" ),
      "Entry age, years (median, IQR)" = paste0(age_entry_median, " (", age_entry_iqr, ")"),
      "Follow-up years (mean, SD)" = paste0(follow_up_yrs_mean, " (", follow_up_yrs_sd, ")"),
      
      # Demographics 
      "Birth Cohort (n, %)" = "",
      "1909 or earlier" =  paste0(birth_cohort1_n, " (", birth_cohort1_pct, "%)"),
      "1910-1914" =  paste0(birth_cohort2_n, " (", birth_cohort2_pct, "%)"),
      "1915-1919" =  paste0(birth_cohort3_n, " (", birth_cohort3_pct, "%)"),
      "1920-1924" =  paste0(birth_cohort4_n, " (", birth_cohort4_pct, "%)"), 
      "1925-1929" =  paste0(birth_cohort5_n, " (", birth_cohort5_pct, "%)"), 
      "1930-1934" =  paste0(birth_cohort6_n, " (", birth_cohort6_pct, "%)"), 
      "1935 or later" =  paste0(birth_cohort7_n, " (", birth_cohort7_pct, "%)"), 
      "Female (n, %)" = paste0(female_n, " (", female_pct, "%)"),
      "White (n, %)" = paste0(race_white_n, " (", race_white_pct, "%)"),
      "Education (n, %)" = "",
      "Less than High School" = paste0(degree0_n, " (", degree0_pct, "%)"),
      "High School or GED" = paste0(degree1_n, " (", degree1_pct, "%)"),
      "Bachelor's Degree" = paste0(degree3_n, " (", degree3_pct, "%)"),
      "Master's Degree" = paste0(degree4_n, " (", degree4_pct, "%)"),
      "Doctorate Degree" = paste0(degree5_n, " (", degree5_pct, "%)"),
      "Other" = paste0(degree6_n, " (", degree6_pct, "%)"),
      "Census Tract Income (n, %)" = "",
      "< $35k" = paste0(income1_n, " (", income1_pct, "%)"),
      "$35-50k" = paste0(income2_n, " (", income2_pct, "%)"),
      "$50-75k" = paste0(income3_n, " (", income3_pct, "%)"),
      "> $75k" = paste0(income4_n, " (", income4_pct, "%)"),
      
      # Health Indicators
      "Smoker (n, %)" = "",
      "Never" = paste0(smoke_never_n, " (", smoke_never_pct, "%)"),
      "Former" = paste0(smoke_former_n, " (", smoke_former_pct, "%)"),
      "Current" = paste0(smoke_current_n, " (", smoke_current_pct, "%)"),
      "Regular exercise (n, %)" = paste0(exercise_regular_n, " (", exercise_regular_pct, "%)"),
      "BMI (n, %)" = "",
      "Underweight" = paste0(bmi_under_n, " (", bmi_under_pct, "%)"),
      "Normal" = paste0(bmi_normal_n, " (", bmi_normal_pct, "%)"),
      "Overweight" = paste0(bmi_over_n, " (", bmi_over_pct, "%)"),
      "Obese" = paste0(bmi_obese_n, " (", bmi_obese_pct, "%)"),
      "Vascular Health (n, %)" = "",
      "Hypertension" = paste0(hypertension_n, " (", hypertension_pct, "%)"),
      "Diabetes" = paste0(diabetes_n, " (", diabetes_pct, "%)"),
      "Cardiovascular Disease" = paste0(cv_dis_n, " (", cv_dis_pct, "%)"),
      "Heart Disease" = paste0(heart_dis_n, " (", heart_dis_pct, "%)"),
      
      # Cognitive fn indicator
      "APOE carrier (n, %)" = paste0(apoe_carrier_n, " (", apoe_carrier_pct, "%)"),
      "Baseline CASI (mean, SD)" = paste0(casi_irt_mean, " (", casi_irt_sd, ")"),
      
      # Study Indicator
      "ACT Cohort (n, %)" = "",
      "Original" =  paste0(act_cohort1_n, " (", act_cohort1_pct, "%)"),
      "Expansion" =  paste0(act_cohort2_n, " (", act_cohort2_pct, "%)"),
      "Replacement" =  paste0(act_cohort3_n, " (", act_cohort3_pct, "%)"),
      
    ) %>%
    #get rid of repeat columns
    select(-c(N, N_perc, follow_up_yrs_mean:casi_irt_sd))  
  
  # transpose
  t1 <- t(t1) %>% 
    as.data.frame() %>% 
    # separate N & %
    #separate(col = V1, into = c("N", "pct"), sep = " ", fill = "right") %>%
    #rename_all(~paste0(column.name, "_", .)) %>%
    
    rownames_to_column(var = "Variable")  
  
  # replace NAs w/ ""
  #t1[3][is.na(t1[3])] <- ""
  
  # #rename "V1" column  
  # if(column.name != "") {
  #   names(t1) <- column.name
  #   }

  return(t1) 
  }

####################################################################################

#function returns model output: HR, 95% CI, p-value
hr.output.fn <- function(model.s = m1.s, no2.coef = "no2") {
  #model.s = m5.s
  hr <- model.s$conf.int[no2.coef, "exp(coef)"] %>% round(model.digits)  
  l95 <- model.s$conf.int[no2.coef, "lower .95"] %>% round(model.digits)  
  u95 <- model.s$conf.int[no2.coef, "upper .95"] %>% round(model.digits)  
  p <- model.s$coefficients[no2.coef, "Pr(>|z|)"] %>% round(model.digits)  
  
  #person-years used in model
  n <- model.s$n
  number_events <- model.s$nevent
  
  result <- data.frame(hr = hr,
                       lower_limit = l95,
                       upper_limit = u95,
                       p = p,
                       n = n,
                       number_events = number_events)  

  return(result)
}

#hr.output.fn(model.s = m2.s)


################################################ new models.fn ###########################################
# returns various models for diff exposure periods

# mydata = dem.w
# models = paste0("m", c(1, 2, "3a", "3b",  "4a", "4b", 5, 6))
# surv.time2 = "age_end_exposure"
# surv.event = "dementia_now"
# no2.var = "no2_10yr"
# pm25.var = "pm25_10yr"
# no2.units = my.no2.units
# pm25.units = my.pm25.units
# surv.time1 = "age_start_exposure"

models.fn <- function(mydata = dem.w,
                       models,
                      surv.time1 = "age_start_exposure",
                      surv.time2 = "age_end_exposure", 
                      surv.event = "dementia_now", 
                      no2.var = "no2_10yr",
                      pm25.var = "pm25_10yr",
                      no2.units = my.no2.units,
                      pm25.units = my.pm25.units) {
  
  #rename variables for fns
  mydata <- mydata %>%
    #want to rename categorical bmi4 as bmi later
    #select(-bmi) %>%
    rename(
      #m1 
      no2 = no2.var,
      #m2
      income = income_cat,
      edu = degree,
      #m3
      #bmi = bmi4,
      #m6
      pm25 = pm25.var
    ) %>%
    mutate_at(
      c("income",
        "edu",
        "birth_cohort",
        "cohort",
        "smoke",
        "bmi"), 
      as.factor) %>%
    mutate(
      #adjust AP units
      no2 = as.double(no2/no2.units),
      pm25 = as.double(pm25/pm25.units),
    )  
  
  #create a survival object
  s.dem <- Surv(
    time = as.numeric(unlist(mydata[surv.time1])),  
    time2 = as.numeric(unlist(mydata[surv.time2])) ,  
    event = as.numeric(unlist(mydata[surv.event])))
  
  #Model 1 (Reduced): Age (time axis), NO2 (time-varying) 
  if("m1" %in% models) {
    m1 <- coxph(s.dem ~ no2 + strata(apoe), 
          data=mydata, 
          cluster = study_id,
          #robust = T,
          weights = model_wt)  
  
    m1.s <- m1 %>% summary()
  }
  
  #Model 2 (a priori): M1 + gender, education, median household income, race, birth cohort; APOE stratification.
  if("m2" %in% models) {  
  #assuming missing values (e.g., APOE) are MCAR and doing a complete case analysis. This method can be bias if values are not MCAR.
  
    m2 <- coxph(s.dem ~ no2 + strata(apoe) + male + race_white + income + 
              edu +  birth_cohort, 
            data=mydata, 
            #robust = T, 
            # --> ? correct? need this for robust SEs
            cluster = study_id,
            weights = model_wt) 
    # 95% CIs here should use robust SEs. 95%CIs is diff when don't run coxph() w/ robust/cluster
    m2.s <- m2 %>% summary()
    }
  
  #M3 (extended): M2 + smoking + physical activity 
  if("m3a" %in% models) {
    m3a <- mydata %>%
      coxph(s.dem ~ no2 + strata(apoe) + male + race_white + income + edu + birth_cohort + 
              smoke + exercise_reg, 
            data=., 
            #robust = T,          
            cluster = study_id,
            weights = model_wt) 
    
    m3a.s <- m3a %>% summary()
    }
  
  #M3b (extended + ACT cohort): M2 + smoking + physical activity + ACT cohort
  if("m3b" %in% models) {
    m3b <-  mydata %>%
      coxph(s.dem ~ no2 + strata(apoe) + male + race_white + income + edu + birth_cohort +
              smoke + exercise_reg  + cohort,
            data=.,
            #robust = T,          
            cluster = study_id,
            weights = model_wt)

    m3b.s <- m3b %>% summary()
  }
  
  
  #Model 4a (Extended & mediation): M3 + hypertension, diabetes, CV summary, heart disease summary, BMI
  if("m4a" %in% models) {
    m4a <- mydata %>%
      coxph(s.dem ~ no2 + 
              male + edu + race_white + income + birth_cohort + strata(apoe) + 
              smoke + exercise_reg +
              Hypertension + Diabetes + CV_DIS + Heart_Dis + bmi, 
            data=., 
            #robust = T,          
            cluster = study_id,
            weights = model_wt) 
    
    m4a.s <- m4a %>% summary()
  }
  
  #Model 4b (Extended & mediation): M3 + hypertension, diabetes, CV summary, heart disease summary, BMI + CASI score
  if("m4b" %in% models) {
    m4b <- mydata %>%
      coxph(s.dem ~ no2 + 
              male + edu + race_white + income + birth_cohort + strata(apoe) + 
              smoke + exercise_reg +
              Hypertension + Diabetes + CV_DIS + Heart_Dis + bmi +
              casi_irt, 
            data=., 
            #robust = T,          
            cluster = study_id,
            weights = model_wt) 
    
    m4b.s <- m4b %>% summary()
  }
  
  
  #Model 5 (APOE interaction): M2 + NO2*APOE
  if("m5" %in% models) {
  ## create single variable for interaction term so that interaction HRs is directly interpretable #see B537 L3 # 56
  mydata <- mydata %>%
    mutate(
      no2_noapoe = (1-apoe)*no2,
      no2_apoe = apoe*no2)
  
  m5 <- mydata %>%
    coxph(s.dem ~ no2_noapoe + no2_apoe + apoe +
            male + edu + race_white + income + birth_cohort + strata(apoe),
          data=., 
          #robust = T,          
          cluster = study_id,
          weights = model_wt)
  
  m5.s <- m5 %>% summary()
  
  }
  
  #Model 6 (copollutant): M2 + PM2.5 (time-varying)
  if("m6" %in% models) {
    m6 <- mydata %>%
      coxph(s.dem ~ no2 +
              male + race_white + income + edu + birth_cohort + strata(apoe) +
              pm25,  
            data=.,
            #robust = T,          
            cluster = study_id,
            weights = model_wt
      )
    
    m6.s <- m6 %>% summary()
    
    }
  
  #raw model output for models run
  raw_model_output <- list()
  for (i in seq_along(models)) {
    #create a list w/ all models that were run
    raw_model_output[[models[i]]] <- get(models[i])
  }
  
  #HR output from model summaries 
  models.s <- paste0(models, ".s")  
  hrs <- data.frame()
  
  for(i in seq_along(models.s)) {
    if (models.s[i] != "m5.s") {
      df <- hr.output.fn(get(models.s[i]))
    }
    #if m5  
    else {
      df1 <- hr.output.fn(get(models.s[i]), no2.coef = "no2_noapoe")
      df2 <- hr.output.fn(get(models.s[i]), no2.coef = "no2_apoe")
      df <- rbind(df1, df2)
    }
    # combine all HRs
    hrs <- rbind(hrs, df)
    
  }
  
  ##add exposure period labels  
  hrs <- cbind(Exposure = substr(no2.var, 5, nchar(no2.var)),
                   hrs) 
 
  return(list(hrs= hrs,
              raw_model_output = raw_model_output,
              survival_object = s.dem
  ))
  
}


###############################################################################
#returns plot with all HRs 

# example code: https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/ 

############## old plot FN - DELETE? ##############  
hr.plot <- function(dt,
                    outcome_var,
                    title_pollutant = "NO2 (10 ppb)"
                    ) {
   
  
  p <- dt %>% 
    ggplot(aes(x=Description, 
               y=hr, ymin=lower_limit, ymax=upper_limit,
               col=Exposure, linetype = grepl("2", Model))
    ) + 
    geom_pointrange() + 
    geom_errorbar() +
    geom_hline(yintercept = 1, linetype=2) + 
    labs(y= "Hazard Ratio (95% CI)",
         x = "Model Description",
         title = paste0(title_pollutant, " hazard ratios for ", outcome_var),
         linetype = "M2 Covariates"
    ) + 
    theme(
      legend.position = "bottom",
      axis.ticks.y=element_blank()
    ) +  
    coord_flip()  

  #p
  
  return(p)
}


############## NEW PLOTS ##############
###############################################################################
#returns HR plot for models.fn()$table_of_no2_HRs  
# example code: https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/ 

## reduced & extended models, 10 yr exposure
hr.plot.m1_m6 <- function(dt,
                          outcome.var,
                          max_hr_upper_lim = NULL
                          ) {
  
  #plot all HRs on same axis
  if(is.null(max_hr_upper_lim)) {max_hr_upper_lim <- max(dt$upper_limit)}
  
  p <- dt %>%
    #only keep models of interest
    filter(Model != "2" |
             (Model == "2" & Description == "Primary")) %>%
    #plot
    ggplot(aes(x=model_description, 
               y=hr, ymin=lower_limit, ymax=upper_limit,
               linetype = (model_description == "2. Primary")
    )) +
    geom_pointrange() + 
    geom_errorbar() +
    geom_hline(yintercept = 1, linetype=2) + 
    labs(y= "Hazard Ratio (95% CI)",
         x = "Reduced & Extended Models",
         linetype = "Primary Model",
         title = paste0("NO2 (10 ppb) hazard ratios for ", outcome.var, " incidence\nusing reduced, primary and extended models"),
         subtitle = "10 yr exposure"
    )  +
    theme(legend.position = "bottom") +  
    ylim(0, max_hr_upper_lim) +
    coord_flip()  
  
  return(p)
  
}


## different exposure periods
hr.plot.diff.expo.prds <- function(dt,
                                   outcome.var,
                                   max_hr_upper_lim = NULL) {
  #plot all HRs on same axis
  if(is.null(max_hr_upper_lim)) {max_hr_upper_lim <- max(dt$upper_limit)}
  
  p <- dt %>%
    filter(Model == "2",
           grepl("Primary|Exposure", Description)
           #Description %in% c("Primary", "Different exposure period")
           ) %>%
    
    ggplot(aes(x=Exposure, 
               y=hr, ymin=lower_limit, ymax=upper_limit,
               #col=Description,
               linetype = (model_description == "2. Primary")
    )) +
    geom_pointrange() + 
    geom_errorbar() +
    geom_hline(yintercept = 1, linetype=2) + 
    labs(y= "Hazard Ratio (95% CI)",
         x = "Exposure Period",
         #col = "Model Description",
         linetype = "Primary Model",
         title = paste0("NO2 (10 ppb) hazard ratios for ", outcome.var, " incidence\nusing different exposure periods"),
         subtitle = "Model 2"
    )  +
    theme(legend.position = "bottom") +  
    ylim(0, max_hr_upper_lim) +
    coord_flip()  
  
  return(p)
  
}

# other sensitivity analyse 
hr.plot.other.models <- function(dt,
                                 outcome.var,
                                 max_hr_upper_lim = NULL) {
   
  #plot all HRs on same axis
  if(is.null(max_hr_upper_lim)) {max_hr_upper_lim <- max(dt$upper_limit)}
  
  p = dt %>%
    filter(Model == "2",
           !grepl("Exposure", Description)
           #Description != "Different exposure period"
           ) %>%
    mutate(
      Description = relevel(as.factor(Description), ref = "Primary")
    ) %>%
    ggplot(aes(x=Description, 
               y=hr, ymin=lower_limit, ymax=upper_limit,
               #col=Description,
               linetype = (model_description == "2. Primary")
    )) +
    geom_pointrange() + 
    geom_errorbar() +
    geom_hline(yintercept = 1, linetype=2) + 
    labs(y= "Hazard Ratio (95% CI)",
         x = "Model Modification",
         #col = "Model Description",
         linetype = "Primary Model",
         title = paste0("NO2 (10 ppb) hazard ratios for ", outcome.var, " incidence\nfor other sensitivity analyses"),
         subtitle = "Model 2"
    )  +
    theme(legend.position = "bottom") +  
    ylim(0, max_hr_upper_lim) +
    coord_flip()  
  
  return(p)
  
}

######################## replace vector NAs for IPW ########################
#returns vector of most frequently used value if variabl is factor or mean if variable is numeric  

### ?? or return most frequent value for all so that don't get strange results for some integer values? 

replace.nas.fn <- function(myvector,
                           numeric) {
 
  
  myvector[is.na(myvector)] <- ifelse(numeric==1,
                                      mean(myvector, na.rm = TRUE),
                                      names(which.max(table(myvector)))
         )
  
  return(myvector)
}

##############################################################################################
#returns plot comparing HRs

comp.x.y.plot.fn <- function(data.wide, 
                                x.variable, y.variable, 
                                int.digits = 0, 
                                r2.digits = 2, 
                                rmse.digits = 0, 
                             mycolour.var = NULL) {
  
  #mycolour.var <- as.name(mycolour.var)
  
  data.wide <- data.wide %>% 
    #only look at rows where have observations for both instruments
    drop_na(x.variable, y.variable) 
  
  lm1 <- lm(formula(paste(y.variable, "~", x.variable)), 
            data = data.wide)
  
  #rmse
  rmse <- (data.wide[[y.variable]] - data.wide[[x.variable]])^2 %>%
    mean() %>%
    sqrt() %>%
    round(digits = rmse.digits)
  
  #compare primary & secondary instrument agreement 
  data.wide %>%
    ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
    geom_point(#alpha=0.3, 
               aes(colour = data.wide[[mycolour.var]])) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", aes(fill="lm")) + 
    labs(fill="", 
         title = paste0(data.wide$variable[1]),
         subtitle = paste0("y = ", round(coef(lm1)[1], int.digits), " + ", round(coef(lm1)[2], 2), 
                           "x \nR2 = ", round(summary(lm1)$r.squared, r2.digits), 
                           "\nRMSE = ", rmse,
                           "\nno. pairs = ", nrow(data.wide)
         ),
         x=x.variable,
         y=y.variable,
         colour = mycolour.var
    )
  
}

####################################################################################
 


####################################################################################

