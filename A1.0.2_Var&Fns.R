###################################################################################
################################ VARIABLES ######################################## 
###################################################################################

#minimum percent of months required for us to consider a prediction "valid"
min.pct <- 0.95

my.no2.units <- 10  #ppb
my.pm25.units <- 5 #ug/m3

model.digits <- 2

###################################################################################
################################ FUNCTIONS ######################################## 
###################################################################################
#function renturns Table 1 summary statistics for a given dataset, with a column name describing group 
t1.fn <- function(data=dem.bsl[dem.bsl$pollutant == "no2",], 
                  #description variables
                  column.name = "entire cohort") {  
  
  t1 <- data %>%
    dplyr::summarize(
      N = n(),
      #person_years = nrow(data),
      follow_up_yrs_mean = round(mean(fu_yrs), 1), 
      follow_up_yrs_sd = round(sd(fu_yrs), 1),
      age_entry_median = round(median(age_intake), 1),
      age_entry_iqr = round(IQR(age_intake), 1),
      male_n = sum(male),
      male_pct = round(male_n/N*100, 1),
      race_known_n = sum(!is.na(race)),
      race_nonwhite_n = sum(race !=1, na.rm = T),
      race_nonwhite_pct = round(race_nonwhite_n/race_known_n*100, 1),
      edu_known_n = sum(!is.na(degree)),
      degree_known_n = sum(!is.na(degree)),
      degree0_n = sum(degree ==0, na.rm = T),
      degree0_pct = round(degree0_n/degree_known_n*100, 1),
      degree1_n = sum(degree ==1, na.rm = T),
      degree1_pct = round(degree1_n/degree_known_n*100, 1),
      degree2_n = sum(degree ==2, na.rm = T),
      degree2_pct = round(degree2_n/degree_known_n*100, 1),
      degree3_n = sum(degree ==3, na.rm = T),
      degree3_pct = round(degree3_n/degree_known_n*100, 1),
      degree4_n = sum(degree ==4, na.rm = T),
      degree4_pct = round(degree4_n/degree_known_n*100, 1),
      degree5_n = sum(degree ==5, na.rm = T),
      degree5_pct = round(degree5_n/degree_known_n*100, 1),
      degree6_n = sum(degree ==6, na.rm = T),
      degree6_pct = round(degree6_n/degree_known_n*100, 1),
      # edu_known_n = sum(!is.na(degree)),
      # edu_beyond_hs_n = sum(degree >2, na.rm = T),
      # edu_beyond_hs_pct = round(edu_beyond_hs_n/N*100, 1),
      income_median = median(tr_med_inc_hshld, na.rm = T),
      income_iqr = round(IQR(tr_med_inc_hshld, na.rm = T), 1),
      income_cat_known_n = sum(!is.na(income_cat)),
      income1_n = sum(income_cat==1, na.rm=T),
      income1_pct = round(income1_n/income_cat_known_n*100, 1),
      income2_n = sum(income_cat==2, na.rm=T),
      income2_pct = round(income2_n/income_cat_known_n*100, 1),
      income3_n = sum(income_cat==3, na.rm=T),
      income3_pct = round(income3_n/income_cat_known_n*100, 1),
      income4_n = sum(income_cat==4, na.rm=T),
      income4_pct = round(income4_n/income_cat_known_n*100, 1),
      #income5_n = sum(income_cat==5, na.rm=T),
      #income5_pct = round(income5_n/income_cat_known_n*100, 1),
      apoe_known_n = sum(!is.na(apoe)),
      apoe_carrier_n = sum(apoe, na.rm=T),
      apoe_carrier_pct = round(apoe_carrier_n/apoe_known_n*100, 1),
      smoke_known_n = sum(!is.na(smoke)),
      smoke_never_n = sum(smoke==0, na.rm = T),
      smoke_never_pct = round(smoke_never_n/smoke_known_n*100, 1),
      smoke_former_n = sum(smoke==1, na.rm = T),
      smoke_former_pct = round(smoke_former_n/smoke_known_n*100, 1),
      smoke_current_n = sum(smoke==2, na.rm = T),
      smoke_current_pct = round(smoke_current_n/smoke_known_n*100, 1),
      # exercise_median = median(exercise, na.rm = T),
      # exercise_iqr = round(IQR(exercise, na.rm = T), 1),
      exercise_regular_known_n = sum(!is.na(exercise_reg)),
      exercise_regular_n = sum(exercise_reg==1, na.rm = T),
      exercise_regular_pct = round(exercise_regular_n/exercise_regular_known_n*100, 1),
      # bmi_median = median(bmi, na.rm = T),
      # bmi_iqr = round(IQR(bmi, na.rm = T), 1),
      #add categorical BMI
      bmi_known_n = sum(!is.na(bmi4)),
      bmi_under_n = sum(bmi4==0, na.rm = T),
      bmi_under_pct = round(bmi_under_n/bmi_known_n*100, 1),
      bmi_normal_n = sum(bmi4==1, na.rm = T),
      bmi_normal_pct = round(bmi_normal_n/bmi_known_n*100, 1),
      bmi_over_n = sum(bmi4==2, na.rm = T),
      bmi_over_pct = round(bmi_over_n/bmi_known_n*100, 1),
      bmi_obese_n = sum(bmi4==3, na.rm = T),
      bmi_obese_pct = round(bmi_obese_n/bmi_known_n*100, 1),
      hypertension_known_n = sum(!is.na(Hypertension)),
      hypertension_n = sum(Hypertension == 1, na.rm=T),
      hypertension_pct = round(hypertension_n/hypertension_known_n*100, 1),
      diabetes_known_n = sum(!is.na(Diabetes)),
      diabetes_n = sum(Diabetes == 1, na.rm=T),
      diabetes_pct = round(diabetes_n/diabetes_known_n*100, 1),
      cv_dis_known_n = sum(!is.na(CV_DIS)),
      cv_dis_n = sum(CV_DIS == 1, na.rm=T),
      cv_dis_pct = round(cv_dis_n/cv_dis_known_n*100, 1),
      heart_dis_known_n = sum(!is.na(Heart_Dis)),
      heart_dis_n = sum(Heart_Dis == 1, na.rm=T),
      heart_dis_pct = round(heart_dis_n/heart_dis_known_n*100, 1)
    ) %>%
    mutate(
      "Follow-up years (mean, SD)" = paste0(follow_up_yrs_mean, " (", follow_up_yrs_sd, ")"),
      "Entry age, years (median, IQR)" = paste0(age_entry_median, " (", age_entry_iqr, ")"),
      "Male (n, %)" = paste0(male_n, " (", male_pct, "%)"),
      "Race, nonwhite (n, %)" = paste0(race_nonwhite_n, " (", race_nonwhite_pct, "%)"),
      #"Beyond HS Education (n, %)" = paste0(edu_beyond_hs_n, " (", edu_beyond_hs_pct, "%)"),
      "Degree, None (n, %)" = paste0(degree0_n, " (", degree0_pct, "%)"),
      "Degree, GED (n, %)" = paste0(degree1_n, " (", degree1_pct, "%)"),
      "Degree, HS (n, %)" = paste0(degree2_n, " (", degree2_pct, "%)"),
      "Degree, Bachelor's (n, %)" = paste0(degree3_n, " (", degree3_pct, "%)"),
      "Degree, Master's (n, %)" = paste0(degree4_n, " (", degree4_pct, "%)"),
      "Degree, Doctorate (n, %)" = paste0(degree5_n, " (", degree5_pct, "%)"),
      "Degree, Other (n, %)" = paste0(degree6_n, " (", degree6_pct, "%)"),
      "Census Tract Income, $ (median, IQR)" = paste0(income_median, " (", income_iqr, ")"),
      "Census Tract Income, $ [Category 1] (n, %)" = paste0(income1_n, " (", income1_pct, "%)"),
      "Census Tract Income, $ [Category 2] (n, %)" = paste0(income2_n, " (", income2_pct, "%)"),
      "Census Tract Income, $ [Category 3] (n, %)" = paste0(income3_n, " (", income3_pct, "%)"),
      "Census Tract Income, $ [Category 4] (n, %)" = paste0(income4_n, " (", income4_pct, "%)"),
      #"Census Tract Income [Category 5] (n, %)" = paste0(income5_n, " (", income5_pct, "%)"),
      "APOE carrier (n, %)" = paste0(apoe_carrier_n, " (", apoe_carrier_pct, "%)"),
      "Smoke, never (n, %)" = paste0(smoke_never_n, " (", smoke_never_pct, "%)"),
      "Smoke, former (n, %)" = paste0(smoke_former_n, " (", smoke_former_pct, "%)"),
      "Smoke, current (n, %)" = paste0(smoke_current_n, " (", smoke_current_pct, "%)"),
      #"Exercise (median, IQR)" = paste0(exercise_median, " (", exercise_iqr, ")"),
      "Regular exercise (n, %)" = paste0(exercise_regular_n, " (", exercise_regular_pct, "%)"),
      #"BMI (median, IQR)" = paste0(bmi_median, " (", bmi_iqr, ")"),
      "BMI, Underweight (n, %)" = paste0(bmi_under_n, " (", bmi_under_pct, "%)"),
      "BMI, Normal (n, %)" = paste0(bmi_normal_n, " (", bmi_normal_pct, "%)"),
      "BMI, Overweight (n, %)" = paste0(bmi_over_n, " (", bmi_over_pct, "%)"),
      "BMI, Obese (n, %)" = paste0(bmi_obese_n, " (", bmi_obese_pct, "%)"),
      "Hypertension (n, %)" = paste0(hypertension_n, " (", hypertension_pct, "%)"),
      "Diabetes (n, %)" = paste0(diabetes_n, " (", diabetes_pct, "%)"),
      "Cardiovascular Dz (n, %)" = paste0(cv_dis_n, " (", cv_dis_pct, "%)"),
      "Heart Dz (n, %)" = paste0(heart_dis_n, " (", heart_dis_pct, "%)")
    ) %>%
    #get rid of repeat columns
    select(
      N, 
      #person_years,
      "Follow-up years (mean, SD)":"Heart Dz (n, %)"
    ) #%>%
  
  # transpose
  t1 <- t(t1) %>% 
    as.data.frame() 
  
  #rename "V1" column  
  names(t1) <- column.name
  
  return(t1) 
  }

# View(t1.fn())
#  
# dem.bsl %>%
#   filter(pollutant == "no2",
#          no2_median == "below") %>%
#   t1.fn(data = ., column.name = "no2 below")

####################################################################################

#function returns model output: HR, 95% CI, p-value
hr.output.fn <- function(model.s = m1.s, no2.coef = "no2") {
  #model.s = m5.s
  hr <- model.s$conf.int[no2.coef, "exp(coef)"] %>% round(model.digits) #%>% format(nsmall=model.digits) 
  l95 <- model.s$conf.int[no2.coef, "lower .95"] %>% round(model.digits) #%>% format(nsmall=model.digits)  
  u95 <- model.s$conf.int[no2.coef, "upper .95"] %>% round(model.digits) #%>% format(nsmall=model.digits)  
  p <- model.s$coefficients[no2.coef, "Pr(>|z|)"] %>% round(model.digits) #%>% format(nsmall=model.digits) 
  
  #person-years used in model
  person_years <- model.s$n
  number_events <- model.s$nevent
  
  result <- data.frame(hr = hr,
                       lower_limit = l95,
                       upper_limit = u95,
                       #ci = paste0(l95, "-", u95),
                       p = p,
                       person_years = person_years,
                       number_events = number_events)  
  
  
  return(result)
}

#hr.output.fn(model.s = m2.s)

####################################################################################
# returns raw model output & table of NO2 HRs

models.fn <- function(mydata = dem.w, 
                      surv.time2 = "age_end_exposure", 
                      surv.event = "dementia_now", 
                      outcome.text = "All-cause Dementia",
                      no2.var = "no2_10yr",
                      pm25.var = "pm25_10yr",
                      no2.units = my.no2.units,
                      pm25.units = my.pm25.units) {
  
  #rename variables for fns
  mydata <- mydata %>%
    #want to rename categorical bmi4 as bmi later
    select(-bmi) %>%
    rename(
      #m1 
      no2 = no2.var,
      #m2
      income = income_cat,
      edu = degree,
      #m3
      bmi = bmi4,
      #m6
      pm25 = pm25.var
    ) %>%
    mutate(
      #make factors
      income = factor(income),
      edu = factor(edu),
      birth_cohort = factor(birth_cohort),
      smoke = factor(smoke),
      bmi = factor(bmi),
      
      #adjust AP units
      no2 = no2/no2.units,
      pm25 = pm25/pm25.units,
    )  
  
  #create a survival object
  s.dem <- Surv(
    time = as.numeric(unlist(mydata["age_start_exposure"])),  
    time2 = as.numeric(unlist(mydata[surv.time2])) ,  
    event = as.numeric(unlist(mydata[surv.event]))
  )
  

  #Model 1 (Reduced): Age (time axis), NO2 (time-varying) 
  m1 <- mydata %>%
    coxph(s.dem ~ no2, 
          data=., 
          robust = T,
    ) 
  
  m1.s <- m1 %>% summary()
  
  #Model 2 (a priori): M1 + gender, education, median household income, race, birth cohort; APOE stratification.
  #-assuming missing values (e.g., APOE) are MCAR and doing a complete case analysis. This method can be bias if values are not MCAR.

  m2 <- mydata %>%
    coxph(s.dem ~ no2 + strata(apoe) + male + race_white + income + 
            edu +  birth_cohort, 
          #don't use edu & birth year categories b/c too few ppl w/ low degrees and early birth years, thus reference category is small/unstable? 
          data=., 
          robust = T,
          #ties = "exact" #output is weird if use this
    ) 
  
  m2.s <- m2 %>% summary()
   
  #M3 (extended): M2 + smoking + physical activity 
  m3 <- mydata %>%
    coxph(s.dem ~ no2 + 
            male + edu + race_white + income + birth_cohort + strata(apoe) + 
            smoke + exercise_reg, 
          data=., 
          robust = T,
    ) 
  
  m3.s <- m3 %>% summary()
  
  
  #Model 4 (Extended & mediation): M3 + hypertension, diabetes, CV summary, heart disease summary, BMI
  m4 <- mydata %>%
    coxph(s.dem ~ no2 + 
            male + edu + race_white + income + birth_cohort + strata(apoe) + 
            smoke + exercise_reg +
            Hypertension + Diabetes + CV_DIS + Heart_Dis + bmi, 
          data=., 
          robust = T,
    ) 
  
  m4.s <- m4 %>% summary()
  
  #Model 5 (APOE interaction): M2 + NO2*APOE
  ## create single variable for interaction term so that interaction HRs is directly interpretable #see B537 L3 # 56
  mydata <- mydata %>%
    mutate(
      no2_noapoe = (1-apoe)*no2,
      no2_apoe = apoe*no2
    )
 
  m5 <- mydata %>%
    coxph(s.dem ~ no2_noapoe + no2_apoe + apoe +
            male + edu + race_white + income + birth_cohort + strata(apoe),
          data=., 
          robust = T,
    ) 
  m5.s <- m5 %>% summary()
  
  #Model 6 (copollutant): M2 + PM2.5 (time-varying)
  m6 <- mydata %>%
    coxph(s.dem ~ no2 +
            male + race_white + income + edu + birth_cohort + strata(apoe) +
            pm25,  
          data=.,
          robust = T,
    )
  
  m6.s <- m6 %>% summary()
  
  #raw model output
  model.ouputs <- list(model1=m1.s, 
                       model2= m2.s, 
                       model3 = m3.s, 
                       model4 = m4.s, 
                       model5 = m5.s, 
                       model6 = m6.s)
  
  #dataframe w/ HR output from models
  hrs <- rbind(
    hr.output.fn(m1.s),
    hr.output.fn(m2.s),
    hr.output.fn(m3.s),
    hr.output.fn(m4.s),
    hr.output.fn(m5.s, no2.coef = "no2_noapoe"),
    hr.output.fn(m5.s, no2.coef = "no2_apoe"),
    hr.output.fn(m6.s)
  )
  
  
  no2.hrs <- cbind(
    Model = factor(c("1. Reduced", "2. Primary", "3. Extended", "4. Extended & Mediation", "5. Interaction (no APOE)", "5. Interaction (APOE)", "6. PM2.5 Adjusted"), 
                   levels = c("1. Reduced", "2. Primary", "3. Extended", "4. Extended & Mediation", "5. Interaction (no APOE)", "5. Interaction (APOE)", "6. PM2.5 Adjusted")),
    exposure = substr(no2.var, 5, nchar(no2.var)),
    hrs
    )  
  
  return(list(table_of_no2_HRs= no2.hrs,
              raw_model_output = model.ouputs
              )
         )
  
}

#models.fn() #[[1]]

###############################################################################
#returns HR plot for models.fn()$table_of_no2_HRs  

# example code: https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/ 

hr.plot <- function(mydata) {
  p <- mydata %>%
    ggplot(aes(x=exposure, 
               y=hr, ymin=lower_limit, ymax=upper_limit,
               col=exposure)) + 
    geom_pointrange() + 
    geom_errorbar() +
    geom_hline(yintercept = 1, linetype=2) + 
    labs(y= "Hazard Ratio (95% CI)",
         x = "NO2 Exposure Period",
         title = "NO2 (10 ppb) hazard ratios for models, using different exposure time periods"
         ) + 
    facet_wrap(~Model, 
               labeller = "label_both",
               #strip.position="left",
               nrow=7) +
    theme(
      legend.position = "none",
      #axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      # strip.text.y = element_text(hjust=0,vjust = 1,angle=180
      ) +  
  coord_flip() 

  return(p)
}


# rbind(no2_1yr_models[[1]],
#       no2_5yr_models[[1]],
#       no2_10yr_models[[1]],
#       #no2_20yr_models[[1]], #error, very large CI
#       no2_10yr10yrlag_models[[1]],
#       no2_10yr20yrlag_models[[1]]
#       )%>%
#   hr.plot(.)
