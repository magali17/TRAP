###################################################################################
################################ VARIABLES ######################################## 
###################################################################################

#minimum percent of months required for us to consider a prediction "valid"
min.pct <- 0.95

no2.units <- 10  #ppb
pm25.units <- 5 #ug/m3

model.digits <- 2

###################################################################################
################################ FUNCTIONS ######################################## 
###################################################################################
#function renturns Table 1 summary statistics for a given dataset, with a column name describing group 
t1.fn <- function(data=dem.bsl, 
                  #description variables
                  column.name = "entire cohort") {  
  
  t1 <- data %>%
    dplyr::summarize(
      N = n(),
      #person_years = nrow(),
      age_entry_median = round(median(age_intake), 1),
      age_entry_iqr = round(IQR(age_intake), 1),
      male_n = sum(male),
      male_pct = round(male_n/N*100, 1),
      race_known_n = sum(!is.na(race)),
      race_nonwhite_n = sum(race !=1, na.rm = T),
      race_nonwhite_pct = round(race_nonwhite_n/race_known_n*100, 1),
      edu_known_n = sum(!is.na(degree)),
      edu_beyond_hs_n = sum(degree >2, na.rm = T),
      edu_beyond_hs_pct = round(edu_beyond_hs_n/N*100, 1),
      income_median = median(tr_med_inc_hshld, na.rm = T),
      income_iqr = round(IQR(tr_med_inc_hshld, na.rm = T), 1),
      apoe_known_n = sum(!is.na(apoe)),
      apoe_carrier_n = sum(apoe, na.rm=T),
      apoe_carrier_pct = round(apoe_carrier_n/apoe_known_n, 1),
      smoke_known_n = sum(!is.na(smoke)),
      smoke_never_n = sum(smoke==0, na.rm = T),
      smoke_never_pct = round(smoke_never_n/smoke_known_n*100, 1),
      smoke_former_n = sum(smoke==1, na.rm = T),
      smoke_former_pct = round(smoke_former_n/smoke_known_n*100, 1),
      smoke_current_n = sum(smoke==2, na.rm = T),
      smoke_current_pct = round(smoke_current_n/smoke_known_n*100, 1),
      exercise_median = median(exercise, na.rm = T),
      exercise_iqr = round(IQR(exercise, na.rm = T), 1),
      exercise_regular_known_n = sum(!is.na(exercise_reg)),
      exercise_regular_n = sum(exercise_reg==1, na.rm = T),
      exercise_regular_pct = round(exercise_regular_n/exercise_regular_known_n*100, 1),
      bmi_median = median(bmi, na.rm = T),
      bmi_iqr = round(IQR(bmi, na.rm = T), 1),
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
      "Entry age, years (median, IQR)" = paste0(age_entry_median, " (", age_entry_iqr, ")"),
      "Male (n, %)" = paste0(male_n, " (", male_pct, "%)"),
      "Race, nonwhite (n, %)" = paste0(race_nonwhite_n, " (", race_nonwhite_pct, "%)"),
      "Beyond HS Education (n, %)" = paste0(edu_beyond_hs_n, " (", edu_beyond_hs_pct, "%)"),
      "APOE carrier (n, %)" = paste0(apoe_carrier_n, " (", apoe_carrier_pct, "%)"),
      "Smoke, never (n, %)" = paste0(smoke_never_n, " (", smoke_never_pct, "%)"),
      "Smoke, former (n, %)" = paste0(smoke_former_n, " (", smoke_former_pct, "%)"),
      "Smoke, current (n, %)" = paste0(smoke_current_n, " (", smoke_current_pct, "%)"),
      "Exercise (median, IQR)" = paste0(exercise_median, " (", exercise_iqr, ")"),
      "Regular exercise (n, %)" = paste0(exercise_regular_n, " (", exercise_regular_pct, "%)"),
      "BMI (median, IQR)" = paste0(bmi_median, " (", bmi_iqr, ")"),
      "Hypertension (n, %)" = paste0(hypertension_n, " (", hypertension_pct, "%)"),
      "Diabetes (n, %)" = paste0(diabetes_n, " (", diabetes_pct, "%)"),
      "Cardiovascular Dz (n, %)" = paste0(cv_dis_n, " (", cv_dis_pct, "%)"),
      "Heart Dz (n, %)" = paste0(heart_dis_n, " (", heart_dis_pct, "%)")
    ) %>%
    #get rid of repeat columns
    select(
      N, 
      #person_years,
      "Entry age, years (median, IQR)":"Heart Dz (n, %)"
    ) #%>%
  #place NO2 and PM25 next to eachother 
  #arrange(group)
   
  
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
  hr <- model.s$conf.int[no2.coef, "exp(coef)"] %>% round(model.digits) %>% format(nsmall=model.digits) 
  l95 <- model.s$conf.int[no2.coef, "lower .95"] %>% round(model.digits) %>% format(nsmall=model.digits)  
  u95 <- model.s$conf.int[no2.coef, "upper .95"] %>% round(model.digits) %>% format(nsmall=model.digits)  
  p <- model.s$coefficients[no2.coef, "Pr(>|z|)"] %>% round(model.digits) %>% format(nsmall=model.digits) 
  
  #person-years used in model
  person_years <- model.s$n
  number_events <- model.s$nevent
  
  result <- data.frame(hr = hr,
                       ci = paste0(l95, "-", u95),
                       p = p,
                       person_years = person_years,
                       number_events = number_events)
  
  
  return(result)
}

#hr.output.fn(model.s = m2.s)

####################################################################################
# function returns raw model output & table of NO2 HRs

## --> FIX: M6 hr for apoe carriers is wrong.

models.fn <- function(surv.data = dem.w, 
                      surv.time2 = "age_end_exposure", 
                      surv.event = "dementia_now", 
                      outcome.text = "All-cause Dementia",
                      model.vars.data = model.vars) {
  
  #create a survival object
  s.dem <- Surv(
    time = as.numeric(unlist(surv.data["age_start_exposure"])),  
    time2 = as.numeric(unlist(surv.data[surv.time2])) ,  
    event = as.numeric(unlist(surv.data[surv.event]))
  )
  
  
  #Model 1 (Reduced): Age (time axis), NO2 (time-varying) 
  m1 <- model.vars.data %>%
    coxph(s.dem ~ no2, 
          data=., 
          robust = F,
    ) 
  
  m1.s <- m1 %>% summary()
  
  #Model 2 (a priori): M1 + gender, education, median household income, race, birth cohort; APOE stratification.
  #-assuming missing values (e.g., APOE) are MCAR and doing a complete case analysis. This method can be bias if values are not MCAR.
  
  ##### --> categorize birth cohort into larger categories - 20 yrs? 
  
  m2 <- model.vars.data %>%
    coxph(s.dem ~ no2 + strata(apoe) + male + race_white + income + 
            #factor(degree) + factor(birth_cohort), 
            edu +  birth_cohort, 
          #don't use edu & birth year categories b/c too few ppl w/ low degrees and early birth years, thus reference category is small/unstable? 
          #education + as.numeric(format(birthdt, "%Y")),
          data=., 
          robust = T,
          #ties = "exact" #output is weird if use this
    ) 
  m2.s <- m2 %>% summary()
  
  #t <- m2 %>% summary()
  #t$coefficients["no2", "exp(coef)"]
  
  
  #M3 (extended): M2 + smoking + physical activity 
  m3 <- model.vars.data %>%
    coxph(s.dem ~ no2 + 
            male + edu + race_white + income + birth_cohort + strata(apoe) + 
            smoke + exercise_reg, 
          data=., 
          robust = T,
    ) 
  m3.s <- m3 %>% summary()
  
  
  #Model 4 (Extended & mediation): M3 + hypertension, diabetes, CV summary, heart disease summary, BMI
  m4 <- model.vars.data %>%
    coxph(s.dem ~ no2 + 
            male + edu + race_white + income + birth_cohort + strata(apoe) + 
            smoke + exercise_reg +
            Hypertension + Diabetes + CV_DIS + Heart_Dis + bmi, 
          data=., 
          robust = T,
    ) 
  m4.s <- m4 %>% summary()
  
  #Model 5 (APOE interaction): M2 + NO2*APOE
  m5 <- model.vars.data %>%
    coxph(s.dem ~ no2 + 
            male + edu + race_white + income + birth_cohort + strata(apoe) +
            no2*apoe , 
          data=., 
          robust = T,
    ) 
  m5.s <- m5 %>% summary()
  
  
  #Model 6 (copollutant): M2 + PM2.5 (time-varying)
  m6 <- model.vars.data %>%
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
    hr.output.fn(m5.s),
    # wrong, have to exponentiate(no2 + no2:apoe)
    hr.output.fn(m5.s, no2.coef = "no2:apoe"),
    hr.output.fn(m6.s)
  )
  
  
  no2.hrs <- cbind(
    Model = c(c(1:4), "5_nonapoe_carriers", "5_apoe_carriers_WRONG", 6),
    hrs)
  
  return(list(
              table_of_no2_HRs= no2.hrs,
              raw_model_output = model.ouputs
              )
         )
  #return(no2.hrs)
}

# models.fn() #[[1]]

####################################################################################

