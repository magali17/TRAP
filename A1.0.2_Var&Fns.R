###################################################################################
################################ VARIABLES ######################################## 
###################################################################################

#minimum percent of months required for us to consider a prediction "valid"
min.pct <- 0.95




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

