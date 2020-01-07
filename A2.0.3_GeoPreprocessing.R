pacman::p_load(tidyverse)  
source("A2.0.3_functions_covariate_preprocessing_regional.R")

# 1. prep dataset for covariate.preprocess() function
#mobile monitoring geocovariates
cov_mm0 <- read.csv(file.path("Data", "Aim 2", "Geocovariates", "dr0311_mobile_locations.txt")) %>%
  #drop columns if any NAs
  select_if(~!any(is.na(.))) %>%
  select(
    #don't need this
    -contains("region"),
    ) %>%
  mutate(
    # --> correct setting???
    site_type = "FIXED"
  )

#save mm location info
locations_mm <- cov_mm0 %>%
  select(
    site_id = native_id,
    latitude:lambert_y
    )

cov_mm0 <- cov_mm0 %>%
  select(-c(location_id:msa, latitude:lambert_y))

#  fake ACT cohort geocovariate dataset so names are the same across datasets
cov_act_fake0 <- cov_mm0 %>%
  slice(1:100) %>%
  mutate(
    # --> ??? Prediction sites??
    site_type = "P"
  )

# # save ACT cohort location info
# act_locations <- cov_act0 %>%
#   select(
#     site_id = native_id,
#     latitude:lambert_y
#   )
# 
# cov_act0 <- cov_act0 %>%
#   select(-c(location_id:msa, latitude:lambert_y))


# 2. clean up geocovariates using lab protocol/code. 
## returns log-transformed proximity variables, drops old land use ("lu") variables, 
cov_preprocessed <- covariate.preprocess(covars.mon = cov_mm0, 
                                         covars.sub = cov_act_fake0, 
                                         region = "NW")
 
# 3. drop variables we won't be using in this specific analysis 
cov_mm <- cov_preprocessed$covars.mon %>%
  select(
    #drop pop estimates for past years 
    #-contains("pop_"), -contains("pop90_"),
    # drop AP predictions (these are based on other covariate models)
    -contains("em_"),  -contains("no2_")) 

# add site info back in 
cov_mm <- cbind(locations_mm, cov_mm)

cov_act_fake <-cov_preprocessed$covars.sub %>%
  select(
    -contains("em_"),  -contains("no2_")
    )

#cov_act <- cbind(locations_act, cov_act)




# 4. save datasets
# saveRDS(cov_mm,file = file.path("Data", "Aim 2", "Geocovariates", "cov_mm_preprocessed.rda"))
# saveRDS(cov_act_fake,file = file.path("Data", "Aim 2", "Geocovariates", "cov_act_fake_preprocessed.rda"))

View(cov_mm)
