################# TO DO ##############
 
##################################
pacman::p_load(dplyr, tidyverse, Hmisc, EnvStats)    
source("A2.0.1_Var&Fns.R")

##########################################################################################
############################# 2. clean up geocovariate data ###############################
##########################################################################################
# Keller/ST Model code: https://github.com/kaufman-lab/STmodel/blob/master/functions_covariate_preprocess_regional.R
# # 2.1 exclude variables with values less than 20% of being different from most common values.
# # 2.2 exclude variables max landuse variable < 10%
# # 2.3 exclude variables with sd of cohort > 5 times sd of monitoring data
# # 2.4 exclude variables more than 2% outliers in monitor sites and ppt sites combined

# notes:
# see DOOP Table 10 for covariate descriptions
# “imp_a< >” variable is labeled as “impervious_a<radius>” in the DOOP


geo.mm0 <- read.csv(file.path("Data", "Aim 2", "Geocovariates", "dr0311_mobile_locations.txt")) %>%
  rename(site_id = native_id) 

site.info <- geo.mm0 %>% select(site_id, latitude, longitude, lambert_x, lambert_y)

t <- geo.mm0 %>%
  select(-starts_with("lu_"))

#Hmisc::describe(geo.mm0)

geo <- geo.mm0 %>% 
  # ? drop site location info
  select(-c(location_id:msa, latitude:lambert_y,
            #don't need this - it's for the national ST model
            region_nw),
  #drop pop estimates for past years; AP predictions (which are based on other covariate models); and bus predictions (these are not yet complete, according to Amanda)
  -contains("em_"),  -contains("no2_"),
         -contains("pop_"), -contains("pop90_"),
         -contains("bus"),
  #will use rlu_ variables since these are more recent 
  -starts_with("lu_"))%>%
  #drop variables if all NAs. other columns don't have NAs  
  select_if(no.missing.values) %>% 
  # drop variables w/ little variation 
  select_if(~varies.enough(., threshold = .2)) %>%
  #drop variables with many outliers (> 2% of values)
  select_if(~few.outliers(.)) 
  # ? or use iqur outlier definition?
  #select_if(~few.outliers.iqr(., max_prop_outliers = 0.02))

#name of land use variables who's max value is < 10%  
drop.lu.rlu.vars <- geo %>% 
  select(contains("lu_"), contains("rlu_")) %>%
  select_if(~max(.) < 10) %>%
  names()

# drop land use variables who's max value is < 10%  
geo <- geo %>% select(-drop.lu.rlu.vars)

## --> ? should variables like this be dropped? has to do with using mode vs mean? rlu_pasture_p00050 has all 0s, but one value of 75%? 


# --> assumption of sampling locations vs cohort variability. Amanda: It's possible that something like cropland would be more variable in the cohort than in the monitoring locations, but I think those variables would drop out for another reason.  Still, it could be useful to think through the extrapolation issue, and possibly even check the predicted PLS scores to see if any cohort locations get really high or low values, or values very different from neighbors, and then if so, double check to make sure it's not because of a really weird variable. 




#?? convert all geocovariates to numer?
#geo <- geo %>% mutate_all(as.numeric) 


geo.mm <- cbind(site.info, geo)  



# # variables excluded by exclusion criteria either: a) have all NAs, b) have all 0s (don't vary)
# # summary(geo.excluded)
# covariates.excluded <- geo.mm0 %>% 
#   # ? drop site location info
#   select(-c(location_id:msa, latitude:lambert_y)) %>%
#   select(-contains("em_"), -contains("no2_"),
#          -contains("pop_"), -contains("pop90_")
#          -contains("bus"))%>%
#   #variables w/ all NAs 
#   select_if(~!no.missing.values(.) |
#               # variables w/ little variation 
#               !varies.enough(., threshold = .2) |
#               #variables with many outliers (> 2% of values)
#               !few.outliers(.)) # |
# # ---> update: add other exclusion criteria: drop.lu.rlu.vars 



# #initial geocovariates: 881
# ncol(geo.mm0 %>% select(-c(site_id, latitude, longitude, lambert_x, lambert_y)))
# #geocovariates left: 444
# ncol(geo)


#saveRDS(geo.mm, file.path("Data", "Aim 2", "Geocovariates", "geo.mm.rda"))
