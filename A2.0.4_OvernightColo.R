# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load packages 
pacman::p_load(dplyr, tidyverse, chron, lubridate, knitr)   

# source global variables & functions
source("A2.0.1_Var&Fns.R")
set.seed(1)


##########################################################################################
########################### 3. overnight collocations ####################################
##########################################################################################

# folders with files to be uploaded. folder should only include ptraks w/o screens. new files added to folder will automatically be uploaded
path_10w <- file.path("Data", "Aim 2", "Overnight Collocations", "10W_raw", "ptrak_noscreen")
path_bh <- file.path("Data", "Aim 2", "Overnight Collocations", "BH_raw", "ptrak_noscreen")

#all files for each AQS site in the ptrak folder are uploaded and combined   
ptrak_10w <- ptrak.bind.fn(folder_path = path_10w)
ptrak_bh <- ptrak.bind.fn(folder_path = path_bh)
ptrak <- rbind(ptrak_10w, ptrak_bh)

ptrak <- ptrak %>%
  mutate(
    #get rid of minutes & seconds
    datetime = format(ptrak$datetime, "%Y-%m-%d %H:%M")
  ) %>%
  group_by(datetime, location) %>%
  summarize(
    #estimate hourly medians
    Conc_pt_cm3 = round(median(Conc_pt_cm3))
  ) %>%
  ungroup() %>%
  mutate(
    #make datetime into POSIXct() format
    datetime = ymd_hm(datetime)
  ) %>%
  #add temporal variables
  add.temporal.variables(data = ., date.var = "datetime")  

############################ save data for quicker access ################################
#saveRDS(ptrak, file.path("Data", "Aim 2", "Overnight Collocations", "ptrak.rda"))


