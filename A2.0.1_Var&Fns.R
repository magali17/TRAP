#scrit of values & functions used in various RMarkdown scripts.


### --> create fn to add temporal variables to any dataset? 

####################################################################################
############################### global variables ####################################
####################################################################################

#data to use  # use all data for now
myquantile_lower <- 0.00 
myquantile_upper <- 1.00 

#sampling dates
start.date = "2019-02-22"

#temporal variables
##seasons
winter1 <- as.Date("2018-12-21")
winter2 <- as.Date("2019-12-21")
spring <- as.Date("2019-03-20")
summer <- as.Date("2019-06-21")
fall <- as.Date("2019-09-23")

##time of day
early_am <- seq(5,8)
am <- seq(9,11)
noon <- seq(12,15)
evening <- seq(16,20)
night <- c(seq(21,23), seq(0,4)) #? 0-4 in "night"?


####################################################################################
#################################### functions ####################################
####################################################################################

#################################### correlation plot ####################################
#fn takes mm dataset (in long format) and creates a scatterplot comparing readings from 2 collocated instruments 

colo.plot <- function(mydata=mm, primary.instrument, secondary.instrument, int.digits = 0, r2.digits = 2, rmse.digits = 0) {
  # primary.instrument = "PMPT_93"
  # secondary.instrument = "PMPT_4"
  
  data.wide <- mydata %>% 
    #select only values from desired instruments
    filter(instrument_id == primary.instrument | 
             instrument_id == secondary.instrument) %>%
    #make wide format
    spread(instrument_id, value) %>% 
    #only look at rows where have observations for both instruments
    drop_na(primary.instrument, secondary.instrument) 
  
  data.wide <- data.wide %>%
    #as.data.frame() %>%
    #calculate percent difference in estimates
    mutate(pct.diff = (data.wide[[secondary.instrument]] - data.wide[[primary.instrument]])/data.wide[[primary.instrument]]*100)
  
  
  lm1 <- lm(formula(paste(secondary.instrument, "~", primary.instrument)), 
            data = data.wide)
  
  #percent difference between instruments
  pct.diff.median <- round(median(data.wide$pct.diff, na.rm = T), 1)
  
  #rmse
  rmse <- (data.wide[[secondary.instrument]] - data.wide[[primary.instrument]])^2 %>%
    mean() %>%
    sqrt() %>%
    round(digits = rmse.digits)
  
  #compare primary & secondary instrument agreement 
  data.wide %>%
    ggplot(aes(x= data.wide[[primary.instrument]], y= data.wide[[secondary.instrument]])) + 
    geom_point(alpha=0.3, aes(colour = route)) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", aes(fill="lm")) + 
    labs(fill="", 
         title = paste0(data.wide$variable[1]),
         subtitle = paste0("y = ", round(coef(lm1)[1], int.digits), " + ", round(coef(lm1)[2], 2), 
                           "x \nR2 = ", round(summary(lm1)$r.squared, r2.digits), 
                           "\nRMSE = ", rmse,
                           #"\nmedian percent diff = ", pct.diff.median, "%",
                           "\nno. pairs = ", nrow(data.wide)
         ),
         x=primary.instrument,
         y=secondary.instrument
    )
}


#################################### ERROR: temporal variables ####################################
# function to add temporal variables to any dataset: time of day, day, time of week, month, season

### ERROR. from A3BH2001.Rmd
#### ---> Make temporal function
# add.temporal.variables <- function(data=mydata,
#                                    hour.var="hour",
#                                    date.var = "date",
#                                    early_am. = early_am, am.=am, noon.=noon, evening.=evening, night.=night
# ){
#   
#   data=bh
#   hour.var="hour"
#   hour.var <- data %>% select(hour.var) #%>% as.vector()
#   
#   date.var = "date"
#   date.var <- data %>% select(date.var) #%>% as.character() %>% as.Date() #%>% as.vector() # %>% as.Date() #%>% format("%Y-%m-%d")
#   
#   
#   early_am. = early_am
#   am.=am
#   noon.=noon
#   evening.=evening
#   night.=night
#   
#   # hour.var <- as.name(hour.var)
#   # date.var <- as.name(date.var)
#   
#   mydata <- data %>% mutate( 
#     time_of_day = factor(ifelse(hour.var %in% early_am., "early_am",
#                                 ifelse(hour.var %in% am., "am",
#                                        ifelse(hour.var %in% noon., "noon",
#                                               ifelse(hour.var %in% evening., "evening", "night")))),
#                          levels= c("early_am", "am", "noon", "evening", "night")),
#     day = factor(format(date.var, "%a"), 
#                  levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
#     time_of_week = factor(ifelse(day =="Sat" | day == "Sun", "weekend", "weekday")),
#     month = factor(format(date.var, "%b"), 
#                    levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
#     season = factor(ifelse((date.var >= winter1 & date.var < spring) | date.var >= winter2, "winter",
#                            ifelse(date.var >= spring & date.var < summer, "spring",
#                                   ifelse(date.var >= summer & date.var < fall, "summer", "fall"))),
#                     levels = c("spring", "summer", "fall", "winter"))
#   )
#   
#   return(mydata)
#   
# }
# 
# test <- add.temporal.variables(data=bh)
