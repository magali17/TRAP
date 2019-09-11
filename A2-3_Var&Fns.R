#scrit of values & functions used in various RMarkdown scripts.


### --> create fn to add temporal variables to any dataset? 

####################################################################################
############################### global variables ####################################
####################################################################################

#data to use
myquantile <- 0.99 

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

colo.plot <- function(mydata=mm, primary.instrument, secondary.instrument) {
  
  data.wide <- mydata %>% 
    #select only values from desired instruments
    filter(instrument_id == primary.instrument | instrument_id == secondary.instrument) %>%
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
  
  #compare primary & secondary instrument agreement 
  data.wide %>%
    ggplot(aes(x= data.wide[[primary.instrument]], y= data.wide[[secondary.instrument]])) + 
    geom_point(alpha=0.3, aes(colour = route)) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", aes(fill="lm")) + 
    labs(fill="", 
         title = paste0(data.wide$variable[1]),
         subtitle = paste0("y = ", round(coef(lm1)[1], 2), " + ", round(coef(lm1)[2], 2), 
                           "x \nR2 = ", round(summary(lm1)$r.squared, 3), 
                           #"\nRMSE = ", rmse,
                           "\nmedian percent diff = ", pct.diff.median, "%",
                           "\nno. pairs = ", nrow(data.wide)
         ),
         x=primary.instrument,
         y=secondary.instrument
    )
}
