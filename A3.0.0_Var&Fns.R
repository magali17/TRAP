


###############################################################################################
# returns plots w/ linear fit, R2 and RMSE.
#x and y should be their own column

compare.fn <- function(mydata, 
                       x, y,
                       x.label = "",
                       y.label = "",
                       plot.title = "") {
  
  mydata <- mydata %>%
    drop_na() %>%
    rename(x = x,
           y = y
           )  
  
  #mydata[,mycol] <- as.factor(mydata[,mycol])
  
  lm1 <- lm(y~x, data=mydata) 
  lm1.s <- lm1 %>% 
    summary()
  
  eqt <- paste0("y = ", 
                round(lm1.s$coefficients["(Intercept)", "Estimate"]),
                " + ",
                round(lm1.s$coefficients["x", "Estimate"], 1),
                "x"
  )
  r2 <- paste0(
    "R2 = ",
    round(lm1.s$r.squared, 2)
  )
  
  rmse <- paste0("RMSE = ", sqrt(mean((predict(lm1) - mydata$y)^2)) %>% round()
  )
  
  myresult <- mydata %>%
    ggplot(aes(x=x, y= y, 
               #colour= mydata[,mycol] 
               )) + 
    geom_point(alpha=0.3) + 
    geom_smooth() + 
    #1-1 line
    geom_abline(intercept = 0, slope = 1) + 
    labs(x = x.label, 
         y=y.label,
         #colour = mycol,
         title = plot.title,
         subtitle = paste(eqt, "\n",
                          r2, "\n",
                          rmse)
    )
  
  return(myresult)
  
  }
  
#mydata0 <- compare_to_buffers %>% filter(buffer_m == 100, point_kind == "hwy")

#############################################################################################
#returns df with median AADT estimates using the points within a buffer

aadt_from_points_within_buffer <- function(mydata,
                                           mybuffer
                                           ) {
  
  # mydata <- Buffer100m.0 
  # mybuffer <- "100m"
  
  all <- mydata %>%
    group_by(OBJECTID, SRID, Location, Shape_Leng) %>%
    #calculate avg AADT based on point counts on each buffered road segment
    summarize(
      AADT1990 = median(X1990),
      AADT1995 = median(X1995),
      AADT2000 = median(X2000),
      AADT2005 = median(X2005),
      AADT2010 = median(X2010),
      AADT2015 = median(X2015),
      # no. of observations w/ point estimates (some rows have NAs if no points were within buffered road segments)
      pts_used = sum(!is.na(X1990))    #unique(points_used)
    ) %>% 
    mutate(
      point_kind = ifelse(pts_used >0,  "hwy or ramp", NA)
    ) %>%
    as.data.frame()
  
  
  by_type <- mydata %>%
    group_by(OBJECTID, Ramp, SRID, Location, Shape_Leng) %>%
    #calculate avg AADT based on point counts on each buffered road segment
    summarize(
      AADT1990 = median(X1990),
      AADT1995 = median(X1995),
      AADT2000 = median(X2000),
      AADT2005 = median(X2005),
      AADT2010 = median(X2010),
      AADT2015 = median(X2015),
      # no. of observations w/ point estimates (some rows have NAs if no points were within buffered road segments)
      pts_used = sum(!is.na(X1990))    #unique(points_used)
    ) %>% 
    ungroup() %>%
    mutate(
      point_kind = ifelse(Ramp==TRUE, "ramp", 
                          ifelse(Ramp==FALSE, "hwy", NA))
    ) %>%
    select(-Ramp) %>%
    as.data.frame()
  
  
  df <- rbind(all, by_type) %>%
    #get rid of duplicate entries from all and by_kind when rows have NAs
    unique() %>%
    arrange(OBJECTID) %>%
    #rename columns? or createn new column...
    #rename_at(., vars(AADT1990:point_kind), funs(paste0(., "_", mybuffer))) %>%
    mutate(buffer_m = as.numeric(mybuffer),
           # ??doesn't factor??
           point_kind = as.factor(point_kind))
  
  return(df)                                             
  
}

#############################################################################################
