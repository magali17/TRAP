


####################################################################################################
# returns PLS model for BC and UFP based on a given set of predictors (x)

fit_pls <- function(dt, 
                    x, #for grepl - part or or an entire variable name
                    .ncomp,
                    y = c("bc", "ufp")
) {
  
  #make grepl friendly in case searcing for >1 variable
  x <- paste0(x, collapse = "|")
  
  x <- names(dt)[grepl(x, names(dt))]
  
  model_results <- list()
  
  for(i in 1:length(y)) {
    #i=1
    model1 <- paste0(y[i], " ~ ", paste(x, collapse = " + "))
    
    # build PLS model using selected Y & predictor variables
    m1 <- plsr(as.formula(model1),
               data=dt, 
               ncomp = .ncomp,
               scale=T)
    
    #save model
    model_results[i] <- list(m1)
    names(model_results)[i] <- y[i]
    
  }
  
  return(model_results)
  
}

####################################################################################################


# returns PLS scores for non-training datasets based on a fitted PLS model. 
# can rename variables (e.g. TVC names) or use the same covariates (space-varying covariates)

# dt = mm
# rename_vars = FALSE
# pls_model = space_models$bc
# rename_components = paste0("space", c(1:n_components))


get_scores <- function(dt,
                       #var names to change to match model variables (for estimating/predicting scores)
                       rename_vars,
                       change_var_name = NULL,
                       #pls model fit
                       pls_model,
                       #rename new linear combination
                       rename_components
) {
  
  if (rename_vars ==TRUE) {
    
    #prefix for predictors in PLS model
    .model_vars <- rownames(pls_model$coefficients)
    #.model_vars <- rownames(pop2010_models$bc$coefficients)
    #only keep name before "_"
    .model_vars <- unique(sub("_.*", "_", .model_vars))
    
    dt <- dt %>%
      # rename predictors from other years to allow fitted model to use these to build scores
      select(starts_with(change_var_name)) %>%
      rename_all(~gsub(change_var_name, .model_vars, .))
  
  }
  
  
  dt <- dt %>%
    # # rename predictors from other years to allow fitted model to use these to build scores
    # select(starts_with(change_var_name)) %>%
    # rename_at(vars(starts_with(change_var_name)), ~gsub(change_var_name, .model_vars, .)) %>%
    # #View() 
    # use fitted model to estimate scores for other year/location
    predict(object = pls_model, 
            newdata = ., type = "score") %>%
    as.data.frame()  
  
  names(dt) <- rename_components
  
  
  return(dt)
  
  
}

############################################################################################################

### --> USING??

# returns PLS scores for training and non-training datasets 

get_pls_scores <- function(y, x,
                           .ncomp,
                           rename_columns,
                           
                           #datasets to use
                           dt_model = mm, 
                           dt_predict1 = cohort, 
                           dt_predict2 = grid
)
{
  
  model1 <- paste0(y, " ~ ", paste(x, collapse = " + "))
  
  # --> ??? use most recent time-varying covariate values to fit PLS model ?
  m1 <- plsr(as.formula(model1),
             data=dt_model, 
             ncomp = .ncomp,
             scale=T)
  
  # scores for mm
  model_scores <- scores(m1)[] %>% 
    as.data.frame()
  
  # scores for cohort
  predict1_scores <- predict(object = m1,
                             newdata = dt_predict1,
                             type = "score") %>%
    as.data.frame()
  
  # scores for grid
  predict2_scores <- predict(object = m1,
                             newdata = dt_predict2,
                             type = "score") %>%
    as.data.frame()
  
  # take out spaces in names
  names(model_scores) <- rename_columns
  names(predict1_scores) <- rename_columns
  names(predict2_scores) <- rename_columns
  
  results <- list(
    dt_model = model_scores,
    dt_predict1 = predict1_scores,
    dt_predict2 = predict2_scores
  )
  
  return(results)
  
}

############################################################################################################





############################################################################################################















############################################ OLD - DELETE? ########################################

# ###############################################################################################
# # returns plots w/ linear fit, R2 and RMSE.
# #x and y should be their own column
# 
# compare.fn <- function(mydata, 
#                        x, y,
#                        x.label = "",
#                        y.label = "",
#                        plot.title = "") {
#   
#   mydata <- mydata %>%
#     drop_na() %>%
#     rename(x = x,
#            y = y
#            )  
#   
#   #mydata[,mycol] <- as.factor(mydata[,mycol])
#   
#   lm1 <- lm(y~x, data=mydata) 
#   lm1.s <- lm1 %>% 
#     summary()
#   
#   eqt <- paste0("y = ", 
#                 round(lm1.s$coefficients["(Intercept)", "Estimate"]),
#                 " + ",
#                 round(lm1.s$coefficients["x", "Estimate"], 1),
#                 "x"
#   )
#   r2 <- paste0(
#     "R2 = ",
#     round(lm1.s$r.squared, 2)
#   )
#   
#   rmse <- paste0("RMSE = ", sqrt(mean((predict(lm1) - mydata$y)^2)) %>% round()
#   )
#   
#   myresult <- mydata %>%
#     ggplot(aes(x=x, y= y, 
#                #colour= mydata[,mycol] 
#                )) + 
#     geom_point(alpha=0.3) + 
#     geom_smooth() + 
#     #1-1 line
#     geom_abline(intercept = 0, slope = 1) + 
#     labs(x = x.label, 
#          y=y.label,
#          #colour = mycol,
#          title = plot.title,
#          subtitle = paste(eqt, "\n",
#                           r2, "\n",
#                           rmse)
#     )
#   
#   return(myresult)
#   
#   }
#   
# #mydata0 <- compare_to_buffers %>% filter(buffer_m == 100, point_kind == "hwy")
# 
# #############################################################################################
# #returns df with median AADT estimates using the points within a buffer
# 
# aadt_from_points_within_buffer <- function(mydata,
#                                            mybuffer
#                                            ) {
#   
#   # mydata <- Buffer100m.0 
#   # mybuffer <- "100m"
#   
#   all <- mydata %>%
#     group_by(OBJECTID, SRID, Location, Shape_Leng) %>%
#     #calculate avg AADT based on point counts on each buffered road segment
#     summarize(
#       AADT1990 = median(X1990),
#       AADT1995 = median(X1995),
#       AADT2000 = median(X2000),
#       AADT2005 = median(X2005),
#       AADT2010 = median(X2010),
#       AADT2015 = median(X2015),
#       # no. of observations w/ point estimates (some rows have NAs if no points were within buffered road segments)
#       pts_used = sum(!is.na(X1990))    #unique(points_used)
#     ) %>% 
#     mutate(
#       point_kind = ifelse(pts_used >0,  "hwy or ramp", NA)
#     ) %>%
#     as.data.frame()
#   
#   
#   by_type <- mydata %>%
#     group_by(OBJECTID, Ramp, SRID, Location, Shape_Leng) %>%
#     #calculate avg AADT based on point counts on each buffered road segment
#     summarize(
#       AADT1990 = median(X1990),
#       AADT1995 = median(X1995),
#       AADT2000 = median(X2000),
#       AADT2005 = median(X2005),
#       AADT2010 = median(X2010),
#       AADT2015 = median(X2015),
#       # no. of observations w/ point estimates (some rows have NAs if no points were within buffered road segments)
#       pts_used = sum(!is.na(X1990))    #unique(points_used)
#     ) %>% 
#     ungroup() %>%
#     mutate(
#       point_kind = ifelse(Ramp==TRUE, "ramp", 
#                           ifelse(Ramp==FALSE, "hwy", NA))
#     ) %>%
#     select(-Ramp) %>%
#     as.data.frame()
#   
#   
#   df <- rbind(all, by_type) %>%
#     #get rid of duplicate entries from all and by_kind when rows have NAs
#     unique() %>%
#     arrange(OBJECTID) %>%
#     #rename columns? or createn new column...
#     #rename_at(., vars(AADT1990:point_kind), funs(paste0(., "_", mybuffer))) %>%
#     mutate(buffer_m = as.numeric(mybuffer),
#            # ??doesn't factor??
#            point_kind = as.factor(point_kind))
#   
#   return(df)                                             
#   
# }
# 
# #############################################################################################
