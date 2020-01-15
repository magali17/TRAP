######################## PLS ########################
# # save site_ids for later
# train.site_id <- annual.all$site_id[annual.all$set== "train"]
# test.site_id <- annual.all$site_id[annual.all$set== "test"]
# aqs.site_ids <- annual.all$site_id[annual.all$set== "aqs"]



################# UK#################

# create dataframe to save model evaluation results
# cv.eval <- list(
#   cv = data.frame(
#    expand.grid(
#      #resid_model = c(
#        #"ML", 
#        #"REML" #, "OLS", "WLS"
#        #),
#      pls_comp = c(1:use_n_scores)
#      ),
#    RMSE = NA,
#    R2 = NA
#    ) #,
#   #residual_models = list(),
#   #variograms = list()
# )




#################### residual model estimation methods ####################
# try various estimation methods. The two basic options are likfit which uses maximum likelihood to do the estimation (with options for ML vs REML), and variofit which uses a parametric model fit using either ordinary or weighted least squares. 

# ml_train <- likfit(geodata = geo_train, ini=wls_ests_train, cov.model = "exp",
#              trend = cov.trend,
#              lik.method = "ML") 


# ols_train <- variofit(vario = variog_train, ini=wls_ests_train, cov.model = "exp", 
#                 weights="equal")
# wls_train <- variofit(vario = variog_train, ini=wls_ests_train, cov.model = "exp",
#                 weights = "npairs")








#cv.eval$cv <- cv.eval$cv %>% left_join(variog_dist_to_plot)


### --> fn for 10FCV instead of split set (test/train)

# ??? add variogram option??
# xv.ml <- xvalid(geo_train, model = ml_train, 
#                 # reestimate = T, 
#                 # variog.obj = variog_train
#                 )


#xv.reml <- xvalid(geo_train, model = reml_train, 
#reestimate = T,
#variog.obj = variog_train
#)


# xv.ols <- xvalid(geo_train, model = ols_train, 
#                  # reestimate = T, 
#                  # variog.obj = variog_train
#                  )
# xv.wls <- xvalid(geo_train, model = wls_train, 
#                  # reestimate = T, 
#                  # variog.obj = variog_train
#                  )
# 





# #calculate out-of-sample RMSE and R2
# mse_test_stops <- mse(obs = pls_df_test_stops$log_ufp, pred = pls_df_test_stops$pred)
# r2_test_stops <- r2_mse_based(obs = pls_df_test_stops$log_ufp, pred = pls_df_test_stops$pred)












## --> ? why is worse looking model (REML) have y slighbetter RMSE, R2 than better looking model (OLS)?

#calculate MSE and R2 (R2 = max(0, 1 – MSE / Var(Y)))
#mse.ml <- mse(xv.ml$data, xv.ml$predicted)

#mse.reml <- mse(xv.reml$data, xv.reml$predicted)

# mse.ols <- mse(xv.ols$data, xv.ols$predicted) 
# mse.wls <- mse(xv.wls$data, xv.wls$predicted) 

#r2.ml <- r2_mse_based(obs = xv.ml$data, pred = xv.ml$predicted)

#r2.reml <- r2_mse_based(obs = xv.reml$data, pred = xv.reml$predicted)

# r2.ols <- r2_mse_based(obs = xv.ols$data, pred = xv.ols$predicted) 
# r2.wls <- r2_mse_based(obs = xv.wls$data, pred = xv.wls$predicted) 

#save RMSE
#cv.eval$cv$RMSE[cv.eval$cv$resid_model=="ML" & cv.eval$cv$pls_comp== i] <-  sqrt(mse.ml)

# cv.eval$cv$RMSE[cv.eval$cv$resid_model=="REML" & cv.eval$cv$pls_comp== i] <-  sqrt(mse.reml)

#cv.eval$cv$RMSE[cv.eval$cv$resid_model=="OLS" & cv.eval$cv$pls_comp== i] <-  sqrt(mse.ols)
#cv.eval$cv$RMSE[cv.eval$cv$resid_model=="WLS" & cv.eval$cv$pls_comp== i] <-  sqrt(mse.wls)

#save R2
#cv.eval$cv$R2[cv.eval$cv$resid_model=="ML" & cv.eval$cv$pls_comp== i] <-  r2.ml

#cv.eval$cv$R2[cv.eval$cv$resid_model=="REML" & cv.eval$cv$pls_comp== i] <-  r2.reml 

#cv.eval$cv$R2[cv.eval$cv$resid_model=="OLS" & cv.eval$cv$pls_comp== i] <- r2.ols  
#cv.eval$cv$R2[cv.eval$cv$resid_model=="WLS" & cv.eval$cv$pls_comp== i] <- r2.wls  

#save residual models
# cv.eval$residual_models[[i]] <- list(ML = ml_train, 
#                                    REML = reml_train, 
#                                    OLS = ols_train, 
#                                    WLS = wls_train)
# 
#save variogram
#cv.eval$variograms[[i]] <- variog_train



# --> ? manually select instead?

# min_rmse <- min(cv.eval$cv$RMSE)
# cv_r2 <- cv.eval$cv$R2[cv.eval$cv$RMSE == min_rmse] #[1] #selected 1st model if multiple
# cv_comp <- cv.eval$cv$pls_comp[cv.eval$cv$RMSE == min_rmse] #[1]
# cv_resid_model <- cv.eval$cv$resid_model[cv.eval$cv$RMSE == min_rmse] %>% as.character()  ## [1]
# 
# cv_comp_names <- names(scores_train)[1:cv_comp]
# #cv_resid_model_train <- cv.eval$residual_models[[cv_comp]] #[[cv_resid_model]]
#  
# #trends
# cv_cov_trend <-  as.formula(paste0("~ ", paste0(cv_comp_names, collapse = " + " )))


#Plot LOOCV results.

### --> ? error: selected 1st option if various models resulted in same RMSE


###########################################################################

           # WORKS # 

#### --> needd to create grp folds Again?

grp <- sample(c(1:k), replace=T, nrow(pls_df_train))

#instead of geo_trainnew, create place to save CV predictions & betas
geo_train$pred <- numeric(length(grp))
geo_train$beta = data.frame(
  cbind(beta0 = numeric(k),
        matrix(ncol = length(score_n_names), nrow = k)
  )
)

#names(geo_trainnew$beta) <- c("beta0", score_n_names)
names(geo_train$beta) <- c("beta0", score_n_names)

#10FCV loop
for (j in seq_len(k)) {
  #j=1
  #generate validate set (1/10 of data)
  is_group <- grp == j
  
  #training geodataset
  train2_geo <- as.geodata(
    obj = pls_df_train[!is_group,],
    coords.col = c("lambert_x", "lambert_y"),
    data.col = "log_ufp", 
    covar.col = score_n_names
  )
  
  #validation dataset 
  valid_geo <- as.geodata(
    obj = pls_df_train[is_group,],
    coords.col = c("lambert_x", "lambert_y"),
    data.col = "log_ufp", 
    covar.col = score_n_names
  )
  
  #trend
  train2_trend <- trend.spatial(trend = cov.trend, train2_geo)
  valid_trend <- trend.spatial(trend = cov.trend, valid_geo)
  
  #UK
  kc_cv <- krige.conv(coords = train2_geo$coords,
                      data = train2_geo$data,
                      locations = valid_geo$coords,
                      krige = krige.control(type = "ok",
                                            # --> ?use initial exp fit estt using WLS?
                                            obj.model = reml_train,
                                            trend.d = train2_trend,
                                            trend.l = valid_trend
                      )
  )
  
  #save CV predictions & beta estiamtes
  geo_train$pred[is_group] <- kc_cv$predict
  # ? need beta estimates?
  geo_train$beta[j,] <- kc_cv$beta.est
  
  
}

################################################################################
# valiate at stops 


```{r}

#update geo_train w/ selected PLS features 
geo_train <- as.geodata(pls_df_train, 
                        coords.col = c("lambert_x", "lambert_y"), 
                        data.col = "log_ufp", 
                        covar.col = cv_comp_names)

#trend
trend_train <- trend.spatial(trend = cv_cov_trend,
                             geodata = geo_train)


```

```{r}
#stop test set
# --> ? use pls_train to estimate test "scores"?
#estimate scores for test set
scores_test_stops <- predict(object = pls_train,
                             newdata = annual.test,
                             ncomp = 1:cv_comp,
                             type = "score"
) %>%
  as.data.frame()

#take out spaces
names(scores_test_stops) <- cv_comp_names

#add site_id, ufp & lat/long
pls_df_test_stops <- cbind(
  annual.test[c("site_id", "log_ufp")],
  scores_test_stops
) %>%
  left_join(geo.mm) 

#create geodata for test set
geo_test_stops <- as.geodata(pls_df_test_stops , 
                             coords.col = c("lambert_x", "lambert_y"), 
                             data.col = "log_ufp", 
                             covar.col = cv_comp_names)

test_stops_trend <- trend.spatial(trend = cv_cov_trend,
                                  geodata = geo_test_stops)

uk_test_stops <- krige.conv(coords = geo_train$coords,
                            data = geo_train$data,
                            locations = geo_test_stops$coords,
                            krige=krige.control(type = "ok",
                                                obj.model = cv_resid_model_train,
                                                trend.d= trend_train,
                                                trend.l= test_stops_trend))

#save UK predictions
pls_df_test_stops$pred  <- uk_test_stops$predict

#calculate out-of-sample RMSE and R2
rmse_test_stops <- rmse(obs = pls_df_test_stops$log_ufp, pred = pls_df_test_stops$pred)
r2_test_stops <- r2_mse_based(obs = pls_df_test_stops$log_ufp, pred = pls_df_test_stops$pred)

```
