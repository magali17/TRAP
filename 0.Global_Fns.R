############################################################################################## 
############################################ basic fns ############################################ 
############################################################################################## 

#returns MSE
mse <- function(obs, pred){
  mean((obs - pred)^2)
  }

rmse <- function(obs, pred){
  sqrt(mean((obs - pred)^2))
  }


#returns MSE-based R2
r2_mse_based <- function(obs, pred) {
  mse.est <- mse(obs, pred)
  r2 <- 1- mse.est/mean((obs - mean(obs))^2)
  max(0, r2)
  }  


############################################################################################## 
#returns table of distribution of a variable
distribution.table <- function(dt,
                               var.string = "ptrak_pt_cm3",
                               round.int = 0) {
  
  t <- dt %>%
    dplyr::rename(var = var.string) %>%
    dplyr::summarize(
      N = n(),
      "Mean (SD)" =  qwraps2::mean_sd (var, digits = round.int, na_rm = T, denote_sd = "paren"),
      "Median (IQR)" =  qwraps2::median_iqr(var, digits = round.int, na_rm = T, ),
      Min = round(min(var), round.int),
      Max = round(max(var), round.int)
    )  
  
  return(t)
}

############################################################################################## 
#wraps text to fit ggplot
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

############################################################################################## 


############################################################################################## 
############################################ lasso ############################################ 

# dt = annual_ufp_and_cov
# y_name = "log_ufp"
# x_names = cov_names
# family. = "gaussian"
# lambda. = ""

# dt = df
# x = cov_names
# y_name = "high_variability"
# family. = "binomial"


#load library for lasso
pacman::p_load(glmnet)

lasso_fn <- function(dt, x_names, y_name, family. = "gaussian", lambda. = "") {
  
  
  x <- model.matrix(as.formula(paste(y_name, "~", 
                                     paste(x_names, collapse = " + "))
  ), dt)[, -1]
  
  #replace y "name" w/ actual data
  y <- dt[[y_name]]   
  
  #select lambda through CV if not supplied
  if(lambda. == ""){
    cv.out <- cv.glmnet(x = x,
                        y = y, 
                        alpha=1, 
                        family= family., 
                        standardize=T)
    
    lambda. <- cv.out$lambda.min
  }
  
  # run Lasso
  lasso.m <- glmnet(x = x,
                    y = y, 
                    alpha = 1, 
                    family= family.,  
                    standardize = T)
  
  #save coefficient estimates
  lasso_coef <- predict(lasso.m, 
                        type= "coefficients",  
                        s= lambda.)[1:(ncol(x)+1),] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(cov = rowname,
           coef = ".") %>%
    #keep coefficients that are not 0 or intercept values
    filter(coef != 0,
           cov != "(Intercept)")
  
  
  results <- list(results = lasso_coef,
                  lambda = lambda.
  )
  
  return(results)
  
}

############################################################################################## 
