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
#returns the minimum and maximum value of two vectors. for plotting purposes when wanting a perfectly diagonal 1-1 line (square plotting area).
plot_range <- function(vector1 = uk_decompose_l$primary_uk, 
                       vector2 = uk_decompose_l$regression_prediction){
  min <- min(min(vector1), min(vector2))
  max <- max(max(vector1), max(vector2))
  
  result <- data.frame(min = min, 
                       max = max)
  
  return(result)
  
}

################################# correlation plot Wide format ####################################

# data.wide <- uk_decompose %>% filter(study_area) 
# y.variable = my_analysis
# x.variable = "regression_prediction"
# x.label = ""
# y.label = ""
# col.by = ""
# mytitle = ""
# coef_digits = 0
# r2.digits = 2
# rmse.digits = 2
# convert_rmse_r2_to_native_scale=T

colo.plot <- function(data.wide=mm.wide, 
                      x.variable, x.label = "",
                      y.variable, y.label = "",
                      col.by = "",
                      mytitle = "", title_width = 60,
                      mysubtitle = NULL,
                      mycaption = NULL,
                      int_digits = 0,
                      slope_digits = 2,
                      r2.digits = 2, 
                      rmse.digits = 0,
                      convert_rmse_r2_to_native_scale=F
                      ) {
  
  #if label is left blank, use variable name
  if(x.label == ""){x.label <- x.variable}
  if(y.label == ""){y.label <- y.variable}
  
  data.wide <- data.wide %>% drop_na(x.variable, y.variable)  
  
  lm1 <- lm(formula(paste(y.variable, "~", x.variable)), data = data.wide)
  #summary(lm1)
  
  #rmse
  if (convert_rmse_r2_to_native_scale==T) {
    rmse <- rmse(obs = exp(data.wide[[x.variable]]), pred = exp(data.wide[[y.variable]])) %>% 
      round(digits = rmse.digits)
    
    r2 <- r2_mse_based(obs = exp(data.wide[[x.variable]]), pred = exp(data.wide[[y.variable]])) %>%
      round(r2.digits)
    } 
  
  else {
    rmse <- rmse(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>% 
    round(digits = rmse.digits)
    
    r2 <- r2_mse_based(obs = data.wide[[x.variable]], pred = data.wide[[y.variable]]) %>%
      round(r2.digits)
  }
  
  
  
  fit.info <- paste0("y = ", round(coef(lm1)[1], int_digits), " + ", round(coef(lm1)[2], slope_digits), 
                     "x \nR2 = ", r2,  
                     "\nRMSE = ", rmse,
                     "\nNo. Pairs = ", nrow(data.wide))
  
  max_plot <- max(max(data.wide[[x.variable]]), max(data.wide[[y.variable]]) )
  min_plot <- min(min(data.wide[[x.variable]]), min(data.wide[[y.variable]]) )
    
  #compare  
  p <- data.wide %>%
    ggplot(aes(x= data.wide[[x.variable]], y= data.wide[[y.variable]])) + 
    geom_point(alpha=0.3, aes(col = data.wide[[col.by]]
    )) + 
    coord_fixed() +
    geom_abline(intercept = 0, slope = 1) +
    #geom_smooth(aes(fill="loess")) +
    geom_smooth(method = "lm", aes(fill="LS")) +
    xlim(min_plot, max_plot) +
    ylim(min_plot, max_plot) +
    labs(title = wrapper(mytitle, width = title_width),
         subtitle = mysubtitle,
         caption = mycaption,
         x = x.label,
         y = y.label,
         col = col.by,
         fill = "fit") +
    annotate("text", -Inf, Inf, label = fit.info, hjust = 0, vjust = 1)

  return(p)
  
}

# issues/notes: 
# coord_fixed() and theme(aspect.ration =1) don't do anything??

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
