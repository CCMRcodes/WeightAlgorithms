#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Maguen et al. 2013 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
# 
# Rationale: For grouped time series, (e.g., per person)
#            remove outliers, then model patients weight trajectories with
#            a linear mixed model with a random slope and intercept, adjusted
#            for age and gender, then remove measurements where the residual
#            meets a certain threshold. Based on work by Maguen et al. 2013
#-----------------------------------------------------------------------------#

#' @title Maguen 2013 Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing id and measures
#' @param id string corresponding to the name of the column of patient IDs in DF
#' @param measures string corresponding to the name of the column of measurements in DF
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in DF. If tmeasures is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers object of type 'list' with numeric inputs corresponding to the upper and lower bound for each time entry. Default is list(LB = c(70), UB = c(700))
#' @param variables character vector describing the terms in `DF` to include on the RHS of the internal mixed model. E.g., c("Age", "Gender") would generate a model of the form "`measures` ~ Age + Gender + (1|`id`)"
#' @param ResidThreshold single numeric value to be used as a cut off value for the conditional (response) residual for each measurement. Low values are more conservative.
#' @param ... any number of named arguments for the lme4::lmer model
Maguen2013.f <- function(DF,
                         id,
                         measures,
                         tmeasures,
                         outliers = list(LB = 70, UB = 700),
                         variables,
                         ResidThreshold = 10,
                         ...) {
  
  if (!require(dplyr))  install.packages("dplyr")
  if (!require(lme4))   install.packages("lme4")
  if (!require(modelr)) install.packages("modelr")
  
  tryCatch(
    if (!is.numeric(DF[[measures]])) {
      stop(
        print("weight data must be a numeric vector")
      )
    }
  )
  
  tryCatch(
    if (!is.list(outliers)) {
      stop(
        print("outliers must be placed into a list object")
      )
    }
  )
  
  # step 1: outliers
  DF <- DF %>%
    mutate(
      Output = ifelse(DF[[measures]] >= outliers$LB 
                      & DF[[measures]] <= outliers$UB,
                      DF[[measures]],
                      NA_real_)
    )
  
  # step 2: linear mixed model
  
  # add time for random slope term
  DF <- DF %>%
    filter(!is.na(Output)) %>%
    group_by_(id) %>%
    arrange_(id, tmeasures) %>%
    mutate(t = row_number()) %>%
    ungroup()
  
  f <- paste("Output",
             paste(variables,
                   collapse = " + "),
             sep = " ~ ")
  f <- paste0(f, " + t", " + (1 + t", " | ", id, ")")
  f <- as.formula(f)
  lmm <- eval(bquote(lme4::lmer(.(f),
                                data = DF,
                                control = lmerControl(calc.derivs = FALSE)
                                )
                     )
              )
  
  DF$resid <- residuals(lmm) # add conditional residuals at ith level
  
  DF <- DF %>%
    mutate(Output = ifelse(abs(resid) >= ResidThreshold,
                           NA_real_,
                           Output))
  DF
}