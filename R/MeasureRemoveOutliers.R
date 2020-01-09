#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Janney 2016 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
# 
# Rationale: Remove outliers in grouped time series 
#            (e.g., per person) based on work by Janney et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney served as a basis for this function/algorithm
#-----------------------------------------------------------------------------#

#' @title outliers.f Measurment Cleaning Algorithm - removing outliers. To be applied following the result of a call to windows.f()
#' @param DF object of class data.frame, containing id and weights
#' @param measures string corresponding to the name of the column of measures in df, e.g., numeric weight data if using to clean weight data.
#' @param t numeric vector of time points to collect measurements, eg. c(0, 182.5, 365) for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is c(0, 182.5, 365) according to Janney et al. 2016
#' @param outliers object of type list with numeric inputs corresponding to the upper and lower bound for each time entry in parameter `t`. Default is list(LB = c(91, 72, 72), UB = c(600, 650, 650)) for t = c(0, 182.56, 365), differing between baseline and subsequent measurement collection dates
outliers.f <- function(DF,
                       measures,
                       t = c(0, 182.5, 365), 
                       outliers = list(LB = c(91, 72, 72),
                                       UB = c(600, 650, 650))) {
  
  if (!require(dplyr))      install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  
  tryCatch(
    if (class(DF[[measures]]) != "numeric") {
      stop(
        print("weight data must be a numeric vector")
      )
    }
  )
  
  tryCatch(
    if (class(outliers) != "list" & !is.null(outliers)) {
      stop(
        print("outliers must be placed into a list object")
      )
    }
  )
  
  bounds.df <- data.frame(
    t = paste0("t_", t),
    LB = outliers[[1]],
    UB = outliers[[2]],
    stringsAsFactors = FALSE
  )
  
  mutate_call <- lazyeval::interp(~ifelse(a < LB | a > UB, NA, a),
                                  a = as.name(measures))
  
  DF <- DF %>%
    left_join(bounds.df, by = c("measureTime" = "t")) %>%
    mutate_(.dots = setNames(list(mutate_call), paste0(measures, "_OR")))
  
  DF
}