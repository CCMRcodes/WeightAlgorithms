#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Goodrich 2016 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
#
# Algorithm reconstructed from methods section in the publication:
# Goodrich DE, Klingaman EA, Verchinina L, et al. Sex differences in weight loss
# among veterans with serious mental illness: Observational study of a national 
# weight management program. Women’s Health Issues [electronic article]. 
# 2016;26(4):410–419. 
# (https://linkinghub.elsevier.com/retrieve/pii/S1049386716300366). 
# (Accessed December 6, 2019
# 
# Rationale: For grouped time series, (e.g., per person)
#            Define time points and collect measurements, optionally applying
#            windows for possible data points for each time point, 
#            then removing outliers. Based on work by Goodrich et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney et al. 2016 served as a basis for this function/algorithm
#
# Requires MeasureWindows.R and MeasureRemoveOutliers.R to be in the same
# directory as Janney2016.f (this file - Goodrich2016.R)
#-----------------------------------------------------------------------------#

#----------------------- weight change moving forward --------------------------

#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measures in `DF`, e.g., numeric weight data if using to clean weight data
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in `DF`
#' @param wtchng_thresh numeric scalar used as a cutoff for higher than (or lower than) expected weight changes from one time point j to time point j + 1, by person or group. Default is 100.
lookForwardAndRemove.f <- function(DF,
                                   id,
                                   measures,
                                   tmeasures,
                                   wtchng_thresh = 100) {
  
  if (!require(data.table)) install.packages("data.table")
  if (!require(dplyr))      install.packages("dplyr")
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  setorderv(DT, c(id, tmeasures))
  
  # fast lead with data.table
  DT[,
     "forward" := shift(get(measures), 
                        n = 1L, 
                        fill = NA, 
                        type = "lead"), 
     by = id
     ]
  
  # remove weight changes > wtchng_thresh
  DT$outlier <- abs(DT[[measures]] - DT[["forward"]]) > wtchng_thresh
  DT <- DT %>%
    mutate(
      output = case_when(
        outlier ~ NA_real_,
        is.na(outlier) ~ DT[[measures]],
        TRUE ~ DT[[measures]]
      )
    )
  
  DT
}

#-------------------- Add weight change to Janney2016.f ---------------------

#' @title Goodrich et al. 2016 Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing id and weights
#' @param id string corresponding to the name of the column of patient IDs in `DF`
#' @param measures string corresponding to the name of the column of measures in `DF`, e.g., numeric weight data if using to clean weight data
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in DF
#' @param startPoint string corresponding to the name of the column in `DF` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then startPoint should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. c(0, 182.5, 365) for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is c(0, 182.5, 365) according to Janney et al. 2016
#' @param windows numeric vector of measurement collection windows to use around each time point in t. Eg. Janney et al. 2016 use c(30, 60, 60) for t of c(0, 182.5, 365), implying that the closest measurement t = 0 will be collected 30 days prior to and 30 days post startPoint. Subsequent measurements will be collected 60 days prior to and 60 days post t0+182.5 days, and t0+365 days
#' @param outliers optional. object of type list with numeric inputs corresponding to the upper and lower bound for each time entry in parameter `t`. Default is list(LB = c(80, 80, 80), UB = c(500, 500, 500)) for t = c(0, 182.56, 365), differing between baseline and subsequent measurment collection dates. If not specified then only the subsetting and window functions will be applied.
#' @param wtchng_thresh numeric scalar used as a cutoff for higher than (or lower than) expected weight changes from one time point j to time point j + 1, by person or group. Default is 100.

Goodrich2016.f <- function(DF,
                           id,
                           measures,
                           tmeasures,
                           startPoint,
                           t = c(0, 182, 365),
                           windows = c(30, 60, 60),
                           outliers = list(LB = c(80, 80, 80),
                                           UB = c(500, 500, 500)),
                           wtchng_thresh = 100,
                           excludeSubject = FALSE){
  
  WindowsAndOutliers.df <- 
    Janney2016.f(
      DF,
      id,
      measures,
      tmeasures,
      startPoint,
      t = t,
      windows = windows,
      outliers = outliers
    )
  
  lookForwardAndRemove.df <- 
    lookForwardAndRemove.f(
      DF = WindowsAndOutliers.df,
      id = id,
      measures = "Weight_OR",
      tmeasures = tmeasures,
      wtchng_thresh = wtchng_thresh
    )
  
  if (excludeSubject) {
    
    excluded.df <- lookForwardAndRemove.df %>%
      filter(is.na(output)) %>%
      select(id) %>%
      distinct() %>%
      mutate(FlagForRemoval = 1) %>%
      right_join(lookForwardAndRemove.df, by = "PatientICN") %>%
      filter(is.na(FlagForRemoval)) %>%
      select(-FlagForRemoval)
    
    return(excluded.df)
    
  } else {
    return(lookForwardAndRemove.df)
  }
}