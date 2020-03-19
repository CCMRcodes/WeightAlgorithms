#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Janney 2016 version
#
# Algorithm reconstructed from methods section in the publication:
# Janney CA, Kilbourne AM, Germain A, et al. The influence of sleep disordered
# breathing on weight loss in a national weight management program. Sleep [elec-
# tronic article]. 2016;39(1):59-65. 
# (https://academic.oup.com/sleep/article/39/1/59/2726067). (Accessed December
# 6, 2019)
# 
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
# 
# Rationale: For grouped time series, (e.g., per person)
#            Define time points and collect measurements, optionally applying
#            windows for possible data points for each time point, 
#            then removing outliers. Based on work by Janney et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney served as a basis for this function/algorithm
#
# Requires MeasureWindows.R and MeasureRemoveOutliers.R to be in the same
# directory as Janney2016.f (this file - Janney2016.R)
#-----------------------------------------------------------------------------#

#-------------------------------- set windows --------------------------------#

#' @title Windows.f Measurment Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `df`
#' @param measures string corresponding to the name of the column of measures in `df`, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in `df`
#' @param startPoint string corresponding to the name of the column in `df` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then `startPoint` should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. `c(0, 182.5, 365)` for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is `c(0, 182.5, 365)` according to Janney et al. 2016
#' @param windows numeric vector of measurement collection windows to use around each time point in t. Eg. Janney et al. 2016 use `c(30, 60, 60)` for t of `c(0, 182.5, 365)`, implying that the closest measurement t = 0 will be collected 30 days prior to and 30 days post startPoint. Subsequent measurements will be collected 60 days prior to and 60 days post t0+182.5 days, and t0+365 days.
windows.f <- function(DF,
                      id,
                      measures,
                      tmeasures,
                      startPoint,
                      t = c(0, 182, 365),
                      windows = c(30, 60, 60)) {
  
  if (!require(dplyr))      install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(rlang))      install.packages("rlang")
  
  tryCatch(
    if (class(DF[[tmeasures]])[1] != class(DF[[startPoint]])[1]) {
      stop(
        print(
          paste0("date type of tmeasures (",
                 class(DF[[tmeasures]]),
                 ") != date type of startPoint (",
                 class(DF[[startPoint]])[1],
                 ")"
          )
        )
      )
    }
  )
  
  tryCatch(
    if (class(t) != "numeric") {
      stop(
        print("t parameter must be a numeric vector")
      )
    }
  )
  
  tryCatch(
    if (class(windows) != "numeric") {
      stop(
        print("windows parameter must be a numeric vector")
      )
    }
  )
  
  tmeasures  <- rlang::sym(tmeasures)
  startPoint <- rlang::sym(startPoint)
  
  DF <- DF %>%
    mutate(
      time = as.numeric(
        difftime(
          !!tmeasures, !!startPoint,
          tz = "utc", units = "days"
        )
      )
    )
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {
    # find measurement closest to t[i]
    x <- DT[DT[, .I[abs(t[i] - time) == min(abs(t[i] - time))],
               by = eval(id)]$V1]
    # apply time outlier window/filter
    x <- x[time >= (t[i] - windows[i]) & time <= (t[i] + windows[i]), ]
    
    # reconcile duplicates
    x <- x[x[, .I[1], by = eval(id)]$V1] # take the first
    
    # tidy up
    meas_tn[[i]] <- x %>%
      as.data.frame() %>%
      mutate(measureTime = paste0("t_", t[i]))
  }
  
  id <- rlang::sym(id)
  
  # return result
  do.call(rbind, meas_tn) %>% 
    arrange(!!id, !!tmeasures)
}

#------------------------------ Remove Outliers ------------------------------#

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
  
  a <- rlang::sym(measures)
  meas_updated <- rlang::quo_name(paste0(measures, "_OR"))
  
  DF <- DF %>%
    left_join(bounds.df, by = c("measureTime" = "t")) %>%
    mutate(!!meas_updated := ifelse(!!a < LB | !!a > UB, NA, !!a))
  
  DF
}

#------------------------- Windows + Outlier Removal -------------------------#

#' @title Janney2016.f Weight Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing id and weights
#' @param id string corresponding to the name of the column of patient IDs in `DF`
#' @param measures string corresponding to the name of the column of measures in `DF`, e.g., numeric weight data if using to clean weight data
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in `DF`
#' @param startPoint string corresponding to the name of the column in `DF` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then startPoint should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. c(0, 182.5, 365) for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is c(0, 182.5, 365) according to Janney et al. 2016
#' @param windows numeric vector of measurement collection windows to use around each time point in t. Eg. Janney et al. 2016 use c(30, 60, 60) for t of c(0, 182.5, 365), implying that the closest measurement t = 0 will be collected 30 days prior to and 30 days post startPoint. Subsequent measurements will be collected 60 days prior to and 60 days post t0+182.5 days, and t0+365 days
#' @param outliers optional. object of type list with numeric inputs corresponding to the upper and lower bound for each time entry in parameter `t`. Default is list(LB = c(91, 72, 72), UB = c(600, 650, 650)) for t = c(0, 182.56, 365), differing between baseline and subsequent measurment collection dates. If not specified then only the subsetting and window functions will be applied.
Janney2016.f <- function(DF,
                         id,
                         measures,
                         tmeasures,
                         startPoint,
                         t = c(0, 182.5, 365),
                         windows = c(30, 60, 60),
                         outliers = list(LB = c(91, 72, 72),
                                         UB = c(600, 650, 650))) {
  
  windowsApplied.df <- windows.f(DF = DF,
                                 id = id,
                                 measures = measures,
                                 tmeasures = tmeasures,
                                 startPoint = startPoint,
                                 t = t,
                                 windows = windows)
  
  outliersRemoved.df <- outliers.f(DF = windowsApplied.df,
                                   measures = measures,
                                   t = t,
                                   outliers = outliers)
  # return fully cleaned data
  outliersRemoved.df
}