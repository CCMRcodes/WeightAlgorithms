#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Janney 2016 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
# 
# Rationale: Set time points and windows in a grouped time series 
#            (e.g., per person) based on work by Janney et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney served as a basis for this function/algorithm
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
                      t = c(0, 182.5, 365),
                      windows = c(30, 60, 60)) {
  
  if (!require(dplyr))      install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  
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
  
  # compute difference in time between t0 and all t_j
  mutate_call <- lazyeval::interp(~as.numeric(difftime(a, b, units = "days")), 
                                  a = as.name(tmeasures),
                                  b = as.name(startPoint))
  
  DF <- DF %>% mutate_(.dots = setNames(list(mutate_call), "time"))
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {
    
    x <- DT[DT[, .I[abs(t[i] - time) == min(abs(t[i] - time))],
               by = eval(id)]$V1]
    
    x <- x[time >= (t[i] - windows[i]) & time <= (t[i] + windows[i]), ]
    
    meas_tn[[i]] <- x %>%
      as.data.frame() %>%
      mutate(measureTime = paste0("t_", t[i]))
  }
  
  # return result
  do.call(rbind, meas_tn) %>% arrange_(.dots = c(id, tmeasures))
}
