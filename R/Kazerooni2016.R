#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Kazerooni & Lim, 2016 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
# 
# Rationale: For grouped time series, (e.g., per person), collect k time points
#            of interest within specified windows, if any of the k time points
#            are missing, exclude that group (person) from the cohort
#-----------------------------------------------------------------------------#

#' @title Kazerooni et al. 2016 Weight Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measures in `DF`, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in `DF`
#' @param startPoint string corresponding to the name of the column in `DF` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then `startPoint` should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. `c(0, 182.5, 365)` for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is `c(0, 182.5, 365)` according to Kazerooni et al. 2016
#' @param windows numeric list of two vectors of measurement collection windows to use around each time point in `t`. E.g. Kazerooni et al. 2016 use `c(30, 0, 0)` for the lower bound and `c(0, 0, 185)` for the upper bound at t of `c(0, 90, 180)`, implying that the closest measurement to t[1] (=0) will be within the window [-30, 0], then the closest to t[2] (=90) will be within [90, 180], t[3] (=180) within (180, 365]
Kazerooni2016.f <- function(DF,
                            id,
                            measures,
                            tmeasures,
                            startPoint,
                            t = c(0, 90, 180),
                            windows = list(LB = c(30, 0, 0),
                                           UB = c(0, 90, 185))) {
  
  if (!require(dplyr)) install.packages("dplyr")
  
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
    if (!is.list(windows)) {
      stop(
        print("windows must be placed into a list object")
      )
    }
  )
  
  # compute difference in time between t0 and all t_j
  mutate_call <- lazyeval::interp(~as.numeric(difftime(a, b, units = "days")), 
                                  a = as.name(tmeasures),
                                  b = as.name(startPoint))
  
  DF <- DF %>% mutate_(.dots = setNames(list(mutate_call), "time"))
  
  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {
    meas_tn[[i]] <- DF %>%
      filter(time >= t[i] - windows$LB[i] & time <= t[i] + windows$UB[i]) %>%
      group_by(!!!syms(id)) %>%
      arrange(abs(time - t[i])) %>%
      slice(1)
  }
  
  # count number of time points available for each subject i
  do.call(rbind, meas_tn) %>%
    arrange(!!!syms(c(id, tmeasures))) %>%
    group_by(!!!syms(id)) %>%
    filter(max(row_number()) >= 3) %>% # must have all 3 time points
    ungroup()
}