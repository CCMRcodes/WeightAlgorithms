#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Breland et al. 2017 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
#
# Algorithm reconstructed from methods section in the publication:
# Breland JY, Phibbs CS, Hoggatt KJ, et al. The obesity epidemic in the veterans
# health administration: Prevalence among key populations of women and men
# veterans. Journal of General Internal Medicine [electronic article].
# 2017;32(1):11-17. (http://link.springer.com/10.1007/s11606-016-3962-1).
# (Accessed December 6, 2019)
# 
# Rationale: For grouped time series, (e.g., per person)
#            Examine ratios of forward and backward measurements per person,
#            if measurement meets outlier criteria, the measurement is removed
#            Based on work by Breland et al. 2017
#-----------------------------------------------------------------------------#

#' @title Breland 2017 Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measurements in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers object of type `list` with numeric inputs corresponding to the upper and lower bound for each time entry. Default is `list(LB = 75, UB = 700)`
#' @param RatioThresholds list of 2 lists, 1 for each ratio (prior and post measurements), with numeric inputs corresponding to the lower bound and upper bound for flagging erroneous measurements. Default lower bound is 0.67 and upper bound 1.50, same as Breland et al. 2017
Breland2017.f <- function(DF,
                          id,
                          measures,
                          tmeasures,
                          outliers = list(LB = 75, UB = 700),
                          RatioThresholds = list(Ratio1 = list(low = 0.67, 
                                                               high = 1.50),
                                                 Ratio2 = list(low = 0.67, 
                                                               high = 1.50))) {
  
  if (!require(dplyr))      install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  
  tryCatch(
    if (!is.numeric(DF[[measures]])) {
      stop(
        print("measure data must be a numeric vector")
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
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  # Round to 2 decimal places
  DT[, measures_aug_ := round(get(measures), 2)]
  
  # Set outliers to NA
  DT[,
     measures_aug_ := ifelse(measures_aug_ < outliers[[1]]
                             | measures_aug_ > outliers[[2]], 
                             NA,
                             measures_aug_)
     ]
  
  # Ratio1: current weight/prior weight (backward)
  # Ratio2: current weight/next weight (forward)
  setorderv(DT, c(id, tmeasures))
  
  # fast lead and lag with data.table
  DT[, "backward" := shift(measures_aug_, 1, NA, "lag"), by = id]
  DT[, "forward"  := shift(measures_aug_, 1, NA, "lead"), by = id]
  
  DT <- DT %>%
    mutate(
      Ratio1 = measures_aug_ / backward,
      R1_ind = case_when(
        Ratio1 <= RatioThresholds[[1]][[1]] ~ -1L,
        Ratio1 >= RatioThresholds[[1]][[2]] ~  1L,
        TRUE ~ 0L
      ),
      Ratio2 = measures_aug_ / forward,
      R2_ind = case_when(
        Ratio2 <= RatioThresholds[[2]][[1]] ~ -1L,
        Ratio2 >= RatioThresholds[[2]][[2]] ~  1L,
        TRUE ~ 0L
      ),
      measures_aug_ = ifelse((R1_ind == 1 & R2_ind == 1) |
                               (R1_ind == -1 & R2_ind == -1),
                             NA,
                             measures_aug_)
    )
  DT
}