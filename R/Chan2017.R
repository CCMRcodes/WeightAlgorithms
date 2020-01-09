#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Chan et al. 2017 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
# 
# Rationale: For grouped time series, (e.g., per person)
#            Algorithm computes mean and SD of measurements, per person
#            then removes measurements > SDthreshold + mean
#            Based on work by Chan & Raffa, 2017
#-----------------------------------------------------------------------------#

#' @title Chan 2017 Measurment Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measurements in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers object of type `list` with numeric inputs corresponding to the upper and lower bound for each time entry. Default is `list(LB = 50, UB = 750)`
#' @param SDthreshold numeric scalar to be multiplied by the `SDMeasures` per `id`. E.g., from Chan 2017, "...weights greater than 3 standard deviations above the mean..." implies `SDthreshold`= 3
Chan2017.f <- function(DF,
                       id,
                       measures,
                       tmeasures,
                       outliers = list(LB = 50, UB = 750),
                       SDthreshold = 3) {
  
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
  
  # Step 1: Set outliers to NA
  DT[,
     measures_aug_ := ifelse(get(measures) < outliers[[1]]
                             | get(measures) > outliers[[2]], 
                             NA,
                             get(measures))
     ]
  
  # calc mean of measures per group
  DT[, meanMeasures := mean(measures_aug_, na.rm = TRUE), by = id]
  # calc SD of weight per group
  DT[, SDMeasures := sd(measures_aug_,   na.rm = TRUE), by = id]
  
  # calc UB and LB
  DT[, LB := meanMeasures - (SDthreshold * SDMeasures)]
  DT[, UB := meanMeasures + (SDthreshold * SDMeasures)]
  
  # Step 2: outliers bounded by SDthreshold
  DT[,
     measures_aug_ := ifelse(measures_aug_ < LB | measures_aug_ > UB,
                             NA, 
                             measures_aug_)
     ]
  
  DT <- DT %>% select(-UB, -LB)
  DT
}