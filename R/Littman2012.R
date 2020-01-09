#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Littman 2012 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
# 
# Rationale: For grouped time series, (e.g., per person)
#            clean numeric measurements collected longitudinally
#  	     first, removing outliers, then examining within-person
#            measurement mean and standard deviation, if measurement
#            is found to be above SDthreshold, then measurement is
#            set to missing. Based on work by Littman et al. 2012.
#
#-----------------------------------------------------------------------------#

#' @title Littman 2012 Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measurements in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers object of type `list` with numeric inputs corresponding to the upper and lower bound for each time entry. Default is `list(LB = c(75), UB = c(600))`
#' @param SDthreshold numeric scalar to be multiplied by the `meanMeasures` per `id`. E.g., from Littman 2012, "...We excluded any weight measurements that met the following 2 criteria: 1) the difference between the mean weight and weight in question was greater than the SD and 2) the SD was greater than 10% of the mean...." implies `SDthreshold`= 0.10
#' @return input data.frame with additional columns: `InputMeasurement`, the original weight data; `OutputMeasurement`, algorithm output; `meanWeight`, mean(weights) per `id`; `SDWeight`, SD(weights) per `id`; `SD_threshold_`, Mean(weights) * `SDthreshold`
Littman2012.f <- function(DF,
                          id,
                          measures,
                          tmeasures,
                          outliers = list(LB = c(75), UB = c(600)),
                          SDthreshold = 0.10) {
  
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
  
  # Compute the mean and sd for each persons weight, if more than 1 value
  # if not more than 1 value, mean is finite, sd is undefined
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  # first set outliers to NA
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
  # calc SD threshold
  DT[, SD_threshold_ := SDthreshold * meanMeasures, by = id]
  
  # exclude any measurements that meet the following 2 criteria: 
  # 1) the difference between the meanMeasures and measures_aug_ in 
  # question is greater than the SDMeasures
  # AND
  # 2) the SDMeasures was greater than SD_threshold_ of the mean
  DT[, cond1 := ifelse(abs(measures_aug_ - meanMeasures) > SDMeasures, T, F)]
  DT[, cond2 := ifelse(SDMeasures > SD_threshold_, T, F)]
  DT[, measures_aug_ := ifelse((cond1 & cond2), NA, measures_aug_)]
  
  # return augmented measurements
  DF <- as.data.frame(DT)
  names(DF)[names(DF) == measures] <- "InputMeasurement"
  names(DF)[names(DF) == "measures_aug_"] <- "OutputMeasurement"
  DF
}