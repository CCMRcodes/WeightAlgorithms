#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Buta et al. 2018 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
# 
# Rationale: This algorithm is dependent on BMI, For grouped time series, 
#            (e.g., per person), if person has <= 1 BMI measure, then exclude 
#            them from the cohort. Would not recommend using this, as most 
#            modern analytical methods can handle imbalanced groups. 
#-----------------------------------------------------------------------------#

#' @title Buta et al. 2018 Measurment Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measurements in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers numeric vector corresponding to the upper and lower bound of `measure` for each time entry. Default is `c(11, 70)` for BMI measurements according to "Buta et al. 2018".
Buta2018.f <- function(DF,
                       id,
                       measures,
                       tmeasures,
                       outliers = c(11, 70)) {
  
  if (!require(data.table)) install.packages("data.table")
  if (!require(dplyr))      install.packages("dplyr")
  
  tryCatch(
    if (!is.numeric(DF[[measures]])) {
      stop(
        print("measure data must be a numeric vector")
      )
    }
  )
  
  tryCatch(
    if (!is.numeric(outliers)) {
      stop(
        print("outliers must be numeric")
      )
    }
  )
  
  # remove NA measures
  DF <- DF[!is.na(DF[[measures]]), ]
  
  # Remove Outlier Measures
  DF <- DF[DF[[measures]] >= outliers[1] & DF[[measures]] <= outliers[2], ]
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  data.table::setkeyv(DT, id)
  
  # Set Order
  data.table::setorderv(DT, c(id, tmeasures), c(1, 1))
  
  # find max count for person i, if k = 1, remove person
  DT[, k := .(.N), by = .(PatientICN)]
  DT <- DT[k > 1, -c("k")]
  DT
}