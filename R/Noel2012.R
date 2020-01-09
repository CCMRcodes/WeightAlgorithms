#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Noel et al., 2012 version
# 
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
# 
# Rationale: For grouped time series, (e.g., per person), collect all
#            measurements within a certain time frame, then break those down
#            into Fiscal Quarters. For each person and each Fiscal Quarter,
#            calculate the median measurement over all measurements by Fiscal
#            Quarter.
#-----------------------------------------------------------------------------#

#' @title Noel 2012 Measurment Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param measures string corresponding to the name of the column of measurements in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param outliers object of type `list` with numeric inputs corresponding to the upper and lower bound for each time entry. Default is `list(LB = 70, UB = 700)`
#' @param fiscal_start integer to be passed to `lubridate::quarter()`. Defaults to 10, indicating October, the Federal Fiscal Year starting month.
#TODO: add @param collapse = FALSE - aggregate to Fiscal Quarter? Default is FALSE, returning `DF` appended with `Qmedian` to be collapsed after calling the function.
Noel2012.f <- function(DF,
                       id,
                       measures,
                       tmeasures,
                       outliers = c(70, 700),
                       fiscal_start = 10) {
  
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
    if (!is.numeric(outliers)) {
      stop(
        print("outliers must be numeric")
      )
    }
  )
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  
  # Step 1: Set outliers to NA
  DT[,
     output := ifelse(get(measures) < outliers[1]
                      | get(measures) > outliers[2], 
                      NA,
                      get(measures))
     ]
  
  # Step 2: Set Fiscal Years and Quarters
  DT[,
     FYQ := lubridate::quarter(get(tmeasures),
                               with_year = TRUE,
                               fiscal_start = fiscal_start)
     ]
  
  # Step 3: aggregate median weight by ID, Fiscal Year and Quarter
  key_cols <- c(id, "FYQ")
  setkeyv(DT, key_cols)
  DT[, `:=` (Qmedian = median(output, na.rm = TRUE)), keyby = key_cols]
  DT
}