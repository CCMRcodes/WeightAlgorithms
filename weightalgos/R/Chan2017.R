#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Chan et al. 2017 version
#
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
#
# Algorithm reconstructed from methods section in the publication:
# Chan SH, Raffa SD. Examining the dose–response relationship in the veterans
# health administration’s move!® weight management program: A nationwide
# observational study. Journal of General Internal Medicine
# [electronic article]. 2017;32(S1):18–23.
# (http://link.springer.com/10.1007/s11606-017-3992-3).
# (Accessed December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person)
#            Algorithm computes mean and SD of measurements, per person
#            then removes measurements > SDthreshold + mean
#-----------------------------------------------------------------------------#

#' Chan 2017 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person) Algorithm computes mean and SD of
#' measurements, per group then removes measurements > \code{SDthreshold} + mean.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one weight on the same day,
#'   if it is a precise datetime object, there may not be more than one weight
#'   on the same day.
#' @param outliers object of type \code{list} with numeric inputs corresponding
#'   to the upper and lower bound for each time entry. Default is
#'   \code{list(LB = 50, UB = 750)}.
#' @param SDthreshold numeric scalar to be multiplied by the \code{SDMeasures}
#'   per \code{id}. E.g., from Chan 2017, "...weights greater than 3 standard
#'   deviations above the mean..." implies \code{SDMeasures = 3}.
#' @param AddInternals logical, adds additional columns to output data frame
#'   describing the grouped mean (\code{meanMeasures}) and standard deviation
#'   (\code{SDMeasures}) used interally to process the data. Defaults to FALSE.
#' @return if AddInternals is FALSE, returns input data frame with processed
#'   data as an additional column \code{measout}. if AddInternals is TRUE then
#'   it returns the input data frame with processed data \code{measout} and
#'   columns described in \code{AddInternals}.
#' @examples
#' library(dplyr)
#' data(cdw32)
#'
#' chan_df <- chan(
#'    df = cdw32,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate"
#' )
#'
#' # dplyr::glimpse(chan_df)
chan <- function(df,
                 id,
                 measures,
                 tmeasures,
                 outliers = list(LB = 50, UB = 750),
                 SDthreshold = 3,
                 AddInternals = FALSE) {

  tryCatch(
    if (!is.numeric(df[[measures]])) {
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
  DT <- data.table::as.data.table(df)
  setkeyv(DT, id)

  # Step 1: Set outliers to NA
  measout <- NULL
  DT[,
     `:=`(
         measout = ifelse(
           get(measures) < outliers[[1]] | get(measures) > outliers[[2]],
           NA,
           get(measures)
         )
       )
     ][]

  meanMeasures <- SDMeasures <- NULL
  # calc mean of measures per group
  DT[, `:=`(meanMeasures = mean(measout, na.rm = TRUE)), by = id][]
  # calc SD of weight per group
  DT[, `:=`(SDMeasures   = sd(measout,   na.rm = TRUE)), by = id][]

  # calc UB and LB
  UB <- LB <- NULL
  DT[, `:=`(LB = meanMeasures - (SDthreshold * SDMeasures))][]
  DT[, `:=`(UB = meanMeasures + (SDthreshold * SDMeasures))][]

  # Step 2: outliers bounded by SDthreshold
  DT[, `:=`(measout = ifelse(measout < LB | measout > UB, NA, measout))][]

  DT <- DT %>%
    select(-UB, -LB) %>%
    as.data.frame()

  if (AddInternals) {
    DT
  } else {
    DT %>%
      select(-meanMeasures, -SDMeasures)
  }
}

