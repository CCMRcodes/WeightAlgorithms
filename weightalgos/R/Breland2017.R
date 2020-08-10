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
#-----------------------------------------------------------------------------#

#' Breland et al. 2017 Weight Cleaning Algorithm
#'
#' \code{breland} returns input data frame with processed data as additional
#' columns.
#'
#' @param df object of class data.frame, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one weight on the same day, if it
#'   precise datetime object, there may not be more than one weight on the same
#'   day.
#' @param outliers object of type \code{list} with numeric inputs corresponding
#'   to the upper and lower bound for each time entry. Default is
#'   \code{list(LB = 75, UB = 700)}.
#' @param RatioThresholds list of 2 lists, 1 for each ratio (prior and post
#'   measurements), with numeric inputs corresponding to the lower bound and
#'   upper bound for flagging erroneous measurements. Default lower bound is
#'   0.67 and upper bound 1.50, same as Breland et al. 2017.
#' @param AddInternals logical, adds additional columns to output data frame
#'   detailing the backward, forward, ratios, and ratio indicators used
#'   interally to process the data. Defaults to FALSE.
#' @return if AddInternals is FALSE, returns input data frame with processed
#'   data as an additional column \code{measout}. if AddInternals is TRUE then
#'   it returns the input data frame with processed data \code{measout} and
#'   columns described in \code{AddInternals}.
#' @examples
#' library(dplyr)
#' data(cdw32)
#'
#' breland_df <- breland(
#'    df = cdw32,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate"
#'   )
#'
#' # dplyr::glimpse(breland_df)
breland <- function(df,
                    id,
                    measures,
                    tmeasures,
                    outliers = list(LB = 75, UB = 700),
                    RatioThresholds = list(Ratio1 = list(low = 0.67,
                                                         high = 1.50),
                                           Ratio2 = list(low = 0.67,
                                                         high = 1.50)),
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

  # Round to 2 decimal places
  measout <- NULL
  DT[, `:=`(measout = round(get(measures), 2))][]

  # Set outliers to NA
  DT[,
     `:=`(
       measout = ifelse(
         measout < outliers[[1]] | measout > outliers[[2]],
         NA,
         measout
       )
      )
     ][]

  # Ratio1: current weight/prior weight (backward)
  # Ratio2: current weight/next weight (forward)
  setorderv(DT, c(id, tmeasures))

  # fast lead and lag with data.table
  backward <- forward <- NULL
  DT[, `:=`(backward = shift(measout, 1, NA, "lag")),  by = id][]
  DT[, `:=`(forward  = shift(measout, 1, NA, "lead")), by = id][]

  R1_ind <- R2_ind <- NULL

  DT <- DT %>%
    dplyr::mutate(
      Ratio1 = measout / backward,
      R1_ind = dplyr::case_when(
        Ratio1 <= RatioThresholds[[1]][[1]] ~ -1L,
        Ratio1 >= RatioThresholds[[1]][[2]] ~  1L,
        TRUE ~ 0L
      ),
      Ratio2 = measout / forward,
      R2_ind = dplyr::case_when(
        Ratio2 <= RatioThresholds[[2]][[1]] ~ -1L,
        Ratio2 >= RatioThresholds[[2]][[2]] ~  1L,
        TRUE ~ 0L
      ),
      measout = ifelse((R1_ind == 1 & R2_ind == 1) |
                               (R1_ind == -1 & R2_ind == -1),
                        NA,
                        measout)
    )

  if (AddInternals) {
    DT
  } else {
    DT %>%
      dplyr::select(-c(backward:R2_ind))
  }
}

