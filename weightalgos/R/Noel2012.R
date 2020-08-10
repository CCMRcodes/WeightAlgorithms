#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Noel et al., 2012 version
#
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
#
# Algorithm reconstructed from methods section in the publication:
# Noel PH, Wang C-P, Bollinger MJ, et al. Intensity and duration of obesity-
# related counseling: Association with 5-year bmi trends among obese primary
# care patients. Obesity [electronic article]. 2012;20(4):773–782.
# (http://doi.wiley.com/10.1038/oby.2011.335). (Accessed December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person), collect all
#            measurements within a certain time frame, then break those down
#            into Fiscal Quarters. For each person and each Fiscal Quarter,
#            calculate the median measurement over all measurements by Fiscal
#            Quarter.
#-----------------------------------------------------------------------------#

#' Noel et al. 2012 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person), \code{noel} collects all
#' measurements within a certain time frame, then categorizes each time frame
#' into Fiscal Quarters. Then, for each person and each Fiscal Quarter,
#' calculate the median measurement over all measurements by Fiscal Quarter.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of group
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a \code{Date} object, there may be more than one measurement on the same
#'   day, if it is a precise date-time object (\code{?POSIXlt}), there may not
#'   be more than one measurement on the same day.
#' @param outliers object of type \code{list} with numeric inputs corresponding
#'   to the upper and lower bound for each time entry. Default is
#'   \code{list(LB = 70, UB = 700)}.
#' @param fiscal_start integer to be passed to \code{lubridate::quarter()}.
#'   Defaults to 10, indicating October, the Federal Fiscal Year starting month.
#' @param collapse = FALSE - aggregate to Fiscal Quarter? Default is
#'   \code{FALSE}, returning \code{df} appended with \code{Qmedian} to be
#'   collapsed after calling the function.
#' @return data frame with the same \code{nrow(df)} number of rows, but
#'   \code{ncol(df) + 3} columns. \code{output}, gives the \code{measures} with
#'   outliers flagged as \code{NA}; \code{FYQ}, numeric fiscal-quarter
#'   (\code{dbl}) in YYYY.Q format, e.g., 1st quarter of 2016 is 2016.1;
#'   \code{Qmedian}, median computed for each \code{id} and \code{FYQ}.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(cdw1000)
#'
#' noel_df <- noel(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    collapse = TRUE
#'   )
#'
#' # dplyr::glimpse(noel_df)
#'
#' samp <- noel_df %>%
#'   distinct(id) %>%
#'   sample_n(16) %>%
#'   pull()
#'
#' noel_df %>%
#'   filter(id %in% samp) %>%
#'   ggplot(aes(x = FYQ, y = Qmedian)) +
#'   geom_line(color = "black") +
#'   geom_point(color = "green") +
#'   facet_wrap(vars(id), ncol = 4, scales = "free_y")
noel <- function(df,
                 id,
                 measures,
                 tmeasures,
                 outliers = c(70, 700),
                 fiscal_start = 10,
                 collapse = FALSE) {

  tryCatch(
    if (!is.numeric(df[[measures]])) {
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
  DT <- data.table::as.data.table(df)

  # Step 1: Set outliers to NA
  output <- NULL
  DT[,
      `:=`(
        output = ifelse(
          get(measures) < outliers[1] | get(measures) > outliers[2],
          NA,
          get(measures)
        )
      )
     ][]

  # Step 2: Set Fiscal Years and Quarters
  FYQ <- NULL
  DT[,
      `:=`(
        FYQ = lubridate::quarter(get(tmeasures),
                                 with_year = TRUE,
                                 fiscal_start = fiscal_start)
      )
     ][]

  # Step 3: aggregate median weight by ID, Fiscal Year and Quarter
  key_cols <- c(id, "FYQ")
  setkeyv(DT, key_cols)

  Qmedian <- NULL
  DT[, `:=`(Qmedian = median(output, na.rm = TRUE)), keyby = key_cols][]
  DT$output <- NULL

  if (collapse) {
    DT %>%
      distinct(id, FYQ, Qmedian, .keep_all = TRUE)
  } else {
    DT
  }
}

