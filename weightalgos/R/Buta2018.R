#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Buta et al. 2018 version
#
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
#
# Algorithm reconstructed from methods section in the publication:
# Buta E, Masheb R, Gueorguieva R, et al. Posttraumatic stress disorder
# diagnosis and gender are associated with accelerated weight gain trajectories
# in veterans during the post-deployment period. Eating Behaviors
# [electronic article]. 2018;29-13.
# (https://linkinghub.elsevier.com/retrieve/pii/S1471015317300521).
# (Accessed December 6, 2019)
#
# Rationale: This algorithm is dependent on BMI, For grouped time series,
#            (e.g., per person), if person has <= 1 BMI measure, then exclude
#            them from the cohort. Would not recommend using this, as most
#            modern analytical methods can handle imbalanced groups.
#-----------------------------------------------------------------------------#

#' Buta et al. 2018 Weight Cleaning Algorithm
#'
#' \code{buta} returns a processed data frame with full removal of \code{id}
#' groups, if not meeting criteria for inclusion. It is dependent on patient
#' body mass index (BMI).
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one weight on the same day, if it
#'   precise datetime object, there may not be more than one weight on the same
#'   day.
#' @param outliers numeric vector corresponding to the upper and lower bound of
#'   \code{measures} for each time entry. Default is \code{c(11, 70)} for BMI
#'   measurements according to Buta et al. 2018.
#' @return returns a processed data frame with full removal of \code{id}
#'   groups and weight measurements, if not meeting criteria for inclusion.
#' @examples
#' library(dplyr)
#' data(cdw32)
#'
#' cdw32 <- cdw32 %>%
#'   mutate(BMI = 703 * (Weight / (Height ^ 2)))
#'
#' buta_df <- buta(
#'    df = cdw32,
#'    id = "id",
#'    measures = "BMI",
#'    tmeasures = "WeightDate"
#'   )
#'
#' # dplyr::glimpse(buta_df)
buta <- function(df,
                 id,
                 measures,
                 tmeasures,
                 outliers = c(11, 70)) {

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

  # remove NA measures
  df <- df[!is.na(df[[measures]]), ]

  # Remove Outlier Measures
  df <- df[df[[measures]] >= outliers[1] & df[[measures]] <= outliers[2], ]

  # convert to data.table
  DT <- data.table::as.data.table(df)
  data.table::setkeyv(DT, id)

  # Set Order
  data.table::setorderv(DT, c(id, tmeasures), c(1, 1))

  # find max count for person i, if k = 1, remove person
  k <- NULL
  DT[, `:=`(k = .N), by = id][]
  DT <- DT[k > 1, -c("k")]
  as.data.frame(DT)
}

