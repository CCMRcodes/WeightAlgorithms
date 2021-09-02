#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Goodrich 2016 version
#
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
#
# Algorithm reconstructed from methods section in the publication:
# Goodrich DE, Klingaman EA, Verchinina L, et al. Sex differences in weight loss
# among veterans with serious mental illness: Observational study of a national
# weight management program. Women's Health Issues [electronic article].
# 2016;26(4):410â419.
# (https://linkinghub.elsevier.com/retrieve/pii/S1049386716300366).
# (Accessed December 6, 2019
#
# Rationale: For grouped time series, (e.g., per person)
#            Define time points and collect measurements, optionally applying
#            windows for possible data points for each time point,
#            then removing outliers. Based on work by Goodrich et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney et al. 2016 served as a basis for this function/algorithm
#
#-----------------------------------------------------------------------------#

#----------------------- weight change moving forward --------------------------

#' Flag Outliers Within Group
#'
#' \code{big_change_outliers} takes a grouped time-series as a data frame and
#' identifies outlier measurements if greater than some threshold chosen by the
#' user. This threshold is used as a cutoff for higher than (or lower than)
#' expected changes in measurements from one time point j to time point j + 1,
#' by person or group.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param wtchng_thresh numeric scalar used as a cutoff for higher than (or
#'   lower than) expected weight changes from one time point j to time point
#'   j + 1, by person or group. Default is 100.
#' @return data frame with outliers set to \code{NA} if measurements fall
#'   outside the range of \code{wtchng_thresh}. Output column designated
#'   \code{measout}.
#' @examples
#' library(dplyr)
#' data(cdw1000)
#'
#' cdw1000_100 <- big_change_outliers(
#'   df = cdw1000,
#'   id = "id",
#'   measures = "Weight",
#'   tmeasures = "WeightDate"
#' )
#'
#' # reduce threshold
#' cdw1000_50 <- big_change_outliers(
#'   df = cdw1000,
#'   id = "id",
#'   measures = "Weight",
#'   tmeasures = "WeightDate",
#'   wtchng_thresh = 50
#' )
#'
#' bind_rows(`100` = cdw1000_100, `50` = cdw1000_50, .id = "Threshold") %>%
#'   select(Threshold, measout) %>%
#'   na.omit() %>%
#'   group_by(Threshold) %>%
#'   summarize(n = n(), mean = mean(measout), sd = sd(measout))
big_change_outliers <- function(df,
                                id,
                                measures,
                                tmeasures,
                                wtchng_thresh = 100) {

  # convert to data.table
  DT <- data.table::as.data.table(df)
  setkeyv(DT, id)

  setorderv(DT, c(id, tmeasures))

  # fast lead with data.table
  forward <- NULL
  DT[,
      `:=`(
        forward = shift(
          get(measures),
          n = 1L,
          fill = NA,
          type = "lead"
        )
      ),
     by = id
     ][]

  # remove weight changes > wtchng_thresh
  outlier <- NULL
  DT$outlier <- abs(DT[[measures]] - DT$forward) > wtchng_thresh
  DT <- DT %>%
    mutate(
      measout = case_when(
        outlier        ~ NA_real_,
        is.na(outlier) ~ DT[[measures]],
        TRUE           ~ DT[[measures]]
      )
    ) %>%
    select(-forward, -outlier)

  DT
}

#-------------------- Add weight change to janney ---------------------

#' Goodrich et al. 2016 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person) \code{goodrich} defines time
#' points and collects measurements, optionally applying windows for possible
#' data points for each time point, then proceeds to flag and/or remove within
#' group outliers. Based on work by Goodrich et al. 2016.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{weights}.
#' @param id string corresponding to the name of the column of group identifiers
#'   in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}.
#' @param start_point string corresponding to the name of the column in
#'   \code{df} holding the time at which subsequent measurement dates will be
#'   assessed, should be the same for each person. Eg., if t = 0 (\code{t[1]})
#'   corresponds to an index visit held by the variable \code{VisitDate}, then
#'   \code{start_point} should be set to \code{VisitDate}.
#' @param t numeric vector of time points to collect measurements, eg.
#'   \code{c(0, 182.5, 365)} for measure collection at t = 0, t = 180 (6 months
#'   from t = 0), and t = 365 (1 year from t = 0). Default is
#'   \code{c(0, 182.5, 365)} according to Janney et al. 2016.
#' @param windows numeric vector of measurement collection windows to use around
#'   each time point in \code{t}. Eg. Janney et al. 2016 use
#'   \code{c(30, 60, 60)} for \code{t} of \code{c(0, 182.5, 365)}, implying that
#'   the closest measurement t = 0 will be collected 30 days prior to and 30
#'   days post \code{startPoint}. Subsequent measurements will be collected 60
#'   days prior to and 60 days post t0+182.5 days, and t0+365 days.
#' @param outliers optional. object of type \code{list} with numeric inputs
#'   corresponding to the upper and lower bound for each time entry in parameter
#'   \code{t}. Default is \code{list(LB = c(80, 80, 80), UB = c(500, 500, 500))}
#'   for \code{t = c(0, 182.56, 365)}, differing between baseline and subsequent
#'   measurment collection dates. If not specified then only the subsetting and
#'   window functions will be applied.
#' @param wtchng_thresh numeric scalar used as a cutoff for higher than (or
#'   lower than) expected weight changes from one time point j to time point
#'   j + 1, by person or group. Default is 100.
#' @param exclude_subject logical. If TRUE remove groups meeting the exclusion
#'   criteria.
#' @return returns a data frame with 3 new columns, \code{time} (the actual time
#'   of captured measurement data as dictated by \code{t} +/- \code{windows}),
#'   \code{tmeasures} (which time-point the data in \code{measout} refers to
#'   as dictated by \code{t}), and \code{measout} (the "cleaned" measure). The
#'   result will contain up to \code{length(t)} rows per group \code{id}. If
#'   \code{exclude_subject} is set to TRUE, those meeting the exclusion criteria
#'   are removed from the resultant data frame.
#' @examples
#' library(dplyr)
#' data(cdw1000)
#'
#' goodrich_df <- goodrich(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    start_point = "VisitDate"
#'   )
#'
#' # dplyr::glimpse(goodrich_df)
#'
#' compare_df <- bind_rows(
#'   "Input"  = cdw1000 %>% select(Weight),
#'   "Output" = goodrich_df %>%
#'      select(measout) %>%
#'      na.omit() %>%
#'      rename(Weight = measout),
#'   .id = "IO"
#' )
#'
#' compare_df %>%
#'   group_by(IO) %>%
#'   summarize(
#'     n      = n(),
#'     mean   = mean(Weight),
#'     sd     = sd(Weight),
#'     min    = min(Weight),
#'     median = median(Weight),
#'     max    = max(Weight)
#'   )
goodrich <- function(df,
                     id,
                     measures,
                     tmeasures,
                     start_point,
                     t = c(0, 182, 365),
                     windows = c(30, 60, 60),
                     outliers = list(LB = c(80, 80, 80),
                                     UB = c(500, 500, 500)),
                     wtchng_thresh = 100,
                     exclude_subject = FALSE){

  WindowsAndOutliers.df <-
    janney(
      df,
      id,
      measures,
      tmeasures,
      start_point,
      t = t,
      windows = windows,
      outliers = outliers
    )

  measout <- NULL
  lookForwardAndRemove.df <-
    big_change_outliers(
      df = WindowsAndOutliers.df,
      id = id,
      measures = "measout",
      tmeasures = tmeasures,
      wtchng_thresh = wtchng_thresh
    )

  if (exclude_subject) {

    FlagForRemoval <- NULL
    excluded.df <- lookForwardAndRemove.df %>%
      filter(is.na(measout)) %>%
      select(id) %>%
      distinct() %>%
      mutate(FlagForRemoval = 1) %>%
      right_join(lookForwardAndRemove.df, by = id) %>%
      filter(is.na(FlagForRemoval)) %>%
      select(-FlagForRemoval)

    return(as.data.frame(excluded.df))

  } else {

    return(as.data.frame(lookForwardAndRemove.df))

  }
}

