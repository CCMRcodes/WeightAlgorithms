#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Kazerooni & Lim, 2016 version
#
# Author: Richard Ryan Evans
# Development Start Date: 04/22/2019
#
# Algorithm reconstructed from methods section in the publication:
# Kazerooni R, Lim J. Topiramate-associated weight loss in a veteran population.
# Military Medicine [electronic article]. 2016;181(3):283–286.
# (https://academic.oup.com/milmed/article/181/3/283-286/4159223). (Accessed
# December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person), collect k time points
#            of interest within specified windows, if any of the k time points
#            are missing, exclude that group (person) from the cohort
#-----------------------------------------------------------------------------#

#' Kazerooni & Lim, 2016 Weight Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person), \code{kazerooni} collects k time
#' points of interest within specified windows, if any of the k time points are
#' missing, exclude that group (person) from the cohort.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}.
#' @param startPoint string corresponding to the name of the column in \code{df}
#'   holding the time at which subsequent measurement dates will be assessed,
#'   should be the same for each person. Eg., if t = 0 (\code{t[1]}) corresponds
#'   to an index visit held by the variable \code{VisitDate}, then
#'   \code{StartPoint} should be set to \code{VisitDate}.
#' @param t numeric vector of time points to collect measurements, e.g.
#'   \code{c(0, 182.5, 365)} for measure collection at t = 0, t = 180 (6 months
#'   from t = 0), and t = 365 (1 year from t = 0). Default is
#'   \code{c(0, 182.5, 365)} according to Kazerooni & Lim 2016
#' @param windows numeric list of two vectors of measurement collection windows
#'   to use around each time point in \code{t}. E.g. Kazerooni & Lim 2016 use
#'   \code{c(30, 0, 0)} for the lower bound and \code{c(0, 0, 185)} for the
#'   upper bound at \code{t} of \code{c(0, 90, 180)}, implying that the closest
#'   measurement to \code{t[1]} (=0) will be within the window [-30, 0], then
#'   the closest to \code{t[2]} (=90) will be within [90, 180], \code{t[3]}
#'   (=180) within (180, 365].
#' @return output is a data frame with 1 extra column, \code{time}, indicating
#'   the actual time of captured measurement data as dictated by \code{t} +/-
#'   \code{windows}. If the group (person) defined by \code{id} does not have
#'   \code{length(t)} measurements, they are removed from the resultant data
#'   frame. Further, the result will contain \code{length(t)} rows per
#'   group \code{id}.
#' @examples
#' library(dplyr)
#' data(cdw1000)
#'
#' kazerooni_df <- kazerooni(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    startPoint = "VisitDate"
#'   )
#'
#' # dplyr::glimpse(kazerooni_df)
#'
#' compare_df <- bind_rows(
#'   "Input"  = cdw1000 %>% select(Weight),
#'   "Output" = kazerooni_df %>% select(Weight),
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
kazerooni <- function(df,
                      id,
                      measures,
                      tmeasures,
                      startPoint,
                      t = c(0, 90, 180),
                      windows = list(LB = c(30, 0, 0),
                                     UB = c(0, 90, 185))) {

  tryCatch(
    if (class(df[[tmeasures]])[1] != class(df[[startPoint]])[1]) {
      stop(
        print(
          paste0(
            "date type of tmeasures (",
            class(DF[[tmeasures]]),
            ") != date type of startPoint (",
            class(DF[[startPoint]])[1],
            ")"
          )
        )
      )
    }
  )

  tryCatch(
    if (class(t) != "numeric") {
      stop(
        print("t parameter must be a numeric vector")
      )
    }
  )

  tryCatch(
    if (!is.list(windows)) {
      stop(
        print("windows must be placed into a list object")
      )
    }
  )

  # compute difference in time between t0 and all t_j
  id         <- rlang::sym(id)
  tmeasures  <- rlang::sym(tmeasures)
  startPoint <- rlang::sym(startPoint)

  df <- df %>%
    mutate(
      dtime = as.numeric(
        difftime(
          !!tmeasures, !!startPoint,
          tz = "utc", units = "days"
        )
      )
    )

  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {
    meas_tn[[i]] <- df %>%
      filter(dtime >= t[i] - windows$LB[i] & dtime <= t[i] + windows$UB[i]) %>%
      group_by(!!id) %>%
      arrange(abs(dtime - t[i])) %>%
      slice(1)
  }

  # count number of time points available for each subject i
  do.call(rbind, meas_tn) %>%
    arrange(!!id, !!tmeasures) %>%
    group_by(!!id) %>%
    filter(max(row_number()) >= 3) %>% # must have all 3 time points
    ungroup()
}

