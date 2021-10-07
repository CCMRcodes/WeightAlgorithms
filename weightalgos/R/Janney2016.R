#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Janney 2016 version
#
# Algorithm reconstructed from methods section in the publication:
# Janney CA, Kilbourne AM, Germain A, et al. The influence of sleep disordered
# breathing on weight loss in a national weight management program. Sleep [elec-
# tronic article]. 2016;39(1):59-65.
# (https://academic.oup.com/sleep/article/39/1/59/2726067). (Accessed December
# 6, 2019)
#
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
#
# Rationale: For grouped time series, (e.g., per person)
#            Define time points and collect measurements, optionally applying
#            windows for possible data points for each time point,
#            then removing outliers. Based on work by Janney et al. 2016,
#            the rationale can be applied to similar data and thus, work by
#            Janney served as a basis for this function/algorithm
#
# Requires MeasureWindows.R and MeasureRemoveOutliers.R to be in the same
# directory as janney (this file - Janney2016.R)
#-----------------------------------------------------------------------------#

#-------------------------------- set windows --------------------------------#

#' Collect Weight Measures from Arbitrary Time-Points
#'
#' \code{meas_collect} For grouped time series, (e.g., per person) define time
#' points and collect measurements, optionally applying windows for possible
#' data points for each time point.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures
#'   in \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}.
#' @param start_point string corresponding to the name of the column in \code{df}
#'   holding the time at which subsequent measurement dates will be assessed,
#'   should be the same for each person. Eg., if \code{t = 0} (\code{t[1]})
#'   corresponds to an index visit held by the variable \code{VisitDate}, then
#'   \code{start_point} should be set to \code{VisitDate}.
#' @param t numeric vector of time points to collect measurements, eg.
#'   \code{c(0, 182.5, 365)} for measure collection at \code{t = 0},
#'   \code{t = 180} (6 months from \code{t = 0}), and \code{t = 365} (1 year
#'   from \code{t = 0}). Default is \code{c(0, 182.5, 365)} according to Janney
#'   et al. 2016
#' @param windows numeric vector of measurement collection windows to use around
#'   each time point in \code{t}. Eg. Janney et al. 2016 use
#'   \code{c(30, 60, 60)} for \code{t} of \code{c(0, 182.5, 365)}, implying that
#'   the closest measurement \code{t = 0} will be collected 30 days prior to and
#'   30 days post \code{start_point}. Subsequent measurements will be collected
#'   60 days prior to and 60 days post t0+182.5 days, and t0+365 days.
#' @return returns a data frame reduced by the inclusion criteria defined in
#'   \code{t} and \code{windows}, the resultant dimensions beng
#'   \code{ncol(df) + 2} columns and up to \code{length(t)} rows per group.
meas_collect <- function(df,
                         id,
                         measures,
                         tmeasures,
                         start_point,
                         t = c(0, 182, 365),
                         windows = c(30, 60, 60)) {

  tryCatch(
    if (class(df[[tmeasures]])[1] != class(df[[start_point]])[1]) {
      stop(
        print(
          paste0(
            "date type of tmeasures (",
            class(df[[tmeasures]]),
            ") != date type of start_point (",
            class(df[[start_point]])[1],
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
    if (class(windows) != "numeric") {
      stop(
        print("windows parameter must be a numeric vector")
      )
    }
  )

  tmeasures  <- rlang::sym(tmeasures)
  start_point <- rlang::sym(start_point)

  dtime <- NULL
  df <- df %>%
    mutate(
      dtime = as.numeric(
        difftime(
          !!tmeasures, !!start_point,
          tz = "utc", units = "days"
        )
      )
    )

  # convert to data.table
  DT <- data.table::as.data.table(df)
  setkeyv(DT, id)

  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {
    # find measurement closest to t[i]
    x <- DT[DT[, .I[abs(t[i] - dtime) == min(abs(t[i] - dtime))],
               by = eval(id)]$V1]
    # apply time outlier window/filter
    x <- x[dtime >= (t[i] - windows[i]) & dtime <= (t[i] + windows[i]), ]

    # reconcile duplicates
    x <- x[x[, .I[1], by = eval(id)]$V1] # take the first

    # tidy up
    meas_tn[[i]] <- x %>%
      as.data.frame() %>%
      mutate(measureTime = paste0("t_", t[i]))
  }

  id <- rlang::sym(id)

  # return result
  do.call(rbind, meas_tn) %>%
    arrange(!!id, !!tmeasures)
}

#------------------------------ Remove Outliers ------------------------------#

#' Outlier Filter
#'
#' \code{flag_out} flags outliers. To be applied following the result of a
#' call to \code{meas_collect}. "Flag" may not be the most appropriate choice
#' of words as it actually sets outlier measurements to \code{NA}.
#'
#' @param df object of class data.frame, containing \code{id} and
#'   \code{weights}.
#' @param measures string corresponding to the name of the column of measures
#'   in \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param t numeric vector of time points to collect measurements,
#'   eg. \code{c(0, 182.5, 365)} for measure collection at \code{t = 0},
#'   \code{t = 180} (6 months from \code{t = 0}), and \code{t = 365} (1 year
#'   from \code{t = 0}). Default is \code{c(0, 182.5, 365)} according to
#'   Janney et al. 2016
#' @param outliers object of type list with numeric inputs corresponding to the
#'   upper and lower bound for each time entry in parameter \code{t}. Default is
#'   \code{list(LB = c(91, 72, 72), UB = c(600, 650, 650))} for
#'   \code{t = c(0, 182.56, 365)}, differing between baseline and subsequent
#'   measurement collection dates
#' @return data frame with \code{measures} flagged as \code{outliers}. The
#'   result is the same number of rows as df, and 1 extra column, named
#'   \code{measout}.
flag_out <- function(df,
                     measures,
                     t = c(0, 182.5, 365),
                     outliers = list(LB = c(91, 72, 72),
                                     UB = c(600, 650, 650))) {

  tryCatch(
    if (class(df[[measures]]) != "numeric") {
      stop(
        print("weight data must be a numeric vector")
      )
    }
  )

  tryCatch(
    if (class(outliers) != "list" & !is.null(outliers)) {
      stop(
        print("outliers must be placed into a list object")
      )
    }
  )

  bounds.df <- data.frame(
    t = paste0("t_", t),
    LB = outliers[[1]],
    UB = outliers[[2]],
    stringsAsFactors = FALSE
  )

  a <- rlang::sym(measures)
  meas_updated <- rlang::quo_name("measout")

  UB <- LB <- NULL
  df <- df %>%
    left_join(bounds.df, by = c("measureTime" = "t")) %>%
    mutate(!!meas_updated := ifelse(!!a < LB | !!a > UB, NA, !!a)) %>%
    select(-LB, -UB)

  df
}

#--------------------- Windows + Outlier Removal/jannney ----------------------#

#' Janney et al. 2016 Weight Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person) Define time points and collect
#' measurements, optionally applying windows for possible data points for each
#' time point, then removing outliers. Based on work by Janney et al. 2016.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{weights}.
#' @param id string corresponding to the name of the column of group identifiers
#'   in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}.
#' @param start_point string corresponding to the name of the column in \code{df}
#'   holding the time at which subsequent measurement dates will be assessed,
#'   should be the same for each group. Eg., if \code{t = 0} (\code{t[1]})
#'   corresponds to an index visit held by the variable \code{VisitDate}, then
#'   \code{start_point} should be set to \code{VisitDate}.
#' @param t numeric vector of time points to collect measurements, eg.
#'   \code{c(0, 182.5, 365)} for measure collection at \code{t = 0},
#'   \code{t = 180} (6 months from \code{t = 0}), and \code{t = 365} (1 year
#'   from \code{t = 0}). Default is \code{c(0, 182.5, 365)} according to
#'   Janney et al. 2016
#' @param windows numeric vector of measurement collection windows to use around
#'   each time point in \code{t}. Eg. Janney et al. 2016 use
#'   \code{c(30, 60, 60)} for \code{t} of \code{c(0, 182.5, 365)}, implying that
#'   the closest measurement \code{t = 0} will be collected 30 days prior to and
#'   30 days post start_point. Subsequent measurements will be collected 60 days
#'   prior to and 60 days post t0+182.5 days, and t0+365 days.
#' @param outliers optional. object of type \code{list} with numeric inputs
#'   corresponding to the upper and lower bound for each time entry in parameter
#'   \code{t}. Default is \code{list(LB = c(91, 72, 72), UB = c(600, 650, 650))}
#'   for \code{t = c(0, 182.56, 365)}, differing between baseline and subsequent
#'   measurment collection dates. If not specified then only the subsetting and
#'   window functions will be applied.
#' @return returns a data frame reduced by the inclusion criteria defined in
#'   \code{t}, \code{windows} and \code{outliers}, the resultant dimensions
#'   being \code{ncol(df) + 2} columns and up to \code{length(t)} rows per
#'   group. Output measurements held in column \code{measout}.
#' @examples
#' library(dplyr)
#' data(cdw1000)
#'
#' janney_df <- janney(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    start_point = "VisitDate"
#'   )
#'
#' # dplyr::glimpse(janney_df)
#'
#' compare_df <- bind_rows(
#'   "Input"  = cdw1000 %>% select(Weight),
#'   "Output" = janney_df %>%
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
janney <- function(df,
                   id,
                   measures,
                   tmeasures,
                   start_point,
                   t = c(0, 182.5, 365),
                   windows = c(30, 60, 60),
                   outliers = list(LB = c(91, 72, 72),
                                   UB = c(600, 650, 650))) {

  windowsApplied.df <- meas_collect(df = df,
                                    id = id,
                                    measures = measures,
                                    tmeasures = tmeasures,
                                    start_point = start_point,
                                    t = t,
                                    windows = windows)

  outliersRemoved.df <- flag_out(df = windowsApplied.df,
                                 measures = measures,
                                 t = t,
                                 outliers = outliers)
  # return "cleaned" data
  outliersRemoved.df
}

