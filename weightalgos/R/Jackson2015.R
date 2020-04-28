#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Jackson et al. 2015 version
#
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
#
# Algorithm reconstructed from methods section in the publication:
# Jackson SL, Long Q, Rhee MK, et al. Weight loss and incidence of diabetes with
# the Veterans Health Administration MOVE! Lifestyle change programme: An
# observational study. The Lancet Diabetes & Endocrinology [electronic article].
# 2015;3(3):173–180.
# (https://linkinghub.elsevier.com/retrieve/pii/S2213858714702670). (Accessed
# December 9, 2019)
#
# Rationale: For grouped time series, (e.g., per person)
#            remove outliers, then apply windows to each weight measurement
#            within each block of time/window, take the average of all weights
#            within that window. Based on work by Jackson et al. 2015
#-----------------------------------------------------------------------------#

#' Jackson 2015 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person), \code{jackson} removes outliers,
#' then apply windows to each measurement within each block of time/window, then
#' averages all measurements within that window. Based on work by Jackson et al.
#' 2015
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifers in \code{df}.
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one weight on the same day, if it is
#'   a precise datetime object, there may not be more than one weight on the
#'   same day. Just something to look out for.
#' @param startPoint string corresponding to the name of the column in \code{df}
#'   holding the time at which subsequent measurement dates will be assessed,
#'   should be the same for each person. Eg., if t = 0 (\code{t[1]}) corresponds
#'   to an index visit held by the variable \code{VisitDate}, then
#'   \code{startPoint} should be set to \code{VisitDate}.
#' @param outliers numeric vector corresponding to the upper and lower bound for
#'   each time entry. Default is \code{c(75, 700)}.
#' @param t numeric vector of time points to collect measurements, e.g.
#'   \code{c(0, 182.5, 365)} for measure collection at t = 0, t = 180 (6 months
#'   from t = 0), and t = 365 (1 year from t = 0). Default is
#'   \code{c(0, 182, 365, 730)} according to Jackson et al. 2015.
#' @param windows numeric vector of measurement collection windows to use around
#'   each time point in \code{t}. E.g. Jackson et al. 2015 use
#'   \code{c(1, 90, 90, 90)} for \code{t = c(0, 182.5, 365, 730)}, implying that
#'   the closest measurement t = 0 will be collected on the day of the
#'   \code{startPoint}. Subsequent measurements will be collected 90 days prior
#'   to and 90 days post t0+182.5 days, t0+365 days, and t0+730 days.
#' @return returns a data frame reduced by the inclusion criteria defined in
#'   \code{t}, \code{windows} and \code{outliers}, and further reduced to 4
#'   columns: the value of \code{id}; the average of the measures defined in
#'   \code{measures} at each \code{t} and \code{windows}, denoted the
#'   \code{measout};the number of measures used to calculate \code{measout} at
#'   time \code{t}; \code{measureTime}, the time-point defined by \code{t}.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(cdw1000)
#'
#' jackson_df <- jackson(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    startPoint = "VisitDate"
#'   )
#'
#' # dplyr::glimpse(jackson_df)
#'
#' samp <- jackson_df %>%
#'   distinct(id) %>%
#'   sample_n(16) %>%
#'   pull()
#'
#' jackson_df %>%
#'   filter(id %in% samp) %>%
#'   ggplot(aes(x = factor(measureTime), y = measout)) +
#'   geom_point(aes(size = n)) +
#'   facet_wrap(vars(id), ncol = 4)
jackson <- function(df,
                    id,
                    measures,
                    tmeasures,
                    startPoint,
                    t = c(0, 182, 365, 730),
                    windows = c(1, 90, 90, 90),
                    outliers = c(75, 700)) {

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

  tryCatch(
    if (class(df[[tmeasures]])[1] != class(df[[startPoint]])[1]) {
      stop(
        print(
          paste0(
            "date type of tmeasures (",
            class(df[[tmeasures]]),
            ") != date type of startPoint (",
            class(df[[startPoint]])[1],
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

  # convert to data.table
  DT <- data.table::as.data.table(df)
  data.table::setkeyv(DT, id)

  # Step 1: Set outliers to NA
  DT[,
      `:=`(
        measout = ifelse(
          get(measures) < outliers[1] | get(measures) > outliers[2],
          NA,
          get(measures)
        )
      )
     ][]

  # Step 2: Set windows
  DT[,
      `:=`(
        dtime = as.numeric(difftime(get(tmeasures),
                                    get(startPoint),
                                    tz = "UTC",
                                    units = "days"))
      )
     ][]

  # loop through each time point in `t`, place into list
  meas_tn <- vector("list", length(t)) # set empty list
  for (i in 1:length(t)) {

    x <- DT[dtime >= (t[i] - windows[i]) & dtime <= (t[i] + windows[i]), ]

    meas_tn[[i]] <- x %>%
      as.data.frame() %>%
      select(eval(id), dtime, measout, !!tmeasures, !!startPoint) %>%
      mutate(measureTime = paste0("t_", t[i]))
  }

  # calculate average weight per measureTime/window
  DT <- data.table::setDT(do.call(rbind, meas_tn))

  key_cols <- c(id, "measureTime")
  data.table::setkeyv(DT, key_cols)
  DT <- DT[,
           list(n = .N, mean = mean(measout, na.rm = TRUE)),
           keyby = key_cols
           ][!is.nan(mean) | !is.na(mean)]

  DF <- DT %>%
    rename(measout = mean) %>%
    as.data.frame()
  DF
}

