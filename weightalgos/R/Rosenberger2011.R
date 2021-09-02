#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Rosenberger et al., 2011 version
#
# Author: Richard Ryan Evans
# Development Start Date: 04/23/2019
#
# Algorithm reconstructed from methods section in the publication:
# Rosenberger PH, Ning Y, Brandt C, et al. BMI trajectory groups in veterans of
# the iraq and afghanistan wars. Preventive Medicine [electronic article].
# 2011;53(3):149-154.
# (https://linkinghub.elsevier.com/retrieve/pii/S0091743511002465).
# (Accessed December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person), collect all
#            measurements within a certain time frame, then break those down
#            into some user defined sequence of dates/times. If the total
#            collected is less than some user defined value, then exclude them
#            from the cohort
#-----------------------------------------------------------------------------#

#' Rosenberger et al. 2011 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person), \code{rosenberger} collects all
#' measurements within a certain time frame, then collapses those into some user
#' defined sequence of dates/times. If the total collected is less than some
#' user defined value, then they are excluded from the cohort
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measuress}.
#' @param id string corresponding to the name of the column of group identifiers
#'   in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a \code{Date} object, there may be more than one measurement on the same
#'   unit of time (day, month, fiscal year, etc.), if it is a precise date-time
#'   object (\code{?POSIXlt}), there may not be more than one measurement within
#'   the same unit of time.
#' @param start_point string corresponding to the name of the column in \code{df}
#'   holding the time at which subsequent measurement dates will be assessed,
#'   should be the same for each group in \code{id}. E.g., if t = 0
#'   (\code{t[1]}) corresponds to an index visit held by the variable
#'   \code{VisitDate}, then \code{start_point} should be set to \code{VisitDate}.
#' @param t numeric vector of time points to collect measurements, E.g.
#'   Rosenberger et al. chose a total time period of 6 years, dividing each year
#'   into intervals of 6-months each, for a total of 12 time points
#'   (\code{seq(0, 6, 0.5)}).
#' @param pad integer to be appended to \code{t}. E.g., if set to 1, \code{t}
#'   becomes \code{t * 365 - 1}, the point is to capture 1 day before t = 0
#'   (\code{t[1]}).
#' @param texclude the total number of time points an experimental unit
#'   (\code{id}) must have in order to be included in the final cohort. There
#'   is no default, but internally it is set to \code{floor(length(t) / 2) + 1}.
#' @return input data frame reduced to groups determined by \code{id} with at
#'   least \code{texclude} measurements, and 1 additional column, giving the
#'   actual time in days each measurement was "collected".
#' @examples
#' library(dplyr)
#' data(cdw1000)
#'
#' rosenberger_df <- rosenberger(
#'    df = cdw1000,
#'    id = "id",
#'    tmeasures = "WeightDate",
#'    start_point = "VisitDate",
#'    pad = 1
#'   )
#'
#' # dplyr::glimpse(rosenberger_df)
#'
#' compare_df <- bind_rows(
#'   "Input"  = cdw1000 %>% select(Weight),
#'   "Output" = rosenberger_df %>% select(Weight),
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
rosenberger <- function(df,
                        id,
                        tmeasures,
                        start_point,
                        t = seq(0, 2, by = 0.5),
                        pad = 0,
                        texclude = NULL) {

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

  # convert to data.table
  DT <- data.table::as.data.table(df)
  data.table::setkeyv(DT, id)

  # Step 1: time from start_point
  time <- NULL
  DT[,
      `:=`(
        time = as.numeric(difftime(get(tmeasures),
                                   get(start_point),
                                   tz = "UTC",
                                   units = "days"))
      )
     ][]

  # Step 2: set time points
  t <- t * 365

  # set pad parameter (within "pad" days)
  t <- t - pad

  # Step 3: split data into intervals defined by t
  DT <- split(DT, cut(DT$time, t, include.lowest = TRUE))

  # select closest measurement to each time point
  DT <- lapply(
    DT,
    function(x) {
      x[order(time)][, .SD[1], by = id]
    }
  )

  if (is.null(texclude)) {
    texclude <- floor(length(t) / 2) + 1
  }

  id <- rlang::sym(id)
  tmeasures <- rlang::sym(tmeasures)

  # return result
  do.call(rbind, DT) %>%
    arrange(!!id, !!tmeasures) %>%
    group_by(!!id) %>%
    filter(max(row_number()) >= texclude) %>% # must have at least texclude
    ungroup() %>%
    as.data.frame()
}

