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
# 2011;53(3):149–154.
# (https://linkinghub.elsevier.com/retrieve/pii/S0091743511002465). 
# (Accessed December 6, 2019)
# 
# Rationale: For grouped time series, (e.g., per person), collect all
#            measurements within a certain time frame, then break those down
#            into some user defined sequence of dates/times. If the total
#            collected is less than some user defined value, then exclude them
#            from the cohort
#-----------------------------------------------------------------------------#

#' @title Rosenberger 2011 Measurment Cleaning Algorithm
#' @param DF object of class `data.frame`, containing `id` and `measures`
#' @param id string corresponding to the name of the column of patient identifiers in `DF`
#' @param tmeasures string corresponding to the name of the column of measurement collection dates or times in `DF`. If `tmeasures` is a date object, there may be more than one weight on the same day, if it precise datetime object, there may not be more than one weight on the same day
#' @param startPoint string corresponding to the name of the column in `DF` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then `startPoint` should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. Rosenberger chose a total time period of 6 years, dividing each year into intervals of 6-months each, for a total of 12 time points (`seq(0, 6, 0.5)`). 
#' @param pad integer to be appended to `t`. e.g., if set to 1, `t` becomes `t * 365 - 1`, the point is to capture 1 day before t = 0 (t[1]).
#' @param texclude the total number of time points an experimental unit (`id`) must have in order to be included in the final cohort. There is no default, but internally it is set to `floor(length(t) / 2) + 1`.
Rosenberger2011.f <- function(DF,
                              id,
                              tmeasures,
                              startPoint,
                              t = seq(0, 2, by = 0.5),
                              pad = 0,
                              texclude = NULL) {
  
  if (!require(dplyr))      install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(rlang))      install.packages("rlang")
  
  tryCatch(
    if (class(DF[[tmeasures]])[1] != class(DF[[startPoint]])[1]) {
      stop(
        print(
          paste0("date type of tmeasures (",
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
  
  # convert to data.table
  DT <- data.table::as.data.table(DF)
  setkeyv(DT, id)
  
  # Step 1: time from startPoint
  DT[,
     time := as.numeric(difftime(get(tmeasures),
                                 get(startPoint),
                                 tz = "UTC",
                                 units = "days"))
     ]
  
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
      x[order(get(id), time)][, .SD[1], by = id]
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
    ungroup()
}