#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Littman 2012 version
#
# Author: Richard Ryan Evans
# Development Start Date: 09/25/2018
#
# Algorithm reconstructed from methods section in the publication:
# Littman AJ, Damschroder LJ, Verchinina L, et al. National evaluation of
# obesity screening and treatment among veterans with and without mental health
# disorders. General Hospital Psychiatry[electronic article]. 2015;37(1):7-13.
# (https;//linkinghub.elsevier.com/retrieve/pii/S0163834314002898). (Accessed
# December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person)
#            clean numeric measurements collected longitudinally
#  	         first, removing outliers, then examining within-person
#            measurement mean and standard deviation, if measurement
#            is found to be above SDthreshold, then measurement is
#            set to missing. Based on work by Littman et al. 2012.
#
#-----------------------------------------------------------------------------#

#' Littman et al. 2012 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person) clean numeric measurements
#' collected longitudinally, first, removing outliers, then examining within-
#' person measurement mean and standard deviation, if measurement is found to be
#' above \code{SDthreshold}, then measurement is set to missing (\code{NA}).
#' Based on work by Littman et al. 2012.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of
#'  measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one measure on the same day, if it is
#'   a precise datetime object, there may not be more than one measure on the
#'   same day.
#' @param outliers object of type \code{list} with numeric inputs corresponding
#'   to the upper and lower bound for each time entry. Default is
#'   \code{list(LB = c(75), UB = c(600))}.
#' @param SDthreshold numeric scalar to be multiplied by the \code{meanMeasures}
#'   per \code{id}. E.g., from Littman 2012, "...We excluded any weight
#'   measurements that met the following 2 criteria: 1) the difference between
#'   the mean weight and weight in question was greater than the SD and 2) the
#'   SD was greater than 10 percent of the mean...." implies
#'   \code{SDthreshold = 0.1}.
#' @param AddInternals logical, adds additional columns to output data frame
#'   detailing the group-wise means and standard deviations used interally to
#'   process the data. Defaults to FALSE.
#' @return input data.frame with additional column, \code{measout}, the
#'   output/"cleaned" data. If \code{AddInternals == TRUE}, 5 additional columns
#'   are added to the resultant data frame: code{meanMeasures},
#'   \code{mean(measures)} per \code{id}; \code{SDMeasures}, \code{SD(measures)}
#'   per \code{id}; \code{SD_threshold_}, \code{mean(measures) * SDthreshold};
#'   and \code{cond1} & \code{cond2}, logical vectors indicating the result of
#'   checking the two conditions; 1, the difference between the
#'   \code{meanMeasures} and code{measures_aug_} in question is greater than the
#'   \code{SDMeasures}, and 2, the \code{SDMeasures} is greater than
#'   \code{SD_threshold_} of the mean.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(cdw1000)
#'
#' littman_df <- littman(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate"
#'   )
#'
#' # dplyr::glimpse(littman_df)
#'
#' samp <- littman_df %>%
#'   distinct(id) %>%
#'   sample_n(16) %>%
#'   pull()
#'
#' littman_df %>%
#'   filter(id %in% samp) %>%
#'   ggplot(aes(x = WeightDate, y = measout)) +
#'   geom_point(color = "green") +
#'   geom_line() +
#'   facet_wrap(vars(id), ncol = 4)
littman <- function(df,
                    id,
                    measures,
                    tmeasures,
                    outliers = list(LB = c(75), UB = c(600)),
                    SDthreshold = 0.10,
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

  # Compute the mean and sd for each `id`s `measures`, if more than 1 value.
  # if not more than 1 value, mean is finite, sd is undefined
  # convert to data.table
  DT <- data.table::as.data.table(df)
  setkeyv(DT, id)

  # first set outliers to NA
  DT[,
      `:=`(
        measures_aug_ = ifelse(
          get(measures) < outliers[[1]] | get(measures) > outliers[[2]],
          NA,
          get(measures)
        )
      )
     ][]

  # calc mean of measures per group
  DT[, `:=`(meanMeasures = mean(measures_aug_, na.rm = TRUE)), by = id][]
  # calc SD of weight per group
  DT[, `:=`(SDMeasures   = sd(measures_aug_,   na.rm = TRUE)), by = id][]
  # calc SD threshold
  DT[, `:=`(SD_threshold_ = SDthreshold * meanMeasures), by = id][]

  # exclude any measurements that meet the following 2 criteria:
  # 1) the difference between the meanMeasures and measures_aug_ in
  # question is greater than the SDMeasures
  # AND
  # 2) the SDMeasures was greater than SD_threshold_ of the mean
  DT[,
     `:=`(cond1 = ifelse(abs(measures_aug_ - meanMeasures) > SDMeasures, T, F))
     ][]

  DT[, `:=`(cond2 = ifelse(SDMeasures > SD_threshold_, T, F))][]

  DT[, `:=`(measures_aug_ = ifelse((cond1 & cond2), NA, measures_aug_))][]

  # return augmented measurements
  DF <- as.data.frame(DT)
  # names(DF)[names(DF) == measures] <- "InputMeasurement"
  names(DF)[names(DF) == "measures_aug_"] <- "measout"

  if (AddInternals) {
    DF
  } else {
    DF %>%
      dplyr::select(-c(meanMeasures:cond2))
  }
}

