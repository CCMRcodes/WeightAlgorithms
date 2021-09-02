#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Maguen et al. 2013 version
#
# Author: Richard Ryan Evans
# Development Start Date: 11/07/2018
#
# Algorithm reconstructed from methods section in the publication:
# Maguen S, Madden E, Cohen B, et al. The relationship between body mass index
# and mental health among iraq and afghanistan veterans. Journal of General
# Internal Medicine [electronic article]. 2013;28(S2):563-570.
# (http://link.springer.com/10.1007/s11606-013-2374-8).
# (Accessed December 6, 2019)
#
# Rationale: For grouped time series, (e.g., per person)
#            remove outliers, then model patients weight trajectories with
#            a linear mixed model with a random slope and intercept, adjusted
#            for age and gender, then remove measurements where the residual
#            meets a certain threshold. Based on work by Maguen et al. 2013
#-----------------------------------------------------------------------------#

#' Maguen 2013 Measurment Cleaning Algorithm
#'
#' For grouped time series, (e.g., per person) \code{maguen} removes outliers,
#' then models patients weight trajectories with a linear mixed model with a
#' random slope and intercept, adjusted for age and gender, it then removes
#' measurements where the residual meets a certain threshold. Based on work by
#' Maguen et al. 2013
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}.
#' @param id string corresponding to the name of the column of group identifiers
#'   in \code{df}.
#' @param measures string corresponding to the name of the column of
#'   measurements in \code{df}.
#' @param tmeasures string corresponding to the name of the column of
#'   measurement collection dates or times in \code{df}. If \code{tmeasures} is
#'   a date object, there may be more than one weight on the same day, if it is
#'   a precise \code{datetime} object, there may not be more than one weight on
#'   the same day.
#' @param outliers object of type \code{list} with numeric inputs corresponding
#'   to the upper and lower bound for each time entry. Default is
#'   \code{list(LB = c(70), UB = c(700))}.
#' @param variables character vector describing the terms in \code{df} to
#'   include on the RHS of the internal mixed model. E.g.,
#'   \code{c("Age", "Gender")} would generate a model of the form
#'   \code{measures ~ Age + Gender + (1|id)}.
#' @param resid_threshold single numeric value to be used as a cut off value for
#'   the conditional (response) residual for each measurement. Lower values are
#'   more conservative.
#' @param add_internals logical, adds additional columns to output data frame:
#'   \code{Output}, input data with outliers set to \code{NA}; \code{t}, time
#'   standardized for each group in \code{id}; \code{resid}, the residual from
#'   the result of \code{lme4::lmer}. Defaults to FALSE.
#' @param ... any number of named arguments for the \code{lme4::lmer} model
#' @return if add_internals is FALSE, returns input data frame with processed
#'   data as an additional column \code{measout}. if \code{add_internals} is TRUE
#'   then it returns the input data frame with processed data \code{measout} and
#'   columns described in \code{add_internals}.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(cdw1000)
#'
#' maguen_df <- maguen(
#'    df = cdw1000,
#'    id = "id",
#'    measures = "Weight",
#'    tmeasures = "WeightDate",
#'    variables = c("Age", "Sex"),
#'    add_internals = TRUE
#'   )
#'
#' # dplyr::glimpse(maguen_df)
#'
#' samp <- maguen_df %>%
#'   filter(is.na(measout)) %>%
#'   distinct(id) %>%
#'   sample_n(16) %>%
#'   pull()
#'
#' maguen_df %>%
#'   filter(id %in% samp) %>%
#'   ggplot() +
#'   geom_line(aes(x = t, y = Weight), color = "black") +
#'   geom_point(aes(x = t, y = measout), color = "green") +
#'   facet_wrap(vars(id), ncol = 4, scales = "free")
maguen <- function(df,
                   id,
                   measures,
                   tmeasures,
                   outliers = list(LB = 70, UB = 700),
                   variables = NULL,
                   resid_threshold = 10,
                   add_internals = FALSE,
                   ...) {

  tryCatch(
    if (!is.numeric(df[[measures]])) {
      stop(
        print("weight data must be a numeric vector")
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

  # step 1: outliers
  Output <- NULL
  df <- df %>%
    mutate(
      Output = ifelse(
        df[[measures]] >= outliers$LB & df[[measures]] <= outliers$UB,
        df[[measures]],
        NA_real_
      )
    )

  # step 2: linear mixed model

  # add time for random slope term
  id <- rlang::sym(id)
  tmeasures <- rlang::sym(tmeasures)

  df <- df %>%
    filter(!is.na(Output)) %>%
    group_by(!!id) %>%
    arrange(!!id, !!tmeasures) %>%
    mutate(t = row_number()) %>%
    ungroup()

  # create formula, include variables or not based on is.null(variables)
  if (is.null(variables)) {
    f <- "Output ~ 1"
  } else {
    f <- paste(
      "Output",
      paste(
        variables,
        collapse = " + "
      ),
      sep = " ~ "
    )
  }

  f <- paste0(f, " + t", " + (1 + t", " | ", id, ")")
  f <- as.formula(f)
  lmm <- eval(
    bquote(
      lme4::lmer(
        .(f),
        data = df,
        control = lme4::lmerControl(calc.derivs = FALSE)
      )
    )
  )

  df$resid <- residuals(lmm) # add conditional residuals at ith level

  resid <- NULL
  df <- df %>%
    mutate(
      measout = ifelse(abs(resid) >= resid_threshold, NA_real_, Output)
    )

  if (add_internals) {
    as.data.frame(df)
  } else {
    df %>%
      dplyr::select(-c(Output, t, resid)) %>%
      as.data.frame()
  }
}

