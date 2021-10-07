#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithms - consolidated
#
# Author: Richard Ryan Evans
# Development Start Date: 08/11/2020
#
# Rationale: run and organize all 12 algorithms
#-----------------------------------------------------------------------------#

#' Run all measurement cleaning algorithms simultaneously
#'
#' \code{all_algos} runs all 12 algorithms with default settings and outputs a
#' named list with each result. Each algorithm requires a basic set of data:
#' a patient identifier, \code{id}; the measures to clean \code{measures}
#' (e.g., weight, continuous lab data, etc.); a recorded time at which the
#' measure was taken \code{tmeasures}. Some require an index date,
#' \code{start_point} (Janney *et al.* 2016, Goodrich *et al.* 2016, Jackson
#' *et al.* 2015, Kazerooni & Lim 2016, Rosenberger *et al.* 2011), Maguen
#' *et al.* 2013 adjusts for optional demographic variables (e.g., age, sex).
#' Buta *et al.* 2018 stands out, requiring BMI measurements as the unit of
#' measurement \code{measures}. Thus, in order to fully utilize this function,
#' \code{df} needs to have at least \code{id}, \code{measures},
#' \code{tmeasures}, \code{start_point}, and BMI.
#'
#' @param df \code{data.frame} holding raw (input) measurements.
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
#' @param variables Optional. Specific to the Maguen *et al.* 2013 algorithm,
#'   character vector describing the terms in \code{df} to include on the RHS
#'   of the internal mixed model. E.g., \code{c("Age", "Gender")} would
#'   generate a model of the form \code{measures ~ Age + Gender + (1|id)}.
#' @param basic run only those algorithms involving **just** \code{id},
#'   \code{measures}, \code{tmeasures}.
#' @return named list with processed data for each algorithm.
#' @example
#' \dontrun{
#' test <- all_algos(
#'   df = cdw1000,
#'   id = "id",
#'   measures = "Weight",
#'   tmeasures = "WeightDate",
#'   basic = TRUE
#' )
#'
#' dplyr::View(test)
#'
#' test <- all_algos(
#'   df = cdw1000,
#'   id = "id",
#'   measures = "Weight",
#'   tmeasures = "WeightDate",
#'   start_point = "VisitDate",
#'   variables = c("Age", "Sex)
#' )
#'
#' dplyr::View(test)
#'
#' }
all_algos <- function(df,
                      id,
                      measures,
                      tmeasures,
                      start_point = NULL,
                      variables = NULL,
                      basic = FALSE) {

  algos_flist <- c(
    "breland"     = breland,
    "buta"        = buta,
    "chan"        = chan,
    "goodrich"    = goodrich,
    "jackson"     = jackson,
    "janney"      = janney,
    "kazerooni"   = kazerooni,
    "littman"     = littman,
    "maciejewski" = maciejewski,
    "maguen"      = maguen,
    "noel"        = noel,
    "rosenberger" = rosenberger
  )

  basic_algos <- c(
    "breland",
    "chan",
    "littman",
    "maciejewski",
    "noel"
  )

  start_point_algos <- c(
    "janney",
    "goodrich",
    "jackson",
    "kazerooni"
  )

  if (basic) {

    results_list <- vector("list", length(basic_algos))
    names(results_list) <- basic_algos

    for (algo in seq_along(algos_flist[basic_algos])) {
      results_list[[algo]] <-
        algos_flist[basic_algos][[algo]](df, id, measures, tmeasures)
    }

  } else {

    results_list <- vector("list", length(algos_flist))
    names(results_list) <- names(algos_flist)

    for (algo in seq_along(algos_flist)) {

      if (names(algos_flist)[algo] %in% start_point_algos) {
        results_list[[algo]] <-
          algos_flist[[algo]](
            df,
            id,
            measures,
            tmeasures,
            start_point
          )

      } else if (names(algos_flist)[algo] == "rosenberger") {

        results_list[[algo]] <-
          algos_flist[[algo]](df, id, tmeasures, start_point)

      } else if (names(algos_flist)[algo] == "maguen") {

        results_list[[algo]] <-
          algos_flist[[algo]](
            df,
            id,
            measures,
            tmeasures,
            variables = variables
          )

      } else if (names(algos_flist)[algo] == "buta") {

        df$BMI <- 703 * df[[measures]] / (df$Height ^ 2)

        results_list[[algo]] <- algos_flist[[algo]](df, id, "BMI", tmeasures)

      } else {

        results_list[[algo]] <- algos_flist[[algo]](df, id, measures, tmeasures)

      } # end inner if ... else

    } # end for

  } # end outer if ... else

  results_list

}
