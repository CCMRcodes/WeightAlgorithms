#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Maciejewski 2016 version
#
# Authors: Theodore S. Z. Berkowitz (Theodore.Berkowitz@va.gov; aut)
#          Lynn Van Scoyoc (ctb)
#          Maren Olsen, Ph.D. (ctb)
#          Richard Evans (Richard.Evans8@va.gov; code edits/R formatting)
#
# Development Start Date: 2020-06-15
#
# Algorithm reconstructed from methods section in the publication:
# Maciejewski ML, Arterburn DE, Van Scoyoc L, et al. Bariatric surgery and
# long-term durability of weight loss. JAMA Surgery [electronic article].
# 2016;151(11):1046.
# (http://archsurg.jamanetwork.com/article.aspx?doi=10.1001/jamasurg.2016.2317).
# (Accessed December 6, 2019)
#
# Rationale: "...If the standard deviation of the same-day weights was less than
# or equal to 2 lb, then the mean was taken. Otherwise, the standard deviation
# of each same-day weight with prior/post weight measurements was calculated and
# the same-day weight leading to the smallest standard deviation was retained.
# After sorting weight measures by date for each individual, rolling standard
# deviations were calculated using consecutive groups of three weight measures
# for each individual. The first group consisted of weight measures 1-3, the
# second 2-4, and so forth. The first and last groups were evaluated separately
# because the first (and last) weight measure could only be included in one
# group and the second (and next to last) could only be included in two groups.
# If the first (or last) two groups' standard deviations were greater than 35
# lb, the second (or next to last) weight measure was deleted. If the first (or
# last) standard deviation was greater than 35 lb, then paired standard
# deviations were calculated for each pair within the first (or last) group. If
# two of the paired standard deviations were greater than 45 lb and the
# remaining was less than 10 lb, the offending weight measure was deleted. After
# these deletions, the weight measures were reassembled in date order for each
# individual and rolling standard deviations were recalculated and assigned to
# the central weight measure of the group. Clusters of high standard deviations,
# indicating a potential outlier, were identified by flagging consecutive
# standard deviations greater than 10 lb. For each cluster of high standard
# deviations of three or more the interior weight measures were deleted keeping
# only the first and last measures of the cluster. Approximately 1.2% of weight
# measurements were identified as outliers and were deleted. Standard deviation
# cutoffs were determined via iterative trial and error driven by clinical
# plausibility of the specific measure rather than a standard rule and with
# guidance from clinical practitioners familiar with context of surgery and
# expected outcomes. Before the same-day and outlier cleaning ..."
#
# This program is a modified version of the original program
# "cleaning_weight_data_functionized.R" that I forked on 2020-07-16, after
# confirming that these functions produce the same results as the weight
# cleaning SAS macros from the same input data sets. (TSZB, 2020-07-16)
#
# The various threshold values used throughout these macros were selected via
# an iterative process where they were (subjectively) judged to provide the
# best balance between removing highly improbable weight measurement values and
# discounting true drastic changes in weight over time. If you believe that
# these threshold values might not be optimal for your data, please feel free
# to adjust them to suit your needs; they need not be considered the absolute
# "gold standard", especially if better criteria are found. (TSZB, 2020-06-15)
#-----------------------------------------------------------------------------#

#------------------ Prepare Inputs for Downstream Processes -------------------

#' Maciejewski Algorithm Part I.a. - Check the data frame and parameters for
#' conformity with expected structure
#'
#' \code{prep_maciejewski} If \code{varvec} was provided and none of \code{id},
#' \code{tmeasures}, and \code{measures} was provided, confirm suitability of
#' \code{varvec} argument value for use in this function. If \code{varvec} is
#' not specified, confirm user has provided variable names. Then confirm
#' specified variable names exist in input \code{df}. Creates a working copy of
#' input data, if \code{df} is not already a \code{data.table}, then coerce to
#' \code{data.table} and subset to just the 3 columns needed. Function then
#' confirms variables are not unusable data types and returns \code{data.table}.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param varvec optional. String vector containing \code{df}, \code{id},
#'   \code{measures}, and \code{tmeasures}.
#' @return \code{data.table} with properly formatted columns for downstream
#'   processing/weight cleaning.
#' @examples
#' \dontrun{
#' x <- 1:5
#'
#' # check each for errors
#' prep_maciejewski(df = x, varvec = c('a', 's', 'd'))
#' prep_maciejewski(df = iris, varvec = c('a', 's', 'd'))
#' prep_maciejewski(df = iris, varvec = c('Sepal.Length', 's', 'd'))
#' prep_maciejewski(df = iris, varvec = c('Sepal.Length', 'Sepal.Width', 'd'))
#' prep_maciejewski(
#'    df = iris,
#'    varvec = c('Sepal.Length', 'Sepal.Width', 'Petal.Width')
#' ) # success
#'
#' air.df <- airquality
#' air.df$date <- with(air.df, as.Date(paste('2020', Month, Day, sep = '-')))
#' air.df$charval <- as.character(air.df$Ozone)
#' prep_maciejewski(df = air.df, varvec = c('a', 's', 'd'))
#' prep_maciejewski(df = air.df, varvec = c('Temp', 's', 'd'))
#' prep_maciejewski(df = air.df, varvec = c('Temp', 'date', 'd'))
#' prep_maciejewski(df = air.df, varvec = c('Temp', 'date', 'charval'))
#' prep_maciejewski(df = air.df, varvec = c('Temp', 'date', 'Wind')) # success
#' }
prep_maciejewski <- function(df,
                             id,
                             measures,
                             tmeasures,
                             varvec = NULL) {

  # disable fancy quotes for the duration of this function
  # (if enabled by default)
  oldufq <- getOption("useFancyQuotes")
  on.exit(options(useFancyQuotes = oldufq))
  options(useFancyQuotes = FALSE)

  # Confirm input data object at least inherits from a data.frame
  if (!inherits(df, 'data.frame')) {
    stop(
      "Object '",
      deparse(substitute(df)),
      "' must be data.frame-like"
    )
  }

  # If `varvec` was provided and none of `id`, `tmeasures`, and `measures` was
  # provided, confirm suitability of `varvec` parameter for use in this function
  if (
    !is.null(varvec)
    &&
    missing(id)
    &&
    missing(tmeasures)
    &&
    missing(measures)
  ) {
    if (!is.character(varvec)) {
      stop('varvec argument must be a character vector of variable names')
    }
    if (length(unique(varvec[!is.na(varvec) & nzchar(varvec)])) != 3L) {
      stop(
        'varvec argument must be a character vector containing 3 distinct
         non-missing values'
      )
    }

    # convert
    varvec <- unique(varvec[!is.na(varvec) & nzchar(varvec)])

    if (!is.null(names(varvec))) {
      # Do not make this case-insensitive. Require user specify names exactly as
      # specified in original SAS macros. (TSZB, 2020-06-08)
      expected_names <- c('id', 'tmeasures', 'measures')

      found_name_indicators <-
        match(expected_names, names(varvec), nomatch = 0L) > 0L

      if (!all(found_name_indicators)) {
        stop(
          ngettext(
            sum(!found_name_indicators),
            sprintf(
              'The following name was not specified in `varvec`: %s',
              expected_names[!found_name_indicators]
            ),
            sprintf(
              'The following names were not specified in `varvec`: %s',
              paste(
                expected_names[!found_name_indicators],
                sep = '',
                collapse = ', '
              )
            ) # end second sprintf()
          ) # end ngettext()
        ) # end stop()
      } # end if()

      id        <- varvec['id']
      tmeasures <- varvec['tmeasures']
      measures  <- varvec['measures']

    } else {
      id        <- varvec[1L]
      tmeasures <- varvec[2L]
      measures  <- varvec[3L]
    }
  } # end main if()

  ### Confirm user has provided variable names (if `varvec` not specified)
  if ((missing(id)) || (missing(tmeasures)) || (missing(measures))) {
    stop("id, tmeasures, and measures must all be supplied")
  }

  if (!(
    (is.character(id))
    &&
    (is.character(tmeasures))
    &&
    (is.character(measures))
  )) {
    stop("id, tmeasures, and measures must all be character values")
  }

  if (length(id[nzchar(id) & !is.na(id)]) < 1L) {
    stop("No valid value specified for id")
  } else if (length(id[nzchar(id) & !is.na(id)]) > 1L) {
    warning(
      length(id[nzchar(id) & !is.na(id)]),
      " valid values specified for id; only first will be used"
    )
  }

  id <- id[nzchar(id) & !is.na(id)][1L]

  if (length(tmeasures[nzchar(tmeasures) & !is.na(measures)]) < 1L) {
    stop("No valid value specified for tmeasures")
  } else if (length(tmeasures[nzchar(tmeasures) & !is.na(tmeasures)]) > 1L) {
    warning(
      length(tmeasures[nzchar(tmeasures) & !is.na(tmeasures)]),
      " valid values specified for tmeasures; only first will be used"
    )
  }

  tmeasures <- tmeasures[nzchar(tmeasures) & !is.na(tmeasures)][1L]

  if (length(measures[nzchar(measures) & !is.na(measures)]) < 1L) {
    stop("No valid value specified for measures")
  } else if (length(measures[nzchar(measures) & !is.na(measures)]) > 1L) {
    warning(
      length(measures[nzchar(measures) & !is.na(measures)]),
      " valid values specified for measures; only first will be used"
    )
  }

  measures <- measures[nzchar(measures) & !is.na(measures)][1L]

  # Capture original variable names in a single vector
  DT_oldnames <- c(id, tmeasures, measures)

  # Confirm specified variable names exist in input data object
  found_name_indicators <- match(DT_oldnames, colnames(df), nomatch = 0L) > 0L

  # NB: For some reason, the `ngettext()` function gives an error message if
  # the `msg1` argument produces more than 1 string when `n` is strictly greater
  # than 1, i.e., when the `msg1` argument wouldn't matter because it wouldn't
  # be displayed to the user. Changed code to match the code in the `msg2`
  # argument as a workaround. (TSZB, 2020-06-08)
  if (!all(found_name_indicators == TRUE)) {
    stop(
      ngettext(
        sum(!found_name_indicators),
        sprintf(
          "Variable not found in object '%s': %s",
          deparse(substitute(df)),
          paste(
            DT_oldnames[!found_name_indicators],
            sep = '',
            collapse = ', '
          )
        ),
        sprintf(
          "Variables not found in object '%s': %s",
          deparse(substitute(df)),
          paste(
            DT_oldnames[!found_name_indicators],
            sep = '',
            collapse = ', '
          )
        )
      ) # end ngettext()
    ) # end stop()
  } # end if()

  ## Create working copy of input data
  # If the input data object is not already a data.table, then coerce to
  # data.table and subset it to just the 3 columns needed; otherwise subset the
  # input object to the 3 columns needed
  if (!is(df, 'data.table')) {
    DT <- setDT(df[, DT_oldnames])
  } else {
    DT <- copy(df[, DT_oldnames, with = FALSE])
  }

  # Confirm variables are not unusable data types (e.g., AnalysisVar must be
  # numeric)
  # TODO: I'm not sure what to do for the `tmeasures` variable. Should the
  #       function force every input object only to (a) use dates as the unit of
  #       time and (b) use the built-in "Date" R class? It seems unreasonable
  #       (to me) to apply such a restriction unilaterally. But how would it
  #       allow for other classes, e.g., POSIXct/POSIXlt and classes from
  #       various packages (chron, lubridate, hms, etc.)? For now I'll just
  #       check that the tmeasures variable has the built-in "Date" class (since
  #       that's the class I used in my tests and in my real application).
  #       (TSZB, 2020-06-08)
  # TODO: Should this function be able to accept and handle time series data
  #       objects? If so, how? (TSZB, 2020-06-08) "I can't think of any cases
  #       where this would benefit the user, these algorithms are already
  #       designed for 'time-series'-like data. If they have weight data as a
  #       time-series object, they'll just have to convert it" (RRE, 2020-07-31)
  if (!is.numeric(DT[[measures]])) {
    warning(
      "Variable ",
      sQuote(measures),
      " must be numeric; will be coerced by as.numeric()"
    )
    DT[[measures]] <- as.numeric(DT[[measures]])
  }

  # Provide replacement variable names to use in the cleaning process
  DT_newnames <- c('subject_id', 'measurement_date', 'measurement')

  # Change existing variable names to new (intermediate) names
  setnames(DT, old = DT_oldnames, new = DT_newnames)

  # Specify key variables in the working copy of the data
  setkeyv(DT, c('subject_id', 'measurement_date'))

  # Add custom attribute to retain variable names from original input object
  setattr(DT, name = 'DT_oldnames', value = DT_oldnames)

  # Return the checked data.table
  return(DT)
}

#------------------------ Consolidate same day records ------------------------

#' Maciejewski algorithm part I.b. - consolidate same day records
#'
#' \code{consolidate_sameday_records} reduces input data set to one unique
#' \code{measures} value per combination of \code{id} and \code{tmeasures}.
#'
#' @param df object of class \code{data.frame}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param varvec optional. String vector containing \code{df}, \code{id},
#'   \code{measures}, and \code{tmeasures}.
#' @param keep_uncleanable subjects having exactly two \code{tmeasures}
#'  (internally `measurement_date`) values each with a standard deviation of the
#'  \code{measures} (internally `measurement`) values that is strictly greater
#'  than 2, are termed "uncleanable", the user can choose to keep or remove the
#'  "uncleanable" subjects. The default is to keep them/\code{TRUE}.
#' @param outliers object of type \code{list} with two values, \code{UB} and
#'  \code{LB}, the lower and upper bounds of plausible values of
#'  \code{measures}. Default is \code{list(`LB` = 75L, `UB` = 700L)}.
#' @param sd_thresholds Object of type \code{list} with 1 numeric value of
#'   same-day standard deviation statistics above which the data of the
#'   associated \code{id}-\code{tmeasures} combination are considered indicative
#'   of a discrepancy in \code{measures} values that requires more scrutiny to
#'   resolve into one unique \code{measures} value per
#'   \code{id}-\code{tmeasures} combination than simply taking the arithmetic
#'   mean of the values.
#' @return \code{data.table} containing the three variables supplied to the
#'   input parameters (\code{id}, \code{tmeasures}, \code{measures}). The data
#'   set contains one value of \code{measures} per \code{id} and
#'   \code{tmeasures} combination. There is also the option to remove
#'   intermediate data sets created to produce the main output data set.
#' @examples
#' \dontrun{
#' # Checking performance
#' data(cdw1000)
#'
#' # with defaults
#' consolidate_sameday_records(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#'
#' # removing "uncleanable" records
#' consolidate_sameday_records(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight',
#'   keep_uncleanable = FALSE
#' )
#'
#' # with varvec
#' varvec <- c(
#'   'id' = 'id',
#'   'tmeasures' = 'WeightDate',
#'   'measures' = 'Weight'
#' )
#' consolidate_sameday_records(
#'   df = cdw1000,
#'   varvec = varvec
#' )
#' }
consolidate_sameday_records <- function(df,
                                        id,
                                        tmeasures,
                                        measures,
                                        varvec = NULL,
                                        keep_uncleanable = TRUE,
                                        outliers = list(
                                          'LB' = 75L,
                                          'UB' = 700L
                                        ),
                                        sd_thresholds = list('same_day' = 2)) {

  # disable fancy quotes for the duration of this function (if enabled by
  # default)
  oldufq <- getOption("useFancyQuotes")
  on.exit(options(useFancyQuotes = oldufq))
  options(useFancyQuotes = FALSE)

  # preliminary data object checks and conversion to data.table
  DT <- prep_maciejewski(
    df        = df,
    id        = id,
    tmeasures = tmeasures,
    measures  = measures,
    varvec    = varvec
  )
  DT_oldnames <- attr(DT, 'DT_oldnames')
  if (missing(id))        id        <- colnames(DT)[1L]
  if (missing(tmeasures)) tmeasures <- colnames(DT)[2L]
  if (missing(measures))  measures  <- colnames(DT)[3L]

  ## Initial summarization of data
  # Restrict to records where `measurement` is a non-missing, finite numeric
  # value that lies in the closed interval [outliers[['LB']], outliers[['UB']]]
  measurement <- NULL
  DT <- DT[
    !is.na(measurement)
    &
    !is.nan(measurement)
    &
    is.finite(measurement)
    &
    is.numeric(measurement)
  ][
    (measurement >= outliers[['LB']])
    &
    (measurement <= outliers[['UB']])
  ]

  ## Calculate mean and standard deviation of values per `subject_id`
  # -`measurement_date` combination
  # NB: Replace NA values of `sd` (for `subject_id`-`measurement_date`
  # combinations that only occur with exactly one unique value in `measurement`
  # in the original data) with 0 so the logical checks
  # `sd <= sd_thresholds[['same_day']]` and `sd > sd_thresholds[['same_day']]`
  # are comprehensive and mutually exclusive. (TSZB, 2020-07-03)
  subject_id <- measurement_date <- NumDates <- NumFixesNeeded <- NULL
  RecordNumberASC <- RecordNumberDESC <- NULL
  DT2 <- DT[
    ,
    .(mean = mean(measurement), sd = sd(measurement)),
    by = .(subject_id, measurement_date)
  ][
    order(measurement_date),
    `:=`(
      NumDates = .N,
      RecordNumberASC = seq_len(.N),
      RecordNumberDESC = .N - seq_len(.N) + 1L
    ),
    by = .(subject_id)
  ][
    is.na(sd),
    sd := 0
  ][]

  # Report how many `subject_id`-`measurement_date` combinations with more
  # than 1 distinct valid `measurement` value were detected (if any) and
  # complete further processing to consolidate them (if needed)
  if (NROW(DT) > NROW(DT2)) {
    message(
      (NROW(DT) - NROW(DT2)),
      ngettext(
        (NROW(DT) - NROW(DT2)),
        sprintf(
          ' %s-%s combination with multiple %s values detected',
          sQuote(id),
          sQuote(tmeasures),
          sQuote(measures)
        ),
        sprintf(
          ' %s-%s combinations with multiple %s values detected',
          sQuote(id),
          sQuote(tmeasures),
          sQuote(measures)
        )
      ) # end ngettext()
    ) # end message

    # Identify `subject_id` values that have at least one `measurement_date`
    # with multiple discrepant `measurement` values that produce an overall
    # standard deviation statistic that is strictly greater than
    # sd_thresholds[['same_day']] and count how many such `measurement_date`
    # values occur for each `subject_id`
    NumFixesNeeded <- NULL
    needs_fixin_ids <- DT2[
      sd > sd_thresholds[['same_day']],
      .(subject_id, measurement_date, NumDates)
    ][
      ,
      NumFixesNeeded := .N,
      by = .(subject_id)
    ][]

    ## Identify `subject_id` values all of whose records are "uncleanable" due
    # to having exactly two `measurement_date` values each with a standard
    # deviation of the `measurement` values that is strictly greater than 2

    ## NB: In order for a `subejct_id` value with more than one
    # `measurement_date` value to need fixin', then at least one of those
    # `measurement_date` values must have more than one distinct `measurement`
    # value associated with it and the difference between those unique
    # `measurement` values must be such that their sample standard deviation
    # statistic is strictly greater than 2. If such a `subject_id` has exactly
    # two distinct values of `measurement_date` and also needs fixin', then the
    # data for this `subject_id` can be fixed IFF one of the `measurement_date`
    # values associated with that `subject_id` value has exactly one unique
    # value of `measurement` or has discrepant values of `measurement` that are
    # so similar that their sample standard deviation statistic is less than or
    # equal to 2 (and the other `measurement_date` value has multiple discrepant
    # values of `measurement` such that the sample standard deviation statistic
    # is strictly greater than 2). In other words, if a `subject_id` has exactly
    # two unique values of `measurement_date` and both of those values of
    # `measurement_date` have multiple discrepant values of `measurement` such
    # that their sample standard deviation statistics (within each
    # `measurement_date` value) are both strictly greater than 2, then we are
    # not able to "fix" (or "clean") the data for this `subject_id`.

    ## EDIT 2020-06-08: Include `subject_id` values with only one distinct
    # `measurement_date` value as an indicator of "uncleanability". (TSZB,
    # 2020-06-08)

    ## NB: Cannot "clean" data for `subject_id` values with only one
    # `measurement_date` value. (TSZB, 2020-06-08)
    cannot_be_fixed_ids <- unique(
      needs_fixin_ids[
        (NumDates == 2L & NumFixesNeeded == 2L) | (NumDates == 1L),
        .(subject_id)
      ]
    )

    ## Capture "uncleanable" `subject_id`-`measurement_date` combinations, if
    # `keep_uncleanable` argument is TRUE
    if (keep_uncleanable == TRUE) {
      ## EDIT 2020-06-08: Return mean value of `measurement` per "uncleanable"
      # `subject_id`-`measurement_date` combination (if `keep_uncleanable`
      # argument is TRUE), instead of NA values. (TSZB, 2020-06-08)
      DT_uncleanable <- DT2[
        cannot_be_fixed_ids,
        .(subject_id, measurement_date, measurement = mean),
        on = .(subject_id)
      ]
    }

    # Remove "uncleanable" records from the summarized data.table object
    DT2 <- DT2[!cannot_be_fixed_ids]

    ## Merge original data.table object with summarized data.table object to add
    # variables enumerating `subject_id`-`measurement_date` combinations in
    # ascending and descending chronological order

    ## NB: Because of the step executed immediately prior to this step,
    # performing this merge will remove all "uncleanable" data records from the
    # original data.table object. (TSZB, 2020-06-08)
    DT <- DT[
      DT2,
      .(
        subject_id,
        measurement_date,
        measurement,
        NumDates,
        RecordNumberASC,
        RecordNumberDESC
      ),
      on = .(subject_id, measurement_date)
    ]

    #-------- "Clean" data with exactly two `measurement_date` values --------#

    ## Identify all `subject_id` values with exactly two `measurement_date`
    # values and flag any that "need fixin'"
    NeedsFixinFlag <- NULL
    onlytwodates_ids <- unique(
      DT2[
        NumDates == 2L,
        .(
          subject_id,
          measurement_date,
          NeedsFixinFlag = (sd > sd_thresholds[['same_day']]
        ),
        RecordNumberASC)
      ]
    )

    ## Pull all *summarized data* records for all `subject_id` values with
    # exactly two `measurement_date` values and exactly one `measurement_date`
    # that "needs fixin'"

    ## NB: Due to previous cleaning steps, the result of this step should not
    # include any `subject_id` values with both `measurement_date` values
    # needing to be "fixed"
    onlytwodates_ids_needsfixin <- onlytwodates_ids[
      onlytwodates_ids[NeedsFixinFlag == TRUE],
      .(
        subject_id,
        measurement_date,
        NeedsFixinFlag,
        RecordNumberASC
      ),
      on = .(subject_id)
    ]

    ## Pull all *original data* records for all `subject_id` values with
    # exactly two `measurement_date` values and exactly one `measurement_date`
    # that "needs fixin'"
    onlytwodates_data <- DT[
      onlytwodates_ids_needsfixin,
      .(
        subject_id,
        measurement_date,
        measurement,
        RecordNumberASC,
        NeedsFixinFlag
      ),
      on = .(subject_id, measurement_date)
    ]

    ## Separate the records that "need fixin'" (the "bad" data) from the records
    # that do not (the "ok" data) and enumerate enumerate each "bad"
    # `measurement` value
    OptionNumber <- NULL
    onlytwodates_bad <- onlytwodates_data[
      NeedsFixinFlag == TRUE
    ][
      order(measurement),
      OptionNumber := seq_len(.N),
      by = .(subject_id)
    ][,
      NeedsFixinFlag := NULL
    ][]

    OKDate <- OKValue <- OKRecordNumberASC <- NULL
    onlytwodates_ok <- onlytwodates_data[
      NeedsFixinFlag == FALSE,
      .(
        subject_id,
        OKDate = measurement_date,
        OKValue = measurement,
        OKRecordNumberASC = RecordNumberASC
      )
    ][]

    ## Melt (reshape from "wide" data to "long" data) the result of merging the
    # "bad" and "ok" data.table objects and then calculate the standard
    # deviation within each `OptionNumber`
    newsd <- variable <- value <- NULL
    onlytwodates_long <- melt(
      onlytwodates_bad[
        onlytwodates_ok,
        .(
          subject_id,
          measurement_date,
          measurement,
          OptionNumber,
          OKValue
        ),
        on = .(subject_id)
      ],
      id.vars = c('subject_id', 'measurement_date', 'OptionNumber'),
      measure.vars = c('measurement', 'OKValue')
    )[,
      variable := NULL
    ][,
      .(newsd = sd(value)),
      by = .(subject_id, measurement_date, OptionNumber)
    ][]

    ## Identify which of the "bad" data "options" produces the smallest standard
    # deviation statistic

    ## Modified on 2020-05-30 from https://stackoverflow.com/a/41838383

    ## TODO: Ensure this reliance on sorted floats is robust to floating point
    # errors. EDIT 2020-07-16: This would be a decent amount of work for
    # probably very little gain but it could still be done so I will leave this
    # TODO here for now. (TSZB, 2020-07-03; TSZB, 2020-07-16)
    onlytwodates_shouldbekept <- onlytwodates_long[
      onlytwodates_long[
        ,
        .I[which.min(newsd)],
        by = .(subject_id, measurement_date)
      ][['V1']],
      .(subject_id, measurement_date, OptionNumber)
    ][]

    setkeyv(
      onlytwodates_shouldbekept,
      c('subject_id', 'measurement_date', 'OptionNumber')
    )

    # Keep only the summarized data records for the "best bad option" records
    onlytwodates_badfixed <- merge(
      onlytwodates_bad,
      onlytwodates_shouldbekept,
      by = c('subject_id', 'measurement_date', 'OptionNumber')
    )

    ## Combine the "best bad option" records with all of the other records (that
    # did not "need fixin'") from `subject_id` values with exactly two distinct
    # `measurement_date` values and populate new variables that exist in
    # data.table `DT` in preparation of the impending `rbind()`
    onlytwodates_fixed <- rbind(
      onlytwodates_data[
        NeedsFixinFlag == FALSE,
        .(
          subject_id,
          measurement_date,
          measurement,
          RecordNumberASC
        )
      ],
      onlytwodates_badfixed[,
        .(
          subject_id,
          measurement_date,
          measurement,
          RecordNumberASC
        )
      ]
    )[,
      `:=`(
        NumDates = 2L,
        RecordNumberDESC = 2L - RecordNumberASC + 1L
      )
    ][]

    setkeyv(onlytwodates_fixed, c('subject_id', 'measurement_date'))

    ## Combine "fixed" data records for `subject_id` values associated with
    # exactly two `measurement_date` values and the remaining original data
    # records that were not associated with exactly two `measurement_date`
    # values
    DT <- rbind(
      DT[!(subject_id %in% unique(onlytwodates_fixed[['subject_id']]))],
      onlytwodates_fixed
    )

    setkeyv(DT, c('subject_id', 'measurement_date'))

    # Clean up intermediate objects
    rm(
      cannot_be_fixed_ids,
      needs_fixin_ids,
      onlytwodates_ids,
      onlytwodates_ids_needsfixin,
      onlytwodates_data,
      onlytwodates_bad,
      onlytwodates_ok,
      onlytwodates_long,
      onlytwodates_shouldbekept,
      onlytwodates_badfixed,
      onlytwodates_fixed
    )

    #--------------- Final summarization of data ----------------#

    ## Calculate mean and standard deviation of values per
    # `subject_id`-`measurement_date` combination

    ## NB: At this point there should not be any `subject_id` values associated
    # with one or more `measurement_date` values that "need fixin'" and who
    # have fewer than three distinct `measurement_date` values. (TSZB,
    # 2020-06-08)
    DT2 <- DT[,
      .(
        mean = mean(measurement),
        sd = sd(measurement)
      ),
      by = .(subject_id, measurement_date)
    ][
      order(measurement_date),
      `:=`(
        NumDates = .N,
        RecordNumberASC = seq_len(.N),
        RecordNumberDESC = .N - seq_len(.N) + 1L
      ),
      by = .(subject_id)
    ][
      is.na(sd),
      sd := 0
    ][]

    ## Identify summarized records with standard deviation strictly greater than
    # sd_thresholds[['same_day']] and enumerate each instance in ascending
    # chronological order per `subject_id` value (that have any such data)
    NeedsFixinSegmentNumber <- NULL
    needsfixin_ids <- DT2[
      sd > sd_thresholds[['same_day']],
      .(
        subject_id,
        measurement_date,
        NumDates,
        RecordNumberASC
      )
    ][
      order(RecordNumberASC),
      NeedsFixinSegmentNumber := seq_len(.N),
      by = .(subject_id)
    ][]

    ## Identify records from `temp` to handle (1) special case where record is
    # from earliest `measurement_date` and (2) special case where record is
    # from latest `measurement_date`
    needsfixin_isfirstrecord_ids <- needsfixin_ids[
      RecordNumberASC == 1L,
      .(subject_id, NeedsFixinSegmentNumber)
    ]

    needsfixin_islastrecord_ids <- needsfixin_ids[
      NumDates == RecordNumberASC,
      .(subject_id, NeedsFixinSegmentNumber)
    ]

    ## Pull original records for `subject_id`-`measurement_date` combinations
    # that "need fixin'"
    NeedsFixinSegmentOptionNumber <- NeedsFixinRecordNumberASC <- NULL
    needsfixin <- DT[
      needsfixin_ids,
      .(
        subject_id,
        measurement_date,
        measurement,
        NumDates,
        RecordNumberASC,
        NeedsFixinSegmentNumber
      ),
      on = .(
        subject_id,
        measurement_date,
        RecordNumberASC,
        NumDates
      )
    ][
      order(measurement),
      NeedsFixinSegmentOptionNumber := seq_len(.N),
      by = .(subject_id, measurement_date, RecordNumberASC)
    ][,
      .(
        subject_id,
        measurement_date,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber,
        measurement,
        NumDates,
        NeedsFixinRecordNumberASC = RecordNumberASC
      )
    ][
      ,
      `:=`(
        EarlierRecordNumberASC = fifelse(
          (
           (NeedsFixinRecordNumberASC > 1L)
           &
           (NeedsFixinRecordNumberASC < NumDates)
          ),
          NeedsFixinRecordNumberASC - 1L,
          fifelse(
            NeedsFixinRecordNumberASC == 1L,
            NeedsFixinRecordNumberASC + 1L,
            NeedsFixinRecordNumberASC - 2L
          ) # end inner fifelse
        ), # end first fifelse
        LaterRecordNumberASC = fifelse(
          (
           (NeedsFixinRecordNumberASC > 1L)
           &
           (NeedsFixinRecordNumberASC < NumDates)
          ),
          NeedsFixinRecordNumberASC + 1L,
          fifelse(
            NeedsFixinRecordNumberASC == 1L,
            NeedsFixinRecordNumberASC + 2L,
            NeedsFixinRecordNumberASC - 1L
          ) # end inner fifelse
        ) # end first fifelse
      )
    ][,
      NumDates := NULL
    ][]

    ## Identify the values of `RecordNumberASC` of the records neighboring the
    # records that "need fixin'"
    #   - If the record that "needs fixin'" is the first record for the
    #     `subject_id` (`RecordNumberASC` == 1), then set the corresponding
    #     "neighboring" values of `RecordNumberASC` as 2 and 3.
    #   - If the record that "needs fixin'" is the last record for the
    #     `subject_id` (`RecordNumberDESC` == 1 &
    #     `RecordNumberASC` == `NumDates`), then set the corresponding
    #     "neighboring" values of `RecordNumberASC` as (`NumDates` - 1) and
    #     (`NumDates` - 2).
    #   - Otherwise set the corresponding "neighboring" values of
    #     `RecordNumberASC` as (`RecordNumberASC` - 1) and
    #     (`RecordNumberASC` + 1).

    ## NB: The `needsfixin` data.table object contains records from the original
    #  (i.e., non-summarized) data.table object so the data are not necessarily
    #  one record per `subject_id`-`measurement_date` combination; as a result,
    #  you cannot simply add/subtract 1 from `RecordNumberASC` to get the
    #  correct "neighboring" values. (TSZB, 2020-06-08)
    needsfixin_neighborids <- melt(
      needsfixin,
      id.vars = c(
        'subject_id',
        'NeedsFixinSegmentNumber',
        'NeedsFixinSegmentOptionNumber'
      ),
      measure.vars = c(
        'EarlierRecordNumberASC',
        'LaterRecordNumberASC'
      ),
      value.name = 'RecordNumberASC'
    )

    ## Combine the original data records that "need fixin'" with the summarized
    # data records of their "neighboring" `measurement_date` values
    needsfixin_data <- rbind(
      needsfixin[,
        .(
          subject_id,
          measurement,
          NeedsFixinSegmentNumber,
          NeedsFixinSegmentOptionNumber,
          RecordNumberASC = NeedsFixinRecordNumberASC
        )
      ],
      DT2[
        needsfixin_neighborids,
        .(
          subject_id,
          measurement = mean,
          NeedsFixinSegmentNumber,
          NeedsFixinSegmentOptionNumber,
          RecordNumberASC
        ),
        on = .(subject_id, RecordNumberASC)
      ]
    )

    setkeyv(
      needsfixin_data,
      c(
        'subject_id',
        'NeedsFixinSegmentNumber',
        'NeedsFixinSegmentOptionNumber',
        'RecordNumberASC'
      )
    )

    # Clean up
    rm(needsfixin_ids, needsfixin_neighborids)

    ## Define convenience functions
    cdiff <- function(x, put_zero_last = FALSE) {
      if (put_zero_last[1L] == TRUE) {
        c(diff(x), 0L)
      } else {
        c(0L, diff(x))
      }
    }

    diff12 <- function(x,
                       absolute_value = FALSE,
                       special_case = c('none', 'first', 'last')) {

      special_case <- match.arg(special_case, c('none', 'first', 'last'))

      x_indices <- switch(
        special_case,
        none  = c(1L, 2L),
        first = c(2L, 1L),
        last  = c(2L, 3L)
      )

      if (absolute_value == TRUE) {
        abs(x[x_indices[2L]] - x[x_indices[1L]])
      } else {
        x[x_indices[2L]] - x[x_indices[1L]]
      }
    }

    newSD <- dif <- absdif <- NULL
    needsfixin_data_first <- needsfixin_data[
      needsfixin_isfirstrecord_ids,
      on = .(subject_id, NeedsFixinSegmentNumber)
    ][,
      .(
        newSD = sd(measurement),
        dif = diff12(measurement, special_case = 'first'),
        absdif = diff12(
          measurement,
          special_case = 'first',
          absolute_value = TRUE
        )
      ),
      by = .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    newSD <- dif <- absdif <- NULL
    needsfixin_data_last  <- needsfixin_data[
      needsfixin_islastrecord_ids,
      on = .(subject_id, NeedsFixinSegmentNumber)
    ][,
      .(
        newSD = sd(measurement),
        dif = diff12(measurement, special_case = 'last'),
        absdif = diff12(
          measurement,
          special_case = 'last',
          absolute_value = TRUE
        )
      ),
      by = .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    newSD <- dif <- absdif <- NULL
    needsfixin_data_none  <- needsfixin_data[
      !rbind(needsfixin_isfirstrecord_ids, needsfixin_islastrecord_ids),
      on = .(subject_id, NeedsFixinSegmentNumber)
    ][,
      .(
        newSD = sd(measurement),
        dif = diff12(measurement),
        absdif = diff12(measurement, absolute_value = TRUE)
      ),
      by = .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    # Combine special cases w/ neighbors and standard cases w/ neighbors
    needsfixin_data_all <- rbind(
      needsfixin_data_first,
      needsfixin_data_last,
      needsfixin_data_none
    )

    # Set key variables and sort the data table
    setkeyv(
      needsfixin_data_all,
      c(
        'subject_id',
        'NeedsFixinSegmentNumber',
        'NeedsFixinSegmentOptionNumber'
      )
    )

    setorder(
      needsfixin_data_all,
      subject_id,
      NeedsFixinSegmentNumber,
      newSD,
      absdif
    )

    ##  Create intermediate variables needed to address issues with sorting by
    # floating point values and how to break ties
    sddif <- NULL
    needsfixin_data_all <- needsfixin_data_all[
      ,
      `:=`(sddif = cdiff(newSD)),
      by = .(subject_id, NeedsFixinSegmentNumber)
    ][

    ][
      sddif > 0L & sddif < 1E-8,
      sddif := 0L
    ][]

    needsfixin_data_all[['rid']] <- rleidv(
      needsfixin_data_all,
      c('subject_id', 'NeedsFixinSegmentNumber', 'sddif')
    )

    newSD <- minSD <- rid <- NULL
    needsfixin_data_all <- needsfixin_data_all[
      needsfixin_data_all[
        sddif == 0L,
        .(cnt = .N, minSD = min(newSD)),
        by = .(subject_id, NeedsFixinSegmentNumber, rid)
      ],
      newSD := minSD,
      on = .(subject_id, NeedsFixinSegmentNumber, rid)
    ][,
      `:=`(sddif = cdiff(newSD)),
      by = .(subject_id, NeedsFixinSegmentNumber)
    ][
      ,
      rid := NULL
    ][]

    ## Merge with original "needs fixin'" data to be able to determine most
    # frequently occurring `measurement` value (to be used in case a tie-
    # breaker is needed)
    measurement_freq <- NumInstances <- NULL
    needsfixin_data_all <- needsfixin_data_all[
      needsfixin[,
        .(
          NeedsFixinSegmentOptionNumber,
          measurement_freq = .N
        ),
        by = .(
          subject_id,
          NeedsFixinSegmentNumber,
          measurement
        )
      ],
      .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber,
        NumInstances = measurement_freq,
        newSD,
        dif,
        absdif,
        sddif
      ),
      on = .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    ## Per `subject_id`-`NeedsFixinSegmentNumber` combination, take option with
    # (1) lowest `newSD` value [`measurement` value that yields lowest SD
    # statistic with previous and next person-date values of `measurement`],
    # (2) highest `NumInstances` value [`measurement` value that appears most
    # frequently], (3) lowest `absdif` value [`measurement` value that is
    # closest in magnitude to value of `measurement` for previous
    # `measurement_date`], and (4) highest `dif` value [if all else fails to
    # produce a unique decision, take the option with the largest `measurement`
    # value;

    ## NB: `dif` and `absdif` are calculated as `x[2] - x[1]` so larger signed
    # values indicate larger `measurement` values from the person-date that
    # needed fixin']. (TSZB, 2020-06-05)
    setorder(
      needsfixin_data_all,
      subject_id,
      NeedsFixinSegmentNumber,
      newSD,
      -NumInstances,
      absdif,
      -dif
    )

    needsfixin_bestoptions <- needsfixin_data_all[
      needsfixin_data_all[
        ,
        .(RN = .I[1L]),
        by = .(subject_id, NeedsFixinSegmentNumber)
      ][['RN']],
      .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    # Use "best options" to extract "fixed" data
    needsfixin_bestdata <- needsfixin[
      needsfixin_bestoptions,
      .(subject_id, measurement_date, measurement),
      on = .(
        subject_id,
        NeedsFixinSegmentNumber,
        NeedsFixinSegmentOptionNumber
      )
    ]

    ## Combine easily cleaned data (`DT2$sd <= sd_thresholds[['same_day']]`)
    # with "fixed" data (`needsfixin_bestdata`)
    measurement <- NULL
    all1pd <- rbind(
      DT2[
        sd <= sd_thresholds[['same_day']],
        .(
          subject_id,
          measurement_date,
          measurement = mean
        )
      ],
      needsfixin_bestdata[
        ,
        .(subject_id, measurement_date, measurement)
      ]
    )

    ## If requested, add the records that could not be "cleaned" back into the
    # data table
    if (keep_uncleanable == TRUE) {
      all1pd <- rbind(
        all1pd,
        DT_uncleanable[, .(subject_id, measurement_date, measurement)]
      )
      rm(DT_uncleanable)
    }

    # Set key variables as IDVar and DateVar
    setkeyv(all1pd, c('subject_id', 'measurement_date'))

    # Clean up
    rm(
      DT,
      DT2,
      needsfixin,
      needsfixin_isfirstrecord_ids,
      needsfixin_islastrecord_ids,
      needsfixin_data_first,
      needsfixin_data_last,
      needsfixin_data_none,
      needsfixin_data_all,
      needsfixin_data,
      needsfixin_bestdata,
      needsfixin_bestoptions,
      cdiff,
      diff12
    )

  } else {
    ## If DT and DT2 have the same number of rows, then just return a copy of
    # the input data object
    all1pd <- copy(DT)
  }

  ## Reset the variable names to the original names before returning the output
  # object
  setnames(
    all1pd,
    old = c('subject_id', 'measurement_date', 'measurement'),
    new = DT_oldnames
  )

  # Return the reduced data.table object
  return(all1pd)
}

#----------------------------- Enumerate records ------------------------------

#' Maciejewski algorithm part I.c. - enumerate records
#'
#' Enumerates records from input data set in ascending chronological order
#' (i.e., 1 = earliest \code{tmeasures} value, 2 = next earliest value, ...,
#' n = latest \code{tmeasures} value) and descending chronological order.
#' Calculates standard deviations between \code{measures} values and the
#' \code{measures} values of the two nearest (chronologically) neighboring
#' \code{tmeasures} values for each \code{id} value. Flags "high" SD values
#' when they occur in the first 3 \code{tmeasures} values or in the last 3
#' \code{tmeasures} values because these scenarios require special handling.
#'
#' @param df object of class \code{data.table}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param varvec optional. String vector containing \code{df}, \code{id},
#'   \code{measures}, and \code{tmeasures}.
#' @param sd_thresholds Object of type \code{list} with 1 Numeric value of the
#'   rolling standard deviation statistic above which the \code{measures} values
#'   of the three associated \code{id}-\code{tmeasures} combinations are
#'   considered indicative of containing one or more "outlier" values.
#' @return `data.table` with properly formatted columns for downstream
#'   processing/weight cleaning.
#' @examples
#' \dontrun{
#' data(cdw1000)
#'
#' consolidated.dt <- consolidate_sameday_records(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#'
#' enumerated.dt <- enum_records(
#'   df = consolidated.dt,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#' }
enum_records <- function(df,
                         id,
                         tmeasures,
                         measures,
                         varvec = NULL,
                         sd_thresholds = list('high_sd_flag' = 20L)) {

  #---------------------- Preliminary data object checks ----------------------
  ## RRE (Comment): At this stage in the Maciejewski algorithm, the user would
  # not need to run the prep_maciejewski() function as it's the first stage. It
  # has little overhead so I guess it's not too much of a burden on computation
  # time. It's also highly specialized and so, its not a standalone function.

  DT <- prep_maciejewski(
    df = df,
    id = id,
    measures = measures,
    tmeasures = tmeasures,
    varvec = varvec
  )

  DT_oldnames <- attr(DT, 'DT_oldnames')

  if (missing(id))        id        = colnames(DT)[1L]
  if (missing(tmeasures)) tmeasures = colnames(DT)[2L]
  if (missing(measures))  measures  = colnames(DT)[3L]

  #----------------------------- Enumerate records ----------------------------

  ## The following data.table chain accomplishes the same thing as the entire
  # %EnumerateRecords SAS macro
  subject_id <- measurement_date <- measurement <- NULL
  HighSDFlag <- stdev <- NULL
  RecordNumberASC <- RecordNumberDESC <- NULL
  DT <- DT[,
    .(
      subject_id,
      measurement_date,
      measurement,
      stdev = frollapply(
        measurement,
        n = 3L,
        FUN = sd,
        align = 'center'
      )
    )
  ][,
    HighSDFlag := stdev > sd_thresholds[['high_sd_flag']] & !is.na(stdev)
  ][
    order(measurement_date),
    `:=`(
      NumDates = .N,
      RecordNumberASC = seq_len(.N),
      RecordNumberDESC = .N - seq_len(.N) + 1L
    ),
    by = .(subject_id)
  ][
    RecordNumberASC == 1L
    |
    RecordNumberDESC == 1L,
    `:=`(stdev = NA_integer_, HighSDFlag = NA_integer_)
  ][]

  #----------------------- Reset names and return output ----------------------

  ## Reset the variable names to the original names before returning the output
  # object
  setnames(
    DT,
    old = c('subject_id', 'measurement_date', 'measurement'),
    new = DT_oldnames
  )

  # Return the enumerated data.table object
  return(DT)
}

#------------------------ Clean records/apply algorithm -----------------------

#' Maciejewski algorithm part II. - "clean" records
#'
#' \code{maciejewski_algo} applies "cleaning" algorithm to \code{measures}
#' values, removing suspected outliers.
#'
#' @param df object of class \code{data.table}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param varvec optional. String vector containing \code{df}, \code{id},
#'   \code{measures}, and \code{tmeasures}.
#' @param sd_thresholds Object of type \code{list} with 5 values
#' \itemize{
#'    \item same_day - numeric value of same-day standard deviation statistics
#'    above which the data of the associated \code{id}-\code{tmeasures}
#'    combination are considered indicative of a discrepancy in \code{measures}
#'    values that requires more scrutiny to resolve into one unique
#'    \code{measures} value per \code{id}-\code{tmeasures} combination than
#'    simply taking the arithmetic mean of the values.
#'    \item high_sd_flag - Numeric value of the rolling standard deviation
#'    statistic above which the \code{measures} values of the three associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing one or more "outlier" values.
#'    \item first_last - Numeric value of the rolling standard deviation
#'    statistic above which the \code{measures} values of the three associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing one or more "outlier" values. This value is used only to detect
#'    outliers among the first three and the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order.
#'    \item abc_low - Numeric value of the pairwise standard deviation statistic
#'    below which the \code{measures} values of the two associated
#'    \code{id}-\code{tmeasures} combinations are considered not indicative of
#'    containing an "outlier" value. This value is used only to detect outliers
#'    in all observations except the first three or the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order. This argument is used in conjunction with the \code{abc_high}
#'    argument.
#'    \item abc_high - Numeric value of the pairwise standard deviation
#'    statistic above which the \code{measures} values of the two associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing an "outlier" value. This value is used only to detect outliers
#'    in all observations except the first three or the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order. This argument is used in conjunction with the \code{abc_low}
#'    argument.
#' }
#' @return \code{data.table} with measurements cleaned according to Maciejewski
#'   et al. 2016.
#' @examples
#' \dontrun{
#' data(cdw1000)
#'
#' # with defaults
#' consolidated.dt <- consolidate_sameday_records(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#'
#' maciejewski.dt <- maciejewski_algo(
#'   df = consolidated.dt,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#'
#' glimpse(maciejewski.dt)
#'
#' uniqueN(maciejewski.dt[, .(id, WeightDate)])
#' }
maciejewski_algo <- function(df,
                             id,
                             tmeasures,
                             measures,
                             varvec = NULL,
                             sd_thresholds = list(
                               'same_day'     = 2L,
                               'high_sd_flag' = 20L,
                               'first_last'   = 35L,
                               'abc_low'      = 10L,
                               'abc_high'     = 45L
                             )) {

  #---------------------- Preliminary data object checks ----------------------

  ## RRE (Comment): At this stage in the Maciejewski algorithm, the user would
  # not need to run the prep_maciejewski() function as it's the first stage. It
  # has little overhead so I guess it's not too much of a burden on computation
  # time. It's also highly specialized and so, its not a standalone function.

  DT <- prep_maciejewski(
    df = df,
    id = id,
    measures = measures,
    tmeasures = tmeasures,
    varvec = varvec
  )

  DT_oldnames <- attr(DT, 'DT_oldnames')
  if (missing(id))        id        = colnames(DT)[1L]
  if (missing(tmeasures)) tmeasures = colnames(DT)[2L]
  if (missing(measures))  measures  = colnames(DT)[3L]

  subject_id <- measurement_date <- measurement <- NULL

  #------------------------ Enumerate records (take 1) ------------------------
  ## RRE (Comment): Similar to the above, this would already be handled in
  # sequence in the following function `maciejewski2016()`

  ## This single line accomplishes the same thing as the entire
  # %EnumerateRecords SAS macro

  ## RRE (Comment/Question): is this the same as `enum_records()`? It must be.
  # It seems you've created enum_records (previously EnumerateRecords), but
  # never call it, even in the complete Maciejewski code below. Was there a
  # problem with calling it outside of this function, before running
  # `maciejewski_algo()`?
  HighSDFlag <- stdev <- NULL
  NumDates <- RecordNumberASC <- RecordNumberDESC <- NULL
  DT <- DT[,
    .(
      subject_id,
      measurement_date,
      measurement,
      stdev = frollapply(
        measurement,
        n = 3L,
        FUN = sd,
        align = 'center'
      )
    )
  ][,
    HighSDFlag := stdev > sd_thresholds[['high_sd_flag']] & !is.na(stdev)
  ][
    order(measurement_date),
    `:=`(
      NumDates = .N,
      RecordNumberASC = seq_len(.N),
      RecordNumberDESC = .N - seq_len(.N) + 1L
    ),
    by = .(subject_id)
  ][
    RecordNumberASC == 1L
    |
    RecordNumberDESC == 1L,
    `:=`(
      stdev = NA_integer_,
      HighSDFlag = NA_integer_
    )
  ][]

  #---------- Prepare to apply cleaning algorithm to first/last 3 obs ----------

  ## Shift records to get lead()ed and lag()ed values in the same record as each
  # `measurement` value
  BeforeValue <- CenterValue <- AfterValue <- NULL
  DT[,
    `:=`(
      BeforeValue = shift(measurement, n = 1L, type = 'lag'),
      CenterValue = measurement,
      AfterValue  = shift(measurement, n = 1L, type = 'lead')
    )
  ]

  ## Identify records where all of the first/last 3 records are flagged for
  # "high SD"

  ## NB: These records will *not* undergo first/last 3 obs processing.
  # (TSZB, 2020-07-03)
  cnt <- NULL
  first3_allhighsd <- DT[
    (
     (HighSDFlag == TRUE)
     &
     (NumDates >= 5L)
     &
     (RecordNumberASC >= 2L)
     &
     (RecordNumberASC <= 4L)
    ),
    .(cnt = .N),
    by = .(subject_id)
  ][
    cnt == 3L,
    .(subject_id)
  ]

  last3_allhighsd <- DT[
    (
     (HighSDFlag == TRUE)
     &
     (NumDates >= 5L)
     &
     (RecordNumberDESC >= 2L)
     &
     (RecordNumberDESC <= 4L)
    ),
    .(cnt = .N),
    by = .(subject_id)
  ][
    cnt == 3L,
    .(subject_id)
  ]

  ## Extract all 2nd and 3rd earliest/latest records with "high" SD
  NumRecords <- NULL
  first3_obsproc_all <- DT[
    (RecordNumberASC >= 2L)
    &
    (RecordNumberASC <= 3L)
    &
    (stdev > sd_thresholds[['first_last']])
  ][
    !first3_allhighsd
  ][,
    NumRecords := .N,
    by = .(subject_id)
  ][]

  last3_obsproc_all <- DT[
    (RecordNumberDESC >= 2L)
    &
    (RecordNumberDESC <= 3L)
    &
    (stdev > sd_thresholds[['first_last']])
  ][
    !last3_allhighsd
  ][,
    NumRecords := .N,
    by = .(subject_id)
  ][]

  # Clean up
  rm(first3_allhighsd, last3_allhighsd)

  ## Identify records from first/last 3 obs with high SD where exactly 1 record
  # was found per `subject_id`-`measurement_date`-'first/last' combination

  ## NB: These will need further processing in the AB-AC-BC steps.
  # (TSZB, 2020-07-04)
  first3_obsproc <- first3_obsproc_all[
    NumRecords == 1L
  ][,
    NumRecords := NULL
  ][]

  last3_obsproc <- last3_obsproc_all[
    NumRecords == 1L
  ][,
    NumRecords := NULL
  ][]

  ## If both the second and third earliest/latest records have "high" SD, then
  # flag the second earliest/latest record for deletion
  RecordNumberASC <- NULL
  WhichToDeleteASC = unique(
    first3_obsproc_all[
      NumRecords == 2L,
      .(
        subject_id,
        RecordNumberASC = 2L
      )
    ]
  )

  RecordNumberDESC <- NULL
  WhichToDeleteDESC <- unique(
    last3_obsproc_all[
      NumRecords == 2L,
      .(
        subject_id,
        RecordNumberDESC = 2L
      )
    ]
  )

  #---------------- Clean first 3 observations with 1 high SD ----------------

  ## Melt the high SD record and its neighboring records from the first 3
  # observations with exactly 1 high SD record
  first3_obsproc_long <- melt(
    DT[
      first3_obsproc,
      on = .(
        subject_id,
        measurement_date,
        NumDates,
        RecordNumberASC
      )
    ][,
      .(
        subject_id,
        measurement_date,
        BeforeValue,
        CenterValue,
        AfterValue
      )
    ],
    id.vars = c('subject_id', 'measurement_date'),
    measure.vars = c('BeforeValue', 'CenterValue', 'AfterValue')
  )

  ## Calculate pairwise SD statistics between the first 3 observations
  variable <- value <- NULL
  AB <- first3_obsproc_long[
    variable != 'CenterValue',
    .(stdev_no_center = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  AC <- first3_obsproc_long[
    variable != 'BeforeValue',
    .(stdev_no_before = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  BC <- first3_obsproc_long[
    variable != 'AfterValue',
    .(stdev_no_after = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  # Merge everything together
  ABC_first <- BC[
    AB[
      AC,
      on = .(subject_id, measurement_date)
    ],
    on = .(subject_id, measurement_date)
  ]

  ## Determine which record to delete by identifying the record whose removal
  # results in the lowest SD statistic between the remaining records
  stdev_no_after <- stdev_no_before <- stdev_no_center <- NULL
  ABC_first[,
    WhichToDeleteASC := fifelse(
      (
        (
          (stdev_no_after < stdev_no_before)
          &
          (stdev_no_after < stdev_no_center)
        )
      ),
      0L,
      fifelse(
        (
          (stdev_no_before < stdev_no_center)
          &
          (0 <= stdev_no_before)
          &
          (stdev_no_before < sd_thresholds[['abc_low']])
          &
          (stdev_no_center >= sd_thresholds[['abc_high']])
          &
          (stdev_no_after >= sd_thresholds[['abc_high']])
        ),
        1L,
        fifelse(
          (
            (0 <= stdev_no_center)
            &
            (stdev_no_center < sd_thresholds[['abc_low']])
            &
            (stdev_no_before >= sd_thresholds[['abc_high']])
            &
            (stdev_no_after >= sd_thresholds[['abc_high']])
          ),
          2L,
          0L
        ) # end third fifelse()
      ) # end second fifelse()
    ) # end first fifelse()
  ]

  ## Append newly-flagged records for deletion from the first 3 observations to
  # those previously flagged for deletion
  RecordNumberASC <- NULL
  WhichToDeleteASC <- rbind(
    WhichToDeleteASC,
    ABC_first[
      WhichToDeleteASC > 0L,
      .(
        subject_id,
        RecordNumberASC = WhichToDeleteASC
      )
    ]
  )

  # Clean up
  rm(
    AB,
    AC,
    BC,
    ABC_first,
    first3_obsproc,
    first3_obsproc_all,
    first3_obsproc_long
  )


  #----------------- Clean last 3 observations with 1 high SD -----------------

  ## Melt the high SD record and its neighboring records from the last 3
  # observations with exactly 1 high SD record
  last3_obsproc_long = melt(
    DT[
      last3_obsproc,
      on = .(
        subject_id,
        measurement_date,
        NumDates,
        RecordNumberASC
      )
    ][,
      .(
        subject_id,
        measurement_date,
        BeforeValue,
        CenterValue,
        AfterValue
      )
    ],
    id.vars = c('subject_id', 'measurement_date'),
    measure.vars = c('BeforeValue', 'CenterValue', 'AfterValue')
  )

  # Calculate pairwise SD statistics between the last 3 observations
  AB <- last3_obsproc_long[
    variable != 'CenterValue',
    .(stdev_no_center = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  AC <- last3_obsproc_long[
    variable != 'BeforeValue',
    .(stdev_no_before = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  BC <- last3_obsproc_long[
    variable != 'AfterValue',
    .(stdev_no_after = sd(value)),
    by = .(subject_id, measurement_date)
  ]

  # Merge everything together
  ABC_last <- BC[
    AB[
      AC,
      on = .(subject_id, measurement_date)
    ],
    on = .(subject_id, measurement_date)
  ]

  ## Determine which record to delete by identifying the record whose removal
  # results in the lowest SD statistic between the remaining records
  ABC_last[,
    WhichToDeleteDESC := fifelse(
      (
        (stdev_no_before < stdev_no_after)
        &
        (stdev_no_before < stdev_no_center)
      ),
      0L,
      fifelse(
        (
          (stdev_no_after < stdev_no_center)
          &
          (0 <= stdev_no_after)
          &
          (stdev_no_after < sd_thresholds[['abc_low']])
          &
          (stdev_no_center >= sd_thresholds[['abc_high']])
          &
          (stdev_no_before >= sd_thresholds[['abc_high']])
        ),
        1L,
        fifelse(
          (
            (0 <= stdev_no_center)
            &
            (stdev_no_center < sd_thresholds[['abc_low']])
            &
            (stdev_no_after >= sd_thresholds[['abc_high']])
            &
            (stdev_no_before >= sd_thresholds[['abc_high']])
          ),
          2L,
          0L
        ) # end third fifelse
      ) # end second fifelse
    ) # end first ifelse/WhichToDeleteDesc
  ]

  ## Append newly-flagged records for deletion from the last 3 observations to
  # those previously flagged for deletion
  RecordNumberDESC <- NULL
  WhichToDeleteDESC <- rbind(
    WhichToDeleteDESC,
    ABC_last[
      WhichToDeleteDESC > 0L,
      .(
        subject_id,
        RecordNumberDESC = WhichToDeleteDESC
      )
    ]
  )

  # Clean up
  rm(
    AB,
    AC,
    BC,
    ABC_last,
    last3_obsproc,
    last3_obsproc_all,
    last3_obsproc_long
  )

  #-------- Combine records flagged for deletion from first/last 3 obs --------

  # Collect record identifiers for records flagged for deletion
  x <- rbind(
    # ascending
    DT[
      WhichToDeleteASC,
      on = .(subject_id, RecordNumberASC)
    ][
      ,
      .(subject_id, measurement_date)
    ],
    # descending
    DT[
      WhichToDeleteDESC,
      on = .(subject_id, RecordNumberDESC)
    ][
      ,
      .(subject_id, measurement_date)
    ]
  )


  #------------------------ Enumerate records (take 2) ------------------------

  ## This single line (a) removes all records flagged for deletion and then (b)
  # accomplishes the same thing as the entire %EnumerateRecords SAS macro
  DT <- DT[
    !x,
    .(subject_id, measurement_date, measurement),
    on = .(subject_id, measurement_date)
  ][
    ,
    .(
      subject_id,
      measurement_date,
      measurement,
      stdev = frollapply(
        measurement,
        n = 3L,
        FUN = sd,
        align = 'center'
      )
    )
  ][
    ,
    HighSDFlag := stdev > sd_thresholds[['high_sd_flag']] & !is.na(stdev)
  ][
    order(measurement_date),
    `:=`(
      NumDates = .N,
      RecordNumberASC = seq_len(.N),
      RecordNumberDESC = .N - seq_len(.N) + 1L
    ),
    by = .(subject_id)
  ][
    RecordNumberASC == 1L | RecordNumberDESC == 1L,
    `:=`(
      stdev = NA_integer_,
      HighSDFlag = NA_integer_
    )
  ][]

  ## Enumerate "high SD segments" and the records within each segment and keep
  # only the first and last records of each segment
  SegmentRecordNumberASC <- SegmentRecordNumberDESC <- HighSDSegment <- NULL
  DT <- DT[
    ,
    .(
      subject_id,
      measurement_date,
      measurement,
      HighSDFlag,
      RecordNumberASC,
      HighSDSegment = rleidv(
        DT,
        cols = c('subject_id', 'HighSDFlag')
      )
    )
  ][
    ,
    `:=`(
      SegmentRecordNumberASC = seq_len(.N),
      SegmentRecordNumberDESC = .N - seq_len(.N) + 1L
    ),
    by = .(subject_id, HighSDSegment)
  ][
    (HighSDFlag == FALSE)
    |
    is.na(HighSDFlag)
    |
    (
      (HighSDFlag == TRUE)
      &
      (
        (SegmentRecordNumberASC == 1L)
        |
        (SegmentRecordNumberDESC == 1L)
      )
    ),
    .(subject_id, measurement_date, measurement)
  ][]

  # Clean up
  rm(WhichToDeleteASC, WhichToDeleteDESC, x)

  #---------------------- Reset names and return output ----------------------

  ## Reset the variable names to the original names before returning the output
  # object
  setnames(
    DT,
    old = c('subject_id', 'measurement_date', 'measurement'),
    new = DT_oldnames
  )

  # Return the cleaned data.table object
  return(DT)
}

#-------------------------------- main function -------------------------------

#' Maciejewski algorithm complete
#'
#' \code{maciejewski} applies both part I and II of the Maciejewski 2016
#' algorithm to a "raw" input dataset, it is basically \code{maciejewski_algo}
#' with additional cleaning processes. The various threshold values used
#' throughout these macros were selected via an iterative process where they
#' were (subjectively) judged to provide the best balance between removing
#' highly improbable weight measurement values and discounting true drastic
#' changes in weight over time. If you believe that these threshold values
#' might not be optimal for your data, please feel free to adjust them to suit
#' your needs; they need not be considered the absolute "gold standard",
#' especially if better criteria are found.
#'
#' @param df object of class \code{data.table}, containing \code{id} and
#'   \code{measures}
#' @param id string corresponding to the name of the column of patient
#'   identifiers in \code{df}.
#' @param measures string corresponding to the name of the column of measures in
#'   \code{df}, e.g., numeric weight data if using to clean weight data.
#' @param tmeasures string corresponding to the name of the column of measure
#'   dates and/or times in \code{df}
#' @param varvec optional. String vector containing \code{df}, \code{id},
#'   \code{measures}, and \code{tmeasures}.
#' @param n_cleaning_iters number of times to pass data through the cleaning
#'   algorithm.
#' @param keep_uncleanable subjects having exactly two \code{tmeasures}
#'  (internally `measurement_date`) values each with a standard deviation of the
#'  \code{measures} (internally `measurement`) values that is strictly greater
#'  than 2, are termed "uncleanable", the user can choose to keep or remove the
#'  "uncleanable" subjects. The default is to keep them/\code{TRUE}.
#' @param outliers object of type \code{list} with two values, \code{UB} and
#'  \code{LB}, the lower and upper bounds of plausible values of
#'  \code{measures}. Default is \code{list(`LB` = 75L, `UB` = 700L)}.
#' @param sd_thresholds Object of type \code{list} with 5 values
#' \itemize{
#'    \item same_day - numeric value of same-day standard deviation statistics
#'    above which the data of the associated \code{id}-\code{tmeasures}
#'    combination are considered indicative of a discrepancy in \code{measures}
#'    values that requires more scrutiny to resolve into one unique
#'    \code{measures} value per \code{id}-\code{tmeasures} combination than
#'    simply taking the arithmetic mean of the values.
#'    \item high_sd_flag - Numeric value of the rolling standard deviation
#'    statistic above which the \code{measures} values of the three associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing one or more "outlier" values.
#'    \item first_last - Numeric value of the rolling standard deviation
#'    statistic above which the \code{measures} values of the three associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing one or more "outlier" values. This value is used only to detect
#'    outliers among the first three and the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order.
#'    \item abc_low - Numeric value of the pairwise standard deviation statistic
#'    below which the \code{measures} values of the two associated
#'    \code{id}-\code{tmeasures} combinations are considered not indicative of
#'    containing an "outlier" value. This value is used only to detect outliers
#'    in all observations except the first three or the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order. This argument is used in conjunction with the \code{abc_high}
#'    argument.
#'    \item abc_high - Numeric value of the pairwise standard deviation
#'    statistic above which the \code{measures} values of the two associated
#'    \code{id}-\code{tmeasures} combinations are considered indicative of
#'    containing an "outlier" value. This value is used only to detect outliers
#'    in all observations except the first three or the last three
#'    \code{id}-\code{tmeasures} combinations, in chronologically ascending
#'    order. This argument is used in conjunction with the \code{abc_low}
#'    argument.
#' }
#' @param skip_consolidation bypass consolidation step for datasets that
#'   already have one record per date per person. Default is \code{FALSE}.
#' @param record_execution_times record computation time? Default is
#'   \code{FALSE}.
#' @param print_execution_times print the recorded computation time? Default is
#'   set to the value of \code{record_execution_times}.
#' @return \code{data.table} with measurements cleaned according to Maciejewski
#'   et al. 2016.
#' @examples
#' \dontrun{
#' data(cdw1000)
#'
#' maciejewski.dt <- maciejewski(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight'
#' )
#'
#' glimpse(maciejewski.dt)
#'
#' maciejewski.dt <- maciejewski(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight',
#'   record_execution_times = TRUE
#' )
#'
#' maciejewski.dt <- maciejewski(
#'   df = cdw1000,
#'   id = 'id',
#'   tmeasures = 'WeightDate',
#'   measures = 'Weight',
#'   skip_consolidation = TRUE,
#'   record_execution_times = TRUE
#' )
#' }
maciejewski <- function(df,
                        id,
                        measures,
                        tmeasures,
                        varvec = NULL,
                        n_cleaning_iters = 1L,
                        keep_uncleanable = TRUE,
                        outliers = list('LB' = 75L, 'UB' = 700L),
                        sd_thresholds = list(
                          'same_day'     = 2L,
                          'high_sd_flag' = 20L,
                          'first_last'   = 35L,
                          'abc_low'      = 10L,
                          'abc_high'     = 45L
                        ),
                        skip_consolidation = FALSE,
                        record_execution_times = FALSE,
                        print_execution_times = record_execution_times) {

  ## Disable fancy quotes and string-to-factor coercion for the duration of this
  # function (if enabled by default)
  oldufq <- getOption("useFancyQuotes")
  oldsaf <- getOption("stringsAsFactors")
  on.exit(options(useFancyQuotes = oldufq, stringsAsFactors = oldsaf))
  options(useFancyQuotes = FALSE, stringsAsFactors = FALSE)

  #---------------------- Preliminary data object checks ----------------------

  DT <- prep_maciejewski(
    df = df,
    id = id,
    tmeasures = tmeasures,
    measures = measures,
    varvec = varvec
  )

  DT_oldnames <- attr(DT, 'DT_oldnames')
  if (missing(id))        id        = colnames(DT)[1L]
  if (missing(tmeasures)) tmeasures = colnames(DT)[2L]
  if (missing(measures))  measures  = colnames(DT)[3L]

  if (record_execution_times == FALSE && print_execution_times == TRUE) {
    record_execution_times = TRUE
  }

  if (record_execution_times == TRUE) {
    RowNumber_TimingDF = 0L
    TimingDF = data.frame(
      InputData = rep(
        deparse(substitute(DSN)),
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      IterationNumber = rep(
        NA_integer_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      TotalIterations = rep(
        NA_integer_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      FunctionName = rep(
        NA_character_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      PreDateTime = rep(
        Sys.time(),
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      PostDateTime = rep(
        Sys.time(),
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      IterationDuration = rep(
        Sys.time() - Sys.time(),
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      PreNumberOfRecords = rep(
        NA_integer_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      PostNumberOfRecords = rep(
        NA_integer_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      RemovedNumberOfRecords = rep(
        NA_integer_,
        times = as.integer(n_cleaning_iters)
                +
                as.integer(!skip_consolidation)
      ),
      stringsAsFactors = FALSE
    )
  }

  #---------------- Consolidate same-day records, if requested ----------------
  if (skip_consolidation == FALSE) {
    if (record_execution_times == TRUE) {
      RowNumber_TimingDF <- RowNumber_TimingDF + 1L
      TimingDF[RowNumber_TimingDF, "IterationNumber"] <- 0L
      TimingDF[RowNumber_TimingDF, "TotalIterations"] <- 0L
      TimingDF[RowNumber_TimingDF, "FunctionName"] <- 'consolidate_sameday_records'
      TimingDF[RowNumber_TimingDF, "PreDateTime"] <- Sys.time()
      TimingDF[RowNumber_TimingDF, "PreNumberOfRecords"] <- NROW(DT)
    }

    # RRE (Comment): Shouldn't this be part of an if-else statement? whether or
    #                not the user specifies `varvec`?
    # DT <- consolidate_sameday_records(
    #   df = DT,
    #   id = id,
    #   tmeasures = tmeasures,
    #   measures = measures,
    #   varvec = varvec,
    #   keep_uncleanable = keep_uncleanable,
    #   outliers = outliers,
    #   sd_thresholds = sd_thresholds
    # )
    DT <- consolidate_sameday_records(
      df = DT,
      varvec = colnames(DT),
      keep_uncleanable = keep_uncleanable,
      outliers = outliers,
      sd_thresholds = sd_thresholds
    )

    if (record_execution_times == TRUE) {
      TimingDF[RowNumber_TimingDF, "PostDateTime"] <- Sys.time()
      TimingDF[RowNumber_TimingDF, "PostNumberOfRecords"] <- NROW(DT)
    }
  }

  #-------- Execute `maciejewski_algo()` the specified number of times --------

  BreakFlag = FALSE

  for (CurrentCleaningIteration in seq_len(as.integer(n_cleaning_iters))) {
    if (record_execution_times == TRUE) {
      RowNumber_TimingDF <- RowNumber_TimingDF + 1L
      TimingDF[RowNumber_TimingDF, "IterationNumber"] <-
        as.integer(CurrentCleaningIteration)
      TimingDF[RowNumber_TimingDF, "TotalIterations"] <-
        as.integer(n_cleaning_iters)
      TimingDF[RowNumber_TimingDF, "FunctionName"] <- 'maciejewski_algo'
      TimingDF[RowNumber_TimingDF, "PreDateTime"] <- Sys.time()
      TimingDF[RowNumber_TimingDF, "PreNumberOfRecords"] <- NROW(DT)
    } else {
      nrow_pre <- NROW(DT)
    }

    DT <- maciejewski_algo(
      df = DT,
      varvec = colnames(DT),
      sd_thresholds = sd_thresholds
    )

    if (record_execution_times == TRUE) {
      TimingDF[RowNumber_TimingDF, "PostDateTime"] <- Sys.time()
      TimingDF[RowNumber_TimingDF, "PostNumberOfRecords"] <- NROW(DT)
      if (
        TimingDF[RowNumber_TimingDF, "PreNumberOfRecords"]
          == TimingDF[RowNumber_TimingDF, "PostNumberOfRecords"]
      ) {
        BreakFlag = TRUE
      }
    } else {
      if (nrow_pre == NROW(DT)) {
        BreakFlag = TRUE
      }
    }
    if (BreakFlag == TRUE) {
      break
    }
  } # end for()

  if (record_execution_times == TRUE) {
    TimingDF <- TimingDF[seq_len(RowNumber_TimingDF), ]

    ## NB: Note `post - pre` for datetime but `pre - post` for record count.
    # (TSZB, 2020-07-16)
    TimingDF[["IterationDuration"]] <- with(
      TimingDF,
      PostDateTime - PreDateTime
    )
    TimingDF[["RemovedNumberOfRecords"]] <- with(
      TimingDF,
      PreNumberOfRecords - PostNumberOfRecords
    )
  }

  if (record_execution_times == TRUE && print_execution_times == TRUE) {
    print(TimingDF)
  }

  return(as.data.frame(DT))
}

