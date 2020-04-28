#' Small sample of weight data
#'
#' A dataset containing 16 individuals sampled from the U.S. Dept. of Veterans
#' Affairs Corporate Data Warehouse. Data contains weights collected +/- 2 years
#' from first VA visit in 2016.
#'
#' @format A data frame with 354 rows and 7 variables:
#' \describe{
#'   \item{id}{unique patient identifier}
#'   \item{WeightDate}{date weight was recorded}
#'   \item{Weight}{weight, in lbs.}
#'   \item{VisitDate}{index visit date in 2016}
#'   \item{Age}{age at index visit}
#'   \item{Sex}{male or female, designated "M" or "F"}
#'   \item{Height}{patient height, in inches}
#' }
"cdw32"

#' Large sample of weight data
#'
#' A dataset containing 1,000 individuals sampled from the U.S. Dept. of Veterans
#' Affairs Corporate Data Warehouse. Data contains weights collected +/- 2 years
#' from first VA visit in 2016.
#'
#' @format A data frame with 11,373 rows and 7 variables:
#' \describe{
#'   \item{id}{unique patient identifier}
#'   \item{WeightDate}{date weight was recorded}
#'   \item{Weight}{weight, in lbs.}
#'   \item{VisitDate}{index visit date in 2016}
#'   \item{Age}{age at index visit}
#'   \item{Sex}{male or female, designated "M" or "F"}
#'   \item{Height}{patient height, in inches}
#' }
"cdw1000"
