% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Janney2016.R
\name{flag_out}
\alias{flag_out}
\title{Outlier Filter}
\usage{
flag_out(
  df,
  measures,
  t = c(0, 182.5, 365),
  outliers = list(LB = c(91, 72, 72), UB = c(600, 650, 650))
)
}
\arguments{
\item{df}{object of class data.frame, containing \code{id} and
\code{weights}.}

\item{measures}{string corresponding to the name of the column of measures
in \code{df}, e.g., numeric weight data if using to clean weight data.}

\item{t}{numeric vector of time points to collect measurements,
eg. \code{c(0, 182.5, 365)} for measure collection at \code{t = 0},
\code{t = 180} (6 months from \code{t = 0}), and \code{t = 365} (1 year
from \code{t = 0}). Default is \code{c(0, 182.5, 365)} according to
Janney et al. 2016}

\item{outliers}{object of type list with numeric inputs corresponding to the
upper and lower bound for each time entry in parameter \code{t}. Default is
\code{list(LB = c(91, 72, 72), UB = c(600, 650, 650))} for
\code{t = c(0, 182.56, 365)}, differing between baseline and subsequent
measurement collection dates}
}
\value{
data frame with \code{measures} flagged as \code{outliers}. The
  result is the same number of rows as df, and 1 extra column, named
  \code{measout}.
}
\description{
\code{flag_out} flags outliers. To be applied following the result of a
call to \code{meas_collect}. "Flag" may not be the most appropriate choice
of words as it actually sets outlier measurements to \code{NA}.
}
