#-----------------------------------------------------------------------------#
# US Dept. of Veterans Affairs - Corporate Data Warehouse
# Weight Measurement "Cleaning" Algorithm - Janney 2016 version
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
# directory as WindowCleaner.f (this file - WindowCleaner.R)
#-----------------------------------------------------------------------------#

#------------------------- Windows + Outlier Removal -------------------------#

#' @title WindowCleaner Measurment Cleaning Algorithm
#' @param DF object of class data.frame, containing id and weights
#' @param id string corresponding to the name of the column of patient IDs in `DF`
#' @param measures string corresponding to the name of the column of measures in `DF`, e.g., numeric weight data if using to clean weight data
#' @param tmeasures string corresponding to the name of the column of measure dates and/or times in `DF`
#' @param startPoint string corresponding to the name of the column in `DF` holding the time at which subsequent measurement dates will be assessed, should be the same for each person. Eg., if t = 0 (t[1]) corresponds to an index visit held by the variable 'VisitDate', then startPoint should be set to 'VisitDate'
#' @param t numeric vector of time points to collect measurements, eg. c(0, 182.5, 365) for measure collection at t = 0, t = 180 (6 months from t = 0), and t = 365 (1 year from t = 0). Default is c(0, 182.5, 365) according to Janney et al. 2016
#' @param windows numeric vector of measurement collection windows to use around each time point in t. Eg. Janney et al. 2016 use c(30, 60, 60) for t of c(0, 182.5, 365), implying that the closest measurement t = 0 will be collected 30 days prior to and 30 days post startPoint. Subsequent measurements will be collected 60 days prior to and 60 days post t0+182.5 days, and t0+365 days
#' @param outliers optional. object of type list with numeric inputs corresponding to the upper and lower bound for each time entry in parameter `t`. Default is list(LB = c(91, 72, 72), UB = c(600, 650, 650)) for t = c(0, 182.56, 365), differing between baseline and subsequent measurment collection dates. If not specified then only the subsetting and window functions will be applied.
windowCleaner.f <- function(DF,
                            id,
                            measures,
                            tmeasures,
                            startPoint,
                            t = c(0, 182.5, 365),
                            windows = c(30, 60, 60),
                            outliers = list(LB = c(91, 72, 72),
                                            UB = c(600, 650, 650))) {
  
  windowsApplied.df <- windows.f(DF = DF,
                                 id = id,
                                 measures = measures,
                                 tmeasures = tmeasures,
                                 startPoint = startPoint,
                                 t = t,
                                 windows = windows)
  
  outliersRemoved.df <- outliers.f(DF = windowsApplied.df,
                                   measures = measures,
                                   t = t,
                                   outliers = outliers)
  # return fully cleaned data
  outliersRemoved.df
}