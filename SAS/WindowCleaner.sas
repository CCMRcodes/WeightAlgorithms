/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm - Janney 2016 Version

Author: Richard Ryan Evans
Development Start Date: 09/25/2018

Rationale: Set time points, set windows around those time points,
		   collect weights, then remove outliers from those collected
		   weights.

Excerpt from published methods: 
	"Baseline, 6- and 12-mo body weight measures were retrieved for each MOVE! 
	veteran from the VHA patient care databases. Baseline weight was measured 
	within 30 d of MOVE! enrollment. The closest weight within a 60-d window 
	of the follow-up target date (180 d for 6 mo and 365 d for 12 mo) was 
	selected/defined as weight for 6- and 12-mo follow-ups.23 Weight was coded 
	missing if not available at baseline, 6 mo, and/or 12 mo. Outliers were 
	defined as baseline weight less than 91 lb or greater than 600 lb; 
	6- or 12-mo weight less than 72 lb or greater than 650 lb; weight change
	from baseline greater than 100 lb..."

NOTE: macro can currently only accept three time points, no more, no less
	  and it will stay this way until someone can tell me how to either loop
	  through any number of time points in SAS with a similar framework
      or how to form the cross-join so that the results are more predictable.
*****************************************************************************/

ods html close; ods html;

/******************************** Set Windows *******************************/

/*
title: Windows.f Measurment Cleaning Algorithm;
param: df: table containing id and weights;
param: id: string corresponding to the name of the column of patient IDs in df
param: measures: string corresponding to the name of the column of measures in 
	   df, e.g., numeric weight data if using to clean weight data.
param: tmeasures: string corresponding to the name of the column of measure 
	   dates and/or times in df
param: startPoint: string corresponding to the name of the column in df holding
	   the time at which subsequent measurement dates will be assessed, 
	   should be the same for each person. Eg., if t = 0 corresponds 
	   to an index visit held by the variable 'VisitDate', then 
	   startPoint should be set to 'VisitDate'
param: TWindows: table with time points to collect data and windows 
	   around each window.
	   Columns should be:
	   column: t: numeric vector of time points to collect measurements, 
	   			  e.g., c(0, 182.5, 365) for measure collection at t = 0, 
			      t = 180 (6 months from t = 0), and t = 365 
			 	  (1 year from t = 0). Default is c(0, 182.5, 365) according 
			      to Janney et al. 2016
	   column: windows: numeric vector of measurement collection windows to use
						around each time point in t. E.g., Janney et al. 2016
						use (30, 60, 60) for t of (0, 182.5, 365), implying 
					    that the closest measurement t = 0 will be collected 
					    30 days prior to and 30 days post startPoint. 
						Subsequent measurements will be collected 60 days prior 
						to and 60 days post t0 + 182.5 days, and t0 + 365 days
*/

%MACRO windows(df = ,
		 	   id = ,
			   measures = ,
		       tmeasures = ,
			   startpoint = ,
			   TWindows = );

	DATA dt;
		SET &df;
			time = (&tmeasures - &startPoint) / 86400;
	RUN;

	PROC SQL NOPRINT;

		SELECT t INTO :timePoints SEPARATED BY ' '
			FROM TWindows;

		CREATE TABLE t1 AS
			SELECT a.*, 
				   b.windows,
				   %scan(&timePoints, 1, ' ') AS t FORMAT BEST.
				FROM dt AS a, (
							   SELECT * 
								 FROM TWindows 
								WHERE t = %scan(&timePoints, 1, ' ')
							  ) AS b
					GROUP BY &id
					HAVING ABS(b.t - a.time) eq MIN(ABS(b.t - a.time))
					ORDER BY &id, &tmeasures;

		CREATE TABLE t2 AS
			SELECT a.*, 
				   b.windows,
				   %scan(&timePoints, 2, ' ') AS t FORMAT BEST.
				FROM dt AS a, (
							   SELECT * 
								 FROM TWindows 
								WHERE t = %scan(&timePoints, 2, ' ')
							  ) AS b
					GROUP BY &id
					HAVING ABS(b.t - a.time) eq MIN(ABS(b.t - a.time))
					ORDER BY &id, &tmeasures;

		CREATE TABLE t3 AS
			SELECT a.*, 
				   b.windows,
				   %scan(&timePoints, 3, ' ') AS t FORMAT BEST.
				FROM dt AS a, (
							   SELECT * 
								 FROM TWindows 
								WHERE t = %scan(&timePoints, 3, ' ')
							  ) AS b
					GROUP BY &id
					HAVING ABS(b.t - a.time) eq MIN(ABS(b.t - a.time))
					ORDER BY &id, &tmeasures;
	QUIT;

	DATA WindowsSet; 
		SET t1 t2 t3;
			IF time >= (t - windows) AND time <= (time + windows);
	RUN;

	PROC SORT DATA = WindowsSet; by PatientICN t; RUN;

	PROC DATASETS nolist; DELETE T1 T2 T3 Dt; QUIT;

%MEND windows;

/****************************** Remove Outliers *****************************/

/*
title: Outliers: Measurment Cleaning Algorithm - to be applied following a call
	   to %WINDOWS
param: df: table containing id and weights
param: measures: string corresponding to the name of the column of measures in 
	   df, e.g., numeric weight data if using to clean weight data.
param: TOutliers: table with time points to collect data and outliers for each
	   time point.
	   Columns should be:
	   column: t: numeric vector of time points to collect measurements, 
	   			  e.g., c(0, 182.5, 365) for measure collection at t = 0, 
			      t = 180 (6 months from t = 0), and t = 365 
			 	  (1 year from t = 0). Default is c(0, 182.5, 365) according 
			      to Janney et al. 2016
	   column: windows: numeric vector of measurement collection windows to use
						around each time point in t. E.g., Janney et al. 2016
						use (30, 60, 60) for t of (0, 182.5, 365), implying 
					    that the closest measurement t = 0 will be collected 
					    30 days prior to and 30 days post startPoint. 
						Subsequent measurements will be collected 60 days prior 
						to and 60 days post t0 + 182.5 days, and t0 + 365 days
	   column: upper: upper bound of plausible measurements, one for each time
		 			  point t.
	   column: lower: lower bound of plausible measurements.
*/

%MACRO outliers(df = , measures = , TOutliers = );
	PROC SQL;
		CREATE TABLE OutliersRemoved AS
			SELECT a.*, b.lower, b.upper
				FROM &df AS a JOIN &TOutliers AS b
					ON a.t = b.t
				WHERE &measures >= lower AND &measures <= upper;
	QUIT;
%MEND outliers;

/*********************** Set Windows + Remove Outliers **********************/

/*
title: WindowCleaner Measurment Cleaning Algorithm;
param: df: table containing id and weights;
param: id: string corresponding to the name of the column of patient IDs in df
param: measures: string corresponding to the name of the column of measures in 
	   df, e.g., numeric weight data if using to clean weight data.
param: tmeasures: string corresponding to the name of the column of measure 
	   dates and/or times in df
param: startPoint: string corresponding to the name of the column in df holding
	   the time at which subsequent measurement dates will be assessed, 
	   should be the same for each person. Eg., if t = 0 corresponds 
	   to an index visit held by the variable 'VisitDate', then 
	   startPoint should be set to 'VisitDate'
param: table: table with time points to collect data, windows around each time
	   		  point, and lower and upper bounds for outliers at each time.
	   Columns should be:
	   column: t: numeric vector of time points to collect measurements, 
	   			  e.g., c(0, 182.5, 365) for measure collection at t = 0, 
			      t = 180 (6 months from t = 0), and t = 365 
			 	  (1 year from t = 0). Default is c(0, 182.5, 365) according 
			      to Janney et al. 2016
	   column: windows: numeric vector of measurement collection windows to use
						around each time point in t. E.g., Janney et al. 2016
						use (30, 60, 60) for t of (0, 182.5, 365), implying 
					    that the closest measurement t = 0 will be collected 
					    30 days prior to and 30 days post startPoint. 
						Subsequent measurements will be collected 60 days prior 
						to and 60 days post t0 + 182.5 days, and t0 + 365 days
	   column: upper: upper bound of plausible measurements, one for each time
		 			  point t.
	   column: lower: lower bound of plausible measurements.
*/

%MACRO WindowCleaner(df = ,
				     id = ,
					 measures = ,
					 tmeasures = ,
					 startPoint = ,
					 table = );

	%windows(df = &df.,
			 id = &id.,
			 measures = &measures.,
			 tmeasures = &tmeasures.,
			 startPoint = &startPoint.,
			 TWindows = &table.);

	%outliers(df = WindowsSet, 
			  measures = &measures., 
			  TOutliers = &table.);

%MEND WindowCleaner;

/****************************** End of Document *****************************/
