/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm - Littman 2012 Version

Author: Richard Ryan Evans
Development Start Date: 10/18/2018

Excerpt from published methods:
	 "For weight, height, and body mass index, we first removed biologically 
	 implausible values (weight <75 lb or >600 lb, height <49 in or >94 in, 
	 and body mass index >80 kg/m ). Next, we applied algorithms to identify 
	 measures that were plausible but appeared to be erroneous on the basis 
	 of a review of all recorded weights and heights during the relevant time 
	 period. After reviewing records that had large standard deviations (SDs) 
	 (explained in more detail below), we used the algorithm that follows to 
	 exclude values that were likely erroneous while keeping values that were 
	 plausible. We excluded any weight measurements that met the following 
	 2 criteria: 1) the difference between the mean weight and weight in 
	 question was greater than the SD and 2) the SD was greater than 10% 
	 of the mean. For example, 1 participant’s weight in pounds was recorded
	 as 300 and 160 lb, both measured on December 7, 2005, 310 lb measured on
	 June 12, 2006, 276 lb measured on August 8, 2006, 291 lb measured on 
	 August 15, 2006, and 291 lb measured on September 13, 2007, resulting 
	 in mean (SD) of 271.3 (55.7) lb. The weight of 160 lb recorded on 
	 December 7, 2005, was considered erroneous and dropped because the 
	 difference between the index weight and mean weight was greater than 
	 the SD (271 - 160 = 113.3 lb) and the SD was greater than 10% of the 
	 mean of all weights ([55.7/271.3] × 100 = 20.5%)."

*****************************************************************************/

ods html close; ods html;

/****************************************************************************/

/*
title Littman 2012 Weight Measurment Algorithm
param: df table containing id and weights
param: id string corresponding to the name of the column of patient IDs in df
param: weights string corresponding to the name of the column of weights in df
param: tweights string corresponding to the name of the column of weight dates 
	   or times in df. If tweights is a date object, there may be more than one
	   weight on the same day, if it precise datetime object, there may not be 
	   more than one weight on the same day.
param: outliers object of type list with numeric inputs corresponding to the 
	   upper and lower bound for each time entry. 
	   Default is list(LB = c(75), UB = c(600))
param: SDthreshold numeric scalar to be multiplied by the `meanWeight` per `id`.
	   E.g., from Littman 2012, "...We excluded any weight measurements that met
	   the following 2 criteria: 1) the difference between the mean weight and 
	   weight in question was greater than the SD and 2) the SD was greater than 
	   10% of the mean...." implies `SDthreshold`= 0.10
*/

%MACRO Littman2012(df = ,
				   id = ,
				   measures = ,
				   tmeasures = ,
				   outliers = ,
			       SDthreshold = );

	* Step 1: set outliers to NA;
	DATA dt;
		SET &df;
			IF &measures < %scan(&outliers, 1, ' ')
				OR &measures > %scan(&outliers, 2, ' ')
			THEN measures_aug_ = .;
			ELSE measures_aug_ = &measures;
	RUN;

	* Step 2: compute mean, sd, SD threshold of measures per group id;
	PROC MEANS DATA = dt NOPRINT NWAY;
		CLASS &id;
		VAR measures_aug_;
		OUTPUT OUT = dt2 (DROP = _TYPE_ _FREQ_)
			   MEAN = meanMeasures 
			   STDDEV = SDMeasures;
	RUN;

	PROC SQL;
		CREATE TABLE descByGroup AS
			SELECT a.*,
				   b.meanMeasures,
				   b.SDMeasures,
				   (&SDthreshold * b.meanMeasures) AS SD_threshold_
				FROM dt AS a INNER JOIN dt2 AS b ON a.&id = b.&id;
	QUIT;

	/*
	Step 3: exclude any measurements that meet the following 2
	        criteria: 1) the difference between the meanMeasures
			and measures_aug_ in question is greater than the SDMeasures
			AND 2) the SDMeasures is greater than the SD_threshold_
	*/

	DATA outputData;
		SET descByGroup;
			IF (ABS(measures_aug_ - meanMeasures) > SDMeasures)
				THEN cond1 = 1;
			ELSE cond1 = 0;
			IF (SDMeasures > SD_threshold_)
				THEN cond2 = 1;
			ELSE cond2 = 0;
			IF (cond1 = 1 AND cond2 = 1)
				THEN measures_aug_ = .;
		RENAME measures_aug_ = outputMeasurement;
	RUN;

%MEND Littman2012;

/****************************** End of Document *****************************/
