/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm Implementation

Author: Richard Ryan Evans
Development Start Date: 09/25/2018

Requires running file: WeightCleaningMacrosRRE.sas or having
\Sasmacr\Dolag & \Sasmacr\Prepost \Sasmacr\
*****************************************************************************/

ods html close; ods html;

libname DCEP "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data\Data";

/****************************** Common Macros *******************************/

%MACRO nrows(df);
	PROC SQL noprint;
		SELECT COUNT(*) INTO :nrows
			FROM &df;
	QUIT;
	%put &nrows;
%MEND nrows;

%MACRO ncols(df);
	DATA tmp;
		SET &df (obs = 0);
	RUN;

	PROC TRANSPOSE data = tmp out = tmp2;
	RUN;

	PROC DATASETS nolist; DELETE tmp; QUIT;

	%nrows(tmp2);

	PROC DATASETS nolist; DELETE tmp2; QUIT;
%MEND ncols;

%MACRO dim(df);
	%nrows(&df);
	%ncols(&df);
%MEND dim;

* this one works most of the time - not tables from a DB though;
%macro obsnvars(ds);
   %global dset nvars nobs;
   %let dset = &ds;
   %let dsid = %sysfunc(open(&dset));
   %if &dsid %then
      %do;
         %let nobs  = %sysfunc(attrn(&dsid, NOBS));
         %let nvars = %sysfunc(attrn(&dsid, NVARS));
         %let rc    = %sysfunc(close(&dsid));
         %put &dset has &nvars variable(s) and &nobs observation(s).;
      %end;
   %else
      %put Open for data set &dset failed - %sysfunc(sysmsg());
%mend obsnvars;

/******************************** Load Data *********************************/

PROC IMPORT OUT = DCEP.NationalWeightSamples 
            DATAFILE = "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms
\CDW Data\Data\NationalWeightSamples.csv" 
            DBMS = CSV REPLACE;
     GETNAMES = YES;
     DATAROW = 2; 
     GUESSINGROWS = 1024; 
RUN;
* nrow = 6890575, ncol = 19;

/******************************* Examine Data *******************************/

PROC UNIVARIATE DATA = DCEP.NationalWeightSamples;
	VAR weight;
RUN;

PROC FREQ DATA = DCEP.NationalWeightSamples;
	TABLES Pregnant;
RUN;

PROC FREQ DATA = DCEP.NationalWeightSamples NOPRINT;
	TABLES Sta3n * SampleYear * Source
		   / NOCUM MISSING OUT = Stations;
RUN;

/**************************** Remove Pregnancies ****************************/

DATA weightSamples;
	SET DCEP.NationalWeightSamples;
		WHERE Pregnant = '';
RUN;
* nrow = 6859463, ncol = 19;

/***************************** Macro invocation *****************************/

/*** PCP Source ***/

DATA PCP;  
	SET weightSamples;
		WHERE Source = 'PCP';  
RUN; 
* nrow = 2428334, ncol = 19;

%LET DSN = PCP;
%LET IDVar = PatientICN;
%LET DateVar = WeightDate;
%LET AnalysisVar = Weight;

/***** Step 1: Remove missing values and outliers, fix same day weights *****/

%SameDayMeasures(DSN = &DSN.,
				 IDVar = &IDVar.,
				 DateVar = &DateVar.,
				 AnalysisVar = &AnalysisVar.,
				 clearSpace = 'TRUE');
* nrow_before = 2428334, ncol = 19;
* nrow_after  = 2316154, ncol = 3;
* reduction by 112,180 (4.6%);

/************ Step 2: Rolling SD by cluster and person algorithm ************/

/*
* internal function for prepost();
* ran here as test;
%dolag(DSN = MOVEweights,
	   IDVar = PatientICN,
	   DateVar = WeightDateTime,
	   AnalysisVar = Weight);
*/

%LET DSN = Phase1Output;
%LET AnalysisVar = AnalysisVarFixed;

%prepost(DSN = &DSN.,
		 IDVar = &IDVar.,
		 DateVar = &DateVar.,
		 AnalysisVar = &AnalysisVar.);
* Augments &DSN. and creates &DSNl;
* nrow_before = 2316154, ncol = 3;
* nrow_after  = 2315379, ncol = 8;
* reduction by 775 (.03%);

DATA PCPcleaned; SET Phase1Output; RUN; * nrow = 2315379, ncol = 8;

/*** MOVE! Source ***/

DATA MOVE; 
	SET weightSamples; 
	WHERE Source = 'MOVE'; 
RUN; 
* nrow = 4431129;

/***** Step 1: Remove missing values and outliers, fix same day weights *****/

%LET DSN = MOVE;
%LET AnalysisVar = Weight;

%SameDayMeasures(DSN = &DSN.,
				 IDVar = &IDVar.,
				 DateVar = &DateVar.,
				 AnalysisVar = &AnalysisVar.,
				 clearSpace = 'TRUE');
* nrow_before = 4431129, ncol = 15;
* nrow_after  = 4226237, ncol = 3;
* reduction by 204892 (4.6%);

/************ Step 2: Rolling SD by Cluster and Group Algorithm ************/

%LET DSN = Phase1Output;
%LET AnalysisVar = AnalysisVarFixed;

%prepost(DSN = &DSN.,
		 IDVar = &IDVar.,
		 DateVar = &DateVar.,
		 AnalysisVar = &AnalysisVar.);
* Augments &DSN. and creates &DSNl;
* nrow_before = 4226237, ncol = 3;
* nrow_after  = 4225213, ncol = 8;
* reduction by 1024 (.02%);

DATA MOVEcleaned; SET Phase1Output; RUN; * nrow = 4225213, ncol = 8;

/*************************** Post-Macro Mess 'Round *************************/

/************** PCP **************/

/* enumerate clusters with high SD */
DATA PCPcleaned;
	SET PCPcleaned;
	BY &IDVAR. &DATEVAR.;
	RETAIN cluster 0;
		lagctr = lag(highctr);
		IF (first.&IDVAR.) THEN cluster = 0;
		ELSE DO;
		  IF ((lagctr = 0) AND (highctr = 1)) THEN cluster + 1;
		END;
RUN;
* nrow = 2315379, ncol = 10;

/* count number of high sd in each cluster */
PROC MEANS DATA = PCPcleaned NOPRINT;
	CLASS &IDVAR. cluster;
	VAR highctr;
	WAYS 2;
	OUTPUT MAX = maxctr OUT = clusters (DROP = _TYPE_ _FREQ_);
RUN;
* nrow = 206489, ncol = 3;

/* sort for merging */
PROC SORT DATA = PCPcleaned;
	BY &IDVAR. cluster highctr;
RUN;

/* merge max number of each cluster to mark interior measures for deletion */
DATA PCPcleaned;
	MERGE PCPcleaned clusters (IN = inc);
	BY &IDVAR. cluster;
	IF inc THEN DO;
	  IF 1 < highctr < maxctr THEN DELETE;
	END;
RUN;
* nrow = 2311755, ncol = 11;

/************** MOVE **************/

/* enumerate clusters with high SD */
DATA MOVEcleaned;
	SET MOVEcleaned;
	BY &IDVAR. &DATEVAR.;
	RETAIN cluster 0;
		lagctr = lag(highctr);
		IF (first.&IDVAR.) THEN cluster = 0;
		ELSE DO;
		  IF ((lagctr = 0) AND (highctr = 1)) THEN cluster + 1;
		END;
RUN;
* nrow = 4225213, ncol = 10;

/* count number of high sd in each cluster */
PROC MEANS DATA = MOVEcleaned NOPRINT;
	CLASS &IDVAR. cluster;
	VAR highctr;
	WAYS 2;
	OUTPUT MAX = maxctr OUT = clusters (DROP = _TYPE_ _FREQ_);
RUN;
* nrow = 185976, ncol = 3;

/* sort for merging */
PROC SORT DATA = MOVEcleaned;
	BY &IDVAR. cluster highctr;
RUN;

/* merge max number of each cluster to mark interior measures for deletion */
DATA MOVEcleaned;
	MERGE MOVEcleaned clusters (IN = inc);
	BY &IDVAR. cluster;
	IF inc THEN DO;
	  IF 1 < highctr < maxctr THEN DELETE;
	END;
RUN;
* nrow = 4217249, ncol = 11;

/* save permanent

* whole sample;
DATA DCEP.NationalInputWeight; SET Samp.NationalWeightSamples; RUN;

* PCP sample;
DATA DCEP.NationalPCPcleaned; SET PCPcleaned; RUN;
* nrow = 2311755, ncol = 11;

* MOVE sample;
DATA DCEP.NationalMOVEcleaned; SET MOVEcleaned; RUN;
* nrow = 4217249, ncol = 11;
*/

/****************************** End of Document *****************************/
