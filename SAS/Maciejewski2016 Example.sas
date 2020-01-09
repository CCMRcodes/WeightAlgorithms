/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm Implementation

Author: Richard Ryan Evans
Development Start Date: 09/25/2018

Requires running file: WeightCleaningMacrosRRE.sas or having
\Sasmacr\Dolag & \Sasmacr\Prepost \Sasmacr\
*****************************************************************************/

ods html close; ods html;

libname DCEP "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data";

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
         %let nobs = %sysfunc(attrn(&dsid, NOBS));
         %let nvars = %sysfunc(attrn(&dsid, NVARS));
         %let rc = %sysfunc(close(&dsid));
         %put &dset has &nvars variable(s) and &nobs observation(s).;
      %end;
   %else
      %put Open for data set &dset failed - %sysfunc(sysmsg());
%mend obsnvars;

/**************************** Import Sample Data ****************************/

* data stored in SQL DB;

LIBNAME samp OLEDB
INIT_STRING = "Provider = SQLNCLI11;
			   Integrated Security = SSPI;
			   Persist Security Info = True;
			   Initial Catalog = NCP_DCEP;
			   Data Source = vhacdwrb03.vha.med.va.gov"
Schema = samp;

/* examine data
%dim(Samp.heightSamples);
* nrow = 94146, ncol = 5;

%dim(Samp.weightSamples);
* nrow = 279604, ncol = 8;
*/

/******************************* Examine Data *******************************/

PROC UNIVARIATE DATA = Samp.heightSamples;
	VAR height;
RUN;

PROC UNIVARIATE DATA = Samp.weightSamples;
	VAR weight;
RUN;

PROC FREQ DATA = Samp.weightSamples;
	TABLES (Sta3n Sta6a DivisionName) * SampleYear
		   (Sta3n Sta6a DivisionName) * Source
		   / nocum missing;
RUN;

/***************************** Macro invocation *****************************/

/*** PCP Source ***/

DATA PCP;  SET Samp.weightSamples; WHERE Source = 'PCP';  RUN; * nrow = 225363;

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
* nrow_before = 225363, ncol = 15;
* nrow_after  = 219958, ncol = 3;
* reduction by 5405 (2.4%);

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
* nrow_before = 219958, ncol = 3;
* nrow_after  = 219871, ncol = 8;
* reduction by 87 (.04%);

DATA PCPcleaned; SET Phase1Output; RUN; * nrow = 219871, ncol = 8;

/*** MOVE! Source ***/

DATA MOVE; SET Samp.weightSamples; WHERE Source = 'MOVE'; RUN; * nrow = 54241;

/***** Step 1: Remove missing values and outliers, fix same day weights *****/

%LET DSN = MOVE;
%LET AnalysisVar = Weight;

%SameDayMeasures(DSN = &DSN.,
				 IDVar = &IDVar.,
				 DateVar = &DateVar.,
				 AnalysisVar = &AnalysisVar.,
				 clearSpace = 'TRUE');
* nrow_before = 54241, ncol = 15;
* nrow_after  = 52752, ncol = 3;
* reduction by 1489 (2.7%);

/************ Step 2: Rolling SD by Cluster and Group Algorithm ************/

%LET DSN = Phase1Output;
%LET AnalysisVar = AnalysisVarFixed;

%prepost(DSN = &DSN.,
		 IDVar = &IDVar.,
		 DateVar = &DateVar.,
		 AnalysisVar = &AnalysisVar.);
* Augments &DSN. and creates &DSNl;
* nrow_before = 52752, ncol = 3;
* nrow_after  = 52744, ncol = 8;
* reduction by 8 (.015%);

DATA MOVEcleaned; SET Phase1Output; RUN; * nrow = 52744, ncol = 8;

/*************************** Post-Macro Mess 'Round *************************/

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
* nrow = 52744, ncol = 10;

/* count number of high sd in each cluster */
PROC MEANS DATA = MOVEcleaned NOPRINT;
	CLASS &IDVAR. cluster;
	VAR highctr;
	WAYS 2;
	OUTPUT MAX = maxctr OUT = clusters (DROP = _TYPE_ _FREQ_);
RUN;
* nrow = 2352, ncol = 3;

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
* nrow = 52692, ncol = 11;

/* save permanent

* whole sample;
DATA Dcep.InputWeight; SET Samp.weightSamples; RUN;

* PCP sample;
DATA Dcep.PCPcleaned; SET PCPcleaned; RUN;

* MOVE sample;
DATA Dcep.MOVEcleaned; SET MOVEcleaned; RUN;
*/

/****************************** End of Document *****************************/
