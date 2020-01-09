/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm - Janney 2016 Version

Author: Richard Ryan Evans
Development Start Date: 10/18/2018

Rationale: Example macro usage with sample data.
		   Algorithm: Set time points, set windows around those time points,
		   collect weights, then remove outliers from those collected
		   weights.

NOTE: macro can currently only accept three time points, no more, no less
	  and it will stay this way until someone can tell me how to either loop
	  through any number of time points in SAS with a similar framework
      or how to form the cross-join so that the results are more predictable.

NOTE: must run script 'WindowCleaner.sas7bdat' prior to using this script,
	  or have macros (Outliers, Windows, WindowCleaner) loaded.
*****************************************************************************/

ods html close; ods html;

/**************************** Import Sample Data ****************************/

* data stored in SQL DB;

LIBNAME samp OLEDB
INIT_STRING = "Provider = SQLNCLI11;
			   Integrated Security = SSPI;
			   Persist Security Info = True;
			   Initial Catalog = NCP_DCEP;
			   Data Source = vhacdwrb03.vha.med.va.gov"
Schema = samp;

/****************************************************************************/

DATA PCPweights;
	SET Samp.weightSamples (WHERE = (Source = "PCP"));
		BY PatientICN WeightDateTime;
			IF first.WeightDateTime THEN COUNT = 0;
				COUNT + 1;
		IF COUNT = 1;
	KEEP PatientICN VisitDateTime WeightDateTime Weight;
RUN;
* nrow = 225363, ncol = 4;

DATA MOVEweights;
	SET Samp.weightSamples (WHERE = (Source = "MOVE"));
		BY PatientICN WeightDateTime;
			IF first.WeightDateTime THEN COUNT = 0;
				COUNT + 1;
		IF COUNT = 1;
	KEEP PatientICN VisitDateTime WeightDateTime Weight;
RUN;
* nrow = 54241, ncol = 4;

/******************************** Set Windows *******************************/

%LET df = MOVEweights;
%LET id = PatientICN;
%LET measures = Weight;
%LET tmeasures = WeightDateTime;
%LET startPoint = VisitDateTime;

* set TWindows with a default table;
DATA TWindows;
	INPUT t windows;
	DATALINES;
0 30
182.5 60
365	60
;
RUN;

%LET TWindows = TWindows;

%windows(df = &df.,
		 id = &id.,
		 measures = &measures.,
		 tmeasures = &tmeasures.,
		 startPoint = &startPoint.,
		 TWindows = &TWindows.);

/****************************** Remove Outliers *****************************/

DATA TOutliers;
	INPUT t lower upper;
	DATALINES;
0 91 600
182.5 72 650
365	72 650
;
RUN;

%LET df = WindowsSet;
%LET TOutliers = TOutliers;

%outliers(df = &df., measures = &measures., TOutliers = &TOutliers);

/*********************** Set Windows + Remove Outliers **********************/

DATA OutlierWindow_table;
	INPUT t windows lower upper;
	DATALINES;
0 30 91 600
182.5 60 72 650
365	60 72 650
;
RUN;

%LET df = MOVEweights;
%LET table = outlierWindow_table;

%WindowCleaner(df = &df.,
			   id = &id.,
			   measures = &measures.,
			   tmeasures = &tmeasures.,
			   startPoint = &startPoint.,
			   table = &table.);
			   
/****************************** End of Document *****************************/
