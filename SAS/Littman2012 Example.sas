/*****************************************************************************
Data Source: US Dept. of Veteran's Affairs - Corporate Data Warehouse
Weight Measurement "Cleaning" Algorithm - Littman 2012 Version

Example Usage

NOTE: must run script 'Littman2012.sas7bdat' prior to using this script,
	  or have macros (%Littman2012) loaded.

Author: Richard Ryan Evans
Development Start Date: 10/18/2018
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

/****************************************************************************/

%LET df = MOVEweights;
%LET id = PatientICN;
%LET measures = Weight;
%LET tmeasures = WeightDateTime;
%LET outliers = 75 600;
%LET SDthreshold = 0.10;

%Littman2012(df = &df.,
			 id = &id.,
			 measures = &measures.,
			 tmeasures = &tmeasures.,
		     outliers = &outliers.,
		     SDthreshold = &SDthreshold.);

PROC MEANS DATA = OutputData;
	VAR &measures
		outputMeasurement;
RUN;

/****************************** End of Document *****************************/
