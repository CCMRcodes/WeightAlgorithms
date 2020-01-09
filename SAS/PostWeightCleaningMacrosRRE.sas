/*****************************************************************************
*In the following SAS code:

     * `&DSN.` is just the name of the data set that was passed to the weight
       cleaning macros. You can replace it with your own data set name as
       necessary.
       
     * `&IDVAR.` is the name of the variable that uniquely identifies each
       patient in your data set (e.g., ScrSSN, PatientICN, PERSON_ID).
     
     * `&DATEVAR.` is the name of the variable that contains the date on which
       the value of &ANALYSISVAR. was recorded, i.e., the date of the weight
       measurement record. NB: All of this code was used with data sets where
       there was only ever one weight measurement record per calendar date.
     
     * `&ANALYSISVAR.` is the name of the numeric variable that the SAS macros
       are supposed to "clean", e.g., the name of the variable that contains
       weight values.

NB: This code can only be run *after* running the weight cleaning SAS macros!
       
(TSZB, 2018-09-11);
*****************************************************************************/

/* enumerate clusters with high sd */
DATA &DSN.;
	SET &DSN.;
	BY &IDVAR. &DATEVAR.;
	RETAIN cluster 0;
		lagctr = lag(highctr);
		IF (first.&IDVAR.) THEN cluster = 0;
		ELSE DO;
		  IF ((lagctr = 0) AND (highctr = 1)) THEN cluster + 1;
		END;
RUN;

/* count number of high sd in each cluster */
PROC MEANS DATA = &DSN. NOPRINT;
	CLASS &IDVAR. cluster;
	VAR highctr;
	WAYS 2;
	OUTPUT MAX = maxctr OUT = clusters (DROP = _TYPE_ _FREQ_);
RUN;

/* sort for merging */
PROC SORT DATA = &DSN.;
	BY &IDVAR. cluster highctr;
RUN;

/* merge max number of each cluster to mark interior measures for deletion */
DATA &DSN.;
	MERGE &DSN. clusters(IN=inc);
	BY &IDVAR. cluster;
	IF inc THEN DO;
	  IF 1 < highctr < maxctr THEN DELETE;
	END;
RUN;

PROC SORT DATA = &DSN.; BY &IDVAR. &DATEVAR.; RUN;

PROC SORT DATA = keep; BY &IDVAR. &DATEVAR.; RUN;

DATA weights_all_clean;
	MERGE keep &DSN.(IN = inn KEEP = &IDVAR. &DATEVAR.);
	BY &IDVAR. &DATEVAR.;
	IF NOT(inn) THEN DELETE;
RUN;

/* combine clean and unclean weights */
DATA all;
	SET weights_all_clean (IN = inc) keep (IN = ink);
	BY &IDVAR. &DATEVAR.;
	IF ((ink) AND (not(inc))) THEN DO;
	  clean = 0;
	  OUTPUT;
	END;
	ELSE DO;
	  clean = 1;
	  OUTPUT;
	END;
RUN;

/*
*NB: This macro will graph both the pre-cleaning and post-cleaning weight
 measurement trajectories for each patient identifier in the data set,
 regardless of whether those patient identifiers had any records removed by
 the cleaning algorithm. I didn't like this so I went one step further with
 the SAS code that comes after this macro. (TSZB, 2018-09-11);

%MACRO dograph(n = 10);
	PROC SGPANEL DATA = all (OBS = &N.) NOCYCLEATTRS;
		PANELBY &IDVAR. clean / COLUMNS = 2;
		SERIES x = &DATEVAR. y = &ANALYSISVAR. 
			   / MARKERS LINEATTRS = (PATTERN = solid);
	RUN;
%MEND dograph;

%*%dograph(n = 30);
%*%dograph(n = 1000);
%*%dograph(n = 10000);
*/

/*
Create the data sets necessary to be able to plot only the pre- and post-
cleaning algorithm weight measurement trajectories for patients who had at
least one weight measurement record removed by the cleaning algorithm
*/

PROC FREQ DATA = all NOPRINT;
	TABLES &IDVAR. * clean / OUT = all_fo (DROP = PERCENT);
RUN;

*NB: WORK.temp contains the weight data (both pre-cleaning and post-cleaning)
     of patients who had at least one weight record removed by the cleaning
     algorithm. (TSZB, 2018-09-11);

PROC SQL NOPRINT;
	CREATE TABLE temp AS
		SELECT t1.*
			FROM WORK.all AS t1
				INNER JOIN (
							SELECT DISTINCT a.&IDVAR.
							FROM all_fo (WHERE = (clean = 0)) AS a
								INNER JOIN all_fo (WHERE = (clean = 1)) AS b 
									ON a.&IDVAR. = b.&IDVAR.
							WHERE (NOT (a.COUNT = b.COUNT))
						    ) AS t2 
					ON t1.&IDVAR. = t2.&IDVAR.
		ORDER BY t1.&IDVAR., t1.&DATEVAR., t1.clean;
QUIT;

/*
*NB: This code works but I think it's too hard to identify which points were
 removed by the cleaning algorithm. (TSZB, 2018-09-11);

PROC SGPANEL DATA = temp(OBS = 1000) NOCYCLEATTRS;
	PANELBY &IDVAR. clean / COLUMNS = 2;
	SERIES x = &DATEVAR. y = &ANALYSISVAR. 
		   / MARKERS LINEATTRS = (PATTERN = solid);
RUN;
*/

/*
Create a data set of all weight values that were removed by the cleaning
algorithm. This data set will be merged with WORK.temp to create a variable
with only "unclean" weight records that were removed by the weight cleaning
algorithm which will then be used to make it clearer on the pre- and post-
cleaning weight trajectory graphs what points were removed during the
cleaning process. This is an unnecessary step but I'm fancy like that so here
it is.
*/

PROC SQL NOPRINT;
	CREATE TABLE temp_excluded AS
		SELECT &IDVAR., &DATEVAR., &ANALYSISVAR.
			FROM keep
		EXCEPT
		SELECT &IDVAR., &DATEVAR., &ANALYSISVAR.
			FROM weights_all_clean
		ORDER BY 1, 2, 3;
QUIT;

/*
Merge the data set of excluded weights with the data set containing all pre-
and post-cleaning weight measurement records.
*/

DATA temp;
	MERGE temp(IN = ina) temp_excluded(IN = inb);
	BY &IDVAR. &DATEVAR. &ANALYSISVAR.;
		IF (inb) THEN ExcludedValue = &ANALYSISVAR.;
RUN;

/*
Plot pre- and post-cleaning weight trajectories for patients with at least
one weight measurement record removed by the cleaning algorithm and visually
highlight which values were removed. Feel free to adjust marker and line
visual attributes as desired.;
*/

PROC SGPANEL DATA = temp (OBS = 1000) NOCYCLEATTRS;
	PANELBY &IDVAR. clean / COLUMNS = 2;
	SERIES x = &DATEVAR. y = &ANALYSISVAR. 
		   / MARKERS LINEATTRS = (PATTERN = solid);
	SCATTER x = &DATEVAR. y = ExcludedValue 
		    / MARKERATTRS = (SYMBOL = circlefilled COLOR = red SIZE = 9pt);
RUN;

/****************************** End of Document *****************************/


