/*****************************************************************************
Random sample of MOVE! and non-MOVE! participants

Author: Richard Evans (Richard.Evans8@va.gov)
        Jenny Burns (jennifer.burns@va.gov)
		Wyndy Wiitala (wyndy.wiitala.va.gov)

Development Start Date: 15AUG2018

Note: Use the NCP_DCEP.samp SCHEMA made in \SCHEMAsamp.sql

Rationale:
Based on our literature review, we will apply the definitions/algorithms 
used to define weight in the articles selected for inclusion.

Our initial sample will include:

Year 2016:
	Random sample of VISN10 Patients with at least one PCP visit in 2016.
	VISN10 Patients with at least one MOVE! visit in 2016

Year 2008:
	Random sample of VISN10 Patients with at least one PCP visit in 2008.
	VISN10 Patients with at least one MOVE! visit in 2008

For each of these cohorts, we will extract weight data 
two years prior and two years after initial PCP (or MOVE!) visit , 
exclude patients based on age > XX, and include site information and patient
demographics.

After our code is developed we will expand to use nationwide data

*****************************************************************************/

USE CDWWork
GO

/*********************** Sample Ann Arbor participants **********************/

-- Ann Arbor patients with at least one PCP visit in 2016

-- collect first PCP visit in 2016
IF OBJECT_ID('tempdb..#sampVisits2016') IS NOT NULL 
	DROP TABLE #sampVisits2016

SELECT *
	INTO #sampVisits2016
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
				     a.VisitSID,
				     a.VisitIEN,
				     a.Sta3n,
				     d.LocationName,
				     e.Sta6a,
				     e.DivisionName,
					 a.VisitDateTime,
					 g.BirthDateTime,
					 DATEDIFF(day, g.BirthDateTime, a.VisitDateTime) / 365.25 AS AgeAtVisit,
					 ROW_NUMBER() OVER (
									    PARTITION BY f.PatientICN
										ORDER BY a.VisitDateTime
									   ) AS Cnt,
					 SampleYear = '2016'
				FROM Outpat.Workload AS a
					LEFT JOIN Dim.StopCode AS b
						ON a.PrimaryStopCodeSID = b.StopCodeSID
					LEFT JOIN Dim.Stopcode AS c
						ON a.SecondaryStopCodeSID = c.StopCodeSID
					LEFT JOIN Dim.Location AS d
						ON a.LocationSID = d.LocationSID
					LEFT JOIN Dim.Division AS e
						ON d.DivisionSID = e.DivisionSID
					LEFT JOIN Patient.PatientICN AS f
						ON a.PatientSID = f.PatientSID
					LEFT JOIN SPatient.SPatient AS g
						ON f.PatientICN = g.PatientICN
				WHERE a.Sta3n = 506 
					  AND a.VisitDateTime > '2015-12-31'
					  AND a.VisitDateTime < '2017-01-01'
					  AND (b.StopCode = 323 OR c.StopCode = 323)
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 40103, ncol = 14

-- apply random sampling to first visits
IF OBJECT_ID('tempdb..#samp2016') IS NOT NULL 
	DROP TABLE #samp2016

SELECT *
	INTO #samp2016
	FROM (
		  SELECT *,
				 ROW_NUMBER() OVER (ORDER BY RandomIndex) AS ran
			FROM (SELECT *,
					     ABS(CHECKSUM(NewId())) AS RandomIndex
			        FROM #sampVisits2016
			     ) AS tmp
		 ) AS tmp2
	WHERE ran <= 10000
-- nrow = 10000, ncol = 15

-- VISN10 patients with at least one PCP visit in 2008

-- collect first PCP visit in 2008
IF OBJECT_ID('tempdb..#sampVisits2008') IS NOT NULL 
	DROP TABLE #sampVisits2008

SELECT *
	INTO #sampVisits2008
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
				     a.VisitSID,
				     a.VisitIEN,
				     a.Sta3n,
				     d.LocationName,
				     e.Sta6a,
				     e.DivisionName,
					 a.VisitDateTime,
					 g.BirthDateTime,
					 DATEDIFF(day, g.BirthDateTime, a.VisitDateTime) / 365.25 AS AgeAtVisit,
					 ROW_NUMBER() OVER (
									    PARTITION BY f.PatientICN
										ORDER BY a.VisitDateTime
									   ) AS Cnt,
					 SampleYear = '2008'
				FROM Outpat.Workload AS a
					LEFT JOIN Dim.StopCode AS b
						ON a.PrimaryStopCodeSID = b.StopCodeSID
					LEFT JOIN Dim.Stopcode AS c
						ON a.SecondaryStopCodeSID = c.StopCodeSID
					LEFT JOIN Dim.Location AS d
						ON a.LocationSID = d.LocationSID
					LEFT JOIN Dim.Division AS e
						ON d.DivisionSID = e.DivisionSID
					LEFT JOIN Patient.PatientICN AS f
						ON a.PatientSID = f.PatientSID
					LEFT JOIN SPatient.SPatient AS g
						ON f.PatientICN = g.PatientICN
				WHERE a.Sta3n = 506 
					  AND a.VisitDateTime > '2007-12-31'
					  AND a.VisitDateTime < '2009-01-01'
					  AND (b.StopCode = 323 OR c.StopCode = 323)
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 25421, ncol = 14

-- apply random sampling to first visits
IF OBJECT_ID('tempdb..#samp2008') IS NOT NULL 
	DROP TABLE #samp2008

SELECT *
	INTO #samp2008
	FROM (
		  SELECT *,
				 ROW_NUMBER() OVER (ORDER BY RandomIndex) AS ran
			FROM (SELECT *,
					     ABS(CHECKSUM(NewId())) AS RandomIndex
			        FROM #sampVisits2008
			     ) AS tmp
		 ) AS tmp2
	WHERE ran <= 10000
-- nrow = 10000, ncol = 16

/******************** Sample Ann Arbor MOVE! Participants *******************/

------------ Ann Arbor MOVE! patients in 2016 ------------

-- collect first MOVE visit in 2016
IF OBJECT_ID('tempdb..#MOVEVisits2016') IS NOT NULL 
	DROP TABLE #MOVEVisits2016

SELECT *
	INTO #MOVEVisits2016
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
				     a.VisitSID,
				     a.VisitIEN,
				     a.Sta3n,
				     d.LocationName,
				     e.Sta6a,
				     e.DivisionName,
					 a.VisitDateTime,
					 g.BirthDateTime,
					 DATEDIFF(day, g.BirthDateTime, a.VisitDateTime) / 365.25 AS AgeAtVisit,
					 ROW_NUMBER() OVER (
									    PARTITION BY f.PatientICN
										ORDER BY a.VisitDateTime
									   ) AS Cnt,
					 SampleYear = '2016'
				FROM Outpat.Workload AS a
					LEFT JOIN Dim.StopCode AS b
						ON a.PrimaryStopCodeSID = b.StopCodeSID
					LEFT JOIN Dim.Stopcode AS c
						ON a.SecondaryStopCodeSID = c.StopCodeSID
					LEFT JOIN Dim.Location AS d
						ON a.LocationSID = d.LocationSID
					LEFT JOIN Dim.Division AS e
						ON d.DivisionSID = e.DivisionSID
					LEFT JOIN Patient.PatientICN AS f
						ON a.PatientSID = f.PatientSID
					LEFT JOIN SPatient.SPatient AS g
						ON f.PatientICN = g.PatientICN
				WHERE a.Sta3n = 506 
					  AND a.VisitDateTime > '2015-12-31'
					  AND a.VisitDateTime < '2017-01-01'
					  AND (b.StopCode IN (372, 373) OR c.StopCode IN (372, 373))
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 1721, ncol = 14

------------ Ann Arbor MOVE! patients in 2008 ------------

-- collect first MOVE visit in 2008
IF OBJECT_ID('tempdb..#MOVEVisits2008') IS NOT NULL 
	DROP TABLE #MOVEVisits2008

SELECT *
	INTO #MOVEVisits2008
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
				     a.VisitSID,
				     a.VisitIEN,
				     a.Sta3n,
				     d.LocationName,
				     e.Sta6a,
				     e.DivisionName,
					 a.VisitDateTime,
					 g.BirthDateTime,
					 DATEDIFF(day, g.BirthDateTime, a.VisitDateTime) / 365.25 AS AgeAtVisit,
					 ROW_NUMBER() OVER (
									    PARTITION BY f.PatientICN
										ORDER BY a.VisitDateTime
									   ) AS Cnt,
					 SampleYear = '2008'
				FROM Outpat.Workload AS a
					LEFT JOIN Dim.StopCode AS b
						ON a.PrimaryStopCodeSID = b.StopCodeSID
					LEFT JOIN Dim.Stopcode AS c
						ON a.SecondaryStopCodeSID = c.StopCodeSID
					LEFT JOIN Dim.Location AS d
						ON a.LocationSID = d.LocationSID
					LEFT JOIN Dim.Division AS e
						ON d.DivisionSID = e.DivisionSID
					LEFT JOIN Patient.PatientICN AS f
						ON a.PatientSID = f.PatientSID
					LEFT JOIN SPatient.SPatient AS g
						ON f.PatientICN = g.PatientICN
				WHERE a.Sta3n = 506 
					  AND a.VisitDateTime > '2007-12-31'
					  AND a.VisitDateTime < '2009-01-01'
					  AND (b.StopCode IN (372, 373) OR c.StopCode IN (372, 373))
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 434, ncol = 14

/********************* Collect all Weights for Samples **********************/

-- collect weights within 2 years of PCP visit

-- 2008 --

IF OBJECT_ID('tempdb..#PCPWeights2008') IS NOT NULL
	DROP TABLE #PCPWeights2008

SELECT a.*,
	   b.VitalSignTakenDateTime AS WeightDateTime,
	   b.VitalResultNumeric AS Weight,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS WeightDate
	INTO #PCPWeights2008
		FROM #samp2008 AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%weight%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 390832, ncol = 19

IF OBJECT_ID('tempdb..#PCPWindowWeights2008') IS NOT NULL 
	DROP TABLE #PCPWindowWeights2008

SELECT PatientICN,
	   PatientSID,
	   VisitSID,
	   VisitIEN,
	   Sta3n,
	   VisitDateTime,
	   LocationName,
	   Sta6a,
	   DivisionName,
	   AgeAtVisit,
	   WeightDateTime,
	   Weight,
	   WeightDate,
	   SampleYear,
	   Source = 'PCP'
	INTO #PCPWindowWeights2008
		FROM #PCPWeights2008
		WHERE WeightDateTime >= DATEADD(year, -2, VisitDateTime)
			  AND WeightDateTime <= DATEADD(year, 2, VisitDateTime)
-- nrow = 111403, ncol = 16

-- 2016 --

IF OBJECT_ID('tempdb..#PCPWeights2016') IS NOT NULL 
	DROP TABLE #PCPWeights2016

SELECT a.*,
	   b.VitalSignTakenDateTime AS WeightDateTime,
	   b.VitalResultNumeric AS Weight,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS WeightDate
	INTO #PCPWeights2016
		FROM #samp2016 AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%weight%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 276950, ncol = 19

IF OBJECT_ID('tempdb..#PCPWindowWeights2016') IS NOT NULL 
	DROP TABLE #PCPWindowWeights2016

SELECT PatientICN,
	   PatientSID,
	   VisitSID,
	   VisitIEN,
	   Sta3n,
	   VisitDateTime,
	   LocationName,
	   Sta6a,
	   DivisionName,
	   AgeAtVisit,
	   WeightDateTime,
	   Weight,
	   WeightDate,
	   SampleYear,
	   Source = 'PCP'
	INTO #PCPWindowWeights2016
		FROM #PCPWeights2016
		WHERE WeightDateTime >= DATEADD(year, -2, VisitDateTime)
			  AND WeightDateTime <= DATEADD(year, 2, VisitDateTime)
-- nrow = 114052, ncol = 16

-- collect weights within 2 years of MOVE! visit

-- 2008 --

IF OBJECT_ID('tempdb..#MOVEWeights2008') IS NOT NULL 
	DROP TABLE #MOVEWeights2008

SELECT a.*,
	   b.VitalSignTakenDateTime AS WeightDateTime,
	   b.VitalResultNumeric AS Weight,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS WeightDate
	INTO #MOVEWeights2008
		FROM #MOVEVisits2008 AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%weight%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 29279, ncol = 15

IF OBJECT_ID('tempdb..#MOVEWindowWeights2008') IS NOT NULL 
	DROP TABLE #MOVEWindowWeights2008

SELECT PatientICN,
	   PatientSID,
	   VisitSID,
	   VisitIEN,
	   Sta3n,
	   VisitDateTime,
	   LocationName,
	   Sta6a,
	   DivisionName,
	   AgeAtVisit,
	   WeightDateTime,
	   Weight,
	   WeightDate,
	   SampleYear,
	   Source = 'MOVE'
	INTO #MOVEWindowWeights2008
		FROM #MOVEWeights2008
		WHERE WeightDateTime >= DATEADD(year, -2, VisitDateTime)
			  AND WeightDateTime <= DATEADD(year, 2, VisitDateTime)
-- nrow = 7900, ncol = 15

-- 2016 --

IF OBJECT_ID('tempdb..#MOVEWeights2016') IS NOT NULL 
	DROP TABLE #MOVEWeights2016

SELECT a.*,
	   b.VitalSignTakenDateTime AS WeightDateTime,
	   b.VitalResultNumeric AS Weight,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS WeightDate
	INTO #MOVEWeights2016
		FROM #MOVEVisits2016 AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%weight%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 82500, ncol = 15

IF OBJECT_ID('tempdb..#MOVEWindowWeights2016') IS NOT NULL 
	DROP TABLE #MOVEWindowWeights2016

SELECT PatientICN,
	   PatientSID,
	   VisitSID,
	   VisitIEN,
	   Sta3n,
	   VisitDateTime,
	   LocationName,
	   Sta6a,
	   DivisionName,
	   AgeAtVisit,
	   WeightDateTime,
	   Weight,
	   WeightDate,
	   SampleYear,
	   Source = 'MOVE'
	INTO #MOVEWindowWeights2016
		FROM #MOVEWeights2016
		WHERE WeightDateTime >= DATEADD(year, -2, VisitDateTime)
			  AND WeightDateTime <= DATEADD(year, 2, VisitDateTime)
-- nrow = 46492, ncol = 15

/************************* Weights: Save Permanent **************************/

IF OBJECT_ID('NCP_DCEP.Samp.weightSamples') IS NOT NULL 
	DROP TABLE NCP_DCEP.Samp.weightSamples

SELECT *
	INTO NCP_DCEP.Samp.weightSamples
		FROM (
			  SELECT * FROM #PCPWindowWeights2008
			  UNION
			  SELECT * FROM #PCPWindowWeights2016
			  UNION
			  SELECT * FROM #MOVEWindowWeights2008
			  UNION
			  SELECT * FROM #MOVEWindowWeights2016
			 ) AS sampleUnion
-- nrow = 279604, ncol = 16

/*
SELECT Source, COUNT(*) AS Freq
	FROM NCP_DCEP.Samp.weightSamples
	GROUP BY Source

Source | Freq
-------|----------
MOVE   | 54241
PCP	   | 225363
*/

/*****************************************************************************

								Post-Processing

*****************************************************************************/

/******************************** Add Gender ********************************/

IF OBJECT_ID('tempdb..#Gender') IS NOT NULL 
	DROP TABLE #Gender

SELECT a.PatientICN,
	   b.Gender
	INTO #Gender
		FROM NCP_DCEP.samp.weightSamples AS a
			LEFT JOIN CDWWork.Patient.Patient AS b
				ON a.PatientSID = b.PatientSID
-- nrow = 279604, ncol = 2

ALTER TABLE NCP_DCEP.samp.weightSamples
	DROP COLUMN Gender

ALTER TABLE NCP_DCEP.samp.weightSamples
	ADD Gender CHAR(1)

UPDATE NCP_DCEP.samp.weightSamples
	SET Gender = b.Gender
		FROM NCP_DCEP.samp.weightSamples AS a
			LEFT JOIN #Gender AS b
				ON a.PatientICN = b.PatientICN
-- nrow = 279604, ncol = 17

/*
SELECT Gender, COUNT(*) 
	FROM NCP_DCEP.samp.weightSamples 
	GROUP BY Gender

Gender	N
------- -----
F		22052
M		257552
*/

/******************** Exclude Pregnant Women from Cohort ********************/

-- Pregnancy codes uploaded here: NCP_DCEP.Samp.PregnancyCodes

-- collect women +/- 2 years from MOVE! date or PCP visit date
IF OBJECT_ID('tempdb..#Women') IS NOT NULL 
	DROP TABLE #Women

SELECT DISTINCT PatientICN,
       PatientSID,
	   DATEADD(year, -2, VisitDateTime) AS alpha,
	   DATEADD(year,  2, VisitDateTime) AS omega
	INTO #Women
		FROM NCP_DCEP.Samp.weightSamples
		WHERE Gender = 'F'
-- nrow = 1409, ncol = 3

----- Outpatient Diagnosis Pregnancy Codes -----

IF OBJECT_ID('tempdb..#OutptPreg') IS NOT NULL 
	DROP TABLE #OutptPreg

SELECT tmp.*
	INTO #OutptPreg
		FROM (
			  (
			   SELECT DISTINCT PatientICN
				 FROM #Women AS a
					 LEFT JOIN Outpat.VDiagnosis AS b
					 	 ON a.PatientSID = b.PatientSID
					 INNER JOIN (
								 SELECT b.ICD9Code,
									    b.ICD9SID
								   FROM (
									     SELECT Code
										   FROM NCP_DCEP.Samp.PregnancyCodes
									      WHERE CodeType = 'ICD9'
										 ) AS a
								 	 LEFT JOIN CDWWork.Dim.ICD9 AS b
										 ON a.Code = b.ICD9Code
								 ) AS c
						 ON b.ICD9SID = c.ICD9SID
				 WHERE CONVERT(date, b.VisitDateTime) >= CONVERT(date, alpha) 
					   AND 
					   CONVERT(date, b.VisitDateTime) <= CONVERT(date, omega)
			  )
			  UNION
			  (
			   SELECT DISTINCT PatientICN
					FROM #Women AS a
						LEFT JOIN Outpat.VDiagnosis AS b
							ON a.PatientSID = b.PatientSID
						INNER JOIN (
									SELECT b.ICD10Code,
										   b.ICD10SID
										FROM (
										      SELECT Code
											    FROM NCP_DCEP.Samp.PregnancyCodes
											   WHERE CodeType = 'ICD10'
											 ) AS a
											LEFT JOIN CDWWork.Dim.ICD10 AS b
												ON a.Code = b.ICD10Code
									) AS c
							ON b.ICD10SID = c.ICD10SID
					WHERE CONVERT(date, b.VisitDateTime) >= CONVERT(date, alpha) 
						  AND 
					      CONVERT(date, b.VisitDateTime) <= CONVERT(date, omega)
			  )
			 ) AS tmp
-- nrow = 60, ncol = 1

----- Inpatient Diagnosis Pregnancy Codes -----

IF OBJECT_ID('tempdb..#InptPreg') IS NOT NULL 
	DROP TABLE #InptPreg

SELECT tmp.*
	INTO #InptPreg
		FROM (
			  (
			   SELECT DISTINCT PatientICN
				 FROM #Women AS a
					 LEFT JOIN Inpat.InpatientDiagnosis AS b
					 	 ON a.PatientSID = b.PatientSID
					 INNER JOIN (
								 SELECT b.ICD9Code,
									    b.ICD9SID
								   FROM (
									     SELECT Code
										   FROM NCP_DCEP.Samp.PregnancyCodes
									      WHERE CodeType = 'ICD9'
										 ) AS a
								 	 LEFT JOIN CDWWork.Dim.ICD9 AS b
										 ON a.Code = b.ICD9Code
								 ) AS c
						 ON b.ICD9SID = c.ICD9SID
				 WHERE CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
					   AND 
					   CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
			  )
			  UNION
			  (
			   SELECT DISTINCT PatientICN
					FROM #Women AS a
						LEFT JOIN Inpat.InpatientDiagnosis AS b
							ON a.PatientSID = b.PatientSID
						INNER JOIN (
									SELECT b.ICD10Code,
										   b.ICD10SID
										FROM (
										      SELECT Code
											    FROM NCP_DCEP.Samp.PregnancyCodes
											   WHERE CodeType = 'ICD10'
											 ) AS a
											LEFT JOIN CDWWork.Dim.ICD10 AS b
												ON a.Code = b.ICD10Code
									) AS c
							ON b.ICD10SID = c.ICD10SID
					WHERE CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
						  AND 
					      CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
			  )
			 ) AS tmp
-- nrow = 21, ncol = 1

----- Combine Inpat + Outpat -----

IF OBJECT_ID('tempdb..#Pregnant') IS NOT NULL 
	DROP TABLE #Pregnant

SELECT tmp.*,
	   1 AS Pregnant
	INTO #Pregnant
		FROM (
			  SELECT * FROM #OutptPreg
			  UNION
			  SELECT * FROM #InptPreg
			 ) AS tmp
-- nrow = 64, ncol = 2

-- Join with NCP_DCEP.Samp.weightSamples

ALTER TABLE NCP_DCEP.Samp.weightSamples
	ADD Pregnant INT

UPDATE a
	SET a.Pregnant = b.Pregnant
		FROM NCP_DCEP.Samp.weightSamples AS a
			INNER JOIN #Pregnant AS b
				ON a.PatientICN = b.PatientICN
-- 727 rows affected
-- nrow = 279604, ncol = 18

DROP TABLE #OutptPreg, #InptPreg, #Pregnant, #Women

/*
SELECT Pregnant, COUNT(*) AS N
	FROM NCP_DCEP.Samp.weightSamples
	GROUP BY Pregnant

Pregnant	N
----------	-------
1			727
NULL		278877
*/

/*************** Add Indicator for Bariatric Surgery Patients ***************/

/*
Bariatric Surgery patients are to be identified by the following codes from:

Maciejewski, M. L., Arterburn, D. E., Berkowitz, T. S., Weidenbacher, H. J., 
Liu, C. , Olsen, M. K., Funk, L. M., Mitchell, J. E. and Smith, V. A. (2018), 
Geographic Variation in Obesity, Behavioral Treatment, and Bariatric Surgery 
for Veterans. Obesity. doi:10.1002/oby.22350

stored in NCP_DCEP.Samp.BariatricSurgCodes
*/

IF OBJECT_ID('tempdb..#folks') IS NOT NULL 
	DROP TABLE #folks

SELECT DISTINCT PatientICN,
	   PatientSID,
	   CONVERT(date, DATEADD(year, -2, VisitDateTime)) AS alpha,
	   CONVERT(date, DATEADD(year,  2, VisitDateTime)) AS omega
	INTO #folks
		FROM NCP_DCEP.Samp.weightSamples
-- nrow = 22046, ncol = 4

----- Inpatient Bariatric Surgery Codes -----

IF OBJECT_ID('tempdb..#InpatBariatric') IS NOT NULL 
	DROP TABLE #InpatBariatric

SELECT *
	INTO #InpatBariatric
		FROM (
			  (
			   SELECT DISTINCT a.PatientICN,
					  b.AdmitDateTime,
					  b.DischargeDateTime
				 FROM #folks AS a
					LEFT JOIN Inpat.InpatientICDProcedure AS b
						ON a.PatientSID = b.PatientSID -- nrow = 45394
					INNER JOIN (
							   SELECT b.ICD10ProcedureSID,
									  b.ICD10ProcedureCode
								 FROM (
									   SELECT Code
										 FROM NCP_DCEP.Samp.BariatricSurgCodes
										WHERE CodeType = 'ICD10'
									  ) AS a
									LEFT JOIN Dim.ICD10Procedure AS b
										ON a.Code = b.ICD10ProcedureCode
							  ) AS c -- nrow = 2471
						ON b.ICD10ProcedureSID = c.ICD10ProcedureSID
				WHERE CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
					  AND 
					  CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
		      )
			  UNION
			  (
			   SELECT DISTINCT a.PatientICN,
					  b.AdmitDateTime,
					  b.DischargeDateTime
				 FROM #folks AS a
					LEFT JOIN Inpat.InpatientCPTProcedure AS b
						ON a.PatientSID = b.PatientSID -- nrow = 24875
					INNER JOIN (
								SELECT b.CPTCode,
									   b.CPTName,
									   b.CPTDescription,
									   b.CPTSID
								 FROM (
									   SELECT Code
									     FROM NCP_DCEP.Samp.BariatricSurgCodes
									    WHERE CodeType = 'CPT'
									  ) AS a -- nrow = 11
									LEFT JOIN Dim.CPT AS b
										ON a.Code = b.CPTCode
							   ) AS c -- nrow = 1430
						ON b.CPTSID = c.CPTSID
				WHERE CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
					  AND 
					  CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
			  )		
	         ) AS tmp
-- nrow = 155, ncol = 3

----- Outpatient Bariatric Surgery Codes -----

IF OBJECT_ID('tempdb..#OutpatBariatric') IS NOT NULL 
	DROP TABLE #OutpatBariatric

SELECT DISTINCT a.PatientICN,
	   b.VProcedureDateTime
	INTO #OutpatBariatric
		FROM #folks AS a
			LEFT JOIN Outpat.VProcedure AS b
				ON a.PatientSID = b.PatientSID
			INNER JOIN (
					   SELECT b.CPTCode,
							  b.CPTName,
							  b.CPTDescription,
							  b.CPTSID
						 FROM (
							   SELECT Code
								 FROM NCP_DCEP.Samp.BariatricSurgCodes
								WHERE CodeType = 'CPT'
							  ) AS a
							  LEFT JOIN Dim.CPT AS b
								ON a.Code = b.CPTCode
					  ) AS c -- nrow = 1430
					ON b.CPTSID = c.CPTSID
		WHERE CONVERT(date, b.VProcedureDateTime) >= CONVERT(date, alpha) 
			  AND 
			  CONVERT(date, b.VProcedureDateTime) <= CONVERT(date, omega)
-- nrow = 175, ncol = 2

----- Fee-Basis/Community Bariatric Surgery Codes -----

IF OBJECT_ID('tempdb..#FeeBariatric') IS NOT NULL 
	DROP TABLE #FeeBariatric

SELECT *
	INTO #FeeBariatric
		FROM (
			  (
			   SELECT DISTINCT a.PatientICN,
					  c.InvoiceReceivedDateTime AS BariatricDateTime
				 FROM #folks AS a
					INNER JOIN Fee.FeeInpatInvoice AS b
						ON a.PatientSID = b.PatientSID
					INNER JOIN Fee.FeeInpatInvoiceICDProcedure AS c
						ON b.FeeInpatInvoiceSID = c.FeeInpatInvoiceSID
					INNER JOIN (        
								SELECT ICD10ProcedureSID
								  FROM (
										SELECT Code
										  FROM NCP_DCEP.Samp.BariatricSurgCodes
										 WHERE CodeType LIKE '%ICD%'
									   ) AS a
									LEFT JOIN Dim.ICD10Procedure AS b
										ON a.Code = b.ICD10ProcedureCode
							   ) AS d
						ON c.ICD10ProcedureSID = d.ICD10ProcedureSID
				WHERE CONVERT(date, c.InvoiceReceivedDateTime) >= CONVERT(date, alpha) 
					  AND 
					  CONVERT(date, c.InvoiceReceivedDateTime) <= CONVERT(date, omega)
			  )
			  UNION
			  (
			   SELECT DISTINCT a.PatientICN,
					  d.InitialTreatmentDateTime AS BariatricDateTime
				 FROM #folks AS a
					LEFT JOIN Fee.FeeCPTModifier AS b
						ON a.PatientSID = b.PatientSID
					INNER JOIN (
							    SELECT b.CPTCode,
									   b.CPTName,
									   b.CPTDescription,
									   b.CPTSID
									FROM (
										  SELECT Code
											FROM NCP_DCEP.Samp.BariatricSurgCodes
									       WHERE CodeType = 'CPT'
										 ) AS a
										LEFT JOIN Dim.CPT AS b
											ON a.Code = b.CPTCode
							   ) AS c
						ON b.CPTSID = c.CPTSID
					INNER JOIN Fee.FeeInitialTreatment AS d
						ON b.FeeInitialTreatmentSID = d.FeeInitialTreatmentSID
				WHERE CONVERT(date, d.InitialTreatmentDateTime) >= CONVERT(date, alpha)
					  AND 
					  CONVERT(date, d.InitialTreatmentDateTime) <= CONVERT(date, omega)
			  )
			 ) AS tmp
-- nrow = 2, ncol = 2

----- Combine Inpat + Outpat + Fee-Basis/Community -----

IF OBJECT_ID('tempdb..#Bariatric') IS NOT NULL 
	DROP TABLE #Bariatric

SELECT tmp.*,
	   1 AS Bariatric
	INTO #Bariatric
		FROM (
			  SELECT PatientICN,
				     VProcedureDateTime AS BariatricDateTime
				FROM #OutpatBariatric
			  UNION
			  SELECT PatientICN,
				     DischargeDateTime AS BariatricDateTime
				FROM #InpatBariatric
			  UNION
			  SELECT PatientICN,
			         BariatricDateTime
				FROM #FeeBariatric
			 ) AS tmp
-- nrow = 332, ncol = 3

----- Join with NCP_DCEP.Samp.weightSamples -----

ALTER TABLE NCP_DCEP.Samp.weightSamples
	DROP COLUMN Bariatric, BariatricDateTime
GO

ALTER TABLE NCP_DCEP.Samp.weightSamples
	ADD Bariatric int
GO

ALTER TABLE NCP_DCEP.Samp.weightSamples
	ADD BariatricDateTime datetime
GO

UPDATE a
	SET a.Bariatric = b.Bariatric,
	    a.BariatricDateTime = b.BariatricDateTime
		FROM NCP_DCEP.Samp.weightSamples AS a
			INNER JOIN #Bariatric AS b
				ON a.PatientICN = b.PatientICN
-- 7995 rows affected
-- nrow = 279604, ncol = 20

UPDATE NCP_DCEP.Samp.weightSamples
	SET Bariatric = CASE
						WHEN BariatricDateTime IS NOT NULL
							 AND
							 BariatricDateTime <= DATEADD(year, 2,  VisitDateTime)
							 AND
							 BariatricDateTime >= DATEADD(year, -2, VisitDateTime)
							THEN 1
						ELSE 0
					END

DROP TABLE #InpatBariatric, #OutpatBariatric, #FeeBariatric, #Bariatric

/*
SELECT Bariatric, COUNT(*) AS N
	FROM NCP_DCEP.Samp.weightSamples
	GROUP BY Bariatric

Bariatric	N
----------- -------
0			271937
1			7667
*/

/******** Add Indicator for Weights Collected During Inpatient Stay *********/

IF OBJECT_ID('tempdb..#InptStays') IS NOT NULL 
	DROP TABLE #InptStays

SELECT DISTINCT a.PatientICN,
	   b.AdmitDateTime,
	   b.DischargeDateTime
	INTO #InptStays
		FROM #folks AS a
			LEFT JOIN Inpat.Inpatient AS b
				ON a.PatientSID = b.PatientSID
		WHERE a.alpha <= CONVERT(date, b.AdmitDateTime)
			  AND
			  a.omega <= CONVERT(date, b.DischargeDateTime)
-- nrow = 8665, ncol = 6

/*
SELECT COUNT(DISTINCT PatientICN) FROM #InptStays
-- 3070 people

SELECT COUNT(DISTINCT PatientICN) FROM #folks
-- 20100 people in total
-- 15.3% were inpatient at some point
*/

IF OBJECT_ID('tempdb..#InptWeights') IS NOT NULL 
	DROP TABLE #InptWeights

SELECT a.PatientICN,
	   a.WeightDateTime,
	   1 AS InptWeight
	INTO #InptWeights
		FROM NCP_DCEP.Samp.weightSamples AS a
			INNER JOIN #InptStays AS b
				ON a.PatientICN = b.PatientICN
		WHERE a.WeightDateTime <= b.DischargeDateTime
			  AND
			  a.WeightDateTime >= b.AdmitDateTime
-- nrow = 1209, ncol = 3

----- Join with NCP_DCEP.Samp.weightSamples -----

ALTER TABLE NCP_DCEP.Samp.weightSamples
	DROP COLUMN InptWeight
GO

ALTER TABLE NCP_DCEP.Samp.weightSamples
	ADD InptWeight int
GO

UPDATE a
	SET a.InptWeight = b.InptWeight
		FROM NCP_DCEP.Samp.weightSamples AS a
			INNER JOIN #InptWeights AS b
				ON a.PatientICN = b.PatientICN
				   AND
				   a.WeightDateTime = b.WeightDateTime
-- 1203 rows affected
-- nrow = 279604, ncol = 21

/*
SELECT InptWeight, COUNT(*)
	FROM NCP_DCEP.Samp.weightSamples
	GROUP BY InptWeight

InptWeight	N
----------- -------
1			1203
NULL		278401
*/

/********************** In Sample Height Measurements ***********************/

-- collect heights within 2 years of PCP or MOVE! visit

IF OBJECT_ID('tempdb..#Heights') IS NOT NULL
	DROP TABLE #Heights

SELECT a.PatientICN,
	   a.PatientSID,
	   a.VisitDateTime,
	   b.VitalSignTakenDateTime AS HeightDateTime,
	   b.VitalResultNumeric AS Height,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS HeightDate
	INTO #Heights
		FROM (
			  SELECT DISTINCT PatientICN,
					 PatientSID,
					 VisitDateTime
			    FROM NCP_DCEP.Samp.weightSamples
			 ) AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%height%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 276913, ncol = 7

IF OBJECT_ID('tempdb..#WindowHeights') IS NOT NULL 
	DROP TABLE #WindowHeights

SELECT PatientICN,
	   PatientSID,
	   VisitDateTime,
	   HeightDateTime,
	   Height,
	   HeightDate
	INTO #WindowHeights
		FROM #Heights
		WHERE HeightDateTime >= DATEADD(year, -2, VisitDateTime)
			  AND HeightDateTime <= DATEADD(year, 2, VisitDateTime)
-- nrow = 94384, ncol = 6

/************************* Heights: Save Permanent **************************/

IF OBJECT_ID('NCP_DCEP.Samp.heightSamples') IS NOT NULL 
	DROP TABLE NCP_DCEP.Samp.heightSamples

SELECT *
	INTO NCP_DCEP.Samp.heightSamples
		FROM #WindowHeights
-- nrow = 94384, ncol = 6

/******************** Remove Temp Tables from Workspace *********************/

SELECT LEFT(name, charindex('_', name) - 1) AS tempTables
	FROM tempdb..sysobjects
	WHERE charindex('_', name) > 0
		  AND xtype = 'u'
		  AND NOT object_id('tempdb..'+name) IS NULL
	ORDER BY tempTables

DROP TABLE #Heights,
		   #PCPWeights2008,
		   #PCPWeights2016,
		   #MOVEVisits2008,
		   #MOVEVisits2016,
		   #samp2008,
		   #samp2016,
		   #sampVisits2008,
		   #sampVisits2016,
		   #PCPWindowWeights2008,
		   #PCPWindowWeights2016,
		   #MOVEWindowWeights2008,
		   #MOVEWindowWeights2016

/***************************** End of Document ******************************/