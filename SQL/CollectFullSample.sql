/*****************************************************************************
National Sample of Veterans + Weights + Other Data Elements

Authors: Richard Evans (Richard.Evans8@va.gov)
         Jenny Burns (jennifer.burns@va.gov)
	     Wyndy Wiitala (wyndy.wiitala.va.gov)

Development Start Date: 04FEB2019

Note: Use the NCP_DCEP.samp SCHEMA made in \SCHEMAsamp.sql

Rationale:
Based on our literature review, we will apply the definitions/algorithms 
used to define weight in the articles selected for inclusion.

Our initial sample will include:

Year 2016:
	Random sample of 100000 Patients with at least one PCP visit in 2016.
	All Patients with at least one MOVE! visit in 2016

Year 2008:
	Random sample of 100000 Patients with at least one PCP visit in 2008.
	All Patients with at least one MOVE! visit in 2008

For each of these cohorts, we will extract weight data 
two years prior and two years after initial PCP (or MOVE!) visit , 
exclude patients based on age > XX, and include site information and patient
demographics.

*****************************************************************************/

USE CDWWork
GO

/******************************* All Patients *******************************/

-- Patients with at least one PCP visit in 2016

-- collect first PCP visit in 2016

IF OBJECT_ID('tempdb..#sampVisits2016') IS NOT NULL 
	DROP TABLE #sampVisits2016

SELECT *
	INTO #sampVisits2016
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
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
				WHERE a.VisitDateTime > '2015-12-31'
					  AND 
					  a.VisitDateTime < '2017-01-01'
					  AND 
					  (b.StopCode = 323 OR c.StopCode = 323)
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 4961072, ncol = 11

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
	WHERE ran <= 100000
-- nrow = 100000, ncol = 13

-- Patients with at least one PCP visit in 2008

-- collect first PCP visit in 2008
IF OBJECT_ID('tempdb..#sampVisits2008') IS NOT NULL 
	DROP TABLE #sampVisits2008

SELECT *
	INTO #sampVisits2008
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
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
				WHERE a.VisitDateTime > '2007-12-31'
					  AND 
					  a.VisitDateTime < '2009-01-01'
					  AND 
					  (b.StopCode = 323 OR c.StopCode = 323)
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 4268777, ncol = 11

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
	WHERE ran <= 100000
-- nrow = 100000, ncol = 13

/************************* Sample MOVE! Participants ************************/

------------ MOVE! patients in 2016 ------------

-- collect first MOVE visit in 2016
IF OBJECT_ID('tempdb..#MOVEVisits2016') IS NOT NULL 
	DROP TABLE #MOVEVisits2016

SELECT *
	INTO #MOVEVisits2016
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
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
				WHERE a.VisitDateTime > '2015-12-31'
					  AND 
					  a.VisitDateTime < '2017-01-01'
					  AND 
					  (b.StopCode IN (372, 373) OR c.StopCode IN (372, 373))
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 126577, ncol = 14

-- apply random sampling to first visits
IF OBJECT_ID('tempdb..#MOVEsamp2016') IS NOT NULL 
	DROP TABLE #MOVEsamp2016

SELECT *
	INTO #MOVEsamp2016
		FROM (
			  SELECT *,
					 ROW_NUMBER() OVER (ORDER BY RandomIndex) AS ran
				FROM (SELECT *,
							 ABS(CHECKSUM(NewId())) AS RandomIndex
						FROM #MOVEVisits2016
					 ) AS tmp
			 ) AS tmp2
		WHERE ran <= 100000
-- nrow = 100000, ncol = 13

------------ MOVE! patients in 2008 ------------

-- collect first MOVE visit in 2008
IF OBJECT_ID('tempdb..#MOVEVisits2008') IS NOT NULL 
	DROP TABLE #MOVEVisits2008

SELECT *
	INTO #MOVEVisits2008
		FROM (
			  SELECT f.PatientICN,
					 a.PatientSID,
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
				WHERE a.VisitDateTime > '2007-12-31'
					  AND 
					  a.VisitDateTime < '2009-01-01'
					  AND 
					  (b.StopCode IN (372, 373) OR c.StopCode IN (372, 373))
			 ) AS Tmp
		WHERE Cnt = 1 AND AgeAtVisit >= 18
-- nrow = 72334, ncol = 11

/********************* Collect all Weights for Samples **********************/

------ collect weights within 2 years of PCP visit ------

----- 2008 -----

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
-- nrow = 4180355, ncol = 19

IF OBJECT_ID('tempdb..#PCPWindowWeights2008') IS NOT NULL 
	DROP TABLE #PCPWindowWeights2008

SELECT PatientICN,
	   PatientSID,
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
-- nrow = 1219745, ncol = 13

----- 2016 -----

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
-- nrow = 3345553, ncol = 19

IF OBJECT_ID('tempdb..#PCPWindowWeights2016') IS NOT NULL 
	DROP TABLE #PCPWindowWeights2016

SELECT PatientICN,
	   PatientSID,
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
-- nrow = 1220985, ncol = 13

------ collect weights within 2 years of MOVE! visit ------

----- 2008 -----

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
-- nrow = 5401576, ncol = 15

IF OBJECT_ID('tempdb..#MOVEWindowWeights2008') IS NOT NULL 
	DROP TABLE #MOVEWindowWeights2008

SELECT PatientICN,
	   PatientSID,
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
-- nrow = 1715502, ncol = 15

-- 2016 --

IF OBJECT_ID('tempdb..#MOVEWeights2016') IS NOT NULL 
	DROP TABLE #MOVEWeights2016

SELECT a.*,
	   b.VitalSignTakenDateTime AS WeightDateTime,
	   b.VitalResultNumeric AS Weight,
	   CAST(CONVERT(VARCHAR(8), b.VitalSignTakenDateTime, 112) AS date) AS WeightDate
	INTO #MOVEWeights2016
		FROM #MOVEsamp2016 AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%weight%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 5741052, ncol = 15

IF OBJECT_ID('tempdb..#MOVEWindowWeights2016') IS NOT NULL 
	DROP TABLE #MOVEWindowWeights2016

SELECT PatientICN,
	   PatientSID,
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
-- nrow = 2748805, ncol = 13

/************************* Weights: Save Permanent **************************/

IF OBJECT_ID('NCP_DCEP.Samp.NationalWeightSamples') IS NOT NULL 
	DROP TABLE NCP_DCEP.Samp.NationalWeightSamples

SELECT *
	INTO NCP_DCEP.Samp.NationalWeightSamples
		FROM (
			  SELECT * FROM #PCPWindowWeights2008
			  UNION
			  SELECT * FROM #PCPWindowWeights2016
			  UNION
			  SELECT * FROM #MOVEWindowWeights2008
			  UNION
			  SELECT * FROM #MOVEWindowWeights2016
			 ) AS sampleUnion
-- nrow = 6890575, ncol = 13

/*
SELECT Source, COUNT(*) AS Freq
	FROM NCP_DCEP.Samp.NationalWeightSamples
	GROUP BY Source

Source | Freq
-------|----------
MOVE   | 4455018
PCP	   | 2435557
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
		FROM NCP_DCEP.samp.NationalWeightSamples AS a
			LEFT JOIN CDWWork.Patient.Patient AS b
				ON a.PatientSID = b.PatientSID
-- nrow = 6890575, ncol = 2

ALTER TABLE NCP_DCEP.samp.NationalWeightSamples
	DROP COLUMN Gender;

ALTER TABLE NCP_DCEP.samp.NationalWeightSamples
	ADD Gender CHAR(1);

UPDATE NCP_DCEP.samp.NationalWeightSamples
	SET Gender = b.Gender
		FROM NCP_DCEP.samp.NationalWeightSamples AS a
			LEFT JOIN #Gender AS b
				ON a.PatientICN = b.PatientICN
-- nrow = 6890575, ncol = 14

/*
SELECT Gender, COUNT(*) 
	FROM NCP_DCEP.samp.NationalWeightSamples
	GROUP BY Gender

Gender	N
------- -----
F		813931
M		6076644
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
		FROM NCP_DCEP.samp.NationalWeightSamples
		WHERE Gender = 'F'
-- nrow = 37262, ncol = 3

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
-- nrow = 1415, ncol = 1

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
-- nrow = 495, ncol = 1

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
-- nrow = 1516, ncol = 2

-- Join with NCP_DCEP.Samp.NationalWeightSamples

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD Pregnant INT

UPDATE a
	SET a.Pregnant = b.Pregnant
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #Pregnant AS b
				ON a.PatientICN = b.PatientICN
-- 31112 rows affected
-- nrow = 279604, ncol = 18

DROP TABLE #OutptPreg, #InptPreg, #Pregnant, #Women

/*
SELECT Pregnant, COUNT(*) AS N
	FROM NCP_DCEP.Samp.NationalWeightSamples
	GROUP BY Pregnant

Pregnant	N
----------	-------
1			31112
NULL		6859463
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
		FROM NCP_DCEP.Samp.NationalWeightSamples
-- nrow = 370077, ncol = 4

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
-- nrow = 510, ncol = 3

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
-- nrow = 1180, ncol = 2

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
-- nrow = 182, ncol = 2

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
-- nrow = 1872, ncol = 3

----- Join with NCP_DCEP.Samp.weightSamples -----

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	DROP COLUMN Bariatric, BariatricDateTime
GO;

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD Bariatric int
GO;

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD BariatricDateTime datetime
GO;

;UPDATE a
	SET a.Bariatric = b.Bariatric,
	    a.BariatricDateTime = b.BariatricDateTime
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #Bariatric AS b
				ON a.PatientICN = b.PatientICN
-- 48279 rows affected
-- nrow = 6890575, ncol = 17

UPDATE NCP_DCEP.Samp.NationalWeightSamples
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
	FROM NCP_DCEP.Samp.NationalWeightSamples
	GROUP BY Bariatric

Bariatric	N
----------- -------
0			6845725
1			44850
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
-- nrow = 204452, ncol = 6

/*
SELECT COUNT(DISTINCT PatientICN) FROM #InptStays
-- 62659 people

SELECT COUNT(DISTINCT PatientICN) FROM #folks
-- 359187 people in total
-- 17% were inpatient at some point
*/

IF OBJECT_ID('tempdb..#InptWeights') IS NOT NULL 
	DROP TABLE #InptWeights

SELECT a.PatientICN,
	   a.WeightDateTime,
	   1 AS InptWeight
	INTO #InptWeights
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #InptStays AS b
				ON a.PatientICN = b.PatientICN
		WHERE a.WeightDateTime <= b.DischargeDateTime
			  AND
			  a.WeightDateTime >= b.AdmitDateTime
-- nrow = 23523, ncol = 3

----- Join with NCP_DCEP.Samp.NationalWeightSamples -----

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	DROP COLUMN InptWeight
GO

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD InptWeight int
GO

;UPDATE a
	SET a.InptWeight = b.InptWeight
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #InptWeights AS b
				ON a.PatientICN = b.PatientICN
				   AND
				   a.WeightDateTime = b.WeightDateTime
-- 23406 rows affected
-- nrow = 6890575, ncol = 18

/*
SELECT InptWeight, COUNT(*)
	FROM NCP_DCEP.Samp.NationalWeightSamples
	GROUP BY InptWeight

InptWeight	N
----------- -------
1			23406
NULL		6867169
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
			    FROM NCP_DCEP.Samp.NationalWeightSamples
			 ) AS a
			LEFT JOIN Vital.VitalSign AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.VitalType AS c
				ON b.VitalTypeSID = c.VitalTypeSID
		WHERE c.VitalType LIKE '%height%'
			  AND b.EnteredInErrorFlag IS NULL
-- nrow = 7338348, ncol = 7

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
-- nrow = 2336827, ncol = 6

/************************* Heights: Save Permanent **************************/

IF OBJECT_ID('NCP_DCEP.Samp.NationalHeightSamples') IS NOT NULL 
DROP TABLE NCP_DCEP.Samp.NationalHeightSamples

SELECT *
	INTO NCP_DCEP.Samp.NationalHeightSamples
		FROM #WindowHeights
-- nrow = 2336827, ncol = 6

/****************************** Collect Race *********************************/

-- add RaceSID to Patient Data
IF OBJECT_ID('tempdb..#PatPlusRace') IS NOT NULL 
DROP TABLE #PatPlusRace

SELECT PatientICN,
       PatientSID,
	   RaceSID,
       CASE WHEN Race LIKE 'White n%'  THEN 'WHITE'
	        WHEN Race LIKE '%decline%' THEN 'Missing'
			WHEN Race LIKE '%unknown%' THEN 'Missing'
			WHEN Race LIKE '%Missing%' THEN 'Missing'
			ELSE Race
		END AS RaceCat
	INTO #PatPlusRace
		FROM (
			  SELECT DISTINCT a.PatientICN,
			         a.PatientSID,
					 b.Race,
					 b.RaceSID
				FROM NCP_DCEP.Samp.NationalWeightSamples AS a
					LEFT JOIN CDWWork.PatSub.PatientRace AS b
						ON a.PatientSID = b.PatientSID
			 ) AS a
-- nrow = 368199, ncol = 4

/*
SELECT COUNT(DISTINCT PatientICN) FROM #PatPlusRace
-- 359187

SELECT RaceCat, COUNT(*) AS _N_
	FROM #PatPlusRace
	GROUP BY RaceCat

RaceCat										_N_
NULL										12636
BLACK OR AFRICAN AMERICAN					68354
AMERICAN INDIAN OR ALASKA NATIVE			3660
Missing										18420
WHITE										258408
NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER	3942
ASIAN										2779
*/

-- find the most common race value

-- first count occurrence of different race values
IF OBJECT_ID('tempdb..#PatRaceCnt') IS NOT NULL 
DROP TABLE #PatRaceCnt

SELECT PatientICN,
	   RaceCat,
	   COUNT(RaceCat) AS Cnt
	INTO #PatRaceCnt
		FROM (
			  SELECT *
			    FROM #PatPlusRace
			   WHERE RaceCat IS NOT NULL
					 OR
					 RaceCat NOT LIKE 'Missing'
			 ) AS a
		GROUP BY PatientICN, RaceCat
		ORDER BY PatientICN
-- nrow = 353851, ncol = 3

IF OBJECT_ID('tempdb..#PatRaceWithCountRace') IS NOT NULL 
DROP TABLE #PatRaceWithCountRace

SELECT PatientICN,
       RaceCat,
	   Cnt,
	   ROW_NUMBER() OVER(
						 PARTITION BY PatientICN
						 ORDER BY Cnt DESC
						) AS RowNumber1_MostFreqRace
	INTO #PatRaceWithCountRace
		FROM #PatRaceCnt
-- nrow = 353851, ncol = 4

/*
SELECT COUNT(DISTINCT PatientICN) FROM #PatRaceWithCountRace
-- 346555

SELECT RowNumber1_MostFreqRace, COUNT(*) AS _N_
	FROM #PatRaceWithCountRace
	GROUP BY RowNumber1_MostFreqRace
    ORDER BY RowNumber1_MostFreqRace

1	346555
2	7058
3	218
4	16
5	4
*/

-- split and combine
IF OBJECT_ID('tempdb..#Row1Race') IS NOT NULL 
DROP TABLE #Row1Race

SELECT PatientICN,
	   RaceCat AS RaceRow1,
	   Cnt AS CntRow1
	INTO #Row1Race
		FROM #PatRaceWithCountRace
		WHERE RowNumber1_MostFreqRace = 1
-- nrow = 346555, ncol = 3

IF OBJECT_ID('tempdb..#Row2Race') IS NOT NULL 
DROP TABLE #Row2Race

SELECT PatientICN,
	   RaceCat AS RaceRow2,
	   Cnt AS CntRow2
	INTO #Row2Race
		FROM #PatRaceWithCountRace
		WHERE RowNumber1_MostFreqRace = 2
-- nrow = 7058, ncol = 3

IF OBJECT_ID('tempdb..#TwoMostCommonRaces') IS NOT NULL 
DROP TABLE #TwoMostCommonRaces

SELECT COALESCE(a.PatientICN, b.PatientICN) AS PatientICN,
	   RaceRow1,
	   CntRow1,
	   RaceRow2,
	   CntRow2
	INTO #TwoMostCommonRaces
		FROM #Row1Race AS a FULL OUTER JOIN #Row2Race AS b
			ON a.PatientICN = b.PatientICN
-- nrow = 346555, ncol = 5

IF OBJECT_ID('tempdb..#MostCommonRace') IS NOT NULL 
DROP TABLE #MostCommonRace

SELECT PatientICN,
	   RaceRow1,
	   RaceRow2,
	   CASE WHEN CntRow2 IS NULL OR CntRow1 > CntRow2 THEN 'Row 1'
	        WHEN CntRow1 = CntRow2 AND RaceRow1 = RaceRow2 THEN 'Row 1'
			WHEN CntRow1 = CntRow2 AND RaceRow1 <> RaceRow2 THEN 'Both'
		ELSE 'Missing Both'
		END AS WhichRaceToKeep
	INTO #MostCommonRace
		FROM #TwoMostCommonRaces
-- nrow = 346555, ncol = 4

/*
SELECT WhichRaceToKeep, COUNT(*) AS _N_
	FROM #MostCommonRace
	GROUP BY WhichRaceToKeep

Row 1	339542
Both	7013
*/

IF OBJECT_ID('tempdb..#DimRaceRecode') IS NOT NULL 
DROP TABLE #DimRaceRecode

-- Use Dim.Race to Break Ties or Populate Missing Values
SELECT Race,
	   RaceSID,
	   CASE WHEN Race LIKE 'Amer%'           THEN 'AMERICAN INDIAN OR ALASKA NATIVE'
	        WHEN Race LIKE 'Asian'           THEN 'ASIAN'
			WHEN Race LIKE 'Black%'          THEN 'BLACK OR AFRICAN AMERICAN'
			WHEN Race LIKE 'Hispanic Black'  THEN 'BLACK OR AFRICAN AMERICAN'
			WHEN Race LIKE 'Hispanic, Black' THEN 'BLACK OR AFRICAN AMERICAN'
			WHEN Race LIKE 'Native%'         THEN 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'
			WHEN Race LIKE 'Caucas%'         THEN 'WHITE'
			WHEN Race LIKE 'Hispanic W%'     THEN 'WHITE'
			WHEN Race LIKE 'White%'          THEN 'WHITE'
		ELSE 'Missing'
		END AS DimRaceRecode
	INTO #DimRaceRecode
		FROM CDWWork.Dim.Race
-- nrow = 1831, ncol = 3

-- joining Race (originally) in Dim.Race (now DimRaceRecode) to patient data
IF OBJECT_ID('tempdb..#AddDimRace') IS NOT NULL 
DROP TABLE #AddDimRace

SELECT a.PatientICN,
	   b.DimRaceRecode,
	   COUNT(*) AS Cnt
	INTO #AddDimRace
		FROM #PatPlusRace AS a
			LEFT JOIN #DimRaceRecode AS b
				ON a.RaceSID = b.RaceSID
		WHERE DimRaceRecode IS NOT NULL
		GROUP BY a.PatientICN, b.DimRaceRecode
		ORDER BY a.PatientICN
-- nrow = 353851, ncol = 3

-- look for most common value of race in Dim.Race ignoring ties
IF OBJECT_ID('tempdb..#OrderDimRace') IS NOT NULL 
DROP TABLE #OrderDimRace

SELECT PatientICN,
	   DimRaceRecode,
	   Cnt,
	   ROW_NUMBER() OVER (
						  PARTITION BY PatientICN
						  ORDER BY Cnt DESC
						 ) AS Row1DimRace
	INTO #OrderDimRace
		FROM #AddDimRace
-- nrow = 353851, ncol = 4

-- select race from row 1
IF OBJECT_ID('tempdb..#DimRace') IS NOT NULL 
DROP TABLE #DimRace

SELECT PatientICN, DimRaceRecode
	INTO #DimRace
		FROM #OrderDimRace
		WHERE Row1DimRace = 1
-- nrow = 346555, ncol = 2

-- Finalize the result (compare all three values of race)
IF OBJECT_ID('tempdb..#RaceChoices') IS NOT NULL 
DROP TABLE #RaceChoices

SELECT a.PatientICN,
       RaceRow1,
	   RaceRow2,
	   WhichRaceToKeep,
	   DimRaceRecode
	INTO #RaceChoices
		FROM #MostCommonRace AS a
			LEFT JOIN #DimRace AS b
				ON a.PatientICN = b.PatientICN
		GROUP BY a.PatientICN,
		         RaceRow1,
				 RaceRow2,
				 WhichRaceToKeep,
				 DimRaceRecode
		ORDER BY PatientICN
-- nrow = 346555, ncol = 5

-- choose
IF OBJECT_ID('tempdb..#FinalRace') IS NOT NULL 
DROP TABLE #FinalRace

SELECT *,
	   CASE WHEN WhichRaceToKeep = 'Row 1'
				THEN RaceRow1
			WHEN (WhichRaceToKeep = 'Both' AND DimRaceRecode NOT LIKE 'Missing')
				THEN DimRaceRecode
			WHEN (WhichRaceToKeep = 'Both' AND DimRaceRecode = 'Missing')
				THEN 'MULTI-RACIAL'
	   END AS Race
	INTO #FinalRace
		FROM #RaceChoices
-- nrow = 346555, ncol = 6

IF OBJECT_ID('tempdb..#RaceData') IS NOT NULL 
DROP TABLE #RaceData

SELECT DISTINCT a.PatientICN,
       Race
	INTO #RaceData
		FROM #PatPlusRace AS a
			LEFT JOIN #FinalRace AS b
				ON a.PatientICN = b.PatientICN
		GROUP BY a.PatientICN, Race
-- nrow = 359187, ncol = 2

----- Join with NCP_DCEP.Samp.NationalWeightSamples -----

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	DROP COLUMN Race
GO

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD Race varchar(255)
GO

;UPDATE a
	SET a.Race = b.Race
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #RaceData AS b
				ON a.PatientICN = b.PatientICN
-- nrow = 6890575, ncol = 18

/***************** Ethnicity *****************/

IF OBJECT_ID('tempdb..#Ethnicity') IS NOT NULL 
	DROP TABLE #Ethnicity

SELECT a.PatientICN,
	   c.EthnicitySID,
       c.Ethnicity,
	   d.Ethnicity AS Dim_Ethnicity
	INTO #Ethnicity
		FROM (
			  SELECT DISTINCT PatientICN 
				FROM NCP_DCEP.Samp.NationalWeightSamples
			 ) AS a
			LEFT JOIN SPatient.SPatient AS b
				ON a.PatientICN = b.PatientICN
			LEFT JOIN PatSub.PatientEthnicity AS c
				ON b.PatientSID = c.PatientSID
			 LEFT JOIN Dim.Ethnicity AS d
				ON c.EthnicitySID = d.EthnicitySID
-- nrow = 1000241, ncol = 4

/*
SELECT Ethnicity, COUNT(*) AS Freq
	FROM #Ethnicity
	GROUP BY Ethnicity

Ethnicity			   | Freq
-----------------------|-------
NOT HISPANIC OR LATINO | 894909
NULL				   | 16580
HISPANIC OR LATINO	   | 56719
UNKNOWN BY PATIENT	   | 15464
DECLINED TO ANSWER	   | 16569

SELECT Ethnicity, Dim_Ethnicity
	FROM #Ethnicity
		WHERE Ethnicity NOT LIKE Dim_Ethnicity
-- nrow = 0, ncol = 2
-- never an issue
*/

---------- check for duplicates ----------

IF OBJECT_ID('tempdb..#EthnicityCNT') IS NOT NULL 
	DROP TABLE #EthnicityCNT

SELECT PatientICN,
	   Ethnicity,
	   COUNT(Ethnicity) AS Cnt
	INTO #EthnicityCNT
		FROM #Ethnicity
			GROUP BY PatientICN, Ethnicity
			ORDER BY PatientICN
-- nrow = 365979, ncol = 3

IF OBJECT_ID('tempdb..#EthWithCountRace') IS NOT NULL 
	DROP TABLE #EthWithCountRace

SELECT PatientICN,
	   Ethnicity,
	   Cnt,
	   ROW_NUMBER() OVER(
					     PARTITION BY PatientICN
					     ORDER BY Cnt DESC
					    ) AS RowNumber1_MostFreqEth
		INTO #EthWithCountRace
			FROM #EthnicityCNT
-- nrow = 365979, ncol = 4

/*
SELECT COUNT(DISTINCT(PatientICN)) FROM #EthWithCountRace -- 359187

SELECT RowNumber1_MostFreqEth, COUNT(*) AS testcnt
	FROM #EthWithCountRace
	GROUP BY RowNumber1_MostFreqEth

RowNumber1_MostFreqEth	| testcnt
------------------------|----------
3						| 109
1						| 359187
4						| 2
2						| 6681
*/

-- split and combine
IF OBJECT_ID('tempdb..#Row1Eth') IS NOT NULL 
	DROP TABLE #Row1Eth

SELECT PatientICN,
	   Ethnicity AS EthRow1,
	   Cnt AS CntRow1
	INTO #Row1Eth
		FROM #EthWithCountRace
		WHERE RowNumber1_MostFreqEth = 1
-- nrow = 359187, ncol = 3

IF OBJECT_ID('tempdb..#Row2Eth') IS NOT NULL 
	DROP TABLE #Row2Eth

SELECT PatientICN,
	   Ethnicity AS EthRow2,
	   Cnt AS CntRow2
	INTO #Row2Eth
		FROM #EthWithCountRace
		WHERE RowNumber1_MostFreqEth = 2
-- nrow = 6681, ncol = 3

IF OBJECT_ID('tempdb..#TwoMostCommonEth') IS NOT NULL 
	DROP TABLE #TwoMostCommonEth

SELECT COALESCE(a.PatientICN, b.PatientICN) AS PatientICN,
	   EthRow1,
	   CntRow1,
	   EthRow2,
	   CntRow2
	INTO #TwoMostCommonEth
		FROM #Row1Eth AS a
			FULL OUTER JOIN #Row2Eth AS b
				ON a.PatientICN = b.PatientICN
-- nrow = 359187, ncol = 5

IF OBJECT_ID('tempdb..#MostCommonEth') IS NOT NULL 
	DROP TABLE #MostCommonEth

SELECT PatientICN,
	   EthRow1,
	   EthRow2,
	   CASE WHEN CntRow2 IS NULL OR CntRow1 > Cntrow2     THEN 'Row 1'
		    WHEN CntRow1 = CntRow2 AND EthRow1 = EthRow2  THEN 'Row 1'
			WHEN CntRow1 = CntRow2 AND EthRow1 <> EthRow2 THEN 'Both'
		ELSE 'Missing Both'
	   END AS WhichEthToKeep
	INTO #MostCommonEth
		FROM #TwoMostCommonEth
-- nrow = 359187, ncol = 4

/*
SELECT WhichEthToKeep, COUNT(*) AS Cnt
	FROM #MostCommonEth
	GROUP BY WhichEthToKeep

WhichEthToKeep | Cnt
---------------|-------
Row 1		   | 358999
Both		   | 188
*/

-- 359187 PatientICN in #MostCommonEth
-- 359187 PatientICN in #Ethnicity
-- 359187 PatientICN in NCP_DCEP.Samp.NationalWeightSamples

-- Joining ethnicity from Dim.Ethnicity to patient data
IF OBJECT_ID('tempdb..#AddDimEth') IS NOT NULL 
	DROP TABLE #AddDimEth

SELECT a.PatientICN,
	   b.Ethnicity,
	   COUNT(*) AS Cnt
	INTO #AddDimEth
		FROM #Ethnicity AS a 
			LEFT JOIN Dim.Ethnicity AS b
				ON a.EthnicitySID = b.EthnicitySID
		WHERE b.Ethnicity IS NOT NULL
		GROUP BY a.PatientICN, b.Ethnicity
		ORDER BY a.PatientICN
-- nrow = 353752, ncol = 3

-- look for most common value of race in Dim.Ethnicity ignoring ties
IF OBJECT_ID('tempdb..#OrderDimEth') IS NOT NULL 
	DROP TABLE #OrderDimEth

SELECT PatientICN,
	   Ethnicity,
	   Cnt,
	   ROW_NUMBER() OVER(
						 PARTITION BY PatientICN
						 ORDER BY Cnt DESC
						) AS Row1DimEth
	INTO #OrderDimEth
		FROM #AddDimEth
-- nrow = 353752, ncol = 4

-- select race from row 1
IF OBJECT_ID('tempdb..#DimEth') IS NOT NULL 
	DROP TABLE #DimEth

SELECT PatientICN, 
	   Ethnicity
	INTO #DimEth
		FROM #OrderDimEth
		WHERE Row1DimEth = 1
-- nrow = 353449, ncol = 2

-- Finalize result (compare all three values of Ethnicity)
IF OBJECT_ID('tempdb..#EthChoices') IS NOT NULL 
	DROP TABLE #EthChoices

SELECT a.PatientICN,
	   EthRow1,
	   EthRow2,
	   WhichEthToKeep,
	   Ethnicity
	INTO #EthChoices
		FROM #MostCommonEth AS a 
			LEFT JOIN #DimEth AS b
				ON a.PatientICN = b.PatientICN
		GROUP BY a.PatientICN,
				 EthRow1,
				 EthRow2,
				 WhichEthToKeep,
				 Ethnicity
		ORDER BY PatientICN
-- nrow = 359187, ncol = 5

/*
SELECT EthRow1, EthRow2, COUNT(*) AS Freq
	FROM #EthChoices
	WHERE WhichEthToKeep LIKE 'Both'
	GROUP BY EthRow1, EthRow2
-- All in disagreement
*/

IF OBJECT_ID('tempdb..#FinalEthnicity') IS NOT NULL 
	DROP TABLE #FinalEthnicity

SELECT PatientICN,
	   CASE WHEN WhichEthToKeep = 'Row 1' THEN EthRow1
			WHEN WhichEthToKeep = 'Both'  THEN Ethnicity
			END AS Eth
	INTO #FinalEthnicity
		FROM #EthChoices
-- nrow = 359187, ncol = 6

/*
SELECT Eth, COUNT(*) AS Freq
	FROM #FinalEthnicity
	GROUP BY Eth

Eth						| Freq
------------------------|---------
NOT HISPANIC OR LATINO	| 321100
NULL					| 5738
HISPANIC OR LATINO		| 21185
UNKNOWN BY PATIENT		| 5287
DECLINED TO ANSWER		| 5877
*/

-- add Eth to demographics
ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD Ethnicity VARCHAR(22)

UPDATE NCP_DCEP.Samp.NationalWeightSamples
	SET NCP_DCEP.Samp.NationalWeightSamples.Ethnicity = b.Eth
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a 
			LEFT JOIN #FinalEthnicity AS b
				ON a.PatientICN = b.PatientICN
-- nrow = 6890575, ncol = 21

/************************ Collect Diabetic Status ***************************/

-- ICD9: 250.xx
-- ICD10: E10.x, E11.x, E13.x

IF OBJECT_ID('tempdb..#folks') IS NOT NULL 
	DROP TABLE #folks

SELECT DISTINCT PatientICN,
	   PatientSID,
	   CONVERT(date, DATEADD(year, -2, VisitDateTime)) AS alpha,
	   VisitDateTime AS IndexDate,
	   CONVERT(date, DATEADD(year,  2, VisitDateTime)) AS omega
	INTO #folks
		FROM NCP_DCEP.Samp.NationalWeightSamples
-- nrow = 370172, ncol = 5

----- Outpatient Diagnosis Codes -----

IF OBJECT_ID('tempdb..#OutptDiabetesVisits') IS NOT NULL 
	DROP TABLE #OutptDiabetesVisits

SELECT tmp.*
	INTO #OutptDiabetesVisits
		FROM (
			  (
			   SELECT DISTINCT a.PatientICN,
					  a.IndexDate,
					  b.VisitDateTime
				 FROM #folks AS a
					 LEFT JOIN Outpat.VDiagnosis AS b
					 	 ON a.PatientSID = b.PatientSID
					 INNER JOIN Dim.ICD9 AS c
						 ON b.ICD9SID = c.ICD9SID
				 WHERE ICD9Code LIKE '250%'
					   AND
					   CONVERT(date, b.VisitDateTime) >= CONVERT(date, alpha) 
					   AND 
					   CONVERT(date, b.VisitDateTime) <= CONVERT(date, omega)
			  )
			  UNION
			  (
			   SELECT DISTINCT PatientICN,
					  a.IndexDate,
			          b.VisitDateTime
				 FROM #folks AS a
					LEFT JOIN Outpat.VDiagnosis AS b
						ON a.PatientSID = b.PatientSID
					INNER JOIN Dim.ICD10 AS c
						ON b.ICD10SID = c.ICD10SID
				WHERE (
					   ICD10Code LIKE 'E10%'
					   OR
					   ICD10Code LIKE 'E11%'
					   OR
					   ICD10Code LIKE 'E13%'
					  )
					  AND
					  CONVERT(date, b.VisitDateTime) >= CONVERT(date, alpha) 
					  AND 
					  CONVERT(date, b.VisitDateTime) <= CONVERT(date, omega)
			  )
			 ) AS tmp
-- nrow = 3107189, ncol = 3

IF OBJECT_ID('tempdb..#OutptDiabetics') IS NOT NULL 
	DROP TABLE #OutptDiabetics

SELECT DISTINCT PatientICN,
       DiabetesTiming
	INTO #OutptDiabetics 
		FROM (
			  SELECT PatientICN,
			         CASE WHEN VisitDateTime < IndexDate
						       THEN 'Diab Dx Before'
						  ELSE 'Diab Dx After'
						  END AS DiabetesTiming,
					 ROW_NUMBER() OVER (
									    PARTITION BY PatientICN
										ORDER BY VisitDateTime
									   ) AS Cnt
				FROM #OutptDiabetesVisits
			 ) AS tmp
		WHERE Cnt >= 2
-- nrow = 225872, ncol = 2

----- Inpatient Diagnosis Codes -----

IF OBJECT_ID('tempdb..#InptDiabetics') IS NOT NULL 
	DROP TABLE #InptDiabetics

SELECT tmp.*
	INTO #InptDiabetics
		FROM (
			  (
			   SELECT DISTINCT a.PatientICN,
			          CASE WHEN b.DischargeDateTime < a.IndexDate
						        THEN 'Diab Dx Before'
						   ELSE 'Diab Dx After'
						   END AS DiabetesTiming
				 FROM #folks AS a
					 LEFT JOIN Inpat.InpatientDiagnosis AS b
					 	 ON a.PatientSID = b.PatientSID
					 INNER JOIN Dim.ICD9 AS c
						 ON b.ICD9SID = c.ICD9SID
				 WHERE ICD9Code LIKE '250%'
					   AND
					   CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
					   AND 
					   CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
			  )
			  UNION
			  (
			   SELECT DISTINCT a.PatientICN,
			          CASE WHEN b.DischargeDateTime < a.IndexDate
						        THEN 'Diab Dx Before'
						   ELSE 'Diab Dx After'
						   END AS DiabetesTiming
					FROM #folks AS a
						LEFT JOIN Inpat.InpatientDiagnosis AS b
							ON a.PatientSID = b.PatientSID
						INNER JOIN Dim.ICD10 AS c
							ON b.ICD10SID = c.ICD10SID
					WHERE (
						   ICD10Code LIKE 'E10%'
						   OR
						   ICD10Code LIKE 'E11%'
						   OR
						   ICD10Code LIKE 'E13%'
						  )
						  AND
						  CONVERT(date, b.DischargeDateTime) >= CONVERT(date, alpha) 
						  AND 
						  CONVERT(date, b.DischargeDateTime) <= CONVERT(date, omega)
			  )
			 ) AS tmp
-- nrow = 40028, ncol = 2

----- Combine Inpat + Outpat -----

IF OBJECT_ID('tempdb..#Diabetics') IS NOT NULL 
	DROP TABLE #Diabetics

SELECT tmp.*
	INTO #Diabetics
		FROM (
			  SELECT * FROM #OutptDiabetics
			  UNION
			  SELECT * FROM #InptDiabetics
			 ) AS tmp
-- nrow = 227747, ncol = 2

IF OBJECT_ID('tempdb..#DiabetesTiming') IS NOT NULL 
	DROP TABLE #DiabetesTiming

;WITH tmp
	AS (
		SELECT PatientICN,
			   MAX(
				   CASE WHEN DiabetesTiming = 'Diab Dx Before' 
						THEN DiabetesTiming 
					END
				  ) DxB4,
			   MAX(
				   CASE WHEN DiabetesTiming = 'Diab Dx After'  
						THEN DiabetesTiming 
					END
				  ) DxAft
			FROM #Diabetics
			GROUP BY PatientICN
	   )
	   SELECT PatientICN,
	          CASE WHEN DxB4 IS NOT NULL AND DxAft IS NOT NULL
				        THEN 'Diabetes Before and After'
				   WHEN DxB4 IS NOT NULL AND DxAft IS NULL
						THEN 'Diabetes Before'
				   WHEN DxB4 IS NULL AND DxAft IS NOT NULL
					    THEN 'Diabetes After'
			  END AS DiabetesTiming
	     INTO #DiabetesTiming
			FROM tmp
-- nrow = 126747, ncol = 2

-- Join with NCP_DCEP.Samp.NationalWeightSamples

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	DROP COLUMN DiabetesTiming
GO

ALTER TABLE NCP_DCEP.Samp.NationalWeightSamples
	ADD DiabetesTiming VARCHAR(25)
GO

UPDATE a
	SET a.DiabetesTiming = b.DiabetesTiming
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			INNER JOIN #DiabetesTiming AS b
				ON a.PatientICN = b.PatientICN
-- 3290568 rows affected
-- nrow = 6890575, ncol = 18

DROP TABLE #DiabetesTiming,
		   #Diabetics,
		   #OutptDiabetesVisits,
		   #OutptDiabetics,
		   #InptDiabetics

/*
SELECT TOP 10 * FROM NCP_DCEP.Samp.NationalWeightSamples

SELECT COUNT(DISTINCT PatientICN)
	FROM NCP_DCEP.Samp.NationalWeightSamples
-- 359187

SELECT DiabetesTiming, COUNT(*) AS N
	FROM NCP_DCEP.Samp.NationalWeightSamples
	GROUP BY DiabetesTiming

DiabetesTiming				N
--------------------------------------
NULL						3600007
Diabetes After				426036
Diabetes Before				71192
Diabetes Before and After	2793340

SELECT COUNT(DISTINCT PatientICN) AS N
	FROM NCP_DCEP.Samp.NationalWeightSamples
	WHERE DiabetesTiming IS NOT NULL
-- 126747
*/

/******************** Collect MOVE Visits within 1 year *********************/

/*
Hi Rich—I have in my notes for DCEP that I was supposed to send this to you! 

We removed any stops where the primary stop code was in (999,683,685 or 686) 
and any stops where the secondary stop code was in 
(999,692,648,693,645,474,371,684,685,686)

This is the info about those stop, just FYI.

--MOVE Stop code combinations to exclude (from Stephanie Chan at NCP)
999 = employeemove  
692 or 648 or 693 or 645 = cvtprovider 
474 = research 
683 or 371 or 685 or 684 or 685 or 686 = hometelehealth 

Jenny
*/

-- Collect all MOVE visits in 2016
IF OBJECT_ID('tempdb..#MOVEVisits2016') IS NOT NULL 
	DROP TABLE #MOVEVisits2016

SELECT a.*,
	   b.EncounterDateTime AS MOVEvisitDate,
	   c.StopCode AS PrimaryStopCode,
	   d.StopCode AS SecondaryStopCode
	INTO #MOVEVisits2016
		FROM (
			  SELECT DISTINCT PatientSID, 
					 VisitDateTime
				FROM NCP_DCEP.Samp.NationalWeightSamples
			   WHERE SampleYear = 2016
					 AND
					 Source = 'MOVE'
			 ) AS a
			LEFT JOIN Outpat.Workload AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN Dim.StopCode AS c
				ON b.PrimaryStopCodeSID = c.StopCodeSID
			LEFT JOIN Dim.Stopcode AS d
				ON b.SecondaryStopCodeSID = d.StopCodeSID
		WHERE b.EncounterDateTime <= DATEADD(year, 1, a.VisitDateTime)
			  AND
			  b.EncounterDateTime >= DATEADD(year, -1, a.VisitDateTime)
			  AND 
			  (c.StopCode IN (372, 373) OR d.StopCode IN (372, 373))
-- nrow = 1118861, ncol = 5

-- Exclude Certain Stop Code Combinations
IF OBJECT_ID('tempdb..#MOVEVisits2016red') IS NOT NULL 
	DROP TABLE #MOVEVisits2016red

SELECT PatientSID,
	   VisitDateTime,
	   MOVEvisitDate
	INTO #MOVEVisits2016red
		FROM #MOVEVisits2016
		WHERE PrimaryStopCode NOT IN (999, 683, 685, 686)
			  AND
			  SecondaryStopCode NOT IN (999, 692, 648, 693, 645, 474, 371, 684, 685, 686)
-- nrow = 767201, ncol = 3

----- save permanent -----

IF OBJECT_ID('NCP_DCEP.Samp.MOVEvisits2016') IS NOT NULL 
	DROP TABLE NCP_DCEP.Samp.MOVEvisits2016

SELECT *
	INTO NCP_DCEP.Samp.MOVEvisits2016
		FROM #MOVEVisits2016red
-- nrow = 767201, ncol = 3

/*************** Collect Distances to Nearest MOVE! facility ****************/

IF OBJECT_ID('tempdb..#Distances') IS NOT NULL 
	DROP TABLE #Distances

SELECT DISTINCT a.PatientICN,
       CLOSESTPCSITE,
	   DRIVETIMEPC,
	   DRIVEDISTANCEPC
	INTO #Distances
		FROM NCP_DCEP.Samp.NationalWeightSamples AS a
			LEFT JOIN CDWWork.SPatient.SPatient AS b
				ON a.PatientSID = b.PatientSID
			LEFT JOIN VINCI_PSSG.PSSG.FY2016 AS c
				ON b.ScrSSN = c.ScrSSN
		WHERE CLOSESTPCSITE IS NOT NULL AND CLOSESTPCSITE != ''
-- nrow = 313049, ncol = 4

----- save permanent -----

IF OBJECT_ID('NCP_DCEP.Samp.Distances2016') IS NOT NULL 
	DROP TABLE NCP_DCEP.Samp.Distances2016

SELECT *
	INTO NCP_DCEP.Samp.Distances2016
		FROM #Distances
-- nrow = 313049, ncol = 4

/******************** Remove Temp Tables from Workspace *********************/

SELECT LEFT(name, charindex('_', name) - 1) AS tempTables
	FROM tempdb..sysobjects
	WHERE charindex('_', name) > 0
		  AND xtype = 'u'
		  AND NOT object_id('tempdb..'+name) IS NULL
	ORDER BY tempTables

DROP TABLE #AddDimEth,
		   #AddDimRace,
		   #DimEth,
	       #DimRace,
		   #DimRaceRecode,
		   #Distances,
		   #EthChoices,
		   #Ethnicity,
		   #EthnicityCNT,
		   #EthWithCountRace,
		   #FinalEthnicity,
		   #FinalRace,
		   #folks,
		   #Heights,
		   #InptStays,
		   #InptWeights,
		   #Gender,
		   #MostCommonEth,
		   #MostCommonRace,
		   #MOVEsamp2016,
		   #MOVEVisits2008,
		   #MOVEVisits2016,
		   #MOVEVisits2016red,
		   #MOVEWeights2008,
		   #MOVEWeights2016,
		   #MOVEWindowWeights2008,
		   #MOVEWindowWeights2016,
		   #OrderDimEth,
		   #OrderDimRace,
		   #PatPlusRace,
		   #PatRaceCnt,
		   #PatRaceWithCountRace,
		   #PCPWeights2008,
		   #PCPWeights2016,
		   #PCPWindowWeights2008,
		   #PCPWindowWeights2016,
		   #RaceChoices,
		   #RaceData,
		   #Row1Eth,
		   #Row1Race,
		   #Row2Eth,
		   #Row2Race,
		   #samp2008,
		   #samp2016,
		   #sampVisits2008,
		   #sampVisits2016,
		   #TwoMostCommonEth,
		   #TwoMostCommonRaces

/***************************** End of Document ******************************/