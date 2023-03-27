-- The following shell command was run on linux bash before creating tha data base. This was done to avoid conflicts with naming conventions across columns
-- sed -iE 's_month_month\_year_g' ./Secondfile.csv

-- Solve format problems in Sales.Date and impute missing data in Sales.GMV
CREATE TABLE Sales2 AS
SELECT ID,
	-- Format Sales.Date column
	SUBSTR(Date, 7, 4) || '/' || SUBSTR(Date, 4, 2) AS Date,
	-- Fill missing values in Sales.Date using means by ID and date
	CASE
		WHEN GMV IS NULL THEN (
			SELECT CAST(ROUND(avg, 0) AS INT)
			FROM (
				SELECT ID, Date, AVG(GMV) AS avg, SUBSTR(Date, 4, 7) AS mon_year, Units_sold
				FROM Sales
				WHERE Units_sold = 1
				GROUP BY ID, mon_year
			)
			WHERE s1.ID = ID
			AND SUBSTR(s1.Date, 4, 7) = mon_year
		-- Impute values according to the number of units sold
		) * Units_sold
		ELSE GMV	
	END AS GMV,
	SLA,
	Procurement_SLA,
	Analytic_Category,
	Sub_category,
	product_analytic_vertical
FROM Sales AS s1;


-- Agregate values in Sales2 by product ID and date
CREATE TABLE Sales3 AS
SELECT
	--ID,
	Date,
	SUM(GMV) AS sum_gmv,
	AVG(SLA) AS avg_sla,
	AVG(Procurement_SLA) AS avg_proc_sla,
	Analytic_Category,
	Sub_category,
	product_analytic_vertical
FROM 
	Sales2
-- GROUP BY ID, Date;
GROUP BY product_analytic_vertical, Date;

-- Create auxiliar table to re-format SpecialSale.Date
DROP VIEW IF EXISTS date_str;
CREATE VIEW date_str AS
SELECT
	month,
	SUBSTR(rest, 1, pos2-1) AS day,
	SUBSTR(rest, pos2+1) AS year,
	SalesName
FROM (
	SELECT month,
			rest, 
		   INSTR(rest,'/') AS pos2,
		   SalesName
	FROM (
		SELECT SUBSTR(Date, 1, pos-1) AS month,
			   SUBSTR(Date, pos+1) AS rest, SalesName
			   
		FROM (
		  SELECT Date, INSTR(Date,'/') AS pos, SalesName
		   FROM SpecialSale)
		)
	);
			   
			   
-- Use date_str to create a corrected version of SpecialSale			   
DROP VIEW IF EXISTS SpecialSale2;
CREATE VIEW SpecialSale2 AS
SELECT
year || '/' || month || '/' || day AS Date,
SalesName
FROM
	(
	SELECT 
		CASE
			WHEN LENGTH(day) = 1 THEN '0' || day
			ELSE day
		END AS day,

		CASE
			WHEN LENGTH(month) = 1 THEN '0' || month
			ELSE month
		END AS month,
		year,
		SalesName
	FROM date_str
	);
	

-- Count the number of sale dates each month			   
CREATE TABLE SpecialSale3 AS
SELECT
	SUBSTR(Date, 1, 7) AS Date,
	COUNT(SalesName) AS total_sales_days 	
FROM SpecialSale2
GROUP BY SUBSTR(Date, 1, 7);


-- Extract and clean relevant data from Secondfile
CREATE TABLE Secondfile2 AS
SELECT 
    SUBSTR(REPLACE(Date, '-', '/'), 1, 7) AS Date,
	TV,
	Digital,
	Sponsorship,
	`Content.Marketing` AS content_marketing,
	`Online.marketing` AS online_marketing,
	Affiliates,
	SEM,	
	CASE
		-- Replace missing values with zeroes
		WHEN Radio = 'NA' THEN 0
		-- Convert string to a number and then cast to integer
		ELSE CAST(CAST(Radio AS REAL) AS INTEGER)
	END AS Radio,	
	CASE
		-- Replace missing values with zeroes
		WHEN Other = 'NA' THEN 0
		-- Convert string to a number and then cast to integer
		ELSE CAST(CAST(Other AS REAL) AS INTEGER)
	END AS Other,
	NPS
FROM Secondfile;


-- Create final version of the database with clean and relevant data, aggregated on amothly basis
CREATE TABLE merged_data AS
SELECT
-- 	ID AS id,
	Date || '/01' AS date,
	sum_gmv,
	avg_sla, 
	avg_proc_sla,
	TV AS tv,
	Digital AS digital,
	Sponsorship AS sponsorship,
	content_marketing AS content_mkt,
	online_marketing AS online_mkt,
	Affiliates AS affiliates,
	SEM AS sem,
	Radio AS radio,
	Other AS other,
	NPS AS nps,
	CASE
		WHEN total_sales_days IS NULL THEN 0
		ELSE total_sales_days
	END AS sales_days_per_month,
	Analytic_Category AS anal_cat,
	Sub_category AS sub_cat,
	product_analytic_vertical AS prod_anal_vert
FROM 
	Sales3
LEFT JOIN 
	SpecialSale3
USING(Date)
LEFT JOIN 
	Secondfile2
USING(Date);
