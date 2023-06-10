**************************************************************************************************
* Name: main.R                                                                                   *
* Description: Taxi Project Main Program To Run Other Programs                                   *
* V1 Creation Date: 01/06/2023                                                                   *
* Publication Date: 09/06/2023                                                                   *
* Created by: James Wright                                                                       *
*             Graduate Programmer                                                                *
*             Katalyze Data Ltd.                                                                 *
**************************************************************************************************;

Hopefully this works - it does on my computer with my version of R and packages!

----------------------------------------------------------Usage--------------------------------------------------------------------
1. Find <path> to root folder Taxi.
2. Optional: Run Package_Manager.R 
   - IMPORTANT: If Reporting.RMD does not knit, it is because a package is missing and it won't install it. If that happens, open and run Package_Manager.R.
   - This *SHOULD* not be needed. :)
3. Navigate to R/Programs/Reporting.RMD
4. Knit with Parameters
   - Insert required <path> and other details in the pop-up menu.
5. 'Run all' in the Reporting.RMD file before running the hypothesis_testing.R file so that everything gets put into the global environment :)
6. Run hypothesis_testing.R
7. Kaffee und Kuchen

----------------------------------------------------------Folder Structure----------------------------------------------------------

Taxi (Root)
	Documents : Case Study PDF Files

	R : R-Language Related Folders and Programs
		Package_Manager.R : Backup program for packages to be installed to run everything (if required manually).

		Data : Folders of Data
			Clean      : Folder containing processed datasets.
			Raw        : Unprocessed data files.

		Programs : Folder of SAS programs
		        autoexec.R           : Auto Execution file to set-up R environment.
			utilities.R.         : Program of support functions
			data_import.R.       : Program to import data
			data_processing.R    : Program to clean data
			data_analysis.R      : Program to perform data analysis
			Reporting.RMD        : Markdown file to produce report
			hypothesis_testing.R : Program to perform hypothesis test of features

		Reports : Folder containing output reports.