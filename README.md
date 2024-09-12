# Replication Materials for "Occupational Selection and the Reliability of Position Generator Measures of Social Capital"

## Software Requirement

To undertake replication, you need R. Required packages are listed at the top of the R scripts.  

## Data

The folder "PG-Reliability/Data and Spreadsheets" contains a data frame (SSND_PGitems_Replication_Data.dta) for the replication data. It contains 998 rows (one for each respondent, excluding those who did not provide a valid response to one or more of the 30 PG items) and 31 columns, as follows:
v48_1 to v48_30 provide information on the relational criterion that links a respondent to an occupation, with values:

- 0: no connection
- 1: connection via a relative (and possibly via a friend or acquaintance as well)
- 2: connection via a friend (and possibly via an acquaintance as well), but not a relative
- 3: connection via an acquaintance, but not via a relative or friend

PS_EDU is an indicator variable scored 1 if the respondent has some post-secondary education.

To replicate our analyses, variables v48_1 to v48_30 must be recoded to specify the relational criterion.
	For the acquaintance criterion, recode values 1-3 to 1 and 0 to 0.
	For the friendship criterion, recode values 1-2 to 1 and values 0 and 3 to 0.
	For the relative criterion, recode value 1 to 1 and values 0, 2, and 3 to 0.

ISEI scores, Sixma-Ultee prestige scores, EGP class codes, and other occupational information (including stata assigments) for the 30 occupations are given in separate spreadsheets (Strata_assignment.xlsx and EGP_assignment.xlsx) that the replication scripts read and use in order to construct the PG measures.

To replicate the analyses discussed in section 8 for respondents who have some post-secondary education, specify an acquaintance criterion and drop respondents who have values of 0 on variable PS_EDU.

We perform the steps above in the script "PG_prepare_data.R" in the folder "PG-Reliability/Scripts."

## Replication

Below are steps for replicating the results presented in the main text. To reproduce the results in the appendix, follow the same steps using the files stored in the "Appendix" subfolders of the "Data," "Run Stan Models," and "Replicate Results" folders.

  1. Save the Stan files stored in "Dichotomous-ARD-Replication-Materials/Stan Files/". This folder contains five subfolders corresponding to the models presented in the paper. Each subfolder, in turn, includes versions of the models for different data treatments (count, dichotomous, or 0/1/2+). 
  2. Go to "Dichotomous-ARD-Replication-Materials/Run Stan Models/Main Text/". Follow the headings and subheadings of "run_stan_maintext.R" to run the Stan models of Step 1. 
  3. Save the objects storing the extracted samples from each model. Their names end with "Extract" in "run_stan_maintext.R" (e.g., mccartyCount_MaltielRDM_Extract).
  4. Using the extracted samples of Step 3 and the data files stored in "Dichotomous-ARD-Replication-Materials/Data/Main Text/", follow the headings and subheadings of "replicate_results_maintext.R" in "Dichotomous-ARD-Replication-Materials/Replicate Results/Main Text/". They provide directions for reproducing each table and figure presented in the paper.

