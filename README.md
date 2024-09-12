# Replication Materials for "Occupational Selection and the Reliability of Position Generator Measures of Social Capital"

## Software Requirement

To undertake replication, you need R. Required packages are listed at the top of the R scripts.  

## Data

The folder "PG-Reliability/Data and Spreadsheets/" contains a data frame (SSND_PGitems_Replication_Data.dta) for the replication data. It contains 998 rows (one for each respondent, excluding those who did not provide a valid response to one or more of the 30 PG items) and 31 columns, as follows:
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

ISEI scores, Sixma-Ultee prestige scores, EGP class codes, and other occupational information (including stata assigments) for the 30 occupations are given in separate spreadsheets ("Strata_assignment.xlsx" and "EGP_assignment.xlsx") that the replication scripts read and use in order to construct the PG measures.

To replicate the analyses discussed in Section 8 for respondents who have some post-secondary education, specify an acquaintance criterion and drop respondents who have values of 0 on variable PS_EDU.

We perform the steps above in the script "PG_prepare_data.R" in the folder "PG-Reliability/Scripts/".

## Replication

Below are steps for replicating the results presented in the article:

  1. Run the script "PG_prepare_data.R" in the folder "PG-Reliability/Scripts/" to prepare the data for analyses.
  2. Run the scripts "PG_pairs.R," "PG_pairs_RelcritCollege.R," "PG_triples_pairs.R," and "PG_triples.R" in the folder "PG-Reliability/Scripts/" to create replicate groups. Refer to Sections 5.1, 5.2, and Appendix A for details.
  3. Using the functions "PG_measures_fun.R" and "PG_measures_equalscores_fun.R" in the folder "PG-Reliability/Functions/", run "PG-Reliability/Scripts/PG_funrun.R" to construct the position generator measures for the replicate groups defined in step 2.
  4. Using the data files created in step 3, run "PG-Reliability/Scripts/PG_analyses.R" to construct ICC measures for each position generator measure in each replicate group.
  5. Using the data files created in step 4, run "PG-Reliability/Scripts/PG_organizefindings.R" to organize the results.
  6. Using the organized results from step 5, run "PG-Reliability/Scripts/PG_prophecy.R" to project the reliability of longer position generator instruments. Refer to Section 7.3.1 for details.
  7. Run the script "PG_agreement.R" to create the Jaccard Coefficient column of Table 1 as well as Figure 1. You will also need the function defined in "PG-Reliability/Functions/PG_gplot_alt_fun.R" to replicate Figure 1.
