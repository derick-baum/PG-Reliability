# This script uses data created in other files
# Scripts to run before running this one:
  # prepare_data.R
  # PG_pairs.R
  # PG_triples.R
  # PG_triples_pairs.R
  # PG_pairs_RelcritCollege.R

# Load packages, class assignment data and extract occupation names ---------------------------------------------

library(readxl)
library(tidyverse)

class_scheme <- read_excel("EGP_assignment.xlsx")

SSND_occup_names <- SSND_strata_assignment_FULL$occname

# Split occupations into pairs based on strata allocation data ---------------------------------------------

SSND_occup_pairs_ISEI_Class_2 <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_2)

# Prepare data to pass to functions ---------------------------------------------

SSND_ISEI_scores <- SSND_strata_assignment_FULL$ISEI
SSND_USPrest_scores <- SSND_strata_assignment_FULL$USPrest

names(SSND_ISEI_scores) <- names(SSND_USPrest_scores) <- SSND_strata_assignment_FULL$occname

SSND_scores_equal_df <- map(
  SSND_occup_pairs_ISEI_Class_2,
  ~ SSND_strata_assignment_FULL %>%
    filter(occname == .x[1] |
             occname == .x[2]) %>%
    select(occname, USPrest, ISEI)
)

SSND_scores_equal <- map_df(SSND_scores_equal_df,
                            ~ .x %>%
                              summarise(
                                mean_USPrest = mean(USPrest),
                                mean_ISEI = mean(ISEI)
                              ))

SSND_ISEI_scores_equal <- rep(SSND_scores_equal$mean_ISEI, each = 2)
SSND_USPrest_scores_equal <- rep(SSND_scores_equal$mean_USPrest, each = 2)

names(SSND_ISEI_scores_equal) <- names(SSND_USPrest_scores_equal) <- unname(unlist(SSND_occup_pairs_ISEI_Class_2))

classes <- split(class_scheme$occname, class_scheme$`Class code`)

# Load functions --------------------------------------------- 

source("PG_measures_fun.R")
source("PG_measures_fun_equalscores.R")

# Run functions for replicate groups ---------------------------------------------

# Replicate Pairs
replicate_pairs_15_updated <- map(replicate_pairs_15, ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))), .progress = TRUE)

# Equating ISEI within each stratum
replicate_pairs_updatedEqualScores <- map(replicate_pairs$ISEI_Class,
                                          ~ map(.x, ~ suppressWarnings(PG_measures_fun_equalscores(.x))),
                                          .progress = TRUE)

# Relationship Criteria
replicate_pairs_relationship_criteria_updated <- map(replicate_pairs_relationship_criteria,
                                                     ~ map(.x, ~ map(.x, ~ suppressWarnings(
                                                       PG_measures_fun(.x)
                                                     )), .progress = TRUE))

# College educated
replicate_pairs_college_updated <- map(replicate_pairs_college,
                                       ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                       .progress = TRUE)

# Replicate Triples
replicate_triples_updated <- map(replicate_triples, ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))), .progress = TRUE)

# Replicate Pairs Mixed 14 (Top and Middle)
replicate_pairs_Mixed14_updated <- map(replicate_pairs_Mixed14, ~ map(.x, ~ map(.x, ~ suppressWarnings(
  PG_measures_fun(.x)
)), .progress = TRUE))

# Replicate Pairs Mixed 14 (Bottom)
replicate_pairs_Mixed14_Bottom_updated <- map(replicate_pairs_Mixed14_Bottom,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 14 (Top-Middle)
replicate_pairs_Mixed14_TopMiddle_updated <- map(replicate_pairs_Mixed14_TopMiddle,
                                                 ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                                 .progress = TRUE)

# Replicate Pairs Mixed 14 (Bottom-Middle)
replicate_pairs_Mixed14_BottomMiddle_updated <- map(replicate_pairs_Mixed14_BottomMiddle,
                                                    ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                                    .progress = TRUE)

# Replicate Pairs Mixed 13 (Extreme)
replicate_pairs_Mixed13_Extreme_updated <- map(replicate_pairs_Mixed13_Extreme,
                                               ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                               .progress = TRUE)

# Replicate Pairs Mixed 13 (Middle)
replicate_pairs_Mixed13_Middle_updated <- map(replicate_pairs_Mixed13_Middle,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 13 (First)
replicate_pairs_Mixed13_First_updated <- map(replicate_pairs_Mixed13_First,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 13 (Second)
replicate_pairs_Mixed13_Second_updated <- map(replicate_pairs_Mixed13_Second,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 13 (Third)
replicate_pairs_Mixed13_Third_updated <- map(replicate_pairs_Mixed13_Third,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 13 (Fourth)
replicate_pairs_Mixed13_Fourth_updated <- map(replicate_pairs_Mixed13_Fourth,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 13 (Fifth)
replicate_pairs_Mixed13_Fifth_updated <- map(replicate_pairs_Mixed13_Fifth,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 13 (Sixth)
replicate_pairs_Mixed13_Sixth_updated <- map(replicate_pairs_Mixed13_Sixth,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 13 (Seventh)
replicate_pairs_Mixed13_Seventh_updated <- map(replicate_pairs_Mixed13_Seventh,
                                               ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                               .progress = TRUE)

# Replicate Pairs Mixed 13 (Eighth)
replicate_pairs_Mixed13_Eighth_updated <- map(replicate_pairs_Mixed13_Eighth,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 13 (Ninth)
replicate_pairs_Mixed13_Ninth_updated <- map(replicate_pairs_Mixed13_Ninth,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 12 (Extreme Middle)
replicate_pairs_Mixed12_ExtremeMiddle_updated <- map(replicate_pairs_Mixed12_ExtremeMiddle,
                                                     ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                                     .progress = TRUE)

# Replicate Pairs Mixed 12 (Middle)
replicate_pairs_Mixed12_Middle_updated <- map(replicate_pairs_Mixed12_Middle,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 12 (First)
replicate_pairs_Mixed12_First_updated <- map(replicate_pairs_Mixed12_First,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 12 (Second)
replicate_pairs_Mixed12_Second_updated <- map(replicate_pairs_Mixed12_Second,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 12 (Third)
replicate_pairs_Mixed12_Third_updated <- map(replicate_pairs_Mixed12_Third,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 12 (Fourth)
replicate_pairs_Mixed12_Fourth_updated <- map(replicate_pairs_Mixed12_Fourth,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 12 (Fifth)
replicate_pairs_Mixed12_Fifth_updated <- map(replicate_pairs_Mixed12_Fifth,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 12 (Sixth)
replicate_pairs_Mixed12_Sixth_updated <- map(replicate_pairs_Mixed12_Sixth,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 12 (Seventh)
replicate_pairs_Mixed12_Seventh_updated <- map(replicate_pairs_Mixed12_Seventh,
                                               ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                               .progress = TRUE)

# Replicate Pairs Mixed 12 (Eighth)
replicate_pairs_Mixed12_Eighth_updated <- map(replicate_pairs_Mixed12_Eighth,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 11 (Extreme)
replicate_pairs_Mixed11_Extreme_updated <- map(replicate_pairs_Mixed11_Extreme,
                                               ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                               .progress = TRUE)

# Replicate Pairs Mixed 11 (Middle)
replicate_pairs_Mixed11_Middle_updated <- map(replicate_pairs_Mixed11_Middle,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 11 (First)
replicate_pairs_Mixed11_First_updated <- map(replicate_pairs_Mixed11_First,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 11 (Second)
replicate_pairs_Mixed11_Second_updated <- map(replicate_pairs_Mixed11_Second,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)

# Replicate Pairs Mixed 11 (Third)
replicate_pairs_Mixed11_Third_updated <- map(replicate_pairs_Mixed11_Third,
                                             ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                             .progress = TRUE)

# Replicate Pairs Mixed 11 (Fourth)
replicate_pairs_Mixed11_Fourth_updated <- map(replicate_pairs_Mixed11_Fourth,
                                              ~ map(.x, ~ suppressWarnings(PG_measures_fun(.x))),
                                              .progress = TRUE)
