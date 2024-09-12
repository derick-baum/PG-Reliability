# Run prepare_data.R first
# This script uses the SSND_strata_assignment_FULL data created in the prepare_data.R file

# Load packages and extract occupation names ---------------------------------------------

library(haven)
library(haven)
library(readxl)
library(tools)
library(stringr)

SSND_occup_names <- SSND_strata_assignment_FULL$occname

# Split occupations into pairs based on strata allocation data ---------------------------------------------

SSND_occup_pairs_ISEI_Class_2 <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_2)

# Create matrix recording allocations of occupations to replicate pairs ---------------------------------------------

Assignments <- matrix(data = c(rep(0, 2 ^ 14)),
                      nrow = 2 ^ 14,
                      ncol = 30)
colnames(Assignments) <- unname(unlist(SSND_occup_pairs_ISEI_Class_2))
Assignments[, SSND_occup_pairs_ISEI_Class_2[[1]][1]] <- 1
Assignments[, SSND_occup_pairs_ISEI_Class_2[[1]][2]] <- 3 - Assignments[, SSND_occup_pairs_ISEI_Class_2[[1]][1]]
for (j in 2:15) {
  Assignments[, SSND_occup_pairs_ISEI_Class_2[[j]][1]] <- c(rep(c(1, 2), each = 2 ^
                                                                  (14 - j + 1)))
  Assignments[, SSND_occup_pairs_ISEI_Class_2[[j]][2]] <- 3 - Assignments[, SSND_occup_pairs_ISEI_Class_2[[j]][1]]
}

pairs_15 <- map(data.frame(t(Assignments)), ~ split(colnames(Assignments), f = unlist(.x)))
replicate_pairs_15 <- vector(mode = "list", length = nrow(Assignments))
for (i in 1:length(replicate_pairs_15)) {
  replicate_pairs_15[[i]][[1]] <- SSND_data_acq[, pairs_15[[i]][[1]]]
  replicate_pairs_15[[i]][[2]] <- SSND_data_acq[, pairs_15[[i]][[2]]]
}
