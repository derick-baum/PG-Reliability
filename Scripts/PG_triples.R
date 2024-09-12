# Run prepare_data.R first
# This script uses the SSND_strata_assignment_FULL data created in the prepare_data.R file

# Load packages and extract occupation names ---------------------------------------------

library(haven)
library(haven)
library(readxl)
library(tools)
library(stringr)

SSND_occup_names <- SSND_strata_assignment_FULL$occname

# Split occupations into triples based on strata allocation data ---------------------------------------------

SSND_occup_pairs_ISEI_Class_3 <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_3)

Assignments <- data.frame(matrix(
  data = c(rep(0, 3 ^ 9)),
  nrow = 3 ^ 9,
  ncol = 30
))
colnames(Assignments) <- unname(unlist(SSND_occup_pairs_ISEI_Class_3))
Assignments[, SSND_occup_pairs_ISEI_Class_3[[1]][1]] <- 1
Assignments[, SSND_occup_pairs_ISEI_Class_3[[1]][2]] <- 3 - Assignments[, SSND_occup_pairs_ISEI_Class_3[[1]][1]]
Assignments[, SSND_occup_pairs_ISEI_Class_3[[1]][3]] <- 4 - Assignments[, SSND_occup_pairs_ISEI_Class_3[[1]][1]]

for (j in 2:10) {
  Assignments[, SSND_occup_pairs_ISEI_Class_3[[j]][1]] <- c(rep(c(1, 2, 3), each = 3 ^
                                                                  (9 - j + 1)))
  Assignments[, SSND_occup_pairs_ISEI_Class_3[[j]][2]] <- c(rep(c(2, 3, 1), each = 3 ^
                                                                  (9 - j + 1)))
  Assignments[, SSND_occup_pairs_ISEI_Class_3[[j]][3]] <- c(rep(c(3, 1, 2), each = 3 ^
                                                                  (9 - j + 1)))
}

triples <- map(data.frame(t(Assignments)), ~ split(colnames(Assignments), f = unlist(.x)))
triples_unlist <- map(data.frame(t(Assignments)), ~ unname(unlist(split(
  colnames(Assignments), f = unlist(.x)
))))

# Checks ---------------------------------------------

## Check there are no repeated profiles ---------------------------------------------

triples_unlist_together <- as.vector(unlist(map(triples, ~ as.vector(unlist(
  map(.x, ~ paste0(.x, collapse = "-"))
)))))
length(unique(triples_unlist_together)) == length(triples) * 3

## Check if triples don't contain repeated occupations ---------------------------------------------

check_matches_fun <- function(triple) {
  (all(is.na(match(triple[[1]], triple[[2]]))) &
     all(is.na(match(triple[[1]], triple[[3]]))) &
     all(is.na(match(triple[[2]], triple[[3]]))))
}
all(map_lgl(triples, check_matches_fun) == TRUE)

## Check if each member in the triple includes an occupation from each stratum ---------------------------------------------

sum(map_lgl(map(triples, ~ unlist(map(
  .x, ~ map2_lgl(.x, SSND_occup_pairs_ISEI_Class_3, ~ .x %in% .y)
))), ~ all(.x == TRUE))) == length(triples)


replicate_triples <- vector(mode = "list", length = nrow(Assignments))
for (i in 1:length(replicate_triples)) {
  replicate_triples[[i]][[1]] <- SSND_data_acq[, triples[[i]][[1]]]
  replicate_triples[[i]][[2]] <- SSND_data_acq[, triples[[i]][[2]]]
  replicate_triples[[i]][[3]] <- SSND_data_acq[, triples[[i]][[3]]]
}
