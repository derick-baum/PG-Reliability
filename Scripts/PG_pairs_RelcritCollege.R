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

SSND_occup_pairs_USPrest_Class_2 <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$USPrest_ClassStrat_2)

strata_list <- list(SSND_occup_pairs_ISEI_Class_2,
                    SSND_occup_pairs_USPrest_Class_2)

# Function for creating replicate pairs ---------------------------------------------

create_replicate_groups <- function(df, strata) {
  if (all(lengths(strata) == 2)) {
    SSND_occup_combs <- expand.grid(strata) %>%
      mutate(across(everything(), ~ as.character(.x)))
    indices <- 1:nrow(SSND_occup_combs)
    pairs_indices <-
      vector(mode = "list", length = nrow(SSND_occup_combs) / 2)
    for (i in 1:length(pairs_indices)) {
      pairs_indices[[i]] <- c(indices[i], (length(indices) + 1) - indices[i])
    }
    pairs <- vector(mode = "list", length = length(pairs_indices))
    for (i in 1:length(pairs)) {
      pairs[[i]] <- rbind(SSND_occup_combs[pairs_indices[[i]][1], ], SSND_occup_combs[pairs_indices[[i]][2], ])
    }
    check_overlap <- map_lgl(pairs, ~ all(is.na(match(
      unlist(unname(.x[1, ])), unlist(unname(.x[2, ]))
    ))))
    SSND_dat_list <- vector(mode = "list", length = length(pairs))
    for (i in 1:length(pairs)) {
      SSND_dat_list[[i]][[1]] <- df[, as.character(unlist(pairs[[i]][1, ]))]
      SSND_dat_list[[i]][[2]] <- df[, as.character(unlist(pairs[[i]][2, ]))]
    }
    SSND_dat_list
  }
}

# Create replicate pairs for each stratification scheme ---------------------------------------------

replicate_pairs <- map(strata_list, ~ create_replicate_groups(SSND_data_acq, .x))
names(replicate_pairs) <- c("ISEI_Class", "USPrest_Class")

# Create replicate pairs using different relationship criteria ---------------------------------------------

dat_list <- list(SSND_data_frd, SSND_data_rel)
replicate_pairs_relationship_criteria <- map(dat_list,
                                             ~ create_replicate_groups(.x, SSND_occup_pairs_ISEI_Class_2))
names(replicate_pairs_relationship_criteria) <- c("Friend", "Relative")

# Create replicate pairs for the subset of college-educated respondents ---------------------------------------------

replicate_pairs_college <- create_replicate_groups(SSND_data_college, SSND_occup_pairs_ISEI_Class_2)
