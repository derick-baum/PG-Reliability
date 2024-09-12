# Load packages and create labels for different measures ---------------------------------------------

library(tidyverse)
library(readxl)
library(CTT)

measure_labels <- c(
  "# known",
  "Max SEI (drop)",
  "Max SEI (impute)",
  "Max Prestige (drop)",
  "Max Prestige (impute)",
  "Range SEI (drop)",
  "Range SEI (impute)",
  "Range Prestige (drop)",
  "Range Prestige (impute)",
  "Avg SEI (drop)",
  "Avg SEI (impute)",
  "Avg Prestige (drop)",
  "Avg Prestige (impute)",
  "PCA SEI (drop)",
  "PCA SEI (impute)",
  "PCA Prestige (drop)",
  "PCA Prestige (impute)",
  "# Service",
  "# Nonservice",
  "# Higher Service",
  "# Lower Service",
  "# Intermediate",
  "# Manual",
  "Class diversity",
  "PCA Class"
)

# Subset to class and service count measures ---------------------------------------------

ICC_measures_pairs_15_classgroups <- ICC_measures_pairs_15[, 1:4]
ICC_measures_pairs_15_servicegroups <- ICC_measures_pairs_15[, 8:9]


# Project reliability values using Spearman-Brown Prophecy formula ---------------------------------------------

## Projecting from 15-occupation ICCs to 16:30 ---------------------------------------------
ICC_measures_pairs15_medians <- map_dbl(ICC_measures_pairs_15, median)
ICC_spearmanbrown_15_16_30_df <- map_df(16:30,
                                        ~ spearman.brown(ICC_measures_pairs15_medians, input = .x / 15)$r.new) %>% dplyr::select(
                                          -c(
                                            class_diversity_std,
                                            min_prestige_ISEI_NA,
                                            min_prestige_USPrest_NA,
                                            min_prestige_ISEI_RI,
                                            min_prestige_USPrest_RI
                                          )
                                        )
ICC_spearmanbrown_15_16_30_df_t <- data.frame(t(ICC_spearmanbrown_15_16_30_df)) %>%
  rownames_to_column()
colnames(ICC_spearmanbrown_15_16_30_df_t) <- c("Measure", paste0("15_", 16:30))
ICC_spearmanbrown_15_16_30_df_t$Measure <- factor(
  ICC_spearmanbrown_15_16_30_df_t$Measure,
  levels = c(
    "total_occupation",
    "max_prestige_ISEI_NA",
    "max_prestige_ISEI_RI",
    "max_prestige_USPrest_NA",
    "max_prestige_USPrest_RI",
    "range_prestige_ISEI_NA",
    "range_prestige_ISEI_RI",
    "range_prestige_USPrest_NA",
    "range_prestige_USPrest_RI",
    "average_prestige_ISEI_NA",
    "average_prestige_ISEI_RI",
    "average_prestige_USPrest_NA",
    "average_prestige_USPrest_RI",
    "PCA_scores_NA_ISEI",
    "PCA_scores_RI_ISEI",
    "PCA_scores_NA_USPrest",
    "PCA_scores_RI_USPrest",
    "sum_Service",
    "sum_NonService",
    "sum_Higher",
    "sum_Lower",
    "sum_Intermediate",
    "sum_Manual",
    "class_diversity_raw",
    "PCA_scores_Class"
  ),
  labels = measure_labels
)

## Projecting from class and service counts ---------------------------------------------

ICC_measures_pairs15_classgroups_medians <- map_dbl(ICC_measures_pairs15[, 1:4], median)
ICC_measures_pairs15_servicegroups_medians <- map_dbl(ICC_measures_pairs15[, 8:9], median)
ICC_spearmanbrown_class <- list()
baselinelenghts_class <- c(3.5, 2.5, 5, 4)
projectionranges_class <- c(4, 6, 8, 10)
for (i in 1:length(ICC_measures_pairs15_classgroups_medians)) {
  ICC_spearmanbrown_class[[i]] = map_df(
    projectionranges_class,
    ~ spearman.brown(
      ICC_measures_pairs15_classgroups_medians[i],
      input = .x / baselinelenghts_class[i]
    )$r.new
  )
}
ICC_spearmanbrown_class_df <- data.frame(Reduce(rbind, unlist(ICC_spearmanbrown_class, recursive = FALSE))) %>%
  rownames_to_column() %>%
  `colnames<-`(c("Measure", "Median_4", "Median_6", "Median_8", "Median_10")) %>%
  mutate(
    Score = "",
    Measure = c(
      "\\# Higher Service",
      "\\# Lower Service",
      "\\# Intermediate",
      "\\# Manual"
    )
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  relocate(Score, .after = Measure)

ICC_spearmanbrown_service <- list()
baselinelenghts_service <- c(6, 9)
projectionranges_service <- c(8, 10, 12, 14)
for (i in 1:length(ICC_measures_pairs15_servicegroups_medians)) {
  ICC_spearmanbrown_service[[i]] = map_df(
    projectionranges_service,
    ~ spearman.brown(
      ICC_measures_pairs15_servicegroups_medians[i],
      input = .x / baselinelenghts_service[i]
    )$r.new
  )
}
ICC_spearmanbrown_service_df <- data.frame(Reduce(rbind, unlist(ICC_spearmanbrown_service, recursive = FALSE))) %>%
  rownames_to_column() %>%
  `colnames<-`(c("Measure", "Median_8", "Median_10", "Median_12", "Median_14")) %>%
  mutate(Score = "",
         Measure = c("\\# Service", "\\# Nonservice")) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  relocate(Score, .after = Measure)
