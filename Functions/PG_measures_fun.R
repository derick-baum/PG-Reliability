# Load tidyverse ---------------------------------------------

library(tidyverse)

# Function code ---------------------------------------------

append_columns_PG <- function(df) {
  start_time <- Sys.time()
  t_df <- data.frame(t(df))
  df_occups <- names(df)
  ncol_initial <- ncol(df)
  classes_merged <- vector(mode = "list", length = 4)
  classes_Service <- vector(mode = "list", length = 2)
  names(classes_merged) <- c("Higher", "Lower", "Intermediate", "Manual")
  names(classes_Service) <- c("Service", "NonService")
  classes_merged$Higher <- classes$I
  classes_merged$Lower <- classes$II
  classes_merged$Intermediate <- c(classes$IIIa, classes$IIIb, classes$IVc, classes$V)
  classes_merged$Manual <- c(classes$VI, classes$VIIa)
  classes_Service$Service <- c(classes_merged$Higher, classes_merged$Lower)
  classes_Service$NonService <- c(classes_merged$Intermediate, classes_merged$Manual)
  classes_merged <- map(classes_merged, ~ .x[.x %in% df_occups])
  classes_merged <- classes_merged[lengths(classes_merged) != 0]
  classes_Service <- map(classes_Service, ~ .x[.x %in% df_occups])
  classes_Service <- classes_Service[lengths(classes_Service) != 0]
  ISEI_scores <- SSND_ISEI_scores[names(df)]
  USPrest_scores <- SSND_USPrest_scores[names(df)]
  all_0s <- map_lgl(t_df, ~ all(.x == 0))
  class_names_merged <- names(classes_merged)
  class_names_Service <- names(classes_Service)
  class_vars_merged <- paste0("sum_", class_names_merged)
  class_vars_Service <- paste0("sum_", class_names_Service)
  df[, class_vars_merged] <- NA
  df[, class_vars_Service] <- NA
  total_occupation <- map_dbl(t_df, ~ sum(.x))
  average_prestige_ISEI <-
    map_dbl(t_df, ~ mean((ISEI_scores * .x)[(ISEI_scores * .x != 0)]))
  average_prestige_USPrest <-
    map_dbl(t_df, ~ mean((USPrest_scores * .x)[(USPrest_scores * .x != 0)]))
  range_prestige_ISEI <-
    map_dbl(t_df, ~ diff(range((ISEI_scores * .x)[(ISEI_scores * .x != 0)])))
  range_prestige_USPrest <-
    map_dbl(t_df, ~ diff(range((
      USPrest_scores * .x
    )[(USPrest_scores * .x != 0)])))
  min_prestige_ISEI <- map_dbl(t_df, ~ min((ISEI_scores * .x)[(ISEI_scores * .x != 0)]))
  min_prestige_USPrest <- map_dbl(t_df, ~ min((USPrest_scores * .x)[(USPrest_scores * .x != 0)]))
  max_prestige_ISEI <- map_dbl(t_df, ~ max((ISEI_scores * .x)[(ISEI_scores * .x != 0)]))
  max_prestige_USPrest <- map_dbl(t_df, ~ max((USPrest_scores * .x)[(USPrest_scores * .x != 0)]))
  for (i in 1:length(class_vars_merged)) {
    df <- df %>%
      mutate(!!sym(class_vars_merged[i]) := rowSums(pick(classes_merged[[i]])))
  }
  for (i in 1:length(class_vars_Service)) {
    df <- df %>%
      mutate(!!sym(class_vars_Service[i]) := rowSums(pick(classes_Service[[i]])))
  }
  id <- 1:nrow(df)
  df <- data.frame(id = id, df) %>%
    dplyr::select(id, sum_Higher:sum_NonService)
  ISEI_USPrest_measures_NA <-
    data.frame(
      id = id,
      total_occupation = total_occupation,
      average_prestige_ISEI_NA = average_prestige_ISEI,
      average_prestige_USPrest_NA = average_prestige_USPrest,
      range_prestige_ISEI_NA = range_prestige_ISEI,
      range_prestige_USPrest_NA = range_prestige_USPrest,
      min_prestige_ISEI_NA = min_prestige_ISEI,
      min_prestige_USPrest_NA = min_prestige_USPrest,
      max_prestige_ISEI_NA = max_prestige_ISEI,
      max_prestige_USPrest_NA = max_prestige_USPrest
    )
  ISEI_USPrest_measures_RI <-
    data.frame(
      id = id,
      total_occupation = total_occupation,
      average_prestige_ISEI_RI = average_prestige_ISEI,
      average_prestige_USPrest_RI = average_prestige_USPrest,
      range_prestige_ISEI_RI = range_prestige_ISEI,
      range_prestige_USPrest_RI = range_prestige_USPrest,
      min_prestige_ISEI_RI = min_prestige_ISEI,
      min_prestige_USPrest_RI = min_prestige_USPrest,
      max_prestige_ISEI_RI = max_prestige_ISEI,
      max_prestige_USPrest_RI = max_prestige_USPrest
    )
  ISEI_USPrest_measures_NA[all_0s, c(
    "average_prestige_ISEI_NA",
    "average_prestige_USPrest_NA",
    "range_prestige_ISEI_NA",
    "range_prestige_USPrest_NA",
    "min_prestige_ISEI_NA",
    "min_prestige_USPrest_NA",
    "max_prestige_ISEI_NA",
    "max_prestige_USPrest_NA"
  )] <- NA
  ISEI_USPrest_measures_RI[all_0s, c(
    "average_prestige_ISEI_RI",
    "average_prestige_USPrest_RI",
    "range_prestige_ISEI_RI",
    "range_prestige_USPrest_RI",
    "min_prestige_ISEI_RI",
    "min_prestige_USPrest_RI",
    "max_prestige_ISEI_RI",
    "max_prestige_USPrest_RI"
  )] <- NA
  reg_avg_total_ISEI <- lm(average_prestige_ISEI_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  reg_avg_total_USPrest <- lm(average_prestige_USPrest_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  reg_min_total_ISEI <- lm(min_prestige_ISEI_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  reg_min_total_USPrest <- lm(min_prestige_USPrest_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  reg_max_total_ISEI <- lm(max_prestige_ISEI_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  reg_max_total_USPrest <- lm(max_prestige_USPrest_RI ~ total_occupation, data = ISEI_USPrest_measures_RI)
  intercept_avg_total_ISEI <- reg_avg_total_ISEI$coefficients["(Intercept)"]
  intercept_avg_total_USPrest <- reg_avg_total_USPrest$coefficients["(Intercept)"]
  intercept_min_total_ISEI <- reg_min_total_ISEI$coefficients["(Intercept)"]
  intercept_min_total_USPrest <- reg_min_total_USPrest$coefficients["(Intercept)"]
  intercept_max_total_ISEI <- reg_max_total_ISEI$coefficients["(Intercept)"]
  intercept_max_total_USPrest <- reg_max_total_USPrest$coefficients["(Intercept)"]
  if (intercept_min_total_ISEI < min(SSND_ISEI_scores)) {
    intercept_min_total_ISEI <- min(SSND_ISEI_scores)
  }
  if (intercept_min_total_USPrest < min(SSND_USPrest_scores)) {
    intercept_min_total_USPrest <- min(SSND_USPrest_scores)
  }
  if (intercept_max_total_ISEI > max(SSND_ISEI_scores)) {
    intercept_max_total_ISEI <- max(SSND_ISEI_scores)
  }
  if (intercept_max_total_USPrest > max(SSND_USPrest_scores)) {
    intercept_max_total_USPrest <- max(SSND_USPrest_scores)
  }
  ISEI_USPrest_measures_RI[all_0s, "average_prestige_ISEI_RI"] <- intercept_avg_total_ISEI
  ISEI_USPrest_measures_RI[all_0s, "average_prestige_USPrest_RI"] <- intercept_avg_total_USPrest
  ISEI_USPrest_measures_RI[all_0s, "min_prestige_ISEI_RI"] <- intercept_min_total_ISEI
  ISEI_USPrest_measures_RI[all_0s, "min_prestige_USPrest_RI"] <- intercept_min_total_USPrest
  ISEI_USPrest_measures_RI[all_0s, "max_prestige_ISEI_RI"] <- intercept_max_total_ISEI
  ISEI_USPrest_measures_RI[all_0s, "max_prestige_USPrest_RI"] <- intercept_max_total_USPrest
  ISEI_USPrest_measures_RI[all_0s, "range_prestige_ISEI_RI"] <- intercept_max_total_ISEI - intercept_min_total_ISEI
  ISEI_USPrest_measures_RI[all_0s, "range_prestige_USPrest_RI"] <- intercept_max_total_USPrest - intercept_min_total_USPrest
  ISEI_USPrest_measures_NA_complete <- na.omit(ISEI_USPrest_measures_NA)
  ISEI_USPrest_measures_NA_complete$PCA_scores_NA_ISEI <-
    princomp(ISEI_USPrest_measures_NA_complete[, c("total_occupation",
                                                   "range_prestige_ISEI_NA",
                                                   "max_prestige_ISEI_NA")],
             cor = TRUE,
             fix_sign = TRUE)$scores[, 1]
  ISEI_USPrest_measures_NA_complete$PCA_scores_NA_USPrest <-
    princomp(ISEI_USPrest_measures_NA_complete[, c("total_occupation",
                                                   "range_prestige_USPrest_NA",
                                                   "max_prestige_USPrest_NA")],
             cor = TRUE,
             fix_sign = TRUE)$scores[, 1]
  ISEI_USPrest_measures_RI$PCA_scores_RI_ISEI <-
    princomp(ISEI_USPrest_measures_RI[, c("total_occupation",
                                          "range_prestige_ISEI_RI",
                                          "max_prestige_ISEI_RI")],
             cor = TRUE,
             fix_sign = TRUE)$scores[, 1]
  ISEI_USPrest_measures_RI$PCA_scores_RI_USPrest <-
    princomp(ISEI_USPrest_measures_RI[, c("total_occupation",
                                          "range_prestige_USPrest_RI",
                                          "max_prestige_USPrest_RI")],
             cor = TRUE,
             fix_sign = TRUE)$scores[, 1]
  ISEI_USPrest_measures_NA_complete <- ISEI_USPrest_measures_NA_complete %>%
    dplyr::select(id, PCA_scores_NA_ISEI, PCA_scores_NA_USPrest)
  ISEI_USPrest_measures_NA <- left_join(ISEI_USPrest_measures_NA,
                                        ISEI_USPrest_measures_NA_complete,
                                        by = "id")
  ISEI_USPrest_measures_RI <- ISEI_USPrest_measures_RI %>%
    dplyr::select(-total_occupation)
  df <- left_join(df, ISEI_USPrest_measures_NA, by = "id")
  df <- left_join(df, ISEI_USPrest_measures_RI, by = "id")
  class_props_merged <- paste0("Prop_", class_names_merged)
  df_class_diversity <- df
  for (i in 1:length(class_props_merged)) {
    df_class_diversity <- df_class_diversity %>%
      mutate(!!sym(class_props_merged[i]) := (!!sym(class_vars_merged[i]) /
                                                total_occupation) ^ 2)
  }
  df_class_diversity <- df_class_diversity %>%
    mutate(
      class_diversity_raw = 1 - rowSums(pick(class_props_merged)),
      class_diversity_std = class_diversity_raw / (1 - 1 / 4)
    ) %>%
    dplyr::select(id,
                  class_diversity_raw,
                  class_diversity_std,
                  total_occupation,
                  sum_Higher)
  df_class_diversity[all_0s, c("class_diversity_raw", "class_diversity_std")] <- NA
  df_class_diversity_complete <- na.omit(df_class_diversity)
  df_class_diversity_complete$PCA_scores_Class <-
    princomp(df_class_diversity_complete[, c("total_occupation", "sum_Higher", "class_diversity_raw")],
             cor = TRUE,
             fix_sign = TRUE)$scores[, 1]
  df_class_diversity_complete <- df_class_diversity_complete %>%
    dplyr::select(id,
                  class_diversity_raw,
                  class_diversity_std,
                  PCA_scores_Class)
  df <- left_join(df, df_class_diversity_complete, by = "id") %>%
    relocate(class_diversity_raw,
             class_diversity_std,
             PCA_scores_Class,
             .after = sum_Manual)
  end_time <- Sys.time()
  list(
    occups_present = df_occups,
    df = df,
    simulation_time = end_time - start_time
  )
}
