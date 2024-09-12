# Run PG_analyses.R first

# Load packages and create labels for different measures ---------------------------------------------

library(tidyverse)
library(readxl)

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

# Organize findings ---------------------------------------------

# 15 pairs
summary_measures_pairs15 <- map(ICC_measures_pairs_15,
                                ~ map_dfc(.x, ~ format(round(summary(
                                  .x
                                ), 3), format = "f")) %>% dplyr::select(
                                  -c(
                                    class_diversity_std,
                                    min_prestige_ISEI_NA,
                                    min_prestige_USPrest_NA,
                                    min_prestige_ISEI_RI,
                                    min_prestige_USPrest_RI
                                  )
                                ))
summary_measures_equate_pairs15 <- map_dfc(ICC_measures_EqualScores, ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
  -c(
    class_diversity_std,
    min_prestige_ISEI_NA,
    min_prestige_USPrest_NA,
    min_prestige_ISEI_RI,
    min_prestige_USPrest_RI
  )
)

ISEI_Class_pairs15_results <- data.frame(t(summary_measures_pairs15$ISEI_Class)) %>%
  rownames_to_column()
ISEI_equate_pairs15_results <- data.frame(t(summary_measures_equate_pairs15)) %>%
  rownames_to_column()

# 14 pairs
ICC_measures_pairs_Mixed14_MiddleTopBottom <- rbind(
  ICC_measures_pairs_Mixed14$Top,
  ICC_measures_pairs_Mixed14$Middle,
  ICC_measures_pairs_Mixed14_Bottom
)
ICC_measures_pairs_Mixed14_All <- rbind(
  ICC_measures_pairs_Mixed14$Top,
  ICC_measures_pairs_Mixed14$Middle,
  ICC_measures_pairs_Mixed14_Bottom,
  ICC_measures_pairs_Mixed14_TopMiddle,
  ICC_measures_pairs_Mixed14_BottomMiddle
)

summary_measures_Mixed14 <- map(ICC_measures_pairs_Mixed14,
                                ~ map_dfc(.x, ~ format(round(summary(
                                  .x
                                ), 3), format = "f")) %>% dplyr::select(
                                  -c(
                                    class_diversity_std,
                                    min_prestige_ISEI_NA,
                                    min_prestige_USPrest_NA,
                                    min_prestige_ISEI_RI,
                                    min_prestige_USPrest_RI
                                  )
                                ))
summary_measures_Mixed14_bottom <- map_dfc(ICC_measures_pairs_Mixed14_Bottom,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed14_TopMiddle <- map_dfc(ICC_measures_pairs_Mixed14_TopMiddle,
                                              ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                                -c(
                                                  class_diversity_std,
                                                  min_prestige_ISEI_NA,
                                                  min_prestige_USPrest_NA,
                                                  min_prestige_ISEI_RI,
                                                  min_prestige_USPrest_RI
                                                )
                                              )
summary_measures_Mixed14_BottomMiddle <- map_dfc(ICC_measures_pairs_Mixed14_BottomMiddle,
                                                 ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                                   -c(
                                                     class_diversity_std,
                                                     min_prestige_ISEI_NA,
                                                     min_prestige_USPrest_NA,
                                                     min_prestige_ISEI_RI,
                                                     min_prestige_USPrest_RI
                                                   )
                                                 )
summary_measures_Mixed14_MiddleTopBottom <- map_dfc(ICC_measures_pairs_Mixed14_MiddleTopBottom,
                                                    ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                                      -c(
                                                        class_diversity_std,
                                                        min_prestige_ISEI_NA,
                                                        min_prestige_USPrest_NA,
                                                        min_prestige_ISEI_RI,
                                                        min_prestige_USPrest_RI
                                                      )
                                                    )
summary_measures_Mixed14_All <- map_dfc(ICC_measures_pairs_Mixed14_All, ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
  -c(
    class_diversity_std,
    min_prestige_ISEI_NA,
    min_prestige_USPrest_NA,
    min_prestige_ISEI_RI,
    min_prestige_USPrest_RI
  )
)

ISEI_Class_Mixed14Top <- data.frame(t(summary_measures_Mixed14$Top)) %>%
  rownames_to_column()
ISEI_Class_Mixed14Middle <- data.frame(t(summary_measures_Mixed14$Middle)) %>%
  rownames_to_column()
ISEI_Class_Mixed14Bottom <- data.frame(t(summary_measures_Mixed14_bottom)) %>%
  rownames_to_column()
ISEI_Class_Mixed14TopMiddle <- data.frame(t(summary_measures_Mixed14_TopMiddle)) %>%
  rownames_to_column()
ISEI_Class_Mixed14BottomMiddle <- data.frame(t(summary_measures_Mixed14_BottomMiddle)) %>%
  rownames_to_column()
ISEI_Class_Mixed14MiddleTopBottom <- data.frame(t(summary_measures_Mixed14_MiddleTopBottom)) %>%
  rownames_to_column()
ISEI_Class_Mixed14All <- data.frame(t(summary_measures_Mixed14_All)) %>%
  rownames_to_column()

# 13 pairs
ICC_measures_pairs_Mixed13_All <- rbind(
  ICC_measures_pairs_Mixed13_Extreme,
  ICC_measures_pairs_Mixed13_First,
  ICC_measures_pairs_Mixed13_Second,
  ICC_measures_pairs_Mixed13_Third,
  ICC_measures_pairs_Mixed13_Fourth,
  ICC_measures_pairs_Mixed13_Fifth,
  ICC_measures_pairs_Mixed13_Sixth,
  ICC_measures_pairs_Mixed13_Seventh,
  ICC_measures_pairs_Mixed13_Eighth,
  ICC_measures_pairs_Mixed13_Ninth
)

summary_measures_Mixed13_Extreme <- map_dfc(ICC_measures_pairs_Mixed13_Extreme,
                                            ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                              -c(
                                                class_diversity_std,
                                                min_prestige_ISEI_NA,
                                                min_prestige_USPrest_NA,
                                                min_prestige_ISEI_RI,
                                                min_prestige_USPrest_RI
                                              )
                                            )
summary_measures_Mixed13_First <- map_dfc(ICC_measures_pairs_Mixed13_First,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed13_Second <- map_dfc(ICC_measures_pairs_Mixed13_Second,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed13_Third <- map_dfc(ICC_measures_pairs_Mixed13_Third,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed13_Fourth <- map_dfc(ICC_measures_pairs_Mixed13_Fourth,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed13_Fifth <- map_dfc(ICC_measures_pairs_Mixed13_Fifth,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed13_Sixth <- map_dfc(ICC_measures_pairs_Mixed13_Sixth,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed13_Seventh <- map_dfc(ICC_measures_pairs_Mixed13_Seventh,
                                            ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                              -c(
                                                class_diversity_std,
                                                min_prestige_ISEI_NA,
                                                min_prestige_USPrest_NA,
                                                min_prestige_ISEI_RI,
                                                min_prestige_USPrest_RI
                                              )
                                            )
summary_measures_Mixed13_Eighth <- map_dfc(ICC_measures_pairs_Mixed13_Eighth,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed13_Ninth <- map_dfc(ICC_measures_pairs_Mixed13_Ninth,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed13_All <- map_dfc(ICC_measures_pairs_Mixed13_All, ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
  -c(
    class_diversity_std,
    min_prestige_ISEI_NA,
    min_prestige_USPrest_NA,
    min_prestige_ISEI_RI,
    min_prestige_USPrest_RI
  )
)

ISEI_Class_Mixed13Extreme <- data.frame(t(summary_measures_Mixed13_Extreme)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Middle <- data.frame(t(summary_measures_Mixed13_Middle)) %>%
  rownames_to_column()
ISEI_Class_Mixed13First <- data.frame(t(summary_measures_Mixed13_First)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Second <- data.frame(t(summary_measures_Mixed13_Second)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Third <- data.frame(t(summary_measures_Mixed13_Third)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Fourth <- data.frame(t(summary_measures_Mixed13_Fourth)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Fifth <- data.frame(t(summary_measures_Mixed13_Fifth)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Sixth <- data.frame(t(summary_measures_Mixed13_Sixth)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Seventh <- data.frame(t(summary_measures_Mixed13_Seventh)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Eighth <- data.frame(t(summary_measures_Mixed13_Eighth)) %>%
  rownames_to_column()
ISEI_Class_Mixed13Ninth <- data.frame(t(summary_measures_Mixed13_Ninth)) %>%
  rownames_to_column()
ISEI_Class_Mixed13All <- data.frame(t(summary_measures_Mixed13_All)) %>%
  rownames_to_column()

# 12 pairs
ICC_measures_pairs_Mixed12_All <- rbind(
  ICC_measures_pairs_Mixed12_ExtremeMiddle,
  ICC_measures_pairs_Mixed12_Middle,
  ICC_measures_pairs_Mixed12_First,
  ICC_measures_pairs_Mixed12_Second,
  ICC_measures_pairs_Mixed12_Third,
  ICC_measures_pairs_Mixed12_Fourth,
  ICC_measures_pairs_Mixed12_Fifth,
  ICC_measures_pairs_Mixed12_Sixth,
  ICC_measures_pairs_Mixed12_Seventh,
  ICC_measures_pairs_Mixed12_Eighth
)

summary_measures_Mixed12_ExtremeMiddle <- map_dfc(ICC_measures_pairs_Mixed12_ExtremeMiddle,
                                                  ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                                    -c(
                                                      class_diversity_std,
                                                      min_prestige_ISEI_NA,
                                                      min_prestige_USPrest_NA,
                                                      min_prestige_ISEI_RI,
                                                      min_prestige_USPrest_RI
                                                    )
                                                  )
summary_measures_Mixed12_Middle <- map_dfc(ICC_measures_pairs_Mixed12_Middle,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed12_First <- map_dfc(ICC_measures_pairs_Mixed12_First,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed12_Second <- map_dfc(ICC_measures_pairs_Mixed12_Second,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed12_Third <- map_dfc(ICC_measures_pairs_Mixed12_Third,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed12_Fourth <- map_dfc(ICC_measures_pairs_Mixed12_Fourth,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed12_Fifth <- map_dfc(ICC_measures_pairs_Mixed12_Fifth,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed12_Sixth <- map_dfc(ICC_measures_pairs_Mixed12_Sixth,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed12_Seventh <- map_dfc(ICC_measures_pairs_Mixed12_Seventh,
                                            ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                              -c(
                                                class_diversity_std,
                                                min_prestige_ISEI_NA,
                                                min_prestige_USPrest_NA,
                                                min_prestige_ISEI_RI,
                                                min_prestige_USPrest_RI
                                              )
                                            )
summary_measures_Mixed12_Eighth <- map_dfc(ICC_measures_pairs_Mixed12_Eighth,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed12_All <- map_dfc(ICC_measures_pairs_Mixed12_All, ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
  -c(
    class_diversity_std,
    min_prestige_ISEI_NA,
    min_prestige_USPrest_NA,
    min_prestige_ISEI_RI,
    min_prestige_USPrest_RI
  )
)

ISEI_Class_Mixed12ExtremeMiddle <- data.frame(t(summary_measures_Mixed12_ExtremeMiddle)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Middle <- data.frame(t(summary_measures_Mixed12_Middle)) %>%
  rownames_to_column()
ISEI_Class_Mixed12First <- data.frame(t(summary_measures_Mixed12_First)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Second <- data.frame(t(summary_measures_Mixed12_Second)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Third <- data.frame(t(summary_measures_Mixed12_Third)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Fourth <- data.frame(t(summary_measures_Mixed12_Fourth)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Fifth <- data.frame(t(summary_measures_Mixed12_Fifth)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Sixth <- data.frame(t(summary_measures_Mixed12_Sixth)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Seventh <- data.frame(t(summary_measures_Mixed12_Seventh)) %>%
  rownames_to_column()
ISEI_Class_Mixed12Eighth <- data.frame(t(summary_measures_Mixed12_Eighth)) %>%
  rownames_to_column()
ISEI_Class_Mixed12All <- data.frame(t(summary_measures_Mixed12_All)) %>%
  rownames_to_column()

# 11 pairs
ICC_measures_pairs_Mixed11_All <- rbind(
  ICC_measures_pairs_Mixed11_Extreme,
  ICC_measures_pairs_Mixed11_First,
  ICC_measures_pairs_Mixed11_Second,
  ICC_measures_pairs_Mixed11_Third,
  ICC_measures_pairs_Mixed11_Fourth
)

summary_measures_Mixed11_Extreme <- map_dfc(ICC_measures_pairs_Mixed11_Extreme,
                                            ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                              -c(
                                                class_diversity_std,
                                                min_prestige_ISEI_NA,
                                                min_prestige_USPrest_NA,
                                                min_prestige_ISEI_RI,
                                                min_prestige_USPrest_RI
                                              )
                                            )
summary_measures_Mixed11_Middle <- map_dfc(ICC_measures_pairs_Mixed11_Extreme,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed11_First <- map_dfc(ICC_measures_pairs_Mixed11_First,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed11_Second <- map_dfc(ICC_measures_pairs_Mixed11_Second,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed11_Third <- map_dfc(ICC_measures_pairs_Mixed11_Third,
                                          ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                            -c(
                                              class_diversity_std,
                                              min_prestige_ISEI_NA,
                                              min_prestige_USPrest_NA,
                                              min_prestige_ISEI_RI,
                                              min_prestige_USPrest_RI
                                            )
                                          )
summary_measures_Mixed11_Fourth <- map_dfc(ICC_measures_pairs_Mixed11_Fourth,
                                           ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
                                             -c(
                                               class_diversity_std,
                                               min_prestige_ISEI_NA,
                                               min_prestige_USPrest_NA,
                                               min_prestige_ISEI_RI,
                                               min_prestige_USPrest_RI
                                             )
                                           )
summary_measures_Mixed11_All <- map_dfc(ICC_measures_pairs_Mixed11_All, ~ format(round(summary(.x), 3), format = "f")) %>% dplyr::select(
  -c(
    class_diversity_std,
    min_prestige_ISEI_NA,
    min_prestige_USPrest_NA,
    min_prestige_ISEI_RI,
    min_prestige_USPrest_RI
  )
)

ISEI_Class_Mixed11Extreme <- data.frame(t(summary_measures_Mixed11_Extreme)) %>%
  rownames_to_column()
ISEI_Class_Mixed11Middle <- data.frame(t(summary_measures_Mixed11_Middle)) %>%
  rownames_to_column()
ISEI_Class_Mixed11First <- data.frame(t(summary_measures_Mixed11_First)) %>%
  rownames_to_column()
ISEI_Class_Mixed11Second <- data.frame(t(summary_measures_Mixed11_Second)) %>%
  rownames_to_column()
ISEI_Class_Mixed11Third <- data.frame(t(summary_measures_Mixed11_Third)) %>%
  rownames_to_column()
ISEI_Class_Mixed11Fourth <- data.frame(t(summary_measures_Mixed11_Fourth)) %>%
  rownames_to_column()
ISEI_Class_Mixed11All <- data.frame(t(summary_measures_Mixed11_All)) %>%
  rownames_to_column()

# 10 triples
summary_measures_triples <- map(
  list(ICC_measures_triples, corAvg_measures_triples),
  ~ map_dfc(.x, ~ format(round(summary(
    .x
  ), 3), format = "f")) %>% dplyr::select(
    -c(
      class_diversity_std,
      min_prestige_ISEI_NA,
      min_prestige_USPrest_NA,
      min_prestige_ISEI_RI,
      min_prestige_USPrest_RI
    )
  )
)

ISEI_Class_tripleICC <- data.frame(t(summary_measures_triples[[1]])) %>%
  rownames_to_column()
ISEI_Class_tripleCor <- data.frame(t(summary_measures_triples[[2]])) %>%
  rownames_to_column()

# Create data frame with the information above ---------------------------------------------

results_full <- rbind(
  ISEI_Class_pairs15_results,
  ISEI_equate_pairs15_results,
  ISEI_Class_Mixed14Top,
  ISEI_Class_Mixed14Middle,
  ISEI_Class_Mixed14Bottom,
  ISEI_Class_Mixed14TopMiddle,
  ISEI_Class_Mixed14BottomMiddle,
  ISEI_Class_Mixed14MiddleTopBottom,
  ISEI_Class_Mixed14All,
  ISEI_Class_Mixed13Extreme,
  ISEI_Class_Mixed13Middle,
  ISEI_Class_Mixed13First,
  ISEI_Class_Mixed13Second,
  ISEI_Class_Mixed13Third,
  ISEI_Class_Mixed13Fourth,
  ISEI_Class_Mixed13Fifth,
  ISEI_Class_Mixed13Sixth,
  ISEI_Class_Mixed13Seventh,
  ISEI_Class_Mixed13Eighth,
  ISEI_Class_Mixed13Ninth,
  ISEI_Class_Mixed13All,
  ISEI_Class_Mixed12ExtremeMiddle,
  ISEI_Class_Mixed12Middle,
  ISEI_Class_Mixed12First,
  ISEI_Class_Mixed12Second,
  ISEI_Class_Mixed12Third,
  ISEI_Class_Mixed12Fourth,
  ISEI_Class_Mixed12Fifth,
  ISEI_Class_Mixed12Sixth,
  ISEI_Class_Mixed12Seventh,
  ISEI_Class_Mixed12Eighth,
  ISEI_Class_Mixed12All,
  ISEI_Class_Mixed11Extreme,
  ISEI_Class_Mixed11First,
  ISEI_Class_Mixed11Second,
  ISEI_Class_Mixed11Third,
  ISEI_Class_Mixed11Fourth,
  ISEI_Class_Mixed11Middle,
  ISEI_Class_Mixed11All,
  ISEI_Class_tripleICC,
  ISEI_Class_tripleCor
)
colnames(results_full) <- c("Measure", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
results_full$stratum_treatment <- c(rep(
  c(
    "Pairs",
    "Pairs, equal SEI/Prestige",
    "Top strata collapsed",
    "Middle strata collapsed",
    "Bottom strata collapsed",
    "Top-Middle strata collapsed",
    "Bottom-Middle strata collapsed",
    "Pooled (Top, Middle, and Bottom)",
    "Pooled (All five)",
    "Pairs 1-3 and 13-15 collapsed",
    "Pairs 5-7 and 9-11 collapsed (symmetric agglomeration)",
    "Pairs 1-6 collapsed",
    "Pairs 1-3 and 7-9 collapsed",
    "Pairs 1-3 and 10-12 collapsed",
    "Pairs 4-9 collapsed",
    "Pairs 4-6 and 10-12 collapsed",
    "Pairs 4-6 and 13-15 collapsed",
    "Pairs 7-12 collapsed",
    "Pairs 7-9 and 13-15 collapsed",
    "Pairs 10-15 collapsed",
    "Pooled (All except symmetric agglomeration)",
    "Pairs 1-3, 7-9, and 13-15",
    "Pairs 4-12",
    "Pairs 1-9 collapsed",
    "Pairs 1-6 and 10-12 collapsed",
    "Pairs 1-6 and 13-15 collapsed",
    "Pairs 1-3 and 7-12 collapsed",
    "Pairs 1-3 and 10-15 collapsed",
    "Pairs 4-9 and 13-15 collapsed",
    "Pairs 4-6 and 10-15 collapsed",
    "Pairs 7-15 collapsed",
    "Pooled",
    "All pairs except 7-9 collapsed",
    "All pairs except 1-3 collapsed",
    "All pairs except 4-6 collapsed",
    "All pairs except 10-12 collapsed",
    "All pairs except 13-15 collapsed",
    "All pairs except 1, 8, and 15 collapsed (symmetric agglomeration)",
    "Pooled (All except symmetric agglomeration)",
    "Triples-ICC",
    "Triples-Avg. Correlation"
  ),
  each = 25
))
results_full$Measure <- factor(
  results_full$Measure,
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
# Data frame with pooled results:
results_full_pooled <- results_full
results_full_pooled[201:225, "stratum_treatment"] <- "Pooled_14"
results_full_pooled[501:525, "stratum_treatment"] <- "Pooled_13"
results_full_pooled[776:800, "stratum_treatment"] <- "Pooled_12"
results_full_pooled[951:975, "stratum_treatment"] <- "Pooled_11"
results_full_pooled[976:1000, "stratum_treatment"] <- "Triples_ICC"
results_full_pooled <- results_full_pooled %>%
  filter(
    stratum_treatment %in% c(
      "Pooled_14",
      "Pooled_13",
      "Pooled_12",
      "Pooled_11",
      "Triples_ICC"
    )
  )

results_full_median <- results_full %>%
  select(Measure, Median, stratum_treatment) %>%
  group_by(Measure) %>%
  mutate(row = row_number()) %>%
  arrange(Measure)

results_full_median_wider <- results_full_median %>%
  pivot_wider(names_from = Measure, values_from = Median) %>%
  select(-row)

results_full_median_wider$`number_occs` <- c(15,
                                             "",
                                             14,
                                             rep("", 6),
                                             13,
                                             rep("", 11),
                                             12,
                                             rep("", 10),
                                             11,
                                             rep("", 6),
                                             10,
                                             "")

results_full_median_wider <- results_full_median_wider %>%
  relocate(number_occs, .before = stratum_treatment) %>%
  rename(`# Occs in PG` = number_occs, `Stratum Treatment` = stratum_treatment)
