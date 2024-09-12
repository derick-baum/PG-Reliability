# Run prepare_data.R first
# This script uses the SSND_strata_assignment_FULL data created in the prepare_data.R file

# Load packages, class assignment data and extract occupational information ---------------------------------------------

library(readxl)
library(tidyverse)
library(statnet)

class_scheme <- read_excel("EGP_assignment.xlsx")

SSND_ISEI_scores <- SSND_strata_assignment_FULL$ISEI
SSND_USPrest_scores <- SSND_strata_assignment_FULL$USPrest

names(SSND_ISEI_scores) <- names(SSND_USPrest_scores) <- SSND_strata_assignment_FULL$occname

SSND_occup_names <- SSND_strata_assignment_FULL$occname

# Split occupations into pairs based on strata allocation data ---------------------------------------------

SSND_occup_pairs_ISEI_Class_2 <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_2)

# Prepare data to construct agreement scores ---------------------------------------------

SSND_occup_pairs_ISEI_Class_2_scores <- map(SSND_occup_pairs_ISEI_Class_2, ~ SSND_ISEI_scores[.x])
SSND_occup_pairs_ISEI_Class_2_scores_diff <- abs(map_dbl(SSND_occup_pairs_ISEI_Class_2_scores, diff))

classes <- split(class_scheme$occname, class_scheme$`Class code`)
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

SSND_classes <- rep(NA, 30)
names(SSND_classes) <- SSND_occup_names
SSND_classes[classes_merged$Higher] <- 1
SSND_classes[classes_merged$Lower] <- 2
SSND_classes[classes_merged$Intermediate] <- 3
SSND_classes[classes_merged$Manual] <- 4

# Agreement scores by relationship criterion ---------------------------------------------

SSND_data_acq_split <- map(SSND_occup_pairs_ISEI_Class_2,
                           ~ SSND_data_acq %>% dplyr::select(all_of(.x)))
tables_SSND_data_acq <- map(SSND_data_acq_split, ~ table(unlist(.x[, 1]), unlist(.x[, 2])))
agreement_SSND_data_acq <- map_dbl(tables_SSND_data_acq, ~ (.x[2, 2]) /
                                     (sum(.x) - .x[1, 1]))

SSND_data_frd_split <- map(SSND_occup_pairs_ISEI_Class_2,
                           ~ SSND_data_PG_frd %>% dplyr::select(all_of(.x)))
tables_SSND_data_frd <- map(SSND_data_frd_split, ~ table(unlist(.x[, 1]), unlist(.x[, 2])))
agreement_SSND_data_frd <- map_dbl(tables_SSND_data_frd, ~ (.x[2, 2]) /
                                     (sum(.x) - .x[1, 1]))

SSND_data_rel_split <- map(SSND_occup_pairs_ISEI_Class_2,
                           ~ SSND_data_rel %>% dplyr::select(all_of(.x)))
tables_SSND_data_rel <- map(SSND_data_rel_split, ~ table(unlist(.x[, 1]), unlist(.x[, 2])))
agreement_SSND_data_rel <- map_dbl(tables_SSND_data_rel, ~ (.x[2, 2]) /
                                     (sum(.x) - .x[1, 1]))

# Create data frame with the information above ---------------------------------------------

agreement_df <- data.frame(
  Acq = agreement_SSND_data_acq,
  Frd = agreement_SSND_data_frd,
  Rel = agreement_SSND_data_rel,
  score_diff = SSND_occup_pairs_ISEI_Class_2_scores_diff
) %>%
  mutate(pair_id = LETTERS[1:n()])

agreement_df_clean <- agreement_df %>%
  relocate(pair_id) %>%
  select(-score_diff)

# Compute agreement scores between all possible occupational pairs and create MDS diagram ---------------------------------------------

occup_combns <- data.frame(combn(SSND_occup_names, 2))
SSND_data_split_alloccups <- map(occup_combns, ~ SSND_data_acq %>% dplyr::select(all_of(.x)))
tables_SSND_data_alloccups <- map(SSND_data_split_alloccups, ~ table(unlist(.x[, 1]), unlist(.x[, 2])))
agreement_SSND_data_alloccups <- map_dbl(tables_SSND_data_alloccups, ~ (.x[2, 2]) /
                                           (sum(.x) - .x[1, 1]))
names(agreement_SSND_data_alloccups) <- map_chr(occup_combns, ~ paste0(.x, collapse = "-"))

agreement_matrix <- matrix(NA, nrow = 30, ncol = 30)
colnames(agreement_matrix) <- rownames(agreement_matrix) <- SSND_occup_names

for (i in 1:nrow(agreement_matrix)) {
  for (k in 1:ncol(agreement_matrix)) {
    agreement_matrix[rownames(agreement_matrix)[i], colnames(agreement_matrix)[k]] <- agreement_SSND_data_alloccups[paste0(c(
      rownames(agreement_matrix)[i],
      colnames(agreement_matrix)[k]
    ), collapse = "-")]
  }
}

agreement_matrix[lower.tri(agreement_matrix)] <- t(agreement_matrix)[lower.tri(t(agreement_matrix))]
diag(agreement_matrix) <- 1

agreement_matrix_dist <- 1 - agreement_matrix
placeholder_adj <- matrix(1, nrow = 30, ncol = 30)
SSND_occup_names[17] <- "Bookkeeper/acct."
colnames(placeholder_adj) <- rownames(placeholder_adj) <- SSND_occup_names

agreement_Coord <- cmdscale(agreement_matrix_dist)
png(
  "MDS_plot.png",
  width = 15,
  height = 15,
  units = 'in',
  res = 300
)
par(mar = c(0, 0, 0, 0))
agreement_colors <-
  c("hotpink", "green", "deepskyblue", "darkorange")

source("PG_gplot_alt.R")

PG_gplot_alt(
  placeholder_adj,
  coord = agreement_Coord,
  displaylabels = TRUE,
  usearrows = FALSE,
  label.pos = 4,
  vertex.cex = 0.6,
  vertex.col = agreement_colors[SSND_classes],
  label.cex = 1,
  edge.col = "grey100"
)

# Trend surface analysis
df_trendsurface <- data.frame(agreement_Coord, SSND_ISEI_scores)
colnames(df_trendsurface) <- c("x", "y", "ISEI")
mod_trendsurface <- lm(ISEI ~ x + y, data = df_trendsurface)
clip(
  x1 = min(df_trendsurface$x) - 0.05,
  x2 = max(df_trendsurface$x) + 0.05,
  y1 = min(df_trendsurface$y) - 0.05,
  y2 = max(df_trendsurface$y) + 0.05
)
abline(
  a = 0,
  b = mod_trendsurface$coefficients["y"] / mod_trendsurface$coefficients["x"],
  col = "#a5a5a5"
)
par(xpd = NA)
text(
  x = c(-0.25, 0.25),
  y = c(-0.295, 0.26),
  labels = c("High ISEI", "Low ISEI"),
  cex = 2
)
legend(
  x = 0,
  y = -0.3,
  legend = c("Higher Service", "Lower Service", "Intermediate", "Manual"),
  title = "EGP Class",
  col = agreement_colors,
  pch = 19,
  cex = 1.5,
  bty = "n"
)
dev.off()
