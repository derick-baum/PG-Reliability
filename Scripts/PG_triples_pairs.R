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

ISEI_ClassStrat_Mixed14_Top <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed14_Top)
ISEI_ClassStrat_Mixed14_TopMiddle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed14_TopMiddle)
ISEI_ClassStrat_Mixed14_Middle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed14_Middle)
ISEI_ClassStrat_Mixed14_Bottom <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed14_Bottom)
ISEI_ClassStrat_Mixed14_BottomMiddle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed14_BottomMiddle)
ISEI_ClassStrat_Mixed13_Extreme <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Extreme)
ISEI_ClassStrat_Mixed13_Middle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Middle)
ISEI_ClassStrat_Mixed13_First <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_First)
ISEI_ClassStrat_Mixed13_Second <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Second)
ISEI_ClassStrat_Mixed13_Third <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Third)
ISEI_ClassStrat_Mixed13_Fourth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Fourth)
ISEI_ClassStrat_Mixed13_Fifth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Fifth)
ISEI_ClassStrat_Mixed13_Sixth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Sixth)
ISEI_ClassStrat_Mixed13_Seventh <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Seventh)
ISEI_ClassStrat_Mixed13_Eighth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Eighth)
ISEI_ClassStrat_Mixed13_Ninth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed13_Ninth)
ISEI_ClassStrat_Mixed12_ExtremeMiddle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_ExtremeMiddle)
ISEI_ClassStrat_Mixed12_Middle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Middle)
ISEI_ClassStrat_Mixed12_First <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_First)
ISEI_ClassStrat_Mixed12_Second <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Second)
ISEI_ClassStrat_Mixed12_Third <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Third)
ISEI_ClassStrat_Mixed12_Fourth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Fourth)
ISEI_ClassStrat_Mixed12_Fifth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Fifth)
ISEI_ClassStrat_Mixed12_Sixth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Sixth)
ISEI_ClassStrat_Mixed12_Seventh <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Seventh)
ISEI_ClassStrat_Mixed12_Eighth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed12_Eighth)
ISEI_ClassStrat_Mixed11_Extreme <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_Extreme)
ISEI_ClassStrat_Mixed11_Middle <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_Middle)
ISEI_ClassStrat_Mixed11_First <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_First)
ISEI_ClassStrat_Mixed11_Second <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_Second)
ISEI_ClassStrat_Mixed11_Third <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_Third)
ISEI_ClassStrat_Mixed11_Fourth <- split(SSND_occup_names, f = SSND_strata_assignment_FULL$ISEI_ClassStrat_Mixed11_Fourth)

ISEI_ClassStrat_Mixed14_Top_relocated <- c(ISEI_ClassStrat_Mixed14_Top[3:14], ISEI_ClassStrat_Mixed14_Top[1:2])
names(ISEI_ClassStrat_Mixed14_Top_relocated) <- 1:14

ISEI_ClassStrat_Mixed14_TopMiddle_relocated <- c(
  ISEI_ClassStrat_Mixed14_TopMiddle[1:3],
  ISEI_ClassStrat_Mixed14_TopMiddle[6:14],
  ISEI_ClassStrat_Mixed14_TopMiddle[4:5]
)
names(ISEI_ClassStrat_Mixed14_TopMiddle_relocated) <- 1:14

ISEI_ClassStrat_Mixed14_Middle_relocated <- c(
  ISEI_ClassStrat_Mixed14_Middle[1:6],
  ISEI_ClassStrat_Mixed14_Middle[9:14],
  ISEI_ClassStrat_Mixed14_Middle[7:8]
)
names(ISEI_ClassStrat_Mixed14_Middle_relocated) <- 1:14

ISEI_ClassStrat_Mixed14_Bottom_relocated <- c(ISEI_ClassStrat_Mixed14_Bottom[1:12],
                                              ISEI_ClassStrat_Mixed14_Bottom[13:14])
names(ISEI_ClassStrat_Mixed14_Bottom_relocated) <- 1:14

ISEI_ClassStrat_Mixed14_BottomMiddle_relocated <- c(
  ISEI_ClassStrat_Mixed14_BottomMiddle[1:9],
  ISEI_ClassStrat_Mixed14_BottomMiddle[12:14],
  ISEI_ClassStrat_Mixed14_BottomMiddle[10:11]
)
names(ISEI_ClassStrat_Mixed14_BottomMiddle_relocated) <- 1:14

ISEI_ClassStrat_Mixed13_Extreme_relocated <- c(
  ISEI_ClassStrat_Mixed13_Extreme[3:11],
  ISEI_ClassStrat_Mixed13_Extreme[1:2],
  ISEI_ClassStrat_Mixed13_Extreme[12:13]
)
names(ISEI_ClassStrat_Mixed13_Extreme_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Middle_relocated <- c(
  ISEI_ClassStrat_Mixed13_Middle[1:4],
  ISEI_ClassStrat_Mixed13_Middle[7],
  ISEI_ClassStrat_Mixed13_Middle[10:13],
  ISEI_ClassStrat_Mixed13_Middle[5:6],
  ISEI_ClassStrat_Mixed13_Middle[8:9]
)
names(ISEI_ClassStrat_Mixed13_Middle_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_First_relocated <- c(ISEI_ClassStrat_Mixed13_First[5:13],
                                             ISEI_ClassStrat_Mixed13_First[1:4])
names(ISEI_ClassStrat_Mixed13_First_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Second_relocated <- c(
  ISEI_ClassStrat_Mixed13_Second[3:5],
  ISEI_ClassStrat_Mixed13_Second[8:13],
  ISEI_ClassStrat_Mixed13_Second[1:2],
  ISEI_ClassStrat_Mixed13_Second[6:7]
)
names(ISEI_ClassStrat_Mixed13_Second_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Third_relocated <- c(
  ISEI_ClassStrat_Mixed13_Third[3:8],
  ISEI_ClassStrat_Mixed13_Third[11:13],
  ISEI_ClassStrat_Mixed13_Third[1:2],
  ISEI_ClassStrat_Mixed13_Third[9:10]
)
names(ISEI_ClassStrat_Mixed13_Third_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Fourth_relocated <- c(
  ISEI_ClassStrat_Mixed13_Fourth[1:3],
  ISEI_ClassStrat_Mixed13_Fourth[8:13],
  ISEI_ClassStrat_Mixed13_Fourth[4:7]
)
names(ISEI_ClassStrat_Mixed13_Fourth_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Fifth_relocated <- c(
  ISEI_ClassStrat_Mixed13_Fifth[1:3],
  ISEI_ClassStrat_Mixed13_Fifth[6:8],
  ISEI_ClassStrat_Mixed13_Fifth[11:13],
  ISEI_ClassStrat_Mixed13_Fifth[4:5],
  ISEI_ClassStrat_Mixed13_Fifth[9:10]
)
names(ISEI_ClassStrat_Mixed13_Fifth_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Sixth_relocated <- c(
  ISEI_ClassStrat_Mixed13_Sixth[1:3],
  ISEI_ClassStrat_Mixed13_Sixth[6:11],
  ISEI_ClassStrat_Mixed13_Sixth[4:5],
  ISEI_ClassStrat_Mixed13_Sixth[12:13]
)
names(ISEI_ClassStrat_Mixed13_Sixth_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Seventh_relocated <- c(
  ISEI_ClassStrat_Mixed13_Seventh[1:6],
  ISEI_ClassStrat_Mixed13_Seventh[11:13],
  ISEI_ClassStrat_Mixed13_Seventh[7:10]
)
names(ISEI_ClassStrat_Mixed13_Seventh_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Eighth_relocated <- c(
  ISEI_ClassStrat_Mixed13_Eighth[1:6],
  ISEI_ClassStrat_Mixed13_Eighth[9:11],
  ISEI_ClassStrat_Mixed13_Eighth[7:8],
  ISEI_ClassStrat_Mixed13_Eighth[12:13]
)
names(ISEI_ClassStrat_Mixed13_Eighth_relocated) <- 1:13

ISEI_ClassStrat_Mixed13_Ninth_relocated <- c(ISEI_ClassStrat_Mixed13_Ninth[1:9],
                                             ISEI_ClassStrat_Mixed13_Ninth[10:13])
names(ISEI_ClassStrat_Mixed13_Ninth_relocated) <- 1:13

ISEI_ClassStrat_Mixed12_ExtremeMiddle_relocated <- c(
  ISEI_ClassStrat_Mixed12_ExtremeMiddle[3:5],
  ISEI_ClassStrat_Mixed12_ExtremeMiddle[8:10],
  ISEI_ClassStrat_Mixed12_ExtremeMiddle[1:2],
  ISEI_ClassStrat_Mixed12_ExtremeMiddle[6:7],
  ISEI_ClassStrat_Mixed12_ExtremeMiddle[11:12]
)
names(ISEI_ClassStrat_Mixed12_ExtremeMiddle_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Middle_relocated <- c(
  ISEI_ClassStrat_Mixed12_Middle[1:3],
  ISEI_ClassStrat_Mixed12_Middle[10:12],
  ISEI_ClassStrat_Mixed12_Middle[4:9]
)
names(ISEI_ClassStrat_Mixed12_Middle_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_First_relocated <- c(ISEI_ClassStrat_Mixed12_First[7:12],
                                             ISEI_ClassStrat_Mixed12_First[1:6])
names(ISEI_ClassStrat_Mixed12_First_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Second_relocated <- c(
  ISEI_ClassStrat_Mixed12_Second[5:7],
  ISEI_ClassStrat_Mixed12_Second[10:12],
  ISEI_ClassStrat_Mixed12_Second[1:4],
  ISEI_ClassStrat_Mixed12_Second[8:9]
)
names(ISEI_ClassStrat_Mixed12_Second_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Third_relocated <- c(
  ISEI_ClassStrat_Mixed12_Third[5:10],
  ISEI_ClassStrat_Mixed12_Third[1:4],
  ISEI_ClassStrat_Mixed12_Third[11:12]
)
names(ISEI_ClassStrat_Mixed12_Third_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Fourth_relocated <- c(
  ISEI_ClassStrat_Mixed12_Fourth[3:5],
  ISEI_ClassStrat_Mixed12_Fourth[10:12],
  ISEI_ClassStrat_Mixed12_Fourth[1:2],
  ISEI_ClassStrat_Mixed12_Fourth[6:9]
)
names(ISEI_ClassStrat_Mixed12_Fourth_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Fifth_relocated <- c(
  ISEI_ClassStrat_Mixed12_Fifth[3:8],
  ISEI_ClassStrat_Mixed12_Fifth[1:2],
  ISEI_ClassStrat_Mixed12_Fifth[9:12]
)
names(ISEI_ClassStrat_Mixed12_Fifth_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Sixth_relocated <- c(
  ISEI_ClassStrat_Mixed12_Sixth[1:3],
  ISEI_ClassStrat_Mixed12_Sixth[8:10],
  ISEI_ClassStrat_Mixed12_Sixth[4:7],
  ISEI_ClassStrat_Mixed12_Sixth[11:12]
)
names(ISEI_ClassStrat_Mixed12_Sixth_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Seventh_relocated <- c(
  ISEI_ClassStrat_Mixed12_Seventh[1:3],
  ISEI_ClassStrat_Mixed12_Seventh[6:8],
  ISEI_ClassStrat_Mixed12_Seventh[4:5],
  ISEI_ClassStrat_Mixed12_Seventh[9:12]
)
names(ISEI_ClassStrat_Mixed12_Seventh_relocated) <- 1:12

ISEI_ClassStrat_Mixed12_Eighth_relocated <- c(ISEI_ClassStrat_Mixed12_Eighth[1:6],
                                              ISEI_ClassStrat_Mixed12_Eighth[7:12])
names(ISEI_ClassStrat_Mixed12_Eighth_relocated) <- 1:12

ISEI_ClassStrat_Mixed11_Extreme_relocated <- c(
  ISEI_ClassStrat_Mixed11_Extreme[5:7],
  ISEI_ClassStrat_Mixed11_Extreme[1:4],
  ISEI_ClassStrat_Mixed11_Extreme[8:11]
)
names(ISEI_ClassStrat_Mixed11_Extreme_relocated) <- 1:11

ISEI_ClassStrat_Mixed11_Middle_relocated <- c(
  ISEI_ClassStrat_Mixed11_Middle[1],
  ISEI_ClassStrat_Mixed11_Middle[6],
  ISEI_ClassStrat_Mixed11_Middle[11],
  ISEI_ClassStrat_Mixed11_Middle[2:5],
  ISEI_ClassStrat_Mixed11_Middle[7:10]
)
names(ISEI_ClassStrat_Mixed11_Middle_relocated) <- 1:11

ISEI_ClassStrat_Mixed11_First_relocated <- c(ISEI_ClassStrat_Mixed11_First[1:3],
                                             ISEI_ClassStrat_Mixed11_First[4:11])
names(ISEI_ClassStrat_Mixed11_First_relocated) <- 1:11

ISEI_ClassStrat_Mixed11_Second_relocated <- c(
  ISEI_ClassStrat_Mixed11_Second[3:5],
  ISEI_ClassStrat_Mixed11_Second[1:2],
  ISEI_ClassStrat_Mixed11_Second[6:11]
)
names(ISEI_ClassStrat_Mixed11_Second_relocated) <- 1:11

ISEI_ClassStrat_Mixed11_Third_relocated <- c(
  ISEI_ClassStrat_Mixed11_Third[7:9],
  ISEI_ClassStrat_Mixed11_Third[1:6],
  ISEI_ClassStrat_Mixed11_Third[10:11]
)
names(ISEI_ClassStrat_Mixed11_Third_relocated) <- 1:11

ISEI_ClassStrat_Mixed11_Fourth_relocated <- c(ISEI_ClassStrat_Mixed11_Fourth[9:11],
                                              ISEI_ClassStrat_Mixed11_Fourth[1:8])
names(ISEI_ClassStrat_Mixed11_Fourth_relocated) <- 1:11

# Create matrix recording allocations of occupations to replicate pairs ---------------------------------------------

assignments_create <- function(strat, l, k) {
  Assignments <- matrix(data = c(rep(0, (2 ^ (
    l - 1
  )) * (3 ^ k))),
  nrow = (2 ^ (l - 1)) * (3 ^ k),
  ncol = 30)
  colnames(Assignments) <- unname(unlist(strat))
  Assignments[, strat[[1]][1]] <- 1
  Assignments[, strat[[1]][2]] <- 3 - Assignments[, strat[[1]][1]]
  for (j in 2:l) {
    Assignments[, strat[[j]][1]] <- c(rep(1, 2 ^ ((l - 1) - j + 1)), rep(2, 2 ^
                                                                           ((l - 1) - j + 1)))
    Assignments[, strat[[j]][2]] <- 3 - Assignments[, strat[[j]][1]]
  }
  for (j in (l + 1):(l + k)) {
    Assignments[, strat[[j]][1]] <- c(rep(c(1, 2, 3), each = 3 ^ ((l + k - 1) -
                                                                    j + 1)))
    Assignments[, strat[[j]][2]] <- c(rep(c(2, 3, 1), each = 3 ^ ((l + k -
                                                                     1) - j + 1)))
    Assignments[, strat[[j]][3]] <- c(rep(c(3, 1, 2), each = 3 ^ ((l + k -
                                                                     1) - j + 1)))
  }
  Assignments
}

Assignments <- map(
  list(
    ISEI_ClassStrat_Mixed14_Top_relocated,
    ISEI_ClassStrat_Mixed14_Middle_relocated
  ),
  ~ assignments_create(.x, l = 12, k = 2)
)
names(Assignments) <- c("Top", "Middle")

pairs_top <- map(data.frame(t(Assignments$Top)), ~ split(colnames(Assignments$Top), f = unlist(.x)))
pairs_middle <- map(data.frame(t(Assignments$Middle)), ~ split(colnames(Assignments$Middle), f = unlist(.x)))

Assignments_bottom <- assignments_create(ISEI_ClassStrat_Mixed14_Bottom_relocated,
                                         l = 12,
                                         k = 2)
pairs_bottom <- map(data.frame(t(Assignments_bottom)), ~ split(colnames(Assignments_bottom), f = unlist(.x)))

Assignments_Mixed14TopMiddle <- assignments_create(ISEI_ClassStrat_Mixed14_TopMiddle_relocated,
                                                   l = 12,
                                                   k = 2)
pairs_Mixed14_TopMiddle <- map(data.frame(t(Assignments_Mixed14TopMiddle)), ~ split(colnames(Assignments_Mixed14TopMiddle), f = unlist(.x)))

Assignments_Mixed14BottomMiddle <- assignments_create(ISEI_ClassStrat_Mixed14_BottomMiddle_relocated,
                                                      l = 12,
                                                      k = 2)
pairs_Mixed14_BottomMiddle <- map(data.frame(t(Assignments_Mixed14BottomMiddle)), ~ split(colnames(Assignments_Mixed14BottomMiddle), f = unlist(.x)))

Assignments_Mixed13 <- map(
  list(
    ISEI_ClassStrat_Mixed13_Extreme_relocated,
    ISEI_ClassStrat_Mixed13_Middle_relocated
  ),
  ~ assignments_create(.x, l = 9, k = 4)
)
names(Assignments_Mixed13) <- c("Extreme", "Middle")
pairs_Mixed13_Extreme <- map(data.frame(t(Assignments_Mixed13$Extreme)), ~ split(colnames(Assignments_Mixed13$Extreme), f = unlist(.x)))
pairs_Mixed13_Middle <- map(data.frame(t(Assignments_Mixed13$Middle)), ~ split(colnames(Assignments_Mixed13$Middle), f = unlist(.x)))

Assignments_Mixed13_Others <- map(
  list(
    ISEI_ClassStrat_Mixed13_First_relocated,
    ISEI_ClassStrat_Mixed13_Second_relocated,
    ISEI_ClassStrat_Mixed13_Third_relocated,
    ISEI_ClassStrat_Mixed13_Fourth_relocated,
    ISEI_ClassStrat_Mixed13_Fifth_relocated,
    ISEI_ClassStrat_Mixed13_Sixth_relocated,
    ISEI_ClassStrat_Mixed13_Seventh_relocated,
    ISEI_ClassStrat_Mixed13_Eighth_relocated,
    ISEI_ClassStrat_Mixed13_Ninth_relocated
  ),
  ~ assignments_create(.x, l = 9, k = 4)
)
names(Assignments_Mixed13_Others) <- c("First",
                                       "Second",
                                       "Third",
                                       "Fourth",
                                       "Fifth",
                                       "Sixth",
                                       "Seventh",
                                       "Eighth",
                                       "Ninth")
pairs_Mixed13_First <- map(data.frame(t(Assignments_Mixed13_Others$First)), ~ split(colnames(Assignments_Mixed13_Others$First), f = unlist(.x)))
pairs_Mixed13_Second <- map(data.frame(t(Assignments_Mixed13_Others$Second)), ~ split(colnames(Assignments_Mixed13_Others$Second), f = unlist(.x)))
pairs_Mixed13_Third <- map(data.frame(t(Assignments_Mixed13_Others$Third)), ~ split(colnames(Assignments_Mixed13_Others$Third), f = unlist(.x)))
pairs_Mixed13_Fourth <- map(data.frame(t(Assignments_Mixed13_Others$Fourth)), ~ split(colnames(Assignments_Mixed13_Others$Fourth), f = unlist(.x)))
pairs_Mixed13_Fifth <- map(data.frame(t(Assignments_Mixed13_Others$Fifth)), ~ split(colnames(Assignments_Mixed13_Others$Fifth), f = unlist(.x)))
pairs_Mixed13_Sixth <- map(data.frame(t(Assignments_Mixed13_Others$Sixth)), ~ split(colnames(Assignments_Mixed13_Others$Sixth), f = unlist(.x)))
pairs_Mixed13_Seventh <- map(data.frame(t(Assignments_Mixed13_Others$Seventh)), ~ split(colnames(Assignments_Mixed13_Others$Seventh), f = unlist(.x)))
pairs_Mixed13_Eighth <- map(data.frame(t(Assignments_Mixed13_Others$Eighth)), ~ split(colnames(Assignments_Mixed13_Others$Eighth), f = unlist(.x)))
pairs_Mixed13_Ninth <- map(data.frame(t(Assignments_Mixed13_Others$Ninth)), ~ split(colnames(Assignments_Mixed13_Others$Ninth), f = unlist(.x)))

Assignments_Mixed12 <- map(
  list(
    ISEI_ClassStrat_Mixed12_ExtremeMiddle_relocated,
    ISEI_ClassStrat_Mixed12_Middle_relocated
  ),
  ~ assignments_create(.x, l = 6, k = 6)
)
names(Assignments_Mixed12) <- c("ExtremeMiddle", "Middle")
pairs_Mixed12_ExtremeMiddle <- map(data.frame(t(Assignments_Mixed12$ExtremeMiddle)), ~ split(colnames(Assignments_Mixed12$ExtremeMiddle), f = unlist(.x)))
pairs_Mixed12_Middle <- map(data.frame(t(Assignments_Mixed12$Middle)), ~ split(colnames(Assignments_Mixed12$Middle), f = unlist(.x)))

Assignments_Mixed12_Others <- map(
  list(
    ISEI_ClassStrat_Mixed12_First_relocated,
    ISEI_ClassStrat_Mixed12_Second_relocated,
    ISEI_ClassStrat_Mixed12_Third_relocated,
    ISEI_ClassStrat_Mixed12_Fourth_relocated,
    ISEI_ClassStrat_Mixed12_Fifth_relocated,
    ISEI_ClassStrat_Mixed12_Sixth_relocated,
    ISEI_ClassStrat_Mixed12_Seventh_relocated,
    ISEI_ClassStrat_Mixed12_Eighth_relocated
  ),
  ~ assignments_create(.x, l = 6, k = 6)
)
names(Assignments_Mixed12_Others) <- c("First",
                                       "Second",
                                       "Third",
                                       "Fourth",
                                       "Fifth",
                                       "Sixth",
                                       "Seventh",
                                       "Eighth")
pairs_Mixed12_First <- map(data.frame(t(Assignments_Mixed12_Others$First)), ~ split(colnames(Assignments_Mixed12_Others$First), f = unlist(.x)))
pairs_Mixed12_Second <- map(data.frame(t(Assignments_Mixed12_Others$Second)), ~ split(colnames(Assignments_Mixed12_Others$Second), f = unlist(.x)))
pairs_Mixed12_Third <- map(data.frame(t(Assignments_Mixed12_Others$Third)), ~ split(colnames(Assignments_Mixed12_Others$Third), f = unlist(.x)))
pairs_Mixed12_Fourth <- map(data.frame(t(Assignments_Mixed12_Others$Fourth)), ~ split(colnames(Assignments_Mixed12_Others$Fourth), f = unlist(.x)))
pairs_Mixed12_Fifth <- map(data.frame(t(Assignments_Mixed12_Others$Fifth)), ~ split(colnames(Assignments_Mixed12_Others$Fifth), f = unlist(.x)))
pairs_Mixed12_Sixth <- map(data.frame(t(Assignments_Mixed12_Others$Sixth)), ~ split(colnames(Assignments_Mixed12_Others$Sixth), f = unlist(.x)))
pairs_Mixed12_Seventh <- map(data.frame(t(Assignments_Mixed12_Others$Seventh)), ~ split(colnames(Assignments_Mixed12_Others$Seventh), f = unlist(.x)))
pairs_Mixed12_Eighth <- map(data.frame(t(Assignments_Mixed12_Others$Eighth)), ~ split(colnames(Assignments_Mixed12_Others$Eighth), f = unlist(.x)))

Assignments_Mixed11 <- map(
  list(
    ISEI_ClassStrat_Mixed11_Extreme_relocated,
    ISEI_ClassStrat_Mixed11_Middle_relocated
  ),
  ~ assignments_create(.x, l = 3, k = 8)
)
names(Assignments_Mixed11) <- c("Extreme", "Middle")
pairs_Mixed11_Extreme <- map(data.frame(t(Assignments_Mixed11$Extreme)), ~ split(colnames(Assignments_Mixed11$Extreme), f = unlist(.x)))
pairs_Mixed11_Middle <- map(data.frame(t(Assignments_Mixed11$Middle)), ~ split(colnames(Assignments_Mixed11$Middle), f = unlist(.x)))

Assignments_Mixed11_Others <- map(
  list(
    ISEI_ClassStrat_Mixed11_First_relocated,
    ISEI_ClassStrat_Mixed11_Second_relocated,
    ISEI_ClassStrat_Mixed11_Third_relocated,
    ISEI_ClassStrat_Mixed11_Fourth_relocated
  ),
  ~ assignments_create(.x, l = 3, k = 8)
)
names(Assignments_Mixed11_Others) <- c("First", "Second", "Third", "Fourth")
pairs_Mixed11_First <- map(data.frame(t(Assignments_Mixed11_Others$First)), ~ split(colnames(Assignments_Mixed11_Others$First), f = unlist(.x)))
pairs_Mixed11_Second <- map(data.frame(t(Assignments_Mixed11_Others$Second)), ~ split(colnames(Assignments_Mixed11_Others$Second), f = unlist(.x)))
pairs_Mixed11_Third <- map(data.frame(t(Assignments_Mixed11_Others$Third)), ~ split(colnames(Assignments_Mixed11_Others$Third), f = unlist(.x)))
pairs_Mixed11_Fourth <- map(data.frame(t(Assignments_Mixed11_Others$Fourth)), ~ split(colnames(Assignments_Mixed11_Others$Fourth), f = unlist(.x)))

# Checks ---------------------------------------------

## Check there are no repeated profiles ---------------------------------------------

pairs_top_reduced <- map(pairs_top, ~ .x[1:2])
pairs_middle_reduced <- map(pairs_middle, ~ .x[1:2])
pairs_bottom_reduced <- map(pairs_bottom, ~ .x[1:2])

pairs_Mixed14_TopMiddle_reduced <-  map(pairs_Mixed14_TopMiddle, ~ .x[1:2])
pairs_Mixed14_BottomMiddle_reduced <-  map(pairs_Mixed14_BottomMiddle, ~ .x[1:2])

pairs_Mixed13_Extreme_reduced <- map(pairs_Mixed13_Extreme, ~ .x[1:2])
pairs_Mixed13_Middle_reduced <- map(pairs_Mixed13_Middle, ~ .x[1:2])
pairs_Mixed13_First_reduced <- map(pairs_Mixed13_First, ~ .x[1:2])
pairs_Mixed13_Second_reduced <- map(pairs_Mixed13_Second, ~ .x[1:2])
pairs_Mixed13_Third_reduced <- map(pairs_Mixed13_Third, ~ .x[1:2])
pairs_Mixed13_Fourth_reduced <- map(pairs_Mixed13_Fourth, ~ .x[1:2])
pairs_Mixed13_Fifth_reduced <- map(pairs_Mixed13_Fifth, ~ .x[1:2])
pairs_Mixed13_Sixth_reduced <- map(pairs_Mixed13_Sixth, ~ .x[1:2])
pairs_Mixed13_Seventh_reduced <- map(pairs_Mixed13_Seventh, ~ .x[1:2])
pairs_Mixed13_Eighth_reduced <- map(pairs_Mixed13_Eighth, ~ .x[1:2])
pairs_Mixed13_Ninth_reduced <- map(pairs_Mixed13_Ninth, ~ .x[1:2])

pairs_Mixed12_ExtremeMiddle_reduced <- map(pairs_Mixed12_ExtremeMiddle, ~ .x[1:2])
pairs_Mixed12_Middle_reduced <- map(pairs_Mixed12_Middle, ~ .x[1:2])
pairs_Mixed12_First_reduced <- map(pairs_Mixed12_First, ~ .x[1:2])
pairs_Mixed12_Second_reduced <- map(pairs_Mixed12_Second, ~ .x[1:2])
pairs_Mixed12_Third_reduced <- map(pairs_Mixed12_Third, ~ .x[1:2])
pairs_Mixed12_Fourth_reduced <- map(pairs_Mixed12_Fourth, ~ .x[1:2])
pairs_Mixed12_Fifth_reduced <- map(pairs_Mixed12_Fifth, ~ .x[1:2])
pairs_Mixed12_Sixth_reduced <- map(pairs_Mixed12_Sixth, ~ .x[1:2])
pairs_Mixed12_Seventh_reduced <- map(pairs_Mixed12_Seventh, ~ .x[1:2])
pairs_Mixed12_Eighth_reduced <- map(pairs_Mixed12_Eighth, ~ .x[1:2])

pairs_Mixed11_Extreme_reduced <- map(pairs_Mixed11_Extreme, ~ .x[1:2])
pairs_Mixed11_Middle_reduced <- map(pairs_Mixed11_Middle, ~ .x[1:2])
pairs_Mixed11_First_reduced <- map(pairs_Mixed11_First, ~ .x[1:2])
pairs_Mixed11_Second_reduced <- map(pairs_Mixed11_Second, ~ .x[1:2])
pairs_Mixed11_Third_reduced <- map(pairs_Mixed11_Third, ~ .x[1:2])
pairs_Mixed11_Fourth_reduced <- map(pairs_Mixed11_Fourth, ~ .x[1:2])

pairs_top_unlist_together <- as.vector(unlist(map(pairs_top_reduced, ~ as.vector(unlist(
  map(.x, ~ paste0(.x, collapse = "-"))
)))))
length(unique(pairs_top_unlist_together)) == (2 ^ 12) * (3 ^ 2)

pairs_middle_unlist_together <- as.vector(unlist(map(
  pairs_middle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_middle_unlist_together)) == (2 ^ 12) * (3 ^ 2)

pairs_bottom_unlist_together <- as.vector(unlist(map(
  pairs_bottom_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_bottom_unlist_together)) == (2 ^ 12) * (3 ^ 2)

pairs_Mixed14_TopMiddle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed14_TopMiddle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed14_TopMiddle_unlist_together)) == (2 ^ 12) * (3 ^
                                                                         2)

pairs_Mixed14_BottomMiddle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed14_BottomMiddle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed14_BottomMiddle_unlist_together)) == (2 ^ 12) *
  (3 ^ 2)

pairs_Mixed13_Extreme_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Extreme_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Extreme_unlist_together)) == (2 ^ 9) * (3 ^
                                                                      4)

pairs_Mixed13_Middle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Middle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Middle_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_First_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_First_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_First_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Second_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Second_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Second_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Third_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Third_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Third_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Fourth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Fourth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Fourth_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Fifth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Fifth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Fifth_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Sixth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Sixth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Sixth_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Seventh_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Seventh_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Seventh_unlist_together)) == (2 ^ 9) * (3 ^
                                                                      4)

pairs_Mixed13_Eighth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Eighth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Eighth_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed13_Ninth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed13_Ninth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed13_Ninth_unlist_together)) == (2 ^ 9) * (3 ^ 4)

pairs_Mixed12_ExtremeMiddle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_ExtremeMiddle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_ExtremeMiddle_unlist_together)) == (2 ^ 6) *
  (3 ^ 6)

pairs_Mixed12_Middle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Middle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Middle_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_First_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_First_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_First_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Second_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Second_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Second_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Third_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Third_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Third_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Fourth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Fourth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Fourth_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Fifth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Fifth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Fifth_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Sixth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Sixth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Sixth_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed12_Seventh_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Seventh_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Seventh_unlist_together)) == (2 ^ 6) * (3 ^
                                                                      6)

pairs_Mixed12_Eighth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed12_Eighth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed12_Eighth_unlist_together)) == (2 ^ 6) * (3 ^ 6)

pairs_Mixed11_Extreme_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_Extreme_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_Extreme_unlist_together)) == (2 ^ 3) * (3 ^
                                                                      8)

pairs_Mixed11_Middle_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_Middle_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_Middle_unlist_together)) == (2 ^ 3) * (3 ^ 8)

pairs_Mixed11_First_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_First_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_First_unlist_together)) == (2 ^ 3) * (3 ^ 8)

pairs_Mixed11_Second_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_Second_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_Second_unlist_together)) == (2 ^ 3) * (3 ^ 8)

pairs_Mixed11_Third_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_Third_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_Third_unlist_together)) == (2 ^ 3) * (3 ^ 8)

pairs_Mixed11_Fourth_unlist_together <- as.vector(unlist(map(
  pairs_Mixed11_Fourth_reduced, ~ as.vector(unlist(map(
    .x, ~ paste0(.x, collapse = "-")
  )))
)))
length(unique(pairs_Mixed11_Fourth_unlist_together)) == (2 ^ 3) * (3 ^ 8)

## Check if pairs don't contain repeated occupations ---------------------------------------------

check_matches_fun <- function(pair) {
  all(is.na(match(pair[[1]], pair[[2]])))
}
all(map_lgl(pairs_top_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_middle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_bottom_reduced, check_matches_fun) == TRUE)

all(map_lgl(pairs_Mixed14_TopMiddle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed14_BottomMiddle_reduced, check_matches_fun) == TRUE)

all(map_lgl(pairs_Mixed13_Extreme_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Middle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_First_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Second_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Third_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Fourth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Fifth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Sixth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Seventh_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Eighth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed13_Ninth_reduced, check_matches_fun) == TRUE)

all(map_lgl(pairs_Mixed12_ExtremeMiddle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Middle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_First_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Second_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Third_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Fourth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Fifth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Sixth_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Seventh_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed12_Eighth_reduced, check_matches_fun) == TRUE)

all(map_lgl(pairs_Mixed11_Extreme_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed11_Middle_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed11_First_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed11_Second_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed11_Third_reduced, check_matches_fun) == TRUE)
all(map_lgl(pairs_Mixed11_Fourth_reduced, check_matches_fun) == TRUE)

## Check if each member in the pair includes an occupation from each stratum ---------------------------------------------

sum(map_lgl(map(pairs_top_reduced, ~ unlist(map(
  .x,
  ~ map2_lgl(.x, ISEI_ClassStrat_Mixed14_Top_relocated, ~ .x %in% .y)
))), ~ all(.x == TRUE))) == length(pairs_top_reduced)
sum(map_lgl(map(pairs_middle_reduced, ~ unlist(map(
  .x,
  ~ map2_lgl(.x, ISEI_ClassStrat_Mixed14_Middle_relocated, ~ .x %in% .y)
))), ~ all(.x == TRUE))) == length(pairs_middle_reduced)
sum(map_lgl(map(pairs_bottom_reduced, ~ unlist(map(
  .x,
  ~ map2_lgl(.x, ISEI_ClassStrat_Mixed14_Bottom_relocated, ~ .x %in% .y)
))), ~ all(.x == TRUE))) == length(pairs_bottom_reduced)

sum(map_lgl(
  map(pairs_Mixed14_TopMiddle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed14_TopMiddle_relocated, ~ .x %in% .y)
  ))),
  ~ all(.x == TRUE)
)) == length(pairs_Mixed14_TopMiddle_reduced)
sum(map_lgl(
  map(pairs_Mixed14_BottomMiddle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(
      .x,
      ISEI_ClassStrat_Mixed14_BottomMiddle_relocated,
      ~ .x %in% .y
    )
  ))),
  ~ all(.x == TRUE)
)) == length(pairs_Mixed14_BottomMiddle_reduced)

sum(map_lgl(map(
  pairs_Mixed13_Extreme_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Extreme_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Extreme_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Middle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Middle_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Middle_reduced)
sum(map_lgl(map(
  pairs_Mixed13_First_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_First_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_First_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Second_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Second_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Second_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Third_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Third_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Third_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Fourth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Fourth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Fourth_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Fifth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Fifth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Fifth_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Sixth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Sixth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Sixth_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Seventh_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Seventh_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Seventh_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Eighth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Eighth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Eighth_reduced)
sum(map_lgl(map(
  pairs_Mixed13_Ninth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed13_Ninth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed13_Ninth_reduced)

sum(map_lgl(
  map(pairs_Mixed12_ExtremeMiddle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(
      .x,
      ISEI_ClassStrat_Mixed12_ExtremeMiddle_relocated,
      ~ .x %in% .y
    )
  ))),
  ~ all(.x == TRUE)
)) == length(pairs_Mixed12_ExtremeMiddle_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Middle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Middle_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Middle_reduced)
sum(map_lgl(map(
  pairs_Mixed12_First_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_First_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_First_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Second_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Second_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Second_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Third_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Third_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Third_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Fourth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Fourth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Fourth_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Fifth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Fifth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Fifth_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Sixth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Sixth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Sixth_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Seventh_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Seventh_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Seventh_reduced)
sum(map_lgl(map(
  pairs_Mixed12_Eighth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed12_Eighth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed12_Eighth_reduced)

sum(map_lgl(map(
  pairs_Mixed11_Extreme_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_Extreme_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_Extreme_reduced)
sum(map_lgl(map(
  pairs_Mixed11_Middle_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_Middle_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_Middle_reduced)
sum(map_lgl(map(
  pairs_Mixed11_First_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_First_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_First_reduced)
sum(map_lgl(map(
  pairs_Mixed11_Second_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_Second_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_Second_reduced)
sum(map_lgl(map(
  pairs_Mixed11_Third_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_Third_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_Third_reduced)
sum(map_lgl(map(
  pairs_Mixed11_Fourth_reduced, ~ unlist(map(
    .x,
    ~ map2_lgl(.x, ISEI_ClassStrat_Mixed11_Fourth_relocated, ~ .x %in% .y)
  ))
), ~ all(.x == TRUE))) == length(pairs_Mixed11_Fourth_reduced)

SSND_dat_list_pairs_top <- vector(mode = "list", length = nrow(Assignments$Top))
for (i in 1:length(SSND_dat_list_pairs_top)) {
  SSND_dat_list_pairs_top[[i]][[1]] <- SSND_data_acq[, pairs_top[[i]][[1]]]
  SSND_dat_list_pairs_top[[i]][[2]] <- SSND_data_acq[, pairs_top[[i]][[2]]]
}

SSND_dat_list_pairs_middle <- vector(mode = "list", length = nrow(Assignments$Middle))
for (i in 1:length(SSND_dat_list_pairs_middle)) {
  SSND_dat_list_pairs_middle[[i]][[1]] <- SSND_data_acq[, pairs_middle[[i]][[1]]]
  SSND_dat_list_pairs_middle[[i]][[2]] <- SSND_data_acq[, pairs_middle[[i]][[2]]]
}

SSND_dat_list_pairs_bottom <- vector(mode = "list", length = nrow(Assignments_bottom))
for (i in 1:length(SSND_dat_list_pairs_bottom)) {
  SSND_dat_list_pairs_bottom[[i]][[1]] <- SSND_data_acq[, pairs_bottom[[i]][[1]]]
  SSND_dat_list_pairs_bottom[[i]][[2]] <- SSND_data_acq[, pairs_bottom[[i]][[2]]]
}

replicate_pairs_Mixed14 <- list(SSND_dat_list_pairs_top, SSND_dat_list_pairs_middle)
names(replicate_pairs_Mixed14) <- c("Top", "Middle")

replicate_pairs_Mixed14_Bottom <- SSND_dat_list_pairs_bottom

replicate_pairs_Mixed14_TopMiddle <- vector(mode = "list",
                                            length = nrow(Assignments_Mixed14TopMiddle))
for (i in 1:length(replicate_pairs_Mixed14_TopMiddle)) {
  replicate_pairs_Mixed14_TopMiddle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed14_TopMiddle[[i]][[1]]]
  replicate_pairs_Mixed14_TopMiddle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed14_TopMiddle[[i]][[2]]]
}

replicate_pairs_Mixed14_BottomMiddle <- vector(mode = "list",
                                               length = nrow(Assignments_Mixed14BottomMiddle))
for (i in 1:length(replicate_pairs_Mixed14_BottomMiddle)) {
  replicate_pairs_Mixed14_BottomMiddle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed14_BottomMiddle[[i]][[1]]]
  replicate_pairs_Mixed14_BottomMiddle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed14_BottomMiddle[[i]][[2]]]
}

replicate_pairs_Mixed13_Extreme <- vector(mode = "list",
                                          length = nrow(Assignments_Mixed13$Extreme))
for (i in 1:length(replicate_pairs_Mixed13_Extreme)) {
  replicate_pairs_Mixed13_Extreme[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Extreme[[i]][[1]]]
  replicate_pairs_Mixed13_Extreme[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Extreme[[i]][[2]]]
}

replicate_pairs_Mixed13_Middle <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed13$Middle))
for (i in 1:length(replicate_pairs_Mixed13_Middle)) {
  replicate_pairs_Mixed13_Middle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Middle[[i]][[1]]]
  replicate_pairs_Mixed13_Middle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Middle[[i]][[2]]]
}

replicate_pairs_Mixed13_First <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed13_Others$First))
for (i in 1:length(replicate_pairs_Mixed13_First)) {
  replicate_pairs_Mixed13_First[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_First[[i]][[1]]]
  replicate_pairs_Mixed13_First[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_First[[i]][[2]]]
}

replicate_pairs_Mixed13_Second <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed13_Others$Second))
for (i in 1:length(replicate_pairs_Mixed13_Second)) {
  replicate_pairs_Mixed13_Second[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Second[[i]][[1]]]
  replicate_pairs_Mixed13_Second[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Second[[i]][[2]]]
}

replicate_pairs_Mixed13_Third <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed13_Others$Third))
for (i in 1:length(replicate_pairs_Mixed13_Third)) {
  replicate_pairs_Mixed13_Third[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Third[[i]][[1]]]
  replicate_pairs_Mixed13_Third[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Third[[i]][[2]]]
}

replicate_pairs_Mixed13_Fourth <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed13_Others$Fourth))
for (i in 1:length(replicate_pairs_Mixed13_Fourth)) {
  replicate_pairs_Mixed13_Fourth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Fourth[[i]][[1]]]
  replicate_pairs_Mixed13_Fourth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Fourth[[i]][[2]]]
}

replicate_pairs_Mixed13_Fifth <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed13_Others$Fifth))
for (i in 1:length(replicate_pairs_Mixed13_Fifth)) {
  replicate_pairs_Mixed13_Fifth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Fifth[[i]][[1]]]
  replicate_pairs_Mixed13_Fifth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Fifth[[i]][[2]]]
}

replicate_pairs_Mixed13_Sixth <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed13_Others$Sixth))
for (i in 1:length(replicate_pairs_Mixed13_Sixth)) {
  replicate_pairs_Mixed13_Sixth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Sixth[[i]][[1]]]
  replicate_pairs_Mixed13_Sixth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Sixth[[i]][[2]]]
}

replicate_pairs_Mixed13_Seventh <- vector(mode = "list",
                                          length = nrow(Assignments_Mixed13_Others$Seventh))
for (i in 1:length(replicate_pairs_Mixed13_Seventh)) {
  replicate_pairs_Mixed13_Seventh[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Seventh[[i]][[1]]]
  replicate_pairs_Mixed13_Seventh[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Seventh[[i]][[2]]]
}

replicate_pairs_Mixed13_Eighth <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed13_Others$Eighth))
for (i in 1:length(replicate_pairs_Mixed13_Eighth)) {
  replicate_pairs_Mixed13_Eighth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Eighth[[i]][[1]]]
  replicate_pairs_Mixed13_Eighth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Eighth[[i]][[2]]]
}

replicate_pairs_Mixed13_Ninth <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed13_Others$Ninth))
for (i in 1:length(replicate_pairs_Mixed13_Ninth)) {
  replicate_pairs_Mixed13_Ninth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed13_Ninth[[i]][[1]]]
  replicate_pairs_Mixed13_Ninth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed13_Ninth[[i]][[2]]]
}

replicate_pairs_Mixed12_ExtremeMiddle <- vector(mode = "list",
                                                length = nrow(Assignments_Mixed12$ExtremeMiddle))
for (i in 1:length(replicate_pairs_Mixed12_ExtremeMiddle)) {
  replicate_pairs_Mixed12_ExtremeMiddle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_ExtremeMiddle[[i]][[1]]]
  replicate_pairs_Mixed12_ExtremeMiddle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_ExtremeMiddle[[i]][[2]]]
}

replicate_pairs_Mixed12_Middle <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed12$Middle))
for (i in 1:length(replicate_pairs_Mixed12_Middle)) {
  replicate_pairs_Mixed12_Middle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Middle[[i]][[1]]]
  replicate_pairs_Mixed12_Middle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Middle[[i]][[2]]]
}

replicate_pairs_Mixed12_First <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed12_Others$First))
for (i in 1:length(replicate_pairs_Mixed12_First)) {
  replicate_pairs_Mixed12_First[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_First[[i]][[1]]]
  replicate_pairs_Mixed12_First[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_First[[i]][[2]]]
}

replicate_pairs_Mixed12_Second <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed12_Others$Second))
for (i in 1:length(replicate_pairs_Mixed12_Second)) {
  replicate_pairs_Mixed12_Second[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Second[[i]][[1]]]
  replicate_pairs_Mixed12_Second[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Second[[i]][[2]]]
}

replicate_pairs_Mixed12_Third <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed12_Others$Third))
for (i in 1:length(replicate_pairs_Mixed12_Third)) {
  replicate_pairs_Mixed12_Third[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Third[[i]][[1]]]
  replicate_pairs_Mixed12_Third[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Third[[i]][[2]]]
}

replicate_pairs_Mixed12_Fourth <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed12_Others$Fourth))
for (i in 1:length(replicate_pairs_Mixed12_Fourth)) {
  replicate_pairs_Mixed12_Fourth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Fourth[[i]][[1]]]
  replicate_pairs_Mixed12_Fourth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Fourth[[i]][[2]]]
}

replicate_pairs_Mixed12_Fifth <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed12_Others$Fifth))
for (i in 1:length(replicate_pairs_Mixed12_Fifth)) {
  replicate_pairs_Mixed12_Fifth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Fifth[[i]][[1]]]
  replicate_pairs_Mixed12_Fifth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Fifth[[i]][[2]]]
}

replicate_pairs_Mixed12_Sixth <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed12_Others$Sixth))
for (i in 1:length(replicate_pairs_Mixed12_Sixth)) {
  replicate_pairs_Mixed12_Sixth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Sixth[[i]][[1]]]
  replicate_pairs_Mixed12_Sixth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Sixth[[i]][[2]]]
}

replicate_pairs_Mixed12_Seventh <- vector(mode = "list",
                                          length = nrow(Assignments_Mixed12_Others$Seventh))
for (i in 1:length(replicate_pairs_Mixed12_Seventh)) {
  replicate_pairs_Mixed12_Seventh[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Seventh[[i]][[1]]]
  replicate_pairs_Mixed12_Seventh[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Seventh[[i]][[2]]]
}

replicate_pairs_Mixed12_Eighth <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed12_Others$Eighth))
for (i in 1:length(replicate_pairs_Mixed12_Eighth)) {
  replicate_pairs_Mixed12_Eighth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed12_Eighth[[i]][[1]]]
  replicate_pairs_Mixed12_Eighth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed12_Eighth[[i]][[2]]]
}

replicate_pairs_Mixed11_Extreme <- vector(mode = "list",
                                          length = nrow(Assignments_Mixed11$Extreme))
for (i in 1:length(replicate_pairs_Mixed11_Extreme)) {
  replicate_pairs_Mixed11_Extreme[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_Extreme[[i]][[1]]]
  replicate_pairs_Mixed11_Extreme[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_Extreme[[i]][[2]]]
}

replicate_pairs_Mixed11_Middle <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed11$Middle))
for (i in 1:length(replicate_pairs_Mixed11_Middle)) {
  replicate_pairs_Mixed11_Middle[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_Middle[[i]][[1]]]
  replicate_pairs_Mixed11_Middle[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_Middle[[i]][[2]]]
}

replicate_pairs_Mixed11_First <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed11_Others$First))
for (i in 1:length(replicate_pairs_Mixed11_First)) {
  replicate_pairs_Mixed11_First[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_First[[i]][[1]]]
  replicate_pairs_Mixed11_First[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_First[[i]][[2]]]
}

replicate_pairs_Mixed11_Second <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed11_Others$Second))
for (i in 1:length(replicate_pairs_Mixed11_Second)) {
  replicate_pairs_Mixed11_Second[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_Second[[i]][[1]]]
  replicate_pairs_Mixed11_Second[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_Second[[i]][[2]]]
}

replicate_pairs_Mixed11_Third <- vector(mode = "list",
                                        length = nrow(Assignments_Mixed11_Others$Third))
for (i in 1:length(replicate_pairs_Mixed11_Third)) {
  replicate_pairs_Mixed11_Third[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_Third[[i]][[1]]]
  replicate_pairs_Mixed11_Third[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_Third[[i]][[2]]]
}

replicate_pairs_Mixed11_Fourth <- vector(mode = "list",
                                         length = nrow(Assignments_Mixed11_Others$Fourth))
for (i in 1:length(replicate_pairs_Mixed11_Fourth)) {
  replicate_pairs_Mixed11_Fourth[[i]][[1]] <- SSND_data_acq[, pairs_Mixed11_Fourth[[i]][[1]]]
  replicate_pairs_Mixed11_Fourth[[i]][[2]] <- SSND_data_acq[, pairs_Mixed11_Fourth[[i]][[2]]]
}
