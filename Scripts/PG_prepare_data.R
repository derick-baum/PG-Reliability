# Load packages ---------------------------------------------

library(tidyverse)
library(haven)
library(readxl)

# Load SSND and strata allocation data ---------------------------------------------

SSND_data <-
  read_dta("SSND_PGitems_Replication_Data.dta")
SSND_strata_assignment <-
  read_excel("Strata_assignment.xlsx")

## Prepare PG data ---------------------------------------------

SSND_occup_names <- SSND_strata_assignment$occname

# Rename columns

SSND_occup_names_reordered <- c(
  SSND_occup_names[1],
  SSND_occup_names[25],
  SSND_occup_names[8],
  SSND_occup_names[12],
  SSND_occup_names[29],
  SSND_occup_names[7],
  SSND_occup_names[6],
  SSND_occup_names[9],
  SSND_occup_names[13],
  SSND_occup_names[10],
  SSND_occup_names[2],
  SSND_occup_names[14],
  SSND_occup_names[17],
  SSND_occup_names[3],
  SSND_occup_names[4],
  SSND_occup_names[11],
  SSND_occup_names[5],
  SSND_occup_names[18],
  SSND_occup_names[16],
  SSND_occup_names[15],
  SSND_occup_names[30],
  SSND_occup_names[22],
  SSND_occup_names[19],
  SSND_occup_names[23],
  SSND_occup_names[21],
  SSND_occup_names[27],
  SSND_occup_names[20],
  SSND_occup_names[28],
  SSND_occup_names[26],
  SSND_occup_names[24]
)

colnames(SSND_data) <- c(SSND_occup_names_reordered, "college")

# Acquaintanceship data

SSND_data_acq <- SSND_data[, 1:30] %>%
  mutate(across(everything(), ~ case_when(. == 0 ~ 0, TRUE ~ 1)))

# Relative data

SSND_data_rel <- SSND_data[, 1:30] %>%
  mutate(across(everything(), ~ case_when(. == 1 ~ 1, TRUE ~ 0)))

# Friend data

SSND_data_frd <- SSND_data[, 1:30] %>%
  mutate(across(everything(), ~ case_when(. == 1 |
                                            . == 2 ~ 1, TRUE ~ 0)))

# College-educated

SSND_data_college <- SSND_data_acq[as.logical(as.vector((SSND_data$college))), ]

## Modify and clean strata allocation data ---------------------------------------------

SSND_strata_assignment$Class_Full <-
  factor(
    SSND_strata_assignment$Class_Full,
    levels = c("VIIa", "VI", "V", "IVc", "IIIb", "IIIa", "II", "I")
  )
SSND_strata_assignment <- SSND_strata_assignment %>%
  rename(
    SEIStrat_2 = SEIStrat,
    SEInum_2 = SEInum,
    USstrat_2 = USstrat,
    USnum_2 = USnum
  )

SSND_strata_ISEIArrange_3 <- SSND_strata_assignment %>%
  arrange(desc(ISEI))
SSND_strata_ISEIArrange_3$SEIStrat_3 <- rep(1:10, each = 3)
SSND_strata_ISEIArrange_3$SEInum_3 <- rep(1:3, 10)
SSND_strata_ISEIArrange_3 <- SSND_strata_ISEIArrange_3 %>%
  dplyr::select(occid, SEIStrat_3, SEInum_3)
SSND_strata_USPrestArrange_3 <- SSND_strata_assignment %>%
  arrange(desc(USPrest))
SSND_strata_USPrestArrange_3$USstrat_3 <- rep(1:10, each = 3)
SSND_strata_USPrestArrange_3$USnum_3 <- rep(1:3, 10)
SSND_strata_USPrestArrange_3 <- SSND_strata_USPrestArrange_3 %>%
  dplyr::select(occid, USstrat_3, USnum_3)

SSND_ISEIClassArrange <- SSND_strata_assignment %>%
  arrange(desc(Class_Full), desc(ISEI))
SSND_USPrestClassArrange <- SSND_strata_assignment %>%
  arrange(desc(Class_Full), desc(USPrest))

SSND_ISEIClassArrange$ISEI_ClassStrat_2 <- rep(1:15, each = 2)
SSND_ISEIClassArrange$ISEI_ClassStrat_3 <- rep(1:10, each = 3)
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed14_Top <-
  c(rep(1:2, each = 3), rep(3:14, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed14_TopMiddle <-
  c(rep(1:3, each = 2), rep(4:5, each = 3), rep(6:14, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed14_Middle <-
  c(rep(1:6, each = 2), rep(7:8, each = 3), rep(9:14, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed14_Bottom <-
  c(rep(1:12, each = 2), rep(13:14, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed14_BottomMiddle <-
  c(rep(1:9, each = 2), rep(10:11, each = 3), rep(12:14, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Extreme <-
  c(rep(1:2, each = 3), rep(3:11, each = 2), rep(12:13, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Middle <-
  c(rep(1:4, each = 2),
    rep(5:6, each = 3),
    rep(7, 2),
    rep(8:9, each = 3),
    rep(10:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_First <-
  c(rep(1:4, each = 3), rep(5:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Second <-
  c(rep(1:2, each = 3),
    rep(3:5, each = 2),
    rep(6:7, each = 3),
    rep(8:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Third <-
  c(rep(1:2, each = 3),
    rep(3:8, each = 2),
    rep(9:10, each = 3),
    rep(11:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Fourth <-
  c(rep(1:3, each = 2), rep(4:7, each = 3), rep(8:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Fifth <-
  c(
    rep(1:3, each = 2),
    rep(4:5, each = 3),
    rep(6:8, each = 2),
    rep(9:10, each = 3),
    rep(11:13, each = 2)
  )
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Sixth <-
  c(rep(1:3, each = 2),
    rep(4:5, each = 3),
    rep(6:11, each = 2),
    rep(12:13, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Seventh <-
  c(rep(1:6, each = 2), rep(7:10, each = 3), rep(11:13, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Eighth <-
  c(rep(1:6, each = 2),
    rep(7:8, each = 3),
    rep(9:11, each = 2),
    rep(12:13, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed13_Ninth <-
  c(rep(1:9, each = 2), rep(10:13, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_ExtremeMiddle <-
  c(
    rep(1:2, each = 3),
    rep(3:5, each = 2),
    rep(6:7, each = 3),
    rep(8:10, each = 2),
    rep(11:12, each = 3)
  )
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Middle <-
  c(rep(1:3, each = 2), rep(4:9, each = 3), rep(10:12, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_First <-
  c(rep(1:6, each = 3), rep(7:12, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Second <-
  c(rep(1:4, each = 3),
    rep(5:7, each = 2),
    rep(8:9, each = 3),
    rep(10:12, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Third <-
  c(rep(1:4, each = 3), rep(5:10, each = 2), rep(11:12, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Fourth <-
  c(rep(1:2, each = 3),
    rep(3:5, each = 2),
    rep(6:9, each = 3),
    rep(10:12, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Fifth <-
  c(rep(1:2, each = 3), rep(3:8, each = 2), rep(9:12, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Sixth <-
  c(rep(1:3, each = 2),
    rep(4:7, each = 3),
    rep(8:10, each = 2),
    rep(11:12, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Seventh <-
  c(rep(1:3, each = 2),
    rep(4:5, each = 3),
    rep(6:8, each = 2),
    rep(9:12, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed12_Eighth <-
  c(rep(1:6, each = 2), rep(7:12, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_Extreme <-
  c(rep(1:4, each = 3), rep(5:7, each = 2), rep(8:11, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_Middle <-
  c(rep(1, 2),
    rep(2:5, each = 3),
    rep(6, 2),
    rep(7:10, each = 3),
    rep(11, 2))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_First <-
  c(rep(1:3, each = 2), rep(4:11, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_Second <-
  c(rep(1:2, each = 3), rep(3:5, each = 2), rep(6:11, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_Third <-
  c(rep(1:6, each = 3), rep(7:9, each = 2), rep(10:11, each = 3))
SSND_ISEIClassArrange$ISEI_ClassStrat_Mixed11_Fourth <-
  c(rep(1:8, each = 3), rep(9:11, each = 2))

SSND_ISEIClassArrange$ISEI_ClassStratNum_2 <- rep(1:2, 15)
SSND_ISEIClassArrange$ISEI_ClassStratNum_3 <- rep(1:3, 10)
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed14_Top <-
  c(rep(1:3, 2), rep(1:2, 12))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed14_TopMiddle <-
  c(rep(1:2, 3), rep(1:3, 2), rep(1:2, 9))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed14_Middle <-
  c(rep(1:2, 6), rep(1:3, 2), rep(1:2, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed14_Bottom <-
  c(rep(1:2, 12), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed14_BottomMiddle <-
  c(rep(1:2, 9), rep(1:3, 2), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Extreme <-
  c(rep(1:3, 2), rep(1:2, 9), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Middle <-
  c(rep(1:2, 4), rep(1:3, 2), rep(1:2, 1), rep(1:3, 2), rep(1:2, 4))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_First <-
  c(rep(1:3, 4), rep(1:2, 9))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Second <-
  c(rep(1:3, 2), rep(1:2, 3), rep(1:3, 2), rep(1:2, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Third <-
  c(rep(1:3, 2), rep(1:2, 6), rep(1:3, 2), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Fourth <-
  c(rep(1:2, 3), rep(1:3, 4), rep(1:2, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Fifth <-
  c(rep(1:2, 3), rep(1:3, 2), rep(1:2, 3), rep(1:3, 2), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Sixth <-
  c(rep(1:2, 3), rep(1:3, 2), rep(1:2, 6), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Seventh <-
  c(rep(1:2, 6), rep(1:3, 4), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Eighth <-
  c(rep(1:2, 6), rep(1:3, 2), rep(1:2, 3), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed13_Ninth <-
  c(rep(1:2, 9), rep(1:3, 4))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_ExtremeMiddle <-
  c(rep(1:3, 2),
    rep(1:2, 3),
    rep(1:3, 2),
    rep(1:2, 3),
    rep(1:3, each = 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Middle <-
  c(rep(1:2, 3), rep(1:3, 6), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_First <-
  c(rep(1:3, 6), rep(1:2, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Second <-
  c(rep(1:3, 4), rep(1:2, 3), rep(1:3, 2), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Third <-
  c(rep(1:3, 4), rep(1:2, 6), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Fourth <-
  c(rep(1:3, 2), rep(1:2, 3), rep(1:3, 4), rep(1:2, 3))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Fifth <-
  c(rep(1:3, 2), rep(1:2, 6), rep(1:3, 4))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Sixth <-
  c(rep(1:2, 3), rep(1:3, 4), rep(1:2, 3), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Seventh <-
  c(rep(1:2, 3), rep(1:3, 2), rep(1:2, 3), rep(1:3, 4))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed12_Eighth <-
  c(rep(1:2, 6), rep(1:3, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_Extreme <-
  c(rep(1:3, 4), rep(1:2, 3), rep(1:3, 4))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_Middle <-
  c(rep(1:2, 1), rep(1:3, 4), rep(1:2, 1), rep(1:3, 4), rep(1:2, 1))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_First <-
  c(rep(1:2, 3), rep(1:3, 8))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_Second <-
  c(rep(1:3, 2), rep(1:2, 3), rep(1:3, 6))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_Third <-
  c(rep(1:3, 6), rep(1:2, 3), rep(1:3, 2))
SSND_ISEIClassArrange$ISEI_ClassStratNum_Mixed11_Fourth <-
  c(rep(1:3, 8), rep(1:2, 3))

SSND_ISEIClassArrange <- SSND_ISEIClassArrange %>%
  dplyr::select(
    occid,
    ISEI_ClassStrat_2,
    ISEI_ClassStrat_3,
    ISEI_ClassStrat_Mixed14_Top,
    ISEI_ClassStrat_Mixed14_Middle,
    ISEI_ClassStrat_Mixed14_Bottom,
    ISEI_ClassStrat_Mixed14_TopMiddle,
    ISEI_ClassStrat_Mixed14_BottomMiddle,
    ISEI_ClassStrat_Mixed13_Extreme,
    ISEI_ClassStrat_Mixed13_Middle,
    ISEI_ClassStrat_Mixed13_First,
    ISEI_ClassStrat_Mixed13_Second,
    ISEI_ClassStrat_Mixed13_Third,
    ISEI_ClassStrat_Mixed13_Fourth,
    ISEI_ClassStrat_Mixed13_Fifth,
    ISEI_ClassStrat_Mixed13_Sixth,
    ISEI_ClassStrat_Mixed13_Seventh,
    ISEI_ClassStrat_Mixed13_Eighth,
    ISEI_ClassStrat_Mixed13_Ninth,
    ISEI_ClassStrat_Mixed12_ExtremeMiddle,
    ISEI_ClassStrat_Mixed12_Middle,
    ISEI_ClassStrat_Mixed12_First,
    ISEI_ClassStrat_Mixed12_Second,
    ISEI_ClassStrat_Mixed12_Third,
    ISEI_ClassStrat_Mixed12_Fourth,
    ISEI_ClassStrat_Mixed12_Fifth,
    ISEI_ClassStrat_Mixed12_Sixth,
    ISEI_ClassStrat_Mixed12_Seventh,
    ISEI_ClassStrat_Mixed12_Eighth,
    ISEI_ClassStrat_Mixed11_Extreme,
    ISEI_ClassStrat_Mixed11_Middle,
    ISEI_ClassStrat_Mixed11_First,
    ISEI_ClassStrat_Mixed11_Second,
    ISEI_ClassStrat_Mixed11_Third,
    ISEI_ClassStrat_Mixed11_Fourth,
    ISEI_ClassStratNum_2,
    ISEI_ClassStratNum_3,
    ISEI_ClassStratNum_Mixed14_Top,
    ISEI_ClassStratNum_Mixed14_Middle,
    ISEI_ClassStratNum_Mixed14_Bottom,
    ISEI_ClassStratNum_Mixed14_TopMiddle,
    ISEI_ClassStratNum_Mixed14_BottomMiddle,
    ISEI_ClassStratNum_Mixed13_Extreme,
    ISEI_ClassStratNum_Mixed13_Middle,
    ISEI_ClassStratNum_Mixed13_First,
    ISEI_ClassStratNum_Mixed13_Second,
    ISEI_ClassStratNum_Mixed13_Third,
    ISEI_ClassStratNum_Mixed13_Fourth,
    ISEI_ClassStratNum_Mixed13_Fifth,
    ISEI_ClassStratNum_Mixed13_Sixth,
    ISEI_ClassStratNum_Mixed13_Seventh,
    ISEI_ClassStratNum_Mixed13_Eighth,
    ISEI_ClassStratNum_Mixed13_Ninth,
    ISEI_ClassStratNum_Mixed12_ExtremeMiddle,
    ISEI_ClassStratNum_Mixed12_Middle,
    ISEI_ClassStratNum_Mixed12_First,
    ISEI_ClassStratNum_Mixed12_Second,
    ISEI_ClassStratNum_Mixed12_Third,
    ISEI_ClassStratNum_Mixed12_Fourth,
    ISEI_ClassStratNum_Mixed12_Fifth,
    ISEI_ClassStratNum_Mixed12_Sixth,
    ISEI_ClassStratNum_Mixed12_Seventh,
    ISEI_ClassStratNum_Mixed12_Eighth,
    ISEI_ClassStratNum_Mixed11_Extreme,
    ISEI_ClassStratNum_Mixed11_Middle,
    ISEI_ClassStratNum_Mixed11_First,
    ISEI_ClassStratNum_Mixed11_Second,
    ISEI_ClassStratNum_Mixed11_Third,
    ISEI_ClassStratNum_Mixed11_Fourth
  )

SSND_USPrestClassArrange$USPrest_ClassStrat_2 <- rep(1:15, each = 2)
SSND_USPrestClassArrange$USPrest_ClassStrat_3 <- rep(1:10, each = 3)
SSND_USPrestClassArrange$USPrest_ClassStratNum_2 <- rep(1:2, 15)
SSND_USPrestClassArrange$USPrest_ClassStratNum_3 <- rep(1:3, 10)
SSND_USPrestClassArrange <- SSND_USPrestClassArrange %>%
  dplyr::select(
    occid,
    USPrest_ClassStrat_2,
    USPrest_ClassStrat_3,
    USPrest_ClassStratNum_2,
    USPrest_ClassStratNum_3
  )

df_list <-
  list(
    SSND_strata_assignment,
    SSND_strata_ISEIArrange_3,
    SSND_strata_USPrestArrange_3,
    SSND_ISEIClassArrange,
    SSND_USPrestClassArrange
  )

SSND_strata_assignment_FULL <-
  reduce(df_list, left_join, by = "occid") %>%
  relocate(Class_Full, Class_Coarse, .after = ISEI) %>%
  relocate(
    ISEI_ClassStrat_2,
    ISEI_ClassStratNum_2,
    USPrest_ClassStrat_2,
    USPrest_ClassStratNum_2,
    .after = USnum_2
  )
