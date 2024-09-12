# Run PG_funrun.R first

# Load packages ---------------------------------------------

library(tidyverse)
library(irr)
library(psych)

# Compute ICC for each replicate group data frame created in PG_funrun.R ---------------------------------------------

# Replicate Pairs
ICC_measures_pairs_15 <-
  map(replicate_pairs_15_updated,
      ~ map_dfr(
        .x,
        ~ map2_dbl(
          .x[[1]]$df[, -1],
          .x[[2]]$df[, -1],
          ~ purrr::quietly(icc)(
            data.frame(.x, .y),
            model = "twoway",
            type = "consistency",
            unit = "single"
          )$result$value
        ),
        .progress = TRUE
      ))

# Equating ISEI within each stratum
ICC_measures_EqualScores <-
  map_dfr(
    replicate_pairs_updatedEqualScores,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Relationship Criteria
ICC_measures_relationship_criteria <-
  map(
    replicate_pairs_relationship_criteria_updated,
    ~ map_dfr(
      .x,
      ~ map2_dbl(
        .x[[1]]$df[, -1],
        .x[[2]]$df[, -1],
        ~ purrr::quietly(icc)(
          data.frame(.x, .y),
          model = "twoway",
          type = "consistency",
          unit = "single"
        )$result$value
      ),
      .progress = TRUE
    )
  )

# College educated
ICC_measures_collegeeducated <-
  map_dfr(
    replicate_pairs_college_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Triples
ICC_measures_triples <-
  map_dfr(replicate_triples_updated,
          ~ pmap(list(
            a = .x[[1]]$df[, -1],
            b = .x[[2]]$df[, -1],
            c = .x[[3]]$df[, -1]
          ), function(a, b, c) {
            purrr::quietly(icc)(data.frame(a, b, c),
                                model = "twoway",
                                type = "consistency",
                                unit = "single")$result$value
          }),
          .progress = TRUE)

# Replicate Pairs Mixed 14 (Top and Middle)
ICC_measures_pairs_Mixed14 <-
  map(replicate_pairs_Mixed14_updated,
      ~ map_dfr(
        .x,
        ~ map2_dbl(
          .x[[1]]$df[, -1],
          .x[[2]]$df[, -1],
          ~ purrr::quietly(icc)(
            data.frame(.x, .y),
            model = "twoway",
            type = "consistency",
            unit = "single"
          )$result$value
        ),
        .progress = TRUE
      ))

# Replicate Pairs Mixed 14 (Bottom)
ICC_measures_pairs_Mixed14_Bottom <-
  map_dfr(
    replicate_pairs_Mixed14_Bottom_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 14 (Top-Middle)
ICC_measures_pairs_Mixed14_TopMiddle <-
  map_dfr(
    replicate_pairs_Mixed14_TopMiddle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 14 (Bottom-Middle)
ICC_measures_pairs_Mixed14_BottomMiddle <-
  map_dfr(
    replicate_pairs_Mixed14_BottomMiddle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Extreme)
ICC_measures_pairs_Mixed13_Extreme <-
  map_dfr(
    replicate_pairs_Mixed13_Extreme_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Middle)
ICC_measures_pairs_Mixed13_Middle <-
  map_dfr(
    replicate_pairs_Mixed13_Middle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (First)
ICC_measures_pairs_Mixed13_First <-
  map_dfr(
    replicate_pairs_Mixed13_First_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Second)
ICC_measures_pairs_Mixed13_Second <-
  map_dfr(
    replicate_pairs_Mixed13_Second_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Third)
ICC_measures_pairs_Mixed13_Third <-
  map_dfr(
    replicate_pairs_Mixed13_Third_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Fourth)
ICC_measures_pairs_Mixed13_Fourth <-
  map_dfr(
    replicate_pairs_Mixed13_Fourth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Fifth)
ICC_measures_pairs_Mixed13_Fifth <-
  map_dfr(
    replicate_pairs_Mixed13_Fifth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Sixth)
ICC_measures_pairs_Mixed13_Sixth <-
  map_dfr(
    replicate_pairs_Mixed13_Sixth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Seventh)
ICC_measures_pairs_Mixed13_Seventh <-
  map_dfr(
    replicate_pairs_Mixed13_Seventh_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Eighth)
ICC_measures_pairs_Mixed13_Eighth <-
  map_dfr(
    replicate_pairs_Mixed13_Eighth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 13 (Ninth)
ICC_measures_pairs_Mixed13_Ninth <-
  map_dfr(
    replicate_pairs_Mixed13_Ninth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )


# Replicate Pairs Mixed 12 (Extreme Middle)
ICC_measures_pairs_Mixed12_ExtremeMiddle <-
  map_dfr(
    replicate_pairs_Mixed12_ExtremeMiddle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Middle)
ICC_measures_pairs_Mixed12_Middle <-
  map_dfr(
    replicate_pairs_Mixed12_Middle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (First)
ICC_measures_pairs_Mixed12_First <-
  map_dfr(
    replicate_pairs_Mixed12_First_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Second)
ICC_measures_pairs_Mixed12_Second <-
  map_dfr(
    replicate_pairs_Mixed12_Second_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Third)
ICC_measures_pairs_Mixed12_Third <-
  map_dfr(
    replicate_pairs_Mixed12_Third_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Fourth)
ICC_measures_pairs_Mixed12_Fourth <-
  map_dfr(
    replicate_pairs_Mixed12_Fourth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Fifth)
ICC_measures_pairs_Mixed12_Fifth <-
  map_dfr(
    replicate_pairs_Mixed12_Fifth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Sixth)
ICC_measures_pairs_Mixed12_Sixth <-
  map_dfr(
    replicate_pairs_Mixed12_Sixth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Seventh)
ICC_measures_pairs_Mixed12_Seventh <-
  map_dfr(
    replicate_pairs_Mixed12_Seventh_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 12 (Eighth)
ICC_measures_pairs_Mixed12_Eighth <-
  map_dfr(
    replicate_pairs_Mixed12_Eighth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (Extreme)
ICC_measures_pairs_Mixed11_Extreme <-
  map_dfr(
    replicate_pairs_Mixed11_Extreme_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (Middle)
ICC_measures_pairs_Mixed11_Middle <-
  map_dfr(
    replicate_pairs_Mixed11_Middle_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (First)
ICC_measures_pairs_Mixed11_First <-
  map_dfr(
    replicate_pairs_Mixed11_First_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (Second)
ICC_measures_pairs_Mixed11_Second <-
  map_dfr(
    replicate_pairs_Mixed11_Second_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (Third)
ICC_measures_pairs_Mixed11_Third <-
  map_dfr(
    replicate_pairs_Mixed11_Third_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )

# Replicate Pairs Mixed 11 (Fourth)
ICC_measures_pairs_Mixed11_Fourth <-
  map_dfr(
    replicate_pairs_Mixed11_Fourth_updated,
    ~ map2_dbl(
      .x[[1]]$df[, -1],
      .x[[2]]$df[, -1],
      ~ purrr::quietly(icc)(
        data.frame(.x, .y),
        model = "twoway",
        type = "consistency",
        unit = "single"
      )$result$value
    ),
    .progress = TRUE
  )
