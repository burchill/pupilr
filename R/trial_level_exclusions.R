#' Remove trial-level exclusions
#'
#' This excludes trials with RTs < 200 ms and RTs with that are greater than 3 SDs
#' from the worker's mean RT.
#'
#' @param df the data frame
#' @return the same data frame, but without the excluded RT
#' @export
exclude_extreme_rts_f <- function(df) {
  df <- df %>%
    # Remove trials with extremely long RTs
    filter(
      RT >= 200
    ) %>%
    # remove subject-wise RTs based on 3 SDs
    group_by(WorkerId) %>%
    filter(
      abs(scale(RT)) <= 3
    ) %>%
    # calculate subject-wise mean RTs *after* trial-wise outlier exclusion
    group_by(WorkerId, Block) %>%
    mutate(
      subjMean_BlockRT_after_trial_exclusion = mean(RT)
    ) %>%
    ungroup()
  return(df)
}
