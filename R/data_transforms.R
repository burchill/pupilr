#' Calculated adjusted RTs
#'
#' Calculates each subject's mean Baseline RT and subtracts that value from
#' the experimental RTs.
#'
#' @param df a data frame (pre-processed by \code{pupilr})
#' @return the input data frame, with the new columns, \code{meanBaselineRT}, \code{AdjustedRT}, and \code{meanAdjustedRT}, the last of which represents the participant's mean adjusted RT for the experimental trials
#'
#' @export
calculate_adjustedRT_f <- function(df) {
  df <- df %>%
    group_by(WorkerId) %>%
    mutate(
      # calculate subject-wise mean RTs during baseline block
      meanBaselineRT = mean(RT[PartOfExp == "baseline"]),

      # calculate normalized RTs
      AdjustedRT = RT - meanBaselineRT,

      # calculate subject-wise mean Adjusted RT across Blocks 1-4
      meanAdjustedRT = mean(AdjustedRT[PartOfExp == "main"])
    ) %>% ungroup()
  return(df)
}

########################################################################################

#' Calculate Z-scores for mean block RTs
#'
#' Calculates the z-scores \strong{within Conditions} and within block number for participants' mean block RT.
#' Will overwrite any columns named \code{BlockmeanRT} or \code{Rt.Z}.
#'
#' @param df a data frame (pre-processed by \code{pupilr})
#' @return the input data frame, with the new columns, \code{BlockmeanRT}, \code{Rt.Z}
#'
#' @export
redo_Z_scores <- function(df) {
  zscores_df <- df %>% group_by(WorkerId,Condition,Block) %>%
    summarise(BlockmeanRT=mean(RT)) %>%
    group_by(Condition,Block) %>%
    mutate(Rt.Z = (BlockmeanRT-mean(BlockmeanRT))/sd(BlockmeanRT)) %>%
    ungroup() %>%
    select(WorkerId,Block,Rt.Z)
  return(left_join_and_overwrite(df,zscores_df,by=c("WorkerId","Block")))
}

#' Initialize scores for data
#'
#' Adds/overwrites columns: \code{Phase}, \code{subjMean_BlockRT}, \code{subjMean_BlockAcc},
#' \code{BlockmeanRT}, and \code{Rt.Z}
#'
#' @param df a data frame (pre-processed by \code{pupilr})
#' @return the input data frame but with the new columns
#'
#' @export
initialize_scores <- function(df) {
  df %<>%
    mutate(Phase=as.factor(ifelse(Block=="4","Test phase",
                                  ifelse(Block %in% c("1","2","3"),"Exposure phase",NA)))) %>%
    group_by(WorkerId, Condition, Block) %>%
    mutate(
      subjMean_BlockRT = mean(RT),
      subjMean_BlockAcc = mean(BinaryCorrect)
    ) %>%
    ungroup() %>%
    redo_Z_scores() %>%
    as.data.frame()
  return(df)
}
