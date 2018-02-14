#' Identify participant-level exclusions based on participant factors
#'
#' Corresponds to Step 1.1:
#' \cr
#' identify subjects who do not meet participation criteria:
#' \describe{
#'   \item{(i)}{not monolingual English speakers}
#'   \item{(ii)}{did not use appropriate audio equipment for the study}
#'   \item{(iii)}{reported high familiarity with accent }
#' }
#'
#' @param d a data frame
#' @param duplicate_turkers a data frame of duplicate participants
#' @param newsurvey a boolean that is \code{TRUE} when the data frame has the updated survey questions
#' @return a data frame of workers who need to be excluded
#'
#' @export
non_eligible_step1_f <- function(d,
                                 duplicate_turkers,
                                 newsurvey=TRUE) {
  base_stuff <- d %>%
    filter(
      WorkerId %in% duplicate_turkers$WorkerId |
        LgBackground != "monolingual" |
        !(AudioType %in% c("in-ear", "over-ear"))
    ) %>%
    select(WorkerId,Condition)

  if (newsurvey==TRUE) {
    acc_familiarity <- d %>%
      filter(Answer.spec_acc_hear_last_week %in% c("daily_plus", NA) |
               Answer.spec_acc_interact_last_week %in% c("daily_plus", NA)) %>%
      select(WorkerId,Condition)
  } else {
    acc_familiarity <- d %>%
      filter(AccentFamFreq %in% c("all_time",NA)) %>%
      select(WorkerId,Condition)
  }
  rejected <- rbind(base_stuff,acc_familiarity)
  #If you don't have LgRating as a column, it won't stop it, but it will give a warning
  if ("LgRating" %in% names(d)) {
    rejected <-rbind(rejected,
                     d %>% filter(LgRating > 1) %>%
                       select(WorkerId,Condition))
  } else {
    warning("Yo, this dataframe doesn't have an LG rating!!!!")
  }
  rejected <- rejected %>% distinct(WorkerId, .keep_all = TRUE)
  return(rejected)
}

######################################################################################

#' Identify participant-level exclusions based on performance factors: 'cheating'
#'
#' Corresponds to Step 1.2.1:
#' \cr
#' identify subjects who have mean block RTs < 200ms
#'
#' @param df a data frame
#' @return a data frame of workers who need to be excluded
#'
#' @export
non_eligible_cheaters_f <- function(df) {
  df <- df %>%
    group_by(WorkerId,Block) %>%
    mutate(uBlockRT = mean(RT)) %>%
    ungroup() %>%
    filter(uBlockRT < 200) %>%
    select(WorkerId, Condition) %>%
    distinct(WorkerId, .keep_all = TRUE)
  return(df)
}

######################################################################################

#' Identify participant-level exclusions based on performance factors: outliers
#'
#' Corresponds to Step 1.2.2:
#' \cr
#' identify *eligible* subjects who had a mean block RT > 3 SDs from their Condition block mean.
#'
#' @param df a data frame
#' @return a data frame of workers who need to be excluded
#'
#' @export
non_eligible_step2_f <- function(df) {
  df <- df %>%
    # ZACHEDIT: to not include the practice block
    subset(Block %in% c("1","2","3","4","5")) %>%
    # from remaining subjs, identify subjects with atypical performance in *any* block (by condition)
    group_by(WorkerId, Condition, Block) %>%
    mutate(subjMean_BlockRT_temp = mean(RT)) %>%
    group_by(Condition, Block) %>%
    filter(abs(scale(subjMean_BlockRT_temp)) > 3) %>%
    ungroup() %>%
    select(WorkerId, Condition) %>%
    distinct(WorkerId, .keep_all = TRUE)
  return(df)
}

######################################################################################

#' Identify participants to be excluded
#'
#' Exclude all participants with the Step 1 ("participant-level") exclusion criteria:
#' \describe{
#'   \item{(i)}{not monolingual English speakers}
#'   \item{(ii)}{did not use appropriate audio equipment for the study}
#'   \item{(iii)}{reported high familiarity with accent}
#'   \item{(iv)}{subjects whose mean RT in any block was > 3 SDs from Condition mean}
#'   \item{(v)}{subjects who had a mean RT in any block < 200 ms}
#' }
#'
#' \strong{NOTE:} We did \emph{not} exclude participants based on mean RTs in the practice block, whereas our previous work had.
#' Additionally, our previous work \emph{did not exclude} participants with block means < 200ms
#' \cr
#' \strong{NOTE:} We did \emph{NOT} exclude participants for reasons 1-4 before calculating the standard deviation for reason 5
#'
#'
#' @param d a data frame
#' @param duplicate_turkers a data frame of duplicate participants
#' @param newsurvey a boolean that is \code{TRUE} when the data frame has the updated survey questions
#' @return a data frame of workers who need to be excluded, with reasons for exclusion
#'
#' @export
non_eligible_participants_f <- function(df,
                                        duplicate_turkers,
                                        newsurvey=TRUE) {
  ## ----------------------------------------
  # Complete Step 1.1
  non_eligible_subjs_01 <- non_eligible_step1_f(df, duplicate_turkers, newsurvey)
  d.step1_1 <- subset(df, !(WorkerId %in% non_eligible_subjs_01$WorkerId))
  ## ----------------------------------------
  # Complete Step 1.2.1
  non_eligible_cheaters <- non_eligible_cheaters_f(d.step1_1)
  d.step1_2_1 <- subset(d.step1_1, !(WorkerId %in% non_eligible_cheaters$WorkerId))
  ## ----------------------------------------
  # Complete Step 1.2.2
  non_eligible_outliers <- non_eligible_step2_f(d.step1_2_1)
  d.step1_2_2 <- subset(d.step1_2_1,
                               !(WorkerId %in% non_eligible_outliers$WorkerId))
  ## ----------------------------------------
  # collect all non eligible subjets into a single df
  non_eligible_subjs_all <- rbind(non_eligible_subjs_01 %>% mutate(Reason="Step1.1"),
                                  non_eligible_cheaters %>% mutate(Reason="Step1.2.1"),
                                  non_eligible_outliers %>% mutate(Reason="Step1.2.2"))
  return(non_eligible_subjs_all)
}
