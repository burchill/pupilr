#'
#' Does a few sanity checks on the experimental data
#'
#' Mostly designed as a sanity check for the pilot data so far
#'
#' @param df The data frame
#' @return Nothing
#' @export sanity_check_data
sanity_check_data <- function(df,pilot=TRUE) {
  # You better have dplyr
  if (requireNamespace("dplyr")) {
    
  } else {
    stop("You need dplyr to run this")
  }
  # Checks to see if all data grouped by a column have the same counts
  column_count_equal <- function(d,column_string) {
    checker <- d %>% group_by_(column_string) %>% summarise(n=n()) %>% select(n) %>% unique() %>% nrow()
    return(checker != 1)
  }
  # Checks the practice/baseline trial audio filenames
  # partofexp should be a string
  fixed_audio_files_per_trial_checker <- function(d,partofexp) {
    unique_practice_trials <- d %>%
      subset(PartOfExp==partofexp) %>%
      group_by(Trial,Filename) %>%
      summarise() %>% nrow()
    number_of_practice_trials <- d %>%
      subset(PartOfExp==partofexp) %>%
      select(Trial) %>% unique() %>% nrow()
    if (unique_practice_trials != number_of_practice_trials) {
      stop(paste0("Something's funky with the audio filenames for ",partofexp,"!"))
    }
  }
  # Checks to make sure there are no repeat participants
  no_repeat_workers <- function(d) {
    if (column_count_equal(d,"WorkerId")) {
      stop("Repeat participants!")
    }
  }

  # Checks the list stuff
  check_list_info <- function(d) {
    if (column_count_equal(d,"List")) {
      warning("Unbalanced lists!",immediate. = TRUE)
      d %>% mutate(Actual = paste0(Condition,List)) %>%
        group_by(Actual) %>%
        summarise(number_of_workers=length(unique(WorkerId)))  %>% print()
    }
    non_test <- d[d$PartOfExp=="main" & d$Block!="4",]
    any_bad_non_test <- FALSE %in% ifelse((grepl("shradd", non_test$Filename) & non_test$Condition=="accented") |
                                     (grepl("sarah", non_test$Filename) & non_test$Condition=="unaccented"),TRUE,FALSE)
    if (any_bad_non_test) {
      stop("There's a file with a wrong speaker in exposure!")
    }
    any_bad_test <- FALSE %in% grepl("shradd", d[d$Block=="4",]$Filename)
    if (any_bad_test) {
      stop("There's an unaccented file in the test block!")
    }
    d$ActualList <- paste0(d$Condition,d$List)
    list_count_stuff <- d[d$PartOfExp=="main",] %>%
      group_by(Block,ActualList) %>%
      mutate(Filesstring=paste0(sort(Filename),collapse="")) %>%
      group_by(Filesstring) %>%
      summarise(n=n(),e=first(Filename)) %>%
      as.data.frame()
    if (pilot) {
      if (FALSE %in% ((list_count_stuff$n==48 & grepl("sarah",list_count_stuff$e)) |
                      (list_count_stuff$n==80 & grepl("shradd",list_count_stuff$e))))
        stop("The blocks aren't all right!")
    }

  }
  # Check all the things
  fixed_audio_files_per_trial_checker(df,"practice")
  fixed_audio_files_per_trial_checker(df,"baseline")
  no_repeat_workers(df)
  check_list_info(df)
  print("Sanity check ok!")
}
