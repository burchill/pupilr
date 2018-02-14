# To cleanse one's palette:
# rm(list = ls())
library(plyr)
library(dplyr)  # MAKE SURE THIS LOADS AFTER plyr, or errors can arise
require(compiler)
library(stringr)
library(tidyr)
library(digest)

## --------------------------------- ##
## Function for making unique identifier
## --------------------------------- ##

make_unique_id <- function(df) {
  return(paste0(df$WorkerId,"+",df$hitid))
}

## ------------------------------------- ##
## Function for making a df to check workers
## ------------------------------------- ##

make_worker_id_compendium <- function(df_list) {
  df_list_nested <- purrr::map(
    df_list,
    function(x) {
      nested_df <- x %>%
        dplyr::group_by(WorkerId,UniqueID,hitid,assignmentaccepttime,assignmentsubmittime,Condition,List,Run) %>%
        tidyr::nest()
      return(nested_df)
    })
  df.all <- purrr::reduce(df_list_nested, rbind) %>%
            distinct(UniqueID,.keep_all=TRUE)
  if (FALSE %in% (make_unique_id(df.all) == df.all$UniqueID)) {
    stop("UniqueIDs and WorkerIds + hitids do not match up! (in 'make_worker_id_compendium')")
  }
  d.repeatWorker <- df.all %>%
    dplyr::group_by(WorkerId) %>%
    dplyr::mutate(n=n()) %>%
    subset(n > 1) %>%
    select(WorkerId,hitid,Condition,List,assignmentaccepttime,assignmentsubmittime) %>%
    as.data.frame()
  if (nrow(d.repeatWorker) > 0) {
    warning_message=paste0("There are ",length(unique(d.repeatWorker$WorkerId))," worker(s) who took this experiment more than once!")
    warning(warning_message)
    a <-capture.output(print(d.repeatWorker))
    c <- paste(a, "\n", sep="")
    cat("These workers took the experiment more than once:\n", c, "\n")
  }
  return(df.all)
}

## --------------------------------------------------- ##
## Make survey
## --------------------------------------------------- ##
preprocess_survey_data_exp1 <- function(d) {
  d.survey = d %>%
    select(WorkerId,
           hitid, hittypeid, creationtime, assignmentid, assignmentaccepttime, assignmentsubmittime,
           Answer.accent_familiarity_frequency:Answer.language_background_free,
           Answer.list, Answer.rsrb.age:Answer.userAgent,UniqueID)
  # Get rid of stuff we don't want
  d.survey$Answer.errors = NULL
  d.survey$Answer.label = NULL
  d.survey$Answer.speaker = NULL
  d.survey$Answer.baselineResp = NULL

  # Did they recognize the accent?
  d.survey$RecognizeAccent <- ifelse(grepl("india|pakistan|south asia|bangladesh|delhi|mumbai|bombay|calcutta|kolkata|chennai",
                                           d.survey$Answer.guess_accent, ignore.case = TRUE),
                                     "yes", "no")

  d.survey<-dplyr::rename(d.survey,AudioQual = Answer.audio_qual,
                          AudioType = Answer.audio_type,
                          AudioStall = Answer.audio_stall,
                          AccentFamFreq = Answer.accent_familiarity_frequency,
                          AccentFamPlace = Answer.accent_familiarity_place,
                          Comments = Answer.comments,
                          GuessAccent = Answer.guess_accent,
                          LgBackground = Answer.language_background,
                          LgBackgroundFree = Answer.language_background_free,
                          AccentExpos = Answer.specific_language_background,
                          Condition = Answer.condition,
                          List = Answer.list)
  return(d.survey)
}

## ---------------------------------------------------- ##
## Function for processing data for Exp 1 (no exposure)
## ---------------------------------------------------- ##

preprocess_data_exp1 <- function(d,word_info,
                                 merge=TRUE,wording_test=FALSE,load_times=FALSE) {
  #Taking care of the worker id's right off the bat
  d <- plyr::rename(d,c("workerid" = "WorkerId"))
  d$WorkerId <- sapply(as.character(d$WorkerId), digest, algo="md5",USE.NAMES = FALSE)
  d$UniqueID<-make_unique_id(d)

  # Break up everything into practice, main, and baseline
#   d.practice = ldply(strsplit(unlist(strsplit(as.character(d$Answer.practiceResp), ';')), ','))
#   d.main =     ldply(strsplit(unlist(strsplit(as.character(d$Answer.mainResp), ';')), ','))
#   d.baseline = ldply(strsplit(unlist(strsplit(as.character(d$Answer.baselineResp), ';')), ','))
  # Zach's new fancy, safer(?) way below:

  # Because I was accidentally using a version that had an empty spot for "speaker" and thus had an extra comma at end
  # (and that worked) I now add a comma on the end if it doesn't have one
  extra_comma=""
  if (str_sub(
        strsplit(as.character(d[1,]$Answer.practiceResp), ';')[[1]][1],
        -1,-1) != ",") {
    extra_comma=","
  }
  d.practice = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.practiceResp), ';'),
                                                   d$UniqueID,
                                                   .f=function(.x,.y) lapply(.x,function(x) paste0(x,extra_comma,.y,",")))), ','))
  d.main =     ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.mainResp), ';'),
                                                   d$UniqueID,
                                                   .f=function(.x,.y) lapply(.x,function(x) paste0(x,extra_comma,.y,",")))), ','))
  d.baseline = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.baselineResp), ';'),
                                                   d$UniqueID,
                                                   .f=function(.x,.y) lapply(.x,function(x) paste0(x,extra_comma,.y,",")))), ','))
  # If you're doing the wording test, add that separate block in
  if (wording_test==TRUE) {
      d.newtest = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.newtestResp), ';'),
                                                    d$UniqueID,
                                                    .f=function(.x,.y) lapply(.x,function(x) paste0(x,extra_comma,.y,",")))), ','))
      # And in the darkness bind them
      d.all = rbind(d.practice, d.main, d.baseline,d.newtest)
  } else {
      # And in the darkness bind them
      d.all = rbind(d.practice, d.main, d.baseline)
  }
  # If you have load times, do stuff a bit differently (ie include loadtimes)
  if (load_times == TRUE) {
      # Rename columns
      names(d.all) = c("PartOfExp", "Trial", "Filename", "Word", "KeyPressed", "Response", "RTStart", "RTEnd", "RT", "WorkerId", "Condition", "List","LoadTime","UniqueID")
      d.all$LoadTime = as.numeric(d.all$LoadTime)
  } else {
      # Rename columns
       names(d.all) = c("PartOfExp", "Trial", "Filename", "Word", "KeyPressed", "Response", "RTStart", "RTEnd", "RT", "WorkerId", "Condition", "List","UniqueID")
  }

  # Fix the WorkerIds again:
  d.all$WorkerId <- sapply(as.character(d.all$WorkerId), digest, algo="md5",USE.NAMES = FALSE)

  # Make variables the right types
  d.all$Trial = as.integer(d.all$Trial)
  d.all$KeyPressed= as.integer(d.all$KeyPressed)
  d.all$RTStart= as.numeric(d.all$RTStart)
  d.all$RTEnd= as.numeric(d.all$RTEnd)
  d.all$RT= as.numeric(d.all$RT)

  # Add in the answer key
  d.all <- left_join(d.all,word_info,by="Filename")
  # Label the answers correct or not
  d.all$BinaryCorrect <- ifelse(d.all$Response=="Same" & d.all$Word == d.all$SpokenWord,1,
                                ifelse(d.all$Response=="Different" & d.all$Word != d.all$SpokenWord,1,
                                       0))
  # Give the rows Block values
  d.all$Block <- factor(ifelse(d.all$PartOfExp=="practice",0,
                        ifelse(d.all$PartOfExp=="baseline",5,
                        ifelse(d.all$PartOfExp=="newtest", 4,
                               floor(d.all$Trial/8)+1))))
  d.all$CGBlock <- factor(ifelse(d.all$PartOfExp=="practice",0,
                               ifelse(d.all$PartOfExp=="baseline",0,
                                      floor(d.all$Trial/4)+1)))

  # Calculate adjusted RTs (i.e., RT on each trial minus subject's baseline RT)
  d.all <- d.all %>%
    group_by(WorkerId) %>%
    mutate(
      # calculate each subject's mean RT in the final block
      BaselineRT = mean(RT[Block == "5"]),
      # calculate adjusted RT
      AdjustedRT = RT - BaselineRT
    ) %>%
    as.data.frame(.)

  # calculate z-scored baseline RT (for easy outlier exclusion)
  d.all <- within(d.all, {
    BaselineRT.Z <- ifelse(Block == "5", RT, NA)
    BaselineRT.Z <- as.numeric(scale(BaselineRT.Z, center = TRUE, scale = TRUE))
  })

  d.all$TrialInExperiment <- ifelse(d.all$PartOfExp == "newtest", d.all$Trial+30, # Needs to come first
                             ifelse(d.all$Block == "5", d.all$Trial+38,
                             ifelse(d.all$Block %in% c("1","2","3","4"), d.all$Trial+6,
                                    d.all$Trial)))

  ## ------------------------------------- ##
  ## Getting the preprocessed survey data
  ## ------------------------------------- ##
  d.survey <- preprocess_survey_data_exp1(d)

  if (merge) {
    d.survey$WorkerId = NULL
    d.survey$Condition = NULL
    d.survey$List = NULL
    d.both <- dplyr::left_join(d.all,d.survey,by="UniqueID")
    return(d.both)
  }
  return(list(d.all,d.survey))
}

## ----------------------------------------------------- ##
## Function for processing non-exposure data for Exp 2 (LONG EXPOSURE)
## ----------------------------------------------------- ##

preprocess_data_exp2_test <- function(d,word_info,pilot_bool=TRUE) {
  #Taking care of the worker id's right off the bat
  d <- plyr::rename(d,c("workerid" = "WorkerId"))
  d$WorkerId <- sapply(as.character(d$WorkerId), digest, algo="md5",USE.NAMES = FALSE)
  d$UniqueID<-make_unique_id(d)

  # Because I was accidentally using a version that had an empty spot for "speaker" and thus had an extra comma at end
  # (and that worked) I now add a comma on the end if it doesn't have one
  extra_comma=""
  if (str_sub(
    strsplit(as.character(d[1,]$Answer.practiceResp), ';')[[1]][1],
    -1,-1) != ",") {
    extra_comma=","
  }
  # Because I didn't know you shouldn't have two blocks with the same namespace, you have to do this
  test_cleaner = function(x,y) {
    lapply(x,function(xprime) {
      block_s<-xprime
      if (substr(block_s,1,1)=="|") {
        block_s<-str_sub(block_s,2)
      }
      paste0(block_s,extra_comma,y,",")
    })
  }

  d.practice = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.practiceResp), ';'),
                                                 d$UniqueID,
                                                 .f=test_cleaner)), ','))
  d.baseline = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.baselineResp), ';'),
                                                 d$UniqueID,
                                                 .f=test_cleaner)), ','))
  if (pilot_bool==TRUE) {
    d.main =   ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.mainResp), ';'),
                                                 d$UniqueID,
                                                 .f=test_cleaner)), ','))
  } else {
    d.main =   ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d[,"Answer.main0Resp"]), ';'),
                                                 d$UniqueID,
                                                 .f=test_cleaner)), ','))
    for (i_main in c("1","2","3")) {
      d.main = rbind(d.main,
               ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d[,paste0("Answer.main",i_main,"Resp")]), ';'),
                                                 d$UniqueID,
                                                 .f=test_cleaner)), ',')))

    }
    d.main$V1 = "main"

  }

  d.all = rbind(d.practice, d.main, d.baseline)

  names(d.all) = c("PartOfExp", "Trial", "Filename", "Word", "KeyPressed", "Response", "RTStart", "RTEnd", "RT", "WorkerId", "Condition", "List","LoadTime","UniqueID")

  # Fix the WorkerIds again:
  d.all$WorkerId <- sapply(as.character(d.all$WorkerId), digest, algo="md5",USE.NAMES = FALSE)

  # Make variables the right types
  d.all$Trial = as.integer(d.all$Trial)
  d.all$KeyPressed= as.integer(d.all$KeyPressed)
  d.all$RTStart= as.numeric(d.all$RTStart)
  d.all$RTEnd= as.numeric(d.all$RTEnd)
  d.all$RT= as.numeric(d.all$RT)
  d.all$LoadTime = as.numeric(d.all$LoadTime)

  # Add in the answer key
  d.all <- left_join(d.all,word_info,by="Filename")
  # Label the answers correct or not
  d.all$BinaryCorrect <- ifelse(d.all$Response=="Same" & d.all$Word == d.all$SpokenWord,1,
                                ifelse(d.all$Response=="Different" & d.all$Word != d.all$SpokenWord,1,
                                       0))

  # Sequentially number the trials and put in block values
  d.all <- d.all %>% group_by(WorkerId) %>%
    arrange(RTStart) %>%
    mutate(TrialInExperiment=seq_len(n())-1) %>%
    ungroup() %>%
    mutate(Block=factor(ifelse(PartOfExp=="practice","0",
                        ifelse(PartOfExp=="baseline","5",
                               as.character(
                                 floor((TrialInExperiment -
                                          max(TrialInExperiment[PartOfExp=="practice"])+7)/8))))))

  # Calculate adjusted RTs (i.e., RT on each trial minus subject's baseline RT)
  d.all <- d.all %>%
    group_by(WorkerId) %>%
    mutate(
      # calculate each subject's mean RT in the final block
      BaselineRT = mean(RT[Block == "5"]),
      # calculate adjusted RT
      AdjustedRT = RT - BaselineRT
    ) %>%
    as.data.frame(.)

  # calculate z-scored baseline RT (for easy outlier exclusion)
  d.all <- within(d.all, {
    BaselineRT.Z <- ifelse(Block == "5", RT, NA)
    BaselineRT.Z <- as.numeric(scale(BaselineRT.Z, center = TRUE, scale = TRUE))
  })
  return(d.all)
}

## ------------------------------------- ##
## Preprocessing the exposure data for Exp2
## ------------------------------------- ##
preprocess_data_exp2_exposure <- function(d,pilot_bool=TRUE) {
  #Taking care of the worker id's right off the bat
  d <- plyr::rename(d,c("workerid" = "WorkerId"))
  d$WorkerId <- sapply(as.character(d$WorkerId), digest, algo="md5",USE.NAMES = FALSE)
  d$UniqueID<-make_unique_id(d)

  # Because I didn't know you shouldn't have two blocks with the same namespace, you have to do this
  exposure_cleaner = function(x,y) {
    lapply(x,function(xprime) {
      block_s<-xprime
      if (substr(block_s,1,1)=="|") {
        block_s<-str_sub(block_s,2)
      }
      paste0(block_s,"|",y,"|")
    })
  }
  d.exposure = ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d$Answer.exposureResp), ';'),
                                                 d$UniqueID,
                                                 .f=exposure_cleaner)), '\\|'))
  if (pilot_bool==FALSE) {
    for (i_exp in c("3","6")) {
      d.exposure = rbind(d.exposure,
               ldply(strsplit(unlist(purrr::map2(strsplit(as.character(d[,paste0("Answer.exposure",i_exp,"Resp")]), ';'),
                                                 d$UniqueID,
                                                 .f=exposure_cleaner)), '\\|')))
    }
  }

  names(d.exposure) <- c("PartOfExp", "Trial", "Block", "Filename", "pressedSpace", "RTStart", "RTEnd", "LoadTime", "Condition", "List","WorkerId","UniqueID")
  # Fix the WorkerIds again:
  d.exposure$WorkerId <- sapply(as.character(d.exposure$WorkerId), digest, algo="md5",USE.NAMES = FALSE)

  d.exposure$PartOfExp <- "exposure"


  d.exposure <- d.exposure %>%
    mutate_each(funs(as.numeric(.)),
                c(Trial,Block,RTStart,RTEnd,LoadTime)) %>%
    mutate(pressedSpace = ifelse(pressedSpace=="true",TRUE,FALSE),
           isCatch=ifelse(RTStart==-1,0,1),
           RT=RTEnd-RTStart,
           Trial=as.numeric(str_sub(Filename,-1,-1)))
  # If you didn't record block numbers...
  slide_num <- as.numeric(str_sub(d.exposure$Filename,-1,-1))
  d.exposure$Block <- (floor(slide_num/3))*2

  return(d.exposure)
}

## ------------------------------------- ##
## Getting the preprocessed survey data for Exp2
## ------------------------------------- ##
preprocess_data_exp2_survey <- function(d) {
  #Taking care of the worker id's right off the bat
  d <- plyr::rename(d,c("workerid" = "WorkerId"))
  d$WorkerId <- sapply(as.character(d$WorkerId), digest, algo="md5",USE.NAMES = FALSE)
  d$UniqueID<-make_unique_id(d)

  d.survey =  d %>%
    select(WorkerId,
           hitid, hittypeid, creationtime, assignmentid, assignmentaccepttime, assignmentsubmittime, UniqueID,
           contains("Answer")) %>%
    select(-contains("Resp")) %>%
    select(-contains("Answer.Submit"))
  # Get rid of stuff we don't want
  d.survey$Answer.errors = NULL
  d.survey$Answer.label = NULL
  d.survey$Answer.speaker = NULL

  d.survey<-dplyr::rename(d.survey,
                          AudioQual = Answer.audio_qual,
                          AudioType = Answer.audio_type,
                          AudioStall = Answer.audio_stall,
                          Comments = Answer.comments,
                          LgBackground = Answer.language_background,
                          LgBackgroundFree = Answer.language_background_free,
                          Condition = Answer.condition,
                          List = Answer.list)
  return(d.survey)
}

merge_survey_and_test <- function(d.test,d.survey) {
  d.survey$WorkerId = NULL
  d.survey$Condition = NULL
  d.survey$List = NULL
  d.both <- dplyr::left_join(d.test,d.survey,by="UniqueID")
  return(d.both)
}



## --------------------------------- ##
## Read in answer sheet
## --------------------------------- ##

word_info <- readRDS("data-raw/Exp1/WhatWordIsSpoken.RDS")

## --------------------------------- ##
## read in Exp1 NoExposure Pilot Data
## --------------------------------- ##

pilot_v1 = read.csv("data-raw/Exp1/NoExposurePilot/noexposure_pilot_6-2-16_results.csv", sep = "\t")
pilot_v1.both <- preprocess_data_exp1(pilot_v1,word_info)
pilot_v1.both$Run <- "PilotV1"

pilot_v2 = read.csv("data-raw/Exp1/NoExposurePilot/noexposure_pilot_6-6-16_results.csv", sep = "\t")
pilot_v2.both <- preprocess_data_exp1(pilot_v2,word_info)
pilot_v2.both$Run <- "PilotV2"

pilot_v3 = read.csv("data-raw/Exp1/NoExposurePilot/noexposure_pilot_6-7-16_results.csv", sep = "\t")
pilot_v3.both <- preprocess_data_exp1(pilot_v3,word_info)
pilot_v3.both$Run <- "PilotV3"

# Because I screwed up again
good <- pilot_v2.both %>% subset(Condition == "accented" | List %in% c("_rshift_0_normal","_rshift_0_reversed"))
bad <- pilot_v2.both %>% subset(Condition == "unaccented" & !(List %in% c("_rshift_0_normal","_rshift_0_reversed")))

d.noExpPilotBad <- rbind(pilot_v1.both, bad)
devtools::use_data(d.noExpPilotBad,overwrite=TRUE)

d.noExpPilot <- rbind(pilot_v3.both, good)
devtools::use_data(d.noExpPilot,overwrite=TRUE)

## --------------------------------- ##
## read in Exp1 NoExposure Real Data
## --------------------------------- ##

exp1_v1 = read.csv("data-raw/Exp1/NoExposureExp1/noexposure_exp1_6-18-16_results.csv", sep = "\t")
exp1_v1.both <- preprocess_data_exp1(exp1_v1,word_info)
exp1_v1.both$Run <- "Exp1V1"
d.ex1 <- rbind(exp1_v1.both,d.noExpPilot)
devtools::use_data(d.ex1,overwrite=TRUE)

## --------------------------------- ##
## read in Exp1 NoExposure Wording Test
## --------------------------------- ##

exp1_wording_unaccented = read.csv("data-raw/Exp1/NoExposureWordingTest/noexposure_wording_test_9-22-16_results.csv", sep = "\t")
exp1_wording_accented = read.csv("data-raw/Exp1/NoExposureWordingTest/noexposure_wording_test_9-27-16_results.csv", sep = "\t")
exp1_wording <- rbind(exp1_wording_unaccented,exp1_wording_accented)
exp1_wording.both <- preprocess_data_exp1(exp1_wording,word_info,
                                          merge=TRUE,wording_test=TRUE,load_times=TRUE)
exp1_wording.both$Run <- "Exp1WordTest"
d.ex1_wording_test <- exp1_wording.both
devtools::use_data(d.ex1_wording_test,overwrite=TRUE)

## --------------------------------- ##
## read in Exp2 Long Exposure pilot data
## --------------------------------- ##

pilot2_v1 = read.csv("data-raw/Exp2/ExposurePilot/exposure_pilot_10-10-16_results.csv", sep = "\t")
pilot2_v1.test =     preprocess_data_exp2_test(pilot2_v1,word_info)
pilot2_v1.survey =   preprocess_data_exp2_survey(pilot2_v1)
pilot2_v1.exposure = preprocess_data_exp2_exposure(pilot2_v1)
pilot2_v1.both = merge_survey_and_test(pilot2_v1.test,pilot2_v1.survey)

d.expPilot <- pilot2_v1.both
d.expPilot.exposure <- pilot2_v1.exposure
d.expPilot$Run <- "ExpPilotv1"
d.expPilot.exposure$Run <- "ExpPilotv1"
devtools::use_data(d.expPilot,overwrite=TRUE)
devtools::use_data(d.expPilot.exposure,overwrite=TRUE)

## --------------------------------- ##
## read in Exp2 Long Exposure Exp2 data
## --------------------------------- ##

exp2_v1 = read.csv("data-raw/Exp2/ExposureExp2/exposure_10-14-16_results.csv", sep = "\t")
exp2_v1.test =     preprocess_data_exp2_test(exp2_v1, word_info, pilot_bool = FALSE)
exp2_v1.survey =   preprocess_data_exp2_survey(exp2_v1)
exp2_v1.exposure = preprocess_data_exp2_exposure(exp2_v1,pilot_bool = FALSE)
exp2_v1.both = merge_survey_and_test(exp2_v1.test,exp2_v1.survey)

exp2_v1.both$Run <- "Exp2v1"
exp2_v1.exposure$Run <- "Exp2v1"

## --------------------------------- ##
## read in Exp2 Long Exposure New Order data
## --------------------------------- ##

exp2_v2 = read.csv("data-raw/Exp2/ExposureExp2NewOrder/exposure_11-10-16_results.csv", sep = "\t")
exp2_v2.test =     preprocess_data_exp2_test(exp2_v2, word_info, pilot_bool = FALSE)
exp2_v2.survey =   preprocess_data_exp2_survey(exp2_v2)
exp2_v2.exposure = preprocess_data_exp2_exposure(exp2_v2,pilot_bool = FALSE)
exp2_v2.both = merge_survey_and_test(exp2_v2.test,exp2_v2.survey)

exp2_v2.both$Run <- "Exp2v2"
exp2_v2.exposure$Run <- "Exp2v2"


d.ex2 <- rbind(d.expPilot,
               exp2_v1.both,
               exp2_v2.both)
d.ex2.exposure <- rbind(d.expPilot.exposure,
                        exp2_v1.exposure,
                        exp2_v2.exposure)
devtools::use_data(d.ex2,overwrite=TRUE)
devtools::use_data(d.ex2.exposure,overwrite=TRUE)




## ------------------------------------------------------- ##
## Make a data-frame with all workers for referencing stuff
## ------------------------------------------------------- ##

# Make a list of all workers who have taken any form of this experiment for referencing
d.WorkerReferenceList <- make_worker_id_compendium(
  list(d.noExpPilotBad,
       d.noExpPilot,
       d.ex1,
       d.ex1_wording_test,
       d.expPilot,
       exp2_v1.both,
       exp2_v2.both))
devtools::use_data(d.WorkerReferenceList,overwrite=TRUE)

