# data download code -----

#' @title Download project data from OSF repository.
#' @param folder Directory on the working directory to store the downloaded data.
#' @param removezips (Boolean) Remove downloaded zipfiles after unzipping.
#' @param checkfiles (Boolean) Check if all files are there after unzipping.
#' @param groups Vector of groups to download data for ('all' for all groups).
#' Possible values: 'control', 'instructed', 'control60', 'instructed60',
#' 'cursorjump', 'handview', 'older_control', 'older_instructed'
#' 'EDS', 'EDSmatch', 'org_control', 'org_instructed', 'org_control60', and
#' 'org_instructed60'. The last 4 groups are incomplete pilot studies.
#' Default: 'all'.
#' @param sections Vector of sections of data to download for each of the groups
#' ('all' for all sections). Possible values: 'aligned', 'rotated', and
#' 'localization-reaches'. Default: c('aligned', 'rotated')
#' @return Nothing
#' @description Use this function to download the data for the project from the
#' OSF repository: https://osf.io/dhk3u/
#' @details Not yet.
#' @export
downloadData <- function(folder='data', unzip=TRUE, removezips=TRUE, checkfiles=TRUE, groups='all', sections=c('aligned','rotated'), overwrite=TRUE) {
  
  filelist <- getDownloadList(groups=groups,
                              sections=sections)
  
  Reach::downloadOSFdata(repository='https://osf.io/dhk3u/',
                         filelist=filelist,
                         folder=folder,
                         overwrite=overwrite,
                         unzip=unzip,
                         removezips=removezips)
  
  #checkFiles(filelist,folder=folder)
  # implement? (should check if all expected files are there or not)
  # not for now...
  
}



getDownloadList <- function(groups='all', sections=c('aligned','rotated')) {
  
  # these are the possible groups:
  allgroups <- c('control','instructed',
                 'control60','instructed60',
                 'cursorjump','handview',
                 'older_control','older_instructed',
                 'EDS','EDSmatch',
                 'org_control','org_instructed',
                 'org_control60','org_instructed60')
  # remove requested groups that don't exist:
  if ( all(groups %in% c('all','a')) ) {
    groups <- allgroups
  } else {
    groups <- groups[which(groups %in% allgroups)]
  }
  
  # remove requested data that doesn't exist:
  allsections<- c('aligned','rotated','localization-reaches')
  if ( all(sections %in% c('all','a'))) {
    sections <- allsections
  } else {
    sections <- sections[which(sections %in% allsections)]
  }
  
  # start vector with demographics file and checklist of files:
  filelist <- c('demographics.csv', 'files.csv')
  # add other files:
  for (group in groups) {
    for (section in sections) {
      if (substr(group,1,4)=='org_' & section =='rotated') {
        # do nothing: there is no rotated data for these groups
      } else {
        filelist <- c(filelist, sprintf('%s_%s.zip',group,section))
      }
    }
  }
  
  # put it in a list, specifying the folder "data"...
  downloadList <- list()
  downloadList[['data']] <- filelist
  
  return(downloadList)
  
}


# data processing -----

addSubTaskNumber <- function(df, session) {
  
  trial2subtask <- c()
  
  # aligned session
  if (session == 'aligned') {
    subtasklengths <- c(45,18,9,18,9,9) # first aligned iteration
    for (repetition in c(1:3)) {
      subtasklengths <- c(subtasklengths, c(9,18,9,18,9,9)) # next 3 iterations:
    }
    offset <- 0
  }
  # rotated session
  if (session == 'rotated') {
    subtasklengths <- c(90,18,30,18,30,9,9) # first aligned iteration
    for (repetition in c(1:3)) {
      subtasklengths <- c(subtasklengths, c(30,18,30,18,30,9,9)) # next 3 iterations:
    }
    offset <- 24
  }
  
  # make a vector where index is trial number (within session, starts at 1 for both the aligned and the rotated)
  # and the values are subtask numbers (across aligned and rotated sessions, starts at 1 and 25 for aligned and rotated respectively)
  for (subtask_idx in c(1:length(subtasklengths))) {
    trial2subtask <- c(trial2subtask, rep(subtask_idx+offset, subtasklengths[subtask_idx]))
  }
  
  # add new subtask column to the data frame:
  df$subtask <- trial2subtask[df$trial_num]
  
  # return the extended data frame:
  return(df)
  
}

getAdaptationParameters <- function(groups=c('control',
                                             'cursorjump',
                                             'handview',
                                             'older_control',
                                             'EDSmatch')) {
  
  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  files <- files[which(files$group %in% groups),]
  
  N <- dim(files)[1]
  
  participant <- c()
  group       <- c()
  lambda      <- c()
  N0          <- c()
  
  for (rown in c(1:dim(files)[1])) {
    cat(sprintf('participant %d/%d\n',rown, N))
    
    # get relevant information from the files data frame:
    pp_name <- files$participant[rown]
    group_name <- files$group[rown]
    baseline_filename <- files$aligned_training[rown]
    adaptation_filename <- files$rotated_training[rown]
    
    baseline_trials <- getBaselineTrials(group_name, pp_name, baseline_filename)

    # get the biases for each target:
    biases <- aggregate(reachdev_deg ~ targetangle_deg, data=baseline_trials, FUN=median, na.rm=TRUE)
    
    # load rotated training data:
    learning_file <- read.csv(sprintf('data/%s/%s/%s',group_name, pp_name, adaptation_filename), stringsAsFactors = FALSE)
    # only keep data from:
    # - the first 90 rotated trials
    # - selected trials
    # - max velocity samples
    learning_trials <- learning_file[which( learning_file$trial_num <= 90 &
                                            learning_file$trialselected == 1 &
                                            learning_file$maxvelocity == 1),]
    # get reach deviations at max velocity for these 90 trials:
    learning_trials$reachdev_deg <- ((atan2(learning_trials$handy_cm, learning_trials$handx_cm) / pi) * 180) - learning_trials$targetangle_deg
    
    # subtract baseline bias from each reach deviation:
    for (target in biases$targetangle_deg) {
      bias <- biases$reachdev_deg[which(biases$targetangle_deg == target)]
      idx <- which(learning_trials$targetangle_deg == target)
      learning_trials$reachdev_deg[idx] <- learning_trials$reachdev_deg[idx] - bias
    }
    
    # remove reach deviations that are 30 degrees or more out of range:
    learning_trials <- learning_trials[which(learning_trials$reachdev_deg <  60 &
                                             learning_trials$reachdev_deg > -30   ),]
    
    # get a learning curve:
    reaches <- rep(NA, 90)
    reaches[learning_trials$trial_num] <- learning_trials$reachdev_deg
    
    # remove participants who didn't adapt or overadapted:
    asymptote <- mean(reaches[61:90], na.rm=TRUE)
    if ((asymptote < 15) | (asymptote > 45)) {
      next()
    }
    
    ADfit <- Reach::asymptoticDecayFit(schedule=rep(-1,90),
                                        signal=reaches)
    
    if ((ADfit['N0'] < 15) | (ADfit['N0'] > 45)) {
      next()
    }
    
    # add to vectors:
    participant <- c(participant, pp_name)
    group       <- c(group, group_name)
    lambda      <- c(lambda, ADfit['lambda'])
    N0          <- c(N0, ADfit['N0'])
    
  }
  
  df <- data.frame(group, participant, lambda, N0)
  write.csv(df, 'data/adaptation_parameters.csv', row.names=FALSE, quote=FALSE)
  return(df)
  
}

getBaselineTrials <- function(group_name, pp_name, baseline_filename) {
  
  # load the aligned training file:
  baseline_file <- read.csv(sprintf('data/%s/%s/%s',group_name, pp_name, baseline_filename), stringsAsFactors = FALSE)
  # only keep the relevant samples:
  # - selected trials
  # - maximum velocity
  baseline_file <- baseline_file[which(   baseline_file$trialselected == 1 &
                                            baseline_file$maxvelocity == 1),]
  # for the nest step, we need to know which subtask each trial belonged to:
  baseline_file <- addSubTaskNumber(baseline_file, session='aligned')
  # keep only trials 7,8,9 from the "top-up" training trials
  # and the last 9 trials from the first aligned training task
  baseline_trials <- rbind(baseline_file[which(baseline_file$subtask > 2 & baseline_file$trial_num > 3),],
                           baseline_file[which(baseline_file$subtask == 1 & baseline_file$trial_num > 33),])
  # calculate the reach deviation in degrees angle
  baseline_trials$reachdev_deg <- ((atan2(baseline_trials$handy_cm, baseline_trials$handx_cm) / pi) * 180) - baseline_trials$targetangle_deg
  
  return(baseline_trials)
   
}

getBaselineDescriptors <- function(group_name, pp_name, baseline_filename) {
  
  # load the aligned training file:
  baseline_file <- read.csv(sprintf('data/%s/%s/%s',group_name, pp_name, baseline_filename), stringsAsFactors = FALSE)
  # only keep relevant trials:
  baseline_file <- rbind(baseline_file[which(baseline_file$subtask > 2 & baseline_file$trial_num > 3),],
                         baseline_file[which(baseline_file$subtask == 1 & baseline_file$trial_num > 33),])
  baseline_file <- baseline_file[which(baseline_file$trialselected == 1),]
  # for the nest step, we need to know which subtask each trial belonged to:
  baseline_file <- addSubTaskNumber(baseline_file, session='aligned')
  
  
  # trajectory length
  # target is at 12 cm
  average_path_length
  sd_path_length
  
  # trajectory duration
  average_path_duration
  sd_path_duration
  
  # maximum velocity:
  average_max_velocity
  sd_max_velocity
  
  # max velocity time:
  average_maxvel_time
  sd_maxvel_time
  
  # max velocity point:
  average_maxvel_distance
  sd_maxvel_distance
  
  
  # only keep the relevant samples:
  # - selected trials
  # - maximum velocity
  baseline_file <- baseline_file[which(   baseline_file$trialselected == 1 &
                                            baseline_file$maxvelocity == 1),]

  # calculate the reach deviation in degrees angle
  baseline_trials$reachdev_deg <- ((atan2(baseline_trials$handy_cm, baseline_trials$handx_cm) / pi) * 180) - baseline_trials$targetangle_deg
  
  # reach bias at maximum velocity:
  average_bias
  sd_bias
  
}