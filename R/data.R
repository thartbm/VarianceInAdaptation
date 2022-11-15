
`%notin%` <- function(x,y) !(x %in% y)


# descriptors participant ----

getParticipantDescriptors <- function(participant) {
  
  data <- prepareParticipantData(participant)
  schedule <- getSchedule()
  
  descriptors <- list()
  
  descriptors[['aligned_training_sd']] <- getBaselineSD(schedule = schedule,
                                                        task = 'training',
                                                        df = data[['aligned_training_reachdeviations']])
  descriptors[['aligned_nocursor_sd']] <- getBaselineSD(schedule = schedule,
                                                        task = 'nocursor',
                                                        df = data[['aligned_nocursor_reachdeviations']])
  
  return(descriptors)
  
}

prepareParticipantData <- function(participant) {
  

  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  
  # get participant group:
  idx <- which(files$participant == participant)
  group <- files$group[idx]
  
  # determine which sessions are available:
  sessions <- c('aligned',
                'rotated')
  if (substr(group,1,4) == 'org_') {
    sessions <- c('aligned')
  }
  
  # read all the data?
  data <- readParticipantData(participant=participant,
                              group=group,
                              sessions=sessions)
  
  schedule <- getSchedule()
  
  # ALIGNED
  
  # get aligned training reach deviations
  data[['aligned_training_reachdeviations']] <- getReachDeviationsMV(data[['aligned_training']])
  
  # get aligned nocursor reach deviations
  data[['aligned_nocursor_reachdeviations']] <- getReachDeviationsMV(data[['aligned_nocursor']])
  
  training_biases <- getBaselineBiases(schedule = schedule, 
                                       task = 'training', 
                                       df = data[['aligned_training_reachdeviations']])
  
  nocursor_biases <- getBaselineBiases(schedule = schedule, 
                                       task = 'nocursor', 
                                       df = data[['aligned_nocursor_reachdeviations']])
  
  
  # baseline training reaches:
  data[['aligned_training_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='training',
                                                                df=data[['aligned_training_reachdeviations']],
                                                                biases=training_biases)
  # baseline nocursor reaches:
  data[['aligned_nocursor_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='nocursor',
                                                                df=data[['aligned_nocursor_reachdeviations']],
                                                                biases=nocursor_biases)
  
  
  # get baseline passive localization bias
  
  # get baseline active localization bias
  
  
  

  # ROTATED
  
  if ('rotated' %notin% sessions) {
    return(data)
  }
  
  # get participant group and EI/IE
  
  
  
  # get rotated training reach deviations
  data[['rotated_training_reachdeviations']] <- getReachDeviationsMV(data[['rotated_training']])
  
  # get rotated nocursor reach deviations
  data[['rotated_nocursor_reachdeviations']] <- getReachDeviationsMV(data[['rotated_nocursor']])
  
  # baseline training reaches:
  data[['rotated_training_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='training',
                                                                df=data[['rotated_training_reachdeviations']],
                                                                biases=training_biases)
  # baseline nocursor reaches:
  data[['rotated_nocursor_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='nocursor',
                                                                df=data[['rotated_nocursor_reachdeviations']],
                                                                biases=nocursor_biases)
  return(data)
  
}



readParticipantData <- function(participant=participant,
                                group=group,
                                sessions=sessions) {
  
  tasks <- c('training',
             'nocursor',
             'activelocalization',
             'passivelocalization')
  
  file_list <- expand.grid(session=sessions,
                           task=tasks)
  
  filenames <- sprintf('%s_%s',file_list$session,file_list$task)
  fullfilenames <- sprintf('data/%s/%s/%s_%s_%s.csv',group,participant,participant,file_list$session,file_list$task)
  
  data <- list()
  for (fileno in c(1:length(filenames))) {
    
    filename <- filenames[fileno]
    df <- read.csv(fullfilenames[fileno], stringsAsFactors = FALSE)
    data[[filename]] <- df
    
  }
  
  return(data)

}

# generic prep functions -----

getReachDeviationsMV <- function(df) {
  
  # select only relevant samples:
  df <- df[which( df$trialselected  == 1 &
                  df$sampleselected == 1 &
                  df$maxvelocity    == 1),]
  
  # get samples on the trajectory:
  x <- df$handx_cm
  y <- df$handy_cm
  
  # opposite of target angle in radians
  theta <- (-df$targetangle_deg/180)*pi
  # un-rotate samples by target angle
  X <- (x * cos(theta)) - (y * sin(theta))
  Y <- (x * sin(theta)) + (y * cos(theta))
  
  # get the remaining reach deviation in degrees:
  df$reachdev_deg <- ((atan2(Y, X) / pi) * 180)
  
  return(df)
  
}

baselineReaches <- function(schedule,
                            task,
                            df,
                            biases) {
  
  for (rown in c(1:dim(biases)[1])) {
    
    target <- biases$targetangle_deg[rown]
    idx <- which(df$targetangle_deg == target)
    df$reachdeviation_deg[idx] <- df$reachdeviation_deg[idx] - biases$reachdev_deg[rown]
    
  }
  
  return(df)
  
}

getBaselineBiases <- function(schedule, task, df) {
  
  # identify the relevant trial numbers:
  aligned_trials <- schedule$trial_num[which(schedule$session=='aligned' & schedule$task==task)]
  
  # we want trials at the end of blocks: when the index jumps more than 1
  ends <- c(aligned_trials[which(diff(aligned_trials) > 1)], max(aligned_trials))
  
  if (task == 'training') {
    ntrials <- rep(2,length(ends))
    #ntrials[1] <- 5
  }
  if (task == 'nocursor') {
    ntrials <- rep(8,length(ends))
  }
  
  trials <- c()
  for (idx in c(1:length(ends))) {
    trials <- c(trials, c((ends[idx]-ntrials[idx]):ends[idx]))
  }
  
  df <- df[which(df$trial_num %in% trials),]
  
  biases <- aggregate(reachdev_deg ~ targetangle_deg, data=df, FUN=median)
  
  return(biases)
  
}

# reach variance -----

getBaselineSD <- function(schedule, task, df) {
  
  # identify the relevant trial numbers:
  aligned_trials <- schedule$trial_num[which(schedule$session=='aligned' & schedule$task==task)]
  
  # we want trials at the end of blocks: when the index jumps more than 1
  ends <- c(aligned_trials[which(diff(aligned_trials) > 1)], max(aligned_trials))
  
  if (task == 'training') {
    ntrials <- rep(2,length(ends))
    #ntrials[1] <- 5
  }
  if (task == 'nocursor') {
    ntrials <- rep(8,length(ends))
  }
  
  trials <- c()
  for (idx in c(1:length(ends))) {
    trials <- c(trials, c((ends[idx]-ntrials[idx]):ends[idx]))
  }
  
  df <- df[which(df$trial_num %in% trials),]
  
  return(sd(df$reachdev_deg))
  
}
