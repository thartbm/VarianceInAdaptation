



# descriptors participant ----

getParticipantDescriptors <- function(participant) {
  
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
  
  # - aligned training
  # - aligned nocursors
  # - aligned active localization
  # - aligned passive localization
  
  # - rotated training
  # - rotated nocursors
  # - rotated active localization
  # - rotated passive localization
  
  
  
  # ALIGNED
  
  # get aligned training reach deviations
  data[['aligned_training_reachdeviatons']] <- getReachDeviations(data[['aligned_training']])
  
  # get aligned nocursor reach deviations
  data[['aligned_nocursor_reachdeviatons']] <- getReachDeviations(data[['aligned_nocursor']])
  
  # get baseline training biases
  
  baseline_biases <- getBaselineTrainingBiases(schedule=schedule, 
                                               df=data[['aligned_training']])
  

  
  # get baseline no-cursor biases
  
  # get baseline passive localization bias
  
  # get baseline active localization bias
  
  
  
  # get participant group and EI/IE
  
  
  # ROTATED
  
  # get rotated training reach deviations
  data[['rotated_training_reachdeviatons']] <- getReachDeviations(data[['rotated_training']])
  
  # get rotated nocursor reach deviations
  data[['rotated_nocursor_reachdeviatons']] <- getReachDeviations(data[['rotated_nocursor']])
  
  
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

getReachDeviations <- function(df) {
  
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
  
  #print(df$reachdev_deg)
  
  return(df$reachdev_deg)
  
}

getBaselineTrainingBiases <- function(schedule, df) {
  
  training_trials <- schedule$trial_num[which(schedule$session=='aligned' & schedule$task=='training')]
  training_block_ends <- which(diff(training_trials) > 1)
  
  return(NULL)
  
}
