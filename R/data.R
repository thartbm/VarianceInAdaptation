
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
  
  
  # get baseline training biases
  
  # get baseline no-cursor biases
  
  # get baseline passive localization bias
  
  # get baseline active localization bias
  
  
  
  # get participant group and EI/IE
  
  
  
  
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