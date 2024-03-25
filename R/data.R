
`%notin%` <- function(x,y) !(x %in% y)

# ALL DESCRIPTORS -----

getAllDescriptors <- function() {
  
  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  
  #participants <- files$participant[1:3]
  participants <- files$participant
  #participants <- files$participant[70:79]
  
  groups <- files$group[which(files$participant %in% participants)]
  
  descriptors <- do.call("rbind",lapply(participants,getParticipantDescriptors))
  
  descriptors <- cbind('group'=groups,
                       'participant'=participants, 
                       as.data.frame(descriptors)    )
  
  for (name in names(descriptors)) {
    descriptors[,name] <- unlist(descriptors[,name])
  }
  
  write.csv(descriptors, 'data/descriptors.csv', quote=F, row.names = F)
  
  return(descriptors)
  
}

# descriptors participant ----

getParticipantDescriptors <- function(participant) {
  
  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  
  # get participant group:
  idx   <- which(files$participant == participant)
  group <- files$group[idx]
  strat <- files$strategy_order[idx]
  
  cat(sprintf('working on: group %s, participant %s\n',group,participant))
  
  # determine which sessions are available:
  sessions <- c('aligned',
                'rotated')
  if (substr(group,1,4) == 'org_') {
    sessions <- c('aligned')
  }
  
  data <- prepareParticipantData(participant)
  
  schedule <- getSchedule()
  
  descriptors <- list()
  
  descriptors[['aligned_training_sd']] <- getBaselineSD(schedule = schedule,
                                                        task = 'training',
                                                        df = data[['aligned_training_reachdeviations']])
  descriptors[['aligned_nocursor_sd']] <- getBaselineSD(schedule = schedule,
                                                        task = 'nocursor',
                                                        df = data[['aligned_nocursor_reachdeviations']])
  descriptors[['aligned_nocursor_sd_endpoint']] <- getBaselineSD(schedule = schedule,
                                                                 task = 'nocursor',
                                                                 df = data[['aligned_nocursor_reachdeviations_endpoint']])
  
  # get bias and error:
  descriptors[['aligned_training_bias']] <- getAverageError(schedule=schedule,
                                             task = 'training',
                                             df = data[['aligned_training_reachdeviations']],
                                             do_abs=FALSE)
  descriptors[['aligned_training_abserror']] <- getAverageError(schedule=schedule,
                                                 task = 'training',
                                                 df = data[['aligned_training_reachdeviations']],
                                                 do_abs=TRUE)
  
  descriptors[['aligned_nocursor_bias']] <- getAverageError(schedule=schedule,
                                             task = 'nocursor',
                                             df = data[['aligned_nocursor_reachdeviations']],
                                             do_abs=FALSE)
  descriptors[['aligned_nocursor_abserror']] <- getAverageError(schedule=schedule,
                                                 task = 'nocursor',
                                                 df = data[['aligned_nocursor_reachdeviations']],
                                                 do_abs=TRUE)
  
  
    
  descriptors[['aligned_activelocalization_sd']] <- getLocalizationSD(data[['aligned_activelocalization']])
  
  descriptors[['aligned_passivelocalization_sd']] <- getLocalizationSD(data[['aligned_passivelocalization']])
  
  # # # # # # # # # # #
  # NO ROTATED SESSION
  
  if ('rotated' %notin% sessions) {
    descriptors[['training_rate']] <- NA
    descriptors[['training_asymptote']] <- NA
    descriptors[['rotated_activelocalization_sd']] <- NA
    descriptors[['rotated_passivelocalization_sd']] <- NA
    descriptors[['activelocalization_shift']] <- NA
    descriptors[['passivelocalization_shift']] <- NA
    descriptors[['inclusion']] <- NA
    descriptors[['exclusion']] <- NA
    
    #print(descriptors)
    
    return(descriptors)
  }
  
  # # # # # # # # # # #
  # YES ROTATED SESSION
  
  if (substr(group,1,10) == 'instructed') {
    descriptors[['training_rate']] <- NA
    descriptors[['training_asymptote']] <- NA
  } else {
    ADfit <- getADfit(df=data[['rotated_training_reachdeviations']])
    descriptors[['training_rate']]      <- ADfit['lambda']
    descriptors[['training_asymptote']] <- ADfit['N0']
  }
  
  descriptors[['rotated_activelocalization_sd']] <- getLocalizationSD(data[['rotated_activelocalization']])
  descriptors[['rotated_passivelocalization_sd']] <- getLocalizationSD(data[['rotated_passivelocalization']])
  
  descriptors[['activelocalization_shift']] <- getLocalizationShift(data[['rotated_activelocalization']])
  descriptors[['passivelocalization_shift']] <- getLocalizationShift(data[['rotated_passivelocalization']])
  
  
  RAE <- getReachAftereffects(df=data[['rotated_nocursor_reachdeviations']],
                              strategy=strat,
                              schedule=schedule)
  
  descriptors[['inclusion']] <- as.numeric(RAE['inclusion'])
  descriptors[['exclusion']] <- as.numeric(RAE['exclusion'])
  
  return(descriptors)
  
}

prepareParticipantData <- function(participant) {
  

  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  
  # get participant group:
  idx <- which(files$participant == participant)
  group <- files$group[idx]
  
  # determine which sessions are available:
  sessions <- c('aligned',
                'rotated') # most groups have both sessions
  if (substr(group,1,4) == 'org_') {
    sessions <- c('aligned') # but not the pilots/orgs
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
  
  # get aligned nocursor reach deviations at endpoint
  data[['aligned_nocursor_reachdeviations_endpoint']] <- getReachDeviationsEP(data[['aligned_nocursor']])
  
  # biases in training reaches:
  data[['training_biases']] <- getBaselineBiases(schedule = schedule, 
                                       task = 'training', 
                                       df   = data[['aligned_training_reachdeviations']])
  
  # biases in no-cursor reaches:
  data[['nocursor_biases']] <- getBaselineBiases(schedule = schedule, 
                                       task = 'nocursor', 
                                       df   = data[['aligned_nocursor_reachdeviations']])

  # biases in no-cursor reaches at endpoint:
  data[['nocursor_biases_endpoint']] <- getBaselineBiases(schedule = schedule,
                                                          task='nocursor',
                                                          df = data[['aligned_nocursor_reachdeviations_endpoint']])
  
  # baseline training reaches:
  data[['aligned_training_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='training',
                                                                df=data[['aligned_training_reachdeviations']],
                                                                biases=data[['training_biases']])
  # baseline nocursor reaches:
  data[['aligned_nocursor_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='nocursor',
                                                                df=data[['aligned_nocursor_reachdeviations']],
                                                                biases=data[['nocursor_biases']])
  
  # baseline nocursor reaches at endpoint:
  data[['aligned_nocursor_reachdeviations_endpoint']] <- baselineReaches(schedule=schedule,
                                                                         task='nocursor',
                                                                         df=data[['aligned_nocursor_reachdeviations_endpoint']],
                                                                         biases=data[['nocursor_biases_endpoint']])
  
  # clean the aligned active localization data:
  data[['aligned_activelocalization']] <- cleanLocalization(data[['aligned_activelocalization']])
  
  # clean the aligned passive localization data:
  data[['aligned_passivelocalization']] <- cleanLocalization(data[['aligned_passivelocalization']])
  
  # get baseline active localization bias (a smooth spline fit object/list)
  data[['activelocaization_bias']] <- getSpline(data[['aligned_activelocalization']])
  
  # get baseline passive localization bias (a smooth spline fit object/list)
  data[['passivelocaization_bias']] <- getSpline(data[['aligned_passivelocalization']])
  
  data[['aligned_activelocalization']]  <- unbiasLocalization(locdf = data[['aligned_activelocalization']],
                                                              bias  = data[['activelocaization_bias']])
  #print(str(data[['aligned_activelocalizations']]))
  data[['aligned_passivelocalization']] <- unbiasLocalization(locdf = data[['aligned_passivelocalization']],
                                                              bias  = data[['passivelocaization_bias']])
  
  
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
                                                                biases=data[['training_biases']])
  # baseline nocursor reaches:
  data[['rotated_nocursor_reachdeviations']] <- baselineReaches(schedule=schedule,
                                                                task='nocursor',
                                                                df=data[['rotated_nocursor_reachdeviations']],
                                                                biases=data[['nocursor_biases']])
  
  
  data[['rotated_activelocalization']] <- cleanLocalization(data[['rotated_activelocalization']])
  
  data[['rotated_passivelocalization']] <- cleanLocalization(data[['rotated_passivelocalization']])
  
  data[['rotated_activelocalization']] <- unbiasLocalization(locdf = data[['rotated_activelocalization']],
                                                             bias  = data[['activelocaization_bias']])
  
  data[['rotated_passivelocalization']] <- unbiasLocalization(locdf = data[['rotated_passivelocalization']],
                                                              bias  = data[['passivelocaization_bias']])
  
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

getReachDeviationsEP <- function(df) {
  
  # select only relevant samples:
  df <- df[which( df$trialselected  == 1 &
                  df$sampleselected == 1),]
  
  # now select only the last sample of the selected samples from each trial:
  dat <- data.table::data.table(df)
  dat <- dat[dat[, .I[which.max(time_ms)], by=trial_num]$V1]
  df <- data.frame(dat)
  
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
  
  biases <- aggregate(reachdev_deg ~ targetangle_deg, data=df, FUN=median) # MEDIAN!?
  
  return(biases)
  
}

# reach variance -----

getBaselineSD <- function(schedule, task, df, useIQR=FALSE) {
  
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
  
  reachdevs <- df$reachdev_deg
  
  # there are TRIMMED and WINSORIZED standard deviations,
  # but they require scaling to be equivalent
  # (unlike mean/median the value changes A LOT with extremes removed)
  # unfortunately, the scaling math is complicated
  # and NOT implemented in any R package afaik
  
  # so, to prevent errors, I will not do this
  
  # assuming a normal distribution, we could remove outliers
  # using 1.5 times the interquartile range...
  # code below
  
  # normal standard deviation:
  std <- sd(reachdevs)
  
  # IQR "trimmed" sd
  if (useIQR) {
    qtls <- quantile(reachdevs, probs=c(0.25, 0.75), names=FALSE)
    iqtr <- diff(qtls)
    inlr <- qtls + (iqtr * c(-1*useIQR,1*useIQR))
    std <- sd(reachdevs[which(reachdevs > inlr[1] & reachdevs < inlr[2])])
  }

  return(std)
  
}

# learning curve -----

getADfit <- function(df) {
  
  #print(str(df))
  
  df <- df[which(df$trial_num <= 90),]
  
  schedule <- rep(-1,90)
  signal <- rep(NA,90)
  
  signal[df$trial_num] <- df$reachdev_deg
  
  # fit <- Reach::asymptoticDecayFit(schedule = schedule,
  #                                  signal =   signal)
  fit <- Reach::exponentialFit(signal = signal) # this function is faster, and gives the same output
  
  return(fit)
  
}

# localization -----

cleanLocalization <- function(df) {
  
  #ntrials <- dim(df)[1]
  df <- df[which(df$selected == 1),]
  
  # remove hand angles too far from the arc?
  idx <- which( df$targetangle_deg > (df$arcangle_deg - 45) &
                df$targetangle_deg < (df$arcangle_deg + 45)    )
  df <- df[idx,]
  
  # remove outliers based on tap errors?
  df$tapangle_deg <- (atan2(df$tapy_cm, df$tapx_cm) / pi) * 180
  df$taperror_deg <- df$tapangle_deg - df$targetangle_deg
  
  qtls <- quantile(df$taperror_deg, probs=c(0.25, 0.75), names=FALSE)
  iqtr <- diff(qtls)
  inlr <- qtls + (iqtr * c(-2,2))
  idx <- which( df$taperror_deg > inlr[1] & 
                df$taperror_deg < inlr[2]   )
  df <- df[idx,]
  
  #print(length(idx))
  
  # further outlier removal?
  # no.
  
  #cat(sprintf('removed %d trials\n',ntrials-dim(df)[1]))
  
  return(df)
  
}


# this is essentially the BIAS in localization
getSpline <- function(df) {
  
  X <- df$targetangle_deg
  Y <- df$taperror_deg
  
  sm.spl <- smooth.spline(x=X,
                          y=Y,
                          spar=0.50,
                          nknots=9)
  
  spl.fit <- sm.spl$fit
  
  return(spl.fit)
  
}

unbiasLocalization <- function(locdf,
                               bias) {
  predicted_errors <- predict(bias,locdf$targetangle_deg)$y
  
  #print(predicted_errors)
  
  locdf$tapangle_deg <- locdf$tapangle_deg - predicted_errors
  locdf$taperror_deg <- locdf$tapangle_deg - locdf$targetangle_deg
  
  return(locdf)
  
}

getLocalizationSD <- function(locdf) {
  
  # assuming the localization bias has been removed
  # we can just get the standard deviation of tap errors:
  
  return(sd(locdf$taperror_deg))
  
}

getLocalizationShift <- function(locdf) {
  
  # assuming the localization bias has been removed
  # we can simply get the median tap error:
  
  return(median(locdf$taperror_deg))
  
}

# reach aftereffects -----

getReachAftereffects <- function(df,
                                 strategy,
                                 schedule) {
  
  schedule <- schedule[which(schedule$session == 'rotated'),]
  
  if (strategy == "") {
    return(c('inclusion'=NA, 'exclusion'=NA))
  }
  
  if (strategy == "EI") {
    excl.trn <- schedule$trial_num[which(schedule$strategy == 0)]
    incl.trn <- schedule$trial_num[which(schedule$strategy == 1)]
  }
  if (strategy == "IE") {
    excl.trn <- schedule$trial_num[which(schedule$strategy == 1)]
    incl.trn <- schedule$trial_num[which(schedule$strategy == 0)]
  }
  
  inclusion <- mean(df$reachdev_deg[which(df$trial_num %in% incl.trn)])
  exclusion <- mean(df$reachdev_deg[which(df$trial_num %in% excl.trn)])
  
  return(c('inclusion'=inclusion,
           'exclusion'=exclusion))
  
}


# errors / bias -----

getAverageError <- function(schedule, task, df, do_abs=FALSE) {
  
  # identify the relevant trial numbers:
  aligned_trials <- schedule$trial_num[which(schedule$session=='aligned' & schedule$task==task)]
  
  # we want trials at the end of blocks: when the index jumps more than 1
  ends <- c(aligned_trials[which(diff(aligned_trials) > 1)], max(aligned_trials))
  
  if (task == 'training') {
    ntrials <- rep(2,length(ends)) # last 3 of each block
    #ntrials[1] <- 5
  }
  if (task == 'nocursor') {
    ntrials <- rep(8,length(ends)) # last (all) 9 of each block
  }
  
  trials <- c()
  for (idx in c(1:length(ends))) {
    trials <- c(trials, c((ends[idx]-ntrials[idx]):ends[idx]))
  }
  
  df <- df[which(df$trial_num %in% trials),]
  
  reachdevs <- df$reachdev_deg
  
  if (do_abs) {reachdevs <- abs(reachdevs)}

  return(mean(reachdevs))
  
}

# NON-DESCRIPTORS -----

# https://www.r-bloggers.com/2014/09/5-ways-to-do-2d-histograms-in-r/

getAlignedLocalization <- function() {
  
  files <- read.csv('data/files.csv', stringsAsFactors = FALSE)
  
  files <- files[,c('participant','group')]
  
  localization <- do.call("rbind",apply(files,1,FUN=readParticipantLocalization))
  
  localization <- localization[which(localization$selected == 1),]
  
  return(localization)
  
}


readParticipantLocalization <- function(params) {
  
  participant <- params['participant']
  group <- params['group']
  
  tasks <- c('activelocalization',
             'passivelocalization')
  
  data <- NA
  
  for (task in tasks) {
    
    filename <- sprintf('data/%s/%s/%s_aligned_%s.csv',group,participant,participant,task)
    df <- read.csv(filename, stringsAsFactors = FALSE)
    df$participant <- participant
    df$group <- group
    df$task <- task

    if (is.data.frame(data)) {
      data <- rbind(data, df)
    } else {
      data <- df
    }
    
  }
  
  return(data)
  
}

hist2D <- function(x, y, n=25, lims=NULL) {
  
  if (is.null(lims)) {
    xmin <- floor(min(x))
    xmax <- ceiling(max(x))
    ymin <- floor(min(y))
    ymax <- ceiling(max(y))
  } else {
    xmin <- lims[1]
    xmax <- lims[2]
    ymin <- lims[3]
    ymax <- lims[4]
  }
  
  if (length(n) == 1) {
    nx <- n
    ny <- n
  }
  if (length(n) == 2) {
    nx <- n[1]
    ny <- n[2]
  }
  
  
  x.bin <- seq(xmin, xmax, length=nx)
  y.bin <- seq(ymin, ymax, length=ny)
  
  x.bw <- (xmax-xmin)/(nx-1)
  y.bw <- (ymax-ymin)/(ny-1)
  
  x.bin.edges <- seq(xmin-(x.bw/2), xmax+(x.bw/2), length=nx+1) # could calculate from x.bin as well...
  y.bin.edges <- seq(ymin-(y.bw/2), ymax+(y.bw/2), length=ny+1)
  
  drop.idx <- unique( c( which(x < x.bin.edges[1]),
                         which(x > x.bin.edges[nx+1]),
                         which(y < y.bin.edges[1]),
                         which(y > y.bin.edges[ny+1]) ) )
  
  x <- x[-drop.idx]
  y <- y[-drop.idx]
  
  freq <-  as.data.frame(table(findInterval(x, x.bin.edges),findInterval(y, y.bin.edges)))
  freq[,1] <- as.numeric(freq[,1])
  freq[,2] <- as.numeric(freq[,2])
  
  freq2D <- matrix(0, nrow=nx, ncol=ny)
  freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
  
  return(list('x'=x.bin, 'y'=y.bin, 'z'=freq2D))
  
}

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

plotLocalizationDistribution <- function(df) {
  
  l <- getAlignedLocalization()
  homebrew <- hist2D(x=l$handx_cm, y=l$handy_cm, n=c(40,20), lims=c(-13,13,0,13))
  hist2d <- MASS::kde2d(x=localization$handx_cm, y=localization$handy_cm, n=c(100,50), lims=c(-13,13,0,13))

  
}