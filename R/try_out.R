
plotLocalizationDecay <- function() {
  
  files <- read.csv('data/files.csv', stringsAsFactors = F)
  
  schedule <- getSchedule()
  
  groups <- c(
              'control',
              'control60',
              'cursorjump',
              'EDS',
              'EDSmatch',
              'handview',
              'instructed',
              'instructed60',
              'older_control',
              'older_instructed'
              )
  
  participants <- files$participant[which(files$group %in% groups)]
  
  ALst <- schedule[which(schedule$task == 'activelocalization'  & schedule$session == 'rotated'),]$subtask
  PLst <- schedule[which(schedule$task == 'passivelocalization' & schedule$session == 'rotated'),]$subtask
  
  all_AL <- NA
  all_PL <- NA
  
  for (participant in participants) {
    
    pdata <- prepareParticipantData(participant)
    
    AL <- addSubTaskNumber( df=pdata[['rotated_activelocalization']],  session='rotated')[,c('trial_num','subtask','taperror_deg')]
    PL <- addSubTaskNumber( df=pdata[['rotated_passivelocalization']], session='rotated')[,c('trial_num','subtask','taperror_deg')]
    
    if (is.data.frame(all_PL)) {
      all_AL <- rbind(all_AL, AL)
      all_PL <- rbind(all_PL, PL)
    } else {
      all_AL <- AL
      all_PL <- PL
    }
  }
  
  avg_AL <- aggregate(taperror_deg ~ trial_num + subtask, data=all_AL, FUN=mean, na.rm=TRUE)
  avg_PL <- aggregate(taperror_deg ~ trial_num + subtask, data=all_PL, FUN=mean, na.rm=TRUE)
  
  AL_subtasks <- unique(schedule[which(schedule$task == 'activelocalization' & schedule$session == 'rotated'),]$subtask)
  PL_subtasks <- unique(schedule[which(schedule$task == 'passivelocalization' & schedule$session == 'rotated'),]$subtask)
  
  # make an empty plot with 8 sub panels
  layout(mat=matrix(c(1:8), nrow=2, ncol=4, byrow = TRUE))
  
  for (loctype in c('active','passive')) {
    
    if (loctype == 'active') {
      df       <- avg_AL
      subtasks <- AL_subtasks
    } else {
      df       <- avg_PL
      subtasks <- PL_subtasks
    }
    
    for (subtask_no in c(1:4)) {
      
      subtask <- subtasks[subtask_no]
      trialnums <- df$trial_num[which(df$subtask == subtask)]
      
      #print(length(trialnums))
      
      offset <- min(trialnums) - 1
      
      st_df <- df[which(df$subtask == subtask),]
      
      #print(str(st_df))
      #print(offset)
      
      plot(st_df$trial_num - offset, st_df$taperror_deg * -1, 
           xlim=c(0,19),ylim=c(0,20),
           main=sprintf('%s %d',loctype,subtask_no),
           xlab='trial number', ylab='hand localization shift [deg]',
           bty='n',ax=F)
      
      axis(side=1,at=c(1,9,18))
      axis(side=2,at=c(0,5,10,15,20))
      
    }
    
  }
  
}


plotRAEdecay <- function() {
  
  files <- read.csv('data/files.csv', stringsAsFactors = F)
  
  schedule <- getSchedule()
  
  groups <- c(
    'control',
    # 'control60',
    # 'cursorjump',
    'EDS',
    'EDSmatch',
    # 'handview',
    # 'instructed',
    # 'instructed60',
    'older_control'
    # 'older_instructed'
  )
  
  participants <- files$participant[which(files$group %in% groups)]
  
  all_EI <- NA
  all_IE <- NA
  
  for (participant in participants) {
    
    idx   <- which(files$participant == participant)
    strat <- files$strategy_order[idx]
    
    if (strat == "EI") {
      excl.trn <- schedule$trial_num[which(schedule$strategy == 0)]
      incl.trn <- schedule$trial_num[which(schedule$strategy == 1)]
    }
    if (strat == "IE") {
      excl.trn <- schedule$trial_num[which(schedule$strategy == 1)]
      incl.trn <- schedule$trial_num[which(schedule$strategy == 0)]
    }
    
    pdata <- prepareParticipantData(participant)
    
    NC <- addSubTaskNumber( df=pdata[['rotated_nocursor_reachdeviations']], session='rotated')[,c('trial_num','strategy','reachdev_deg','subtask')]
    
    NC$strategy[which(NC$trial_num %in% excl.trn)] <- 'exclude'
    NC$strategy[which(NC$trial_num %in% incl.trn)] <- 'include'
    
    if (strat == "EI") {
      if (is.data.frame(all_EI)) {
        all_EI <- rbind(all_EI, NC)
      } else {
        all_EI <- NC
      }
    }

    if (strat == "IE") {
      if (is.data.frame(all_IE)) {
        all_IE <- rbind(all_IE, NC)
      } else {
        all_IE <- NC
      }
    }

  }
  
  avg_EI <- aggregate(reachdev_deg ~ trial_num + subtask + strategy, data=all_EI, FUN=mean, na.rm=TRUE)
  avg_IE <- aggregate(reachdev_deg ~ trial_num + subtask + strategy, data=all_IE, FUN=mean, na.rm=TRUE)
  
  layout(mat=matrix(c(1:8), nrow=2, ncol=4, byrow = TRUE))
  
  subtask_pairs <- list( c(30,31),
                         c(37,38),
                         c(44,45),
                         c(51,52)  )

  for (order in c('EI','IE')) {

    if (order == 'EI') {
      df       <- avg_EI
    } else {
      df       <- avg_IE
    }
    
    for (block_no in c(1:4)) {
    
      subtasks <- subtask_pairs[[block_no]]
      
      trialnums <- df$trial_num[which(df$subtask %in% subtasks)]
    
      offset <- min(trialnums) - 1
      
      plot(-1000, -1000,
           xlim=c(0,19),ylim=c(0,30),
           main=sprintf('%s %d',order,block_no),
           xlab='trial number', ylab='reach aftereffect [deg]',
           bty='n',ax=F)
      
      for (strategy in c('include','exclude')) {
        
        col <- c('include'='#FF0000', 'exclude'='#0000FF')[strategy]
        sub_df <- df[which(df$strategy == strategy),]
        
        points(sub_df$trial_num - offset, sub_df$reachdev_deg, col=col)

      }
      
      axis(side=1,at=c(1,9,18))
      axis(side=2,at=c(0,10,20,30))
      
    }

  }
  
  legend(10,10,c('include','exclude'),col=c('#FF0000','#0000FF'),pch=1,cex=1,bty='n')
  
}



OLSslopeTest <- function(ns=c(5,10,50,100,500), reps=2000) {
  
  slopes <- list()
  
  for (n in ns) {
    
    slopes[sprintf('%d', n)] <- c()
    
    for (rep in c(1:reps)) {
      
      X <- rnorm(n, sd=4)
      Y <- X + rnorm(n, sd=1)
      
      temp_mod <- lm(Y~X)
      
      slopes[[sprintf('%d', n)]] <- c(slopes[[sprintf('%d', n)]], temp_mod$coeff[2])
      
    }
    
  }
  
  # return(slopes)
  
  plot(-1000,-1000, 
       main='',xlab='sample size',ylab='recovered slope',
       xlim=c(0,length(ns)+1),ylim=c(0.9,1.1),
       ax=F,bty='n'
  )
  
  lines(x=c(0,length(ns)+1),
        y=c(1,1),
        col='#666666',
        lty=3 )
  
  for (nidx in c(1:length(ns))) {
    S <- slopes[[sprintf('%d',ns[nidx])]]
    avg <- mean(S)
    CI <- Reach::getConfidenceInterval(S,method='b')
    print(c(ns[nidx], avg, CI))
    
    polygon(x=c(-0.4,0.4,0.4,-0.4)+nidx,
            y=rep(CI,each=2), border=NA, col='#FF000066')
    lines(x=c(-0.4,0.4)+nidx,
          y=rep(avg,2),
          col='#FF0000')
    
  }
  
  axis(side=1,at=c(1:length(ns)),labels=sprintf('%d',ns))
  axis(side=2,at=seq(0.9,1.1,0.05))
  
}