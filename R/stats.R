`%notin%` <- Negate(`%in%`)

library(reshape2)
library(afex)
library(BayesFactor)

# variance comparisons -----

getDF4stats <- function(groups='all',variables=c('aligned_training_sd', 'aligned_nocursor_sd')) {
  
  df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  # check groups for sanity:
  datagroups <- unique(df$group)
  
  if (is.character(groups) & length(groups) == 1) {
    if (groups == 'all') {
      select_groups <- datagroups
    } else if (groups %in% datagroups) {
      select_groups <- c(groups)
    }
  } else if (length(groups) > 1) {
    select_groups <- intersect(groups, datagroups)
    if (length(select_groups) != length(groups)) {
      cat('some groups not found in data, skipping:\n')
      print(setdiff(groups, datagroups))
    }
  }
  
  df <- df[which(df$group %in% select_groups),]
  
  
  # check variables for sanity:
  datavariables <- names(df)[3:dim(df)[2]]
  
  
  if (is.character(variables) & length(variables) == 1) {
    if (variables == 'all') {
      select_variables <- datavariables
    } else if (variables %in% datavariables) {
      select_variables <- c(variables)
    }
  } else if (length(variables) > 1) {
    select_variables <- intersect(variables, datavariables)
    if (length(select_variables) != length(variables)) {
      cat('some groups not found in data, skipping:\n')
      print(setdiff(variables, datavariables))
    }
  }
  
  df <- reshape2::melt(df[,c('group','participant',select_variables)],
                       id.vars=c('group','participant'))
  
  return(df)
  
}

baselineVarianceTests <- function() {
  
  df      <- getDF4stats(groups    = 'all',
                         variables = c('aligned_training_sd',
                                       'aligned_nocursor_sd',
                                       'aligned_activelocalization_sd',
                                       'aligned_passivelocalization_sd'))
  
  df$age <- 'younger'
  df$age[which(df$group %in% c('older_control', 'older_instructed'))] <- 'older'
  
  df$age <- as.factor(df$age)
  df$variable <- as.factor(df$variable)
  
  baymod <- BayesFactor::anovaBF(value ~ variable + age, data=df)
  # bm_e   <- BayesFactor::extractBF( baymod )
  
  # cat('main effects:\n')
  # print(baymod[c(1:2)])
  # cat('interaction:\n')
  # print(bm_e$bf[4] / bm_e$bf[3])
  
  print(bayestestR::bayesfactor_inclusion(baymod))
  
  cat('\n\nwith younger =< 35:\n\n')
  
  dems <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  
  idx <- which(dems$group %in% c('older_control', 'older_instructed'))
  idx <- c(idx, which( dems$group %notin% c('older_control', 'older_instructed') & dems$age < 36) )
  participants <- dems$participant[idx]
  
  selectdf <- df[which(df$participant %in% participants),]
  Abaymod <- BayesFactor::anovaBF(value ~ variable + age, data=selectdf)
  print(bayestestR::bayesfactor_inclusion(Abaymod))
  
}


baselineVarianceFollowUps <- function() {
  
  variables <- c('aligned_nocursor_sd',
                 'aligned_training_sd',
                 'aligned_activelocalization_sd',
                 'aligned_passivelocalization_sd')
  
  df      <- getDF4stats(groups    = 'all',
                         variables = variables)
  
  # df$age <- 'younger'
  # df$age[which(df$group %in% c('older_control', 'older_instructed'))] <- 'older'
  # df$age <- as.factor(df$age)
  
  df$variable <- as.factor(df$variable)
  
  # BFvals <- matrix(data=NA, nrow=length(variables), ncol=length(variables))
  
  for (idx1 in c(1:(length(variables)-1))) {
    for (idx2 in c((idx1+1):length(variables))) {
      var1 <- variables[idx1]
      var2 <- variables[idx2]
      cat(sprintf('= = = = = = = = =\ncompare %s with %s:\n',toupper(var1),toupper(var2)))
      var1vals <- df$value[which(df$variable == var1)]
      var2vals <- df$value[which(df$variable == var2)]
      bt <- BayesFactor::ttestBF(var1vals, var2vals, paired=TRUE)
      bt_e <- BayesFactor::extractBF( bt )
      # cat( sprintf('BF: %0.4f\n',bt_e$bf ) )
      
      cat( sprintf('BF: %s\n',formatC(bt_e$bf, format = "e", digits = 2) ) )
      
      # BFvals[idx1,idx2] <- bt_e$bf 
      cat('\n')
    }
  }

  
  print(t.test(df$value[which(df$variable == 'aligned_activelocalization_sd')],
                df$value[which(df$variable == 'aligned_passivelocalization_sd')],
                paired=TRUE))
  
  # colnames( BFvals ) <- variables
  # rownames( BFvals ) <- variables
  
  # print(BFvals)
  
}

# localizationMLEcomparison <- function() {
#   
#   df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
# 
#   d <- df$aligned_passivelocalization_sd > df$aligned_activelocalization_sd
#   
#   # what is the hypothesized probability of success?
#   # default is 50%... but that's pretty low
#   # if we'd want this to work on individual participants, 95% seems like a fair low bar
#   
#   binom.test(x=sum(d), n=length(d), p=0.50, alternative = "greater")
#   
# }

openClosedLoopDifference <- function() {
  
  df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  # get the difference between open and closed loop:
  closed <- df$aligned_training_sd
  open   <- df$aligned_nocursor_sd
  
  prop_incr <- ((mean(open) - mean(closed)) / mean(closed))
  cat(sprintf('increase in the averages: %0.1f%%\n',(prop_incr*100)))
  prop_incr <- ((median(open) - median(closed)) / median(closed))
  cat(sprintf('increase in the medians:  %0.1f%%\n',(prop_incr*100)))
  prop_incr <- mean((open - closed) / closed)
  cat(sprintf('average of the increases: %0.1f%%\n',(prop_incr*100)))
  prop_incr <- median((open - closed) / closed)
  cat(sprintf('median of the increases:  %0.1f%%\n\n',(prop_incr*100)))
  
                          
  # prop_incr <- ((open - closed) / closed)
  # print(BayesFactor::ttestBF(prop_incr, mu=0))
  
}

# reverse Maximum Likelihood Estimate -----

MLE_weights <- function(FUN=mean) {
  
  df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  sig2_ae <- FUN(df$aligned_activelocalization_sd)^2
  sig2_a  <- FUN(df$aligned_passivelocalization_sd)^2
  
  sig2_e <- -sig2_ae * (sig2_a / (sig2_ae-sig2_a))
  
  w_a <- (1/sig2_a) / ((1/sig2_a) + (1/sig2_e))
  w_e <- (1/sig2_e) / ((1/sig2_a) + (1/sig2_e))
  
  return(data.frame('variable' = c('sig2_ae', 'sig2_a', 'sig2_e', 'w_a', 'w_e'),
                    'value'    = c( sig2_ae,   sig2_a,   sig2_e,   w_a,   w_e)))
  
}

# multiple regressions on aftereffects -----

predictAftereffects <- function(trace=0) {
  
  # this is not the data set we are looking for:
  # df4 <- getDF4stats(groups    = c("handview", "instructed60", "older_instructed", "control60", "EDSmatch", "instructed", "cursorjump", "EDS", "control", "older_control"),
  #                    variables = c('aligned_activelocalization_sd', 'aligned_passivelocalization_sd', 'exclusion', 'activelocalization_shift', 'passivelocalization_shift'))
  
  # get our data set:
  df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  # remove the 'org' groups, since they don't have a rotated phase:
  df <- df[which(df$group %in% c("handview", "instructed60", "older_instructed", "control60", "EDSmatch", "instructed", "cursorjump", "EDS", "control", "older_control")),]
  
  # demo_df <- df[,c('group', 'participant')]
  # now keep only the columns we're interested in:
  df <- df[,c('group', 'participant', 'exclusion','aligned_activelocalization_sd', 'aligned_passivelocalization_sd', 'activelocalization_shift', 'passivelocalization_shift')]
  
  # cat('########## predict aftereffects from ACTIVE localization:\n\n')
  # active.model <- step(lm(exclusion ~ activelocalization_shift + aligned_activelocalization_sd, data=df),
  #                      direction='both',
  #                      trace=trace)
  
  active_both  <- lm(exclusion ~ activelocalization_shift + aligned_activelocalization_sd, data=df)
  active_sd    <- lm(exclusion ~ aligned_activelocalization_sd, data=df)
  active_shift <- lm(exclusion ~ activelocalization_shift, data=df)
  active.AIC <- stats::AIC(  
                             active_both,
                             active_sd,
                             active_shift, k=4 )
  
  # active.AIC <- stats::AIC(  lm(exclusion ~ activelocalization_shift + aligned_activelocalization_sd, data=df),
  #                            lm(exclusion ~ aligned_activelocalization_sd, data=df),
  #                            lm(exclusion ~ activelocalization_shift, data=df ) )
  
  active.rll <- relativeLikelihood(active.AIC$AIC)
  
  if (trace) {
    cat('active model AICs:\n')
    print(active.AIC)
    cat('relative log likelihoods:\n')
    print(active.rll)
  }
  
  # print(summary(active.model))
  # cat('########## predict aftereffects from PASSIVE localization:\n\n')
  # passive.model <- step(lm(exclusion ~ passivelocalization_shift + aligned_passivelocalization_sd, data=df),
  #                       direction='both',
  #                       trace=trace)
  
  passive_both  <- lm(exclusion ~ passivelocalization_shift + aligned_passivelocalization_sd, data=df)
  passive_sd    <- lm(exclusion ~ aligned_passivelocalization_sd, data=df)
  passive_shift <- lm(exclusion ~ passivelocalization_shift, data=df )
  passive.AIC <- stats::AIC(  
                              passive_both,
                              passive_sd,
                              passive_shift )
  # passive.AIC <- stats::AIC(  lm(exclusion ~ passivelocalization_shift + aligned_passivelocalization_sd, data=df),
  #                             lm(exclusion ~ aligned_passivelocalization_sd, data=df),
  #                             lm(exclusion ~ passivelocalization_shift, data=df ) )
  passive.rll <- relativeLikelihood(passive.AIC$AIC)

  if (trace) {
    cat('passive model AICs:\n')
    print(passive.AIC)
    cat('relative likelihoods:\n')
    print(passive.rll)
  }
  
  # print(which.max(passive.rll))
  # print(summary(passive.model))

  # this gives somewhat weird results: it keeps 1 of the shifts, and one of the sds... but I suppose the two shifts and the two sds share a lot of variance, so this should have been expected?
  # passive.model <- step(lm(exclusion ~ passivelocalization_shift + aligned_passivelocalization_sd + activelocalization_shift + aligned_activelocalization_sd, data=df),
  #                       direction='both')
  # 
  
  if (trace == 0) {
    return(list('data'=df,
                'active.model'  = c(active_both,  active_sd,  active_shift)[ which.max(active.rll)],
                'passive.model' = c(passive_both, passive_sd, passive_shift)[which.max(passive.rll)]))
  }
  
}



AIC <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}

AICc <- function(MSE, k, N) {
  return( AIC(MSE, k, N) * (((2*k^2) + 2*k) / (N - k - 1)) )
}

relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}

compareAftereffectPredictions <- function() {
  
  models <- predictAftereffects()
  active.model <- models$active.model
  passive.model <- models$passive.model
  
  # cat('AIC comparison\nof multiple regression models predicting aftereffects\nwith localization variance and shift:\n\n')
  AICs <- c('active'=active.model$anova$AIC, 'passive'=passive.model$anova$AIC)
  
  restab <- matrix(NA, ncol=2, nrow=2)
  restab[,1] <- AICs
  restab[,2] <- relativeLikelihood(AICs)
  row.names(restab) <- names(AICs)
  colnames(restab) <- c('AIC','p')
  print(as.data.frame(restab))
  
}

sharedVariance <- function() {
  
  models <- predictAftereffects()
  df <- models$data
  # usdm::vif(df[,c(4:7)])
  
  y <- df$exclusion
  
  vars <- c('aligned_activelocalization_sd', 'aligned_passivelocalization_sd', 'activelocalization_shift', 'passivelocalization_shift')
  for (var1 in c(1:(length(vars)-1))) {
    for (var2 in c((var1+1):length(vars))) {
      
      x1 <- df[,vars[var1]]
      x2 <- df[,vars[var2]]
      VS <- car::vif(lm(y ~ x1 + x2))[1]
      cat(sprintf('%s, %s: VIF = %0.3f\n',vars[var1],vars[var2],VS))

    }
  }  
}

# old code ------

stats4_sd_distributions <- function() {
  
  all_df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  # drop older groups
  df     <- all_df[which(all_df$group %notin% c("older_control", "older_instructed")),]
  
  old_df <- all_df[which(all_df$group %in% c("older_control", "older_instructed")),]
  
  dfs <- list('young'=df, 'old'=old_df)
  
  for(group in names(dfs)) {
    
    cat(sprintf('\n --------\n   %s\n --------\n\n',toupper(group)))
    
    # prepare the data:
    sub_df <- reshape2::melt(dfs[[group]][,c('group','participant','aligned_training_sd','aligned_nocursor_sd','aligned_activelocalization_sd','aligned_passivelocalization_sd')],
                             id.vars=c('group','participant'))
    colnames(sub_df)[which(colnames(sub_df) == 'value')] <- 'sd'
    sub_df$participant <- as.factor(sub_df$participant)
    sub_df$group <- as.factor(sub_df$group)
    
    # traditional ANOVA:
    print(afex::aov_ez(id='participant',dv='sd',data=sub_df,within='variable',type=3))
    
    # Bayesian stats:
    BF <- BayesFactor::anovaBF(sd ~ variable + participant, data=sub_df, whichRandom = 'participant')
    print(BF)
  }
  
}


stats7_variance2_learning <- function() {
  
  descriptors <- read.csv('data/descriptors.csv', 
                          stringsAsFactors = FALSE)
  
  df <- descriptors[which(descriptors$group %in% c('handview','EDSmatch','cursorjump','EDS','control','older_control')),]
  
  df$participant <- as.factor(df$participant)
  df$group <- as.factor(df$group)
  
  print(afex::aov_ez(id='participant',dv='training_rate',data=df,between='group',type=3))
  
  BF <- BayesFactor::anovaBF(training_rate ~ group, data=df)
  print(BF)
  
  # reach aftereffects
  # shift in active and passive hand localization
  #(aftereffects: F(9.192) = 13.213, p <0.001, BF10 = 37.54; passive-shift: F(9.192) = 4.07, p <0.001; active-shift: F(9.192) = 4.06, p <0.001, with BF10 = 003 for both shifts). 
  
  print(afex::aov_ez(id='participant',dv='exclusion',data=df,between='group',type=3))
  
  BF <- BayesFactor::anovaBF(exclusion ~ group, data=df)
  print(BF)
  
  print(afex::aov_ez(id='participant',dv='activelocalization_shift',data=df,between='group',type=3))
  
  BF <- BayesFactor::anovaBF(activelocalization_shift ~ group, data=df)
  print(BF)
  
  print(afex::aov_ez(id='participant',dv='passivelocalization_shift',data=df,between='group',type=3))
  
  BF <- BayesFactor::anovaBF(passivelocalization_shift ~ group, data=df)
  print(BF)
  
  
  
}