`%notin%` <- Negate(`%in%`)

library(reshape2)
library(afex)
library(BayesFactor)

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