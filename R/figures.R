
plotBaselineVariability <- function(groups='all', x, y) {
  
  descriptors <- read.csv('data/descriptors.csv', 
                          stringsAsFactors = FALSE)
  
  if (length(groups) == 1 & groups[1] == 'all') {
    # simply use all the data?
    # or remove the older participants?
  } else {
    descriptors <- descriptors[which(descriptors$group %in% groups),]
  }
  
  # get the data:
  x_col <- descriptors[,sprintf('aligned_%s_sd',x)]
  y_col <- descriptors[,sprintf('aligned_%s_sd',y)]
  
  layout( mat = matrix( c(1:2),
                        ncol=2,
                        nrow=1,
                        byrow=TRUE))
  
  ax_scale <- max(c(max(x_col),max(y_col)))
  ax_scale <- 30 #? this would work for all data... but it's not ideal
  
  x_dens <- density(x_col,
                    n=201, from=0, to=ax_scale) 
  y_dens <- density(y_col,
                    n=201, from=0, to=ax_scale)
  
  dens_y_scale <- max(c(max(x_dens$y),max(y_dens$y)))
  
  plot(x_dens,bty='n',ylim=c(0,dens_y_scale),col='blue',xlab=sprintf('N=%d',length(x_col)),main='')
  lines(y_dens$x,y_dens$y,col='red')
  legend(ax_scale*(1/3),dens_y_scale*(2/3),legend=c(x,y),col=c('blue','red'),bty='n',lty=1)
  #plot(y_dens,bty='n',ylim=c(0,dens_y_scale))
  
  plot(x_col, y_col,
       xlim=c(0,ax_scale), ylim=c(0,ax_scale),
       xlab=sprintf('aligned %s sd',x),
       ylab=sprintf('aligned %s sd',y),
       bty='n')
  
  lines(c(0,ax_scale),c(0,ax_scale),col='#999999',lty=2)
  
}

plotPredictedAdaptation <- function(IVs,DVs,groups='all') {
  
  #IVs have to be from aligned-, and
  #DVs have to be from rotated session
  
  descriptors <- read.csv('data/descriptors.csv', 
                          stringsAsFactors = FALSE)
  
  if (length(groups) == 1 & groups[1] == 'all') {
    # simply use all the data?
    # or remove the older participants?
  } else {
    descriptors <- descriptors[which(descriptors$group %in% groups),]
  }
  
  npairs <- min(length(IVs),length(DVs))
  
  layout( mat = matrix( c(1:(ceiling(npairs/2)*2)),
                        nrow = ceiling(npairs/2),
                        ncol = 2,
                        byrow = TRUE))
  
  for (pair in c(1:npairs)) {
    
    x <- descriptors[,sprintf('aligned_%s',IVs[pair])]
    y <- descriptors[,sprintf('%s',DVs[pair])]
    
    plot(x,y,
         main='',xlab='',ylab='',
         bty='n')
    
  }
  
  
}



# sex/gender ------

plotBaselineVariabilitySex <- function(groups='all',
                                       x='activelocalization',
                                       y='passivelocalization') {
  
  descriptors <- read.csv('data/descriptors.csv', 
                          stringsAsFactors = FALSE)
  
  demographics <- read.csv('data/demographics.csv',
                           stringsAsFactors = FALSE)
  
  males   <- demographics$participant[which(demographics$sex == 'M')]
  females <- demographics$participant[which(demographics$sex == 'F')]
  
  m_idx <- which(demographics$participant %in%   males)
  f_idx <- which(demographics$participant %in% females)
  
  if (length(groups) == 1 & groups[1] == 'all') {
    # simply use all the data?
    # or remove the older participants?
  } else {
    descriptors <- descriptors[which(descriptors$group %in% groups),]
  }
  
  # get the data:
  x_col <- descriptors[,sprintf('aligned_%s_sd',x)]
  y_col <- descriptors[,sprintf('aligned_%s_sd',y)]
  
  layout( mat = matrix( c(1:2),
                        ncol=2,
                        nrow=1,
                        byrow=TRUE))
  
  ax_scale <- max(c(max(x_col),max(y_col)))
  ax_scale <- 30 #? this would work for all data... but it's not ideal
  
  x_m_dens <- density(x_col[m_idx],
                      n=201, from=0, to=ax_scale) 
  x_f_dens <- density(x_col[f_idx],
                      n=201, from=0, to=ax_scale) 
  
  print( BayesFactor::ttestBF(x=x_col[m_idx],
                              y=x_col[f_idx])   )
  
  y_m_dens <- density(y_col[m_idx],
                      n=201, from=0, to=ax_scale)
  y_f_dens <- density(y_col[f_idx],
                      n=201, from=0, to=ax_scale)
  
  print( BayesFactor::ttestBF(x=y_col[m_idx],
                              y=y_col[f_idx])   )
  
  dens_y_scale <- max(c(max(x_m_dens$y),max(x_f_dens$y),max(y_m_dens$y),max(y_f_dens$y)))
  
  legendary <- c( sprintf('male [%d]',   length(m_idx)),
                  sprintf('female [%d]', length(f_idx))  )
  
  
  
  plot(x_m_dens,bty='n',ylim=c(0,dens_y_scale),col='blue',xlab='localization SD [deg]',main='active',ylab='relative density')
  lines(x_f_dens$x,x_f_dens$y,col='red')
  legend(ax_scale*(1/3),dens_y_scale*(2/3),legend=legendary,col=c('blue','red'),bty='n',lty=1)
  #plot(y_dens,bty='n',ylim=c(0,dens_y_scale))
  
  plot(y_m_dens,bty='n',ylim=c(0,dens_y_scale),col='blue',xlab='localization SD [deg]',main='passive',ylab='relative density')
  lines(y_f_dens$x,y_f_dens$y,col='red')
  legend(ax_scale*(1/3),dens_y_scale*(2/3),legend=legendary,col=c('blue','red'),bty='n',lty=1)
  #plot(y_dens,bty='n',ylim=c(0,dens_y_scale))
  
}
