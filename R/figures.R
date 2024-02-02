

`%notin%` <- Negate(`%in%`)

# NCM poster figures ----


fig4_sd_distributions <- function(target='inline') {
  
  colors <- getColors()
  
  all_df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  # drop older groups
  df     <- all_df[which(all_df$group %notin% c("older_control", "older_instructed")),]
  
  old_df <- all_df[which(all_df$group %in% c("older_control", "older_instructed")),]
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig4_sd_distributions.svg', width=9.5, height=4, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig4_sd_distributions.pdf', width=11, height=4)
  }
  

  par(mar=c(3.5, 3.5, 0, 0.1))
  
  plot(-1000, -1000,
       main='', xlab='', ylab='',
       xlim=c(0,8.5), ylim=c(0,30), 
       ax=F, bty='n',
       cex.lab=0.8)
  
  title(ylab='standard deviation [°]',line=2.5,cex.lab=0.8)
  
  axis(side=1, at=c(0.5, 1.5, 2.5, 3.5), labels=c('training','no-cursor','active\nlocalization','passive\nlocalization'),mgp=c(3,2,0),cex.axis=0.8)
  axis(side=1, at=c(5.0, 6.0, 7.0, 8.0), labels=c('training','no-cursor','active\nlocalization','passive\nlocalization'),mgp=c(3,2,0),cex.axis=0.8)
  axis(side=2, at=c(0,10,20,30),cex.axis=0.8)
  
  variables <- c('aligned_training_sd',
                 'aligned_nocursor_sd',
                 'aligned_activelocalization_sd',
                 'aligned_passivelocalization_sd')
  
  for (age_group in c('younger','older')) {
    
    if (age_group == 'younger') {
      use_df <- df
      offset <- 0
      x_offset <- 0
    } else {
      use_df <- old_df
      offset <- 4
      x_offset <- 0.5
    }
    
    for (v_idx in c(1:length(variables))) {
      
      col <- colors[v_idx+offset]
      data <- use_df[,variables[v_idx]]
      lim <- c(-0.95,-0.05) + v_idx + offset + x_offset
      
      addRaincloud(x=data,
                   ori='v',
                   lim=lim,
                   col=col,
                   pch=1)
      
    }
    
    text(x=2.5+offset+x_offset,
         y=25,
         sprintf('%s adults\n(N=%d)',age_group,dim(use_df)[1]),cex=0.8)
    
  }
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


fig5_a_variance_relations <- function(target='inline') {
  
  var_pairs <- list( c('aligned_training_sd','aligned_nocursor_sd'),
                     c('aligned_training_sd','aligned_activelocalization_sd'),
                     c('aligned_training_sd','aligned_passivelocalization_sd'))
  
  exclude_groups <- c('older')
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig5_sd_relations',
                col_idx=c(2,3,4),
                exclude_groups=exclude_groups)
  
}


fig5_variance_relations <- function(target='inline') {
  
  var_pairs <- list( c('aligned_training_sd','aligned_nocursor_sd'),
                     c('aligned_training_sd','aligned_activelocalization_sd'),
                     c('aligned_training_sd','aligned_passivelocalization_sd'),
                     c('aligned_nocursor_sd','aligned_training_sd'),
                     c('aligned_nocursor_sd','aligned_activelocalization_sd'),
                     c('aligned_nocursor_sd','aligned_passivelocalization_sd'))
  
  exclude_groups <- c('older')
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig5_sd_relations',
                col_idx=c(2,3,4,1,3,4),
                pch=1,
                asp=1,
                exclude_groups=exclude_groups,
                addregression='ODR')
  
}


fig6_a_efferent_localization <- function(target='inline') {
  
  var_pairs <- list( c('aligned_activelocalization_sd','aligned_passivelocalization_sd') )
  
  exclude_groups <- c('older')
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig6_efferent_localization',
                col_idx=c(3),
                pch=1,
                lim=20,
                identity=TRUE,
                asp=1,
                exclude_group=exclude_groups)
  
}

fig6_efferent_localization <- function(target='inline') {
  
  var_pairs <- list( c('aligned_activelocalization_sd','aligned_passivelocalization_sd') )
  
  exclude_groups <- c('older')
  
  figX_multi_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig6_efferent_localization',
                col_idx=c(3,7),
                pch=1,
                lim=20,
                identity=TRUE,
                asp=1,
                exclude_group=list(exclude_groups,NULL),
                include_group=list(NULL,exclude_groups),
                addregression='ODR')
  
}

fig7_a_variance2_learning <- function(target='inline') {
  
  var_pairs <- list( 
                     c('aligned_training_sd','training_rate'),
                     c('aligned_training_sd','training_asymptote'),
                     c('aligned_nocursor_sd','training_rate'),
                     c('aligned_nocursor_sd','training_asymptote')
                   )
  
  exclude_groups <- c('older', 'org', '60', 'instructed')
  
  limits <- list( c(30,1),
                  c(30,50),
                  c(30,1),
                  c(30,50) )
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig7_a_basevar_2_learningpars',
                col_idx=c(1,1,2,2),
                pch=16,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=exclude_groups)
  
}

fig7_b_variance2_learning <- function(target='inline') {
  
  var_pairs <- list( 
    c('aligned_activelocalization_sd','training_rate'),
    c('aligned_activelocalization_sd','training_asymptote'),
    c('aligned_passivelocalization_sd','training_rate'),
    c('aligned_passivelocalization_sd','training_asymptote')
  )
  
  exclude_groups <- c('older', 'org', '60', 'instructed')
  
  limits <- list( c(20,1),
                  c(20,50),
                  c(20,1),
                  c(20,50) )
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig7_b_locvar_2_learningpars',
                col_idx=c(3,3,4,4),
                pch=16,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=exclude_groups)
  
}

fig7_variance2_learning <- function(target='inline') {
  
  var_pairs <- list( 
    c('aligned_training_sd','training_rate'),
    c('aligned_nocursor_sd','training_rate'),
    c('aligned_activelocalization_sd','training_rate'),
    c('aligned_passivelocalization_sd','training_rate'),
    
    c('aligned_training_sd','training_asymptote'),
    c('aligned_nocursor_sd','training_asymptote'),
    c('aligned_activelocalization_sd','training_asymptote'),
    c('aligned_passivelocalization_sd','training_asymptote')
  )
  
  exclude_groups <- c('org', '60', 'instructed') # include older?
  
  limits <- list( c(30,1),
                  c(30,1),
                  c(20,1),
                  c(20,1),

                  c(30,50),
                  c(30,50),
                  c(20,50),
                  c(20,50)
                  )
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig7_basevar_2_learningpars',
                col_idx=c(1,2,3,4,1,2,3,4),
                pch=16,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=exclude_groups)
  
}


fig8_localizationSD2_implicit <- function(target='inline') {
  
  var_pairs <- list( 
    c('aligned_activelocalization_sd', 'exclusion'),
    c('aligned_passivelocalization_sd','exclusion'),
    c('aligned_activelocalization_sd', 'activelocalization_shift'),
    c('aligned_passivelocalization_sd','passivelocalization_shift')
  )
  
  exclude_groups <- c('older', 'org', '60', 'instructed')
  
  limits <- list( c(0,20), c(-5,35),
                  c(0,20), c(-5,35),
                  c(0,20), c(10,-20),
                  c(0,20), c(10,-20)   )
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig8_localizationvar_2_implicit',
                col_idx=c(3,4,3,4),
                pch=1,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=exclude_groups)
  
}

# FIGURE 9
# 
# Localization shifts -> Reach aftereffects
# 
# [1,2]
# 
# A: Active localization shift / Reach aftereffects
# B: Passive localization shift / Reach aftereffects


fig9_localizationShift2_implicit <- function(target='inline') {
  
  var_pairs <- list( 
    c('activelocalization_shift',  'exclusion'),
    c('passivelocalization_shift', 'exclusion')
  )
  
  exclude_groups <- c('older', 'org', '60', 'instructed')
  
  limits <- list( c(10,-20), c(-5,35),
                  c(10,-20), c(-5,35)   )
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig9_localizationvShift_2_implicit',
                col_idx=c(3,4),
                pch=16,
                lim=limits,
                identity=TRUE,
                asp=NA,
                exclude_groups=exclude_groups,
                addregression='ODR')
  
}

getLabels <- function(variables) {
  
  map <- c('aligned_training_sd'            = 'training\nstandard deviation [°]',
           'aligned_nocursor_sd'            = 'no-cursor\nstandard deviation [°]',
           'aligned_activelocalization_sd'  = 'active localization\nstandard deviation [°]',
           'aligned_passivelocalization_sd' = 'passive localization\nstandard deviation [°]',
           'training_rate'                  = 'learning rate [%/trial]',
           'training_asymptote'             = 'learning asymptote [°]',
           'exclusion'                      = 'implicit aftereffects [°]',
           'activelocalization_shift'       = 'active localization shift [°]',
           'passivelocalization_shift'      = 'passive localization shift [°]',
           'height_cm'                      = 'height [cm]')

  labels <- c()
  
  for (v in variables) {
    labels <- c(labels, map[v])
  }
  
  return(labels)
  
}


figX_scatters <- function(target='inline',
                          var_pairs,
                          filename,
                          col_idx,
                          pch=16,
                          lim=30,
                          identity=FALSE,
                          asp=NA,
                          exclude_groups=c(),
                          add_info=TRUE,
                          addregression='OLS') {
  
  colors <- getColors()
  
  descriptors <- read.csv('data/descriptors.csv', 
                          stringsAsFactors = FALSE)
  
  demographics <- read.csv('data/demographics.csv',
                           stringsAsFactors = FALSE)
  
  df <- merge(descriptors, demographics, by=c('participant','group'))
  
  
  # drop exclude_groups
  all_groups <- unique(df$group)
  remove_groups <- c()
  # print(exclude_groups)
  for (exclude_group in exclude_groups) {
    for (group in all_groups) {
      # print(sprintf('%s IN %s ?',exclude_group,group))
      if (grepl(exclude_group, group, fixed=TRUE)) {
        # print(c(exclude_group, group))
        remove_groups <- c(remove_groups, group)
      }
    }
  }
  
  # print(unique(remove_groups))
  df <- df[!df$group %in% unique(remove_groups),]
  
  npanels <- length(var_pairs)
  
  width  <- c(4,8,7.5,6,NA,7.5,NA,11)[npanels]
  height <- c(4,4,2.5,6,NA,5.0,NA,4 )[npanels]
  
  if (target=='svg') {
    svglite::svglite(file=sprintf('doc/%s.svg',filename), width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename=sprintf('doc/%s.pdf',filename), width=width, height=height)
  }
  
  par(mar=c(4.5, 4.5, 0.5, 0.5))
  
  layout(mat=matrix(c(1:npanels), 
                    ncol=c(1,2,3,2,NA,3,NA,4)[npanels], 
                    byrow=TRUE)
         )
  
  tick_map <- list('40'=c(0,10,20,30,40),
                   '30'=c(0,10,20,30),
                   '20'=c(0,5,10,15,20),
                   '1'=c(0,0.5,1),
                   '-20'=c(10,0,-10,-20),
                   '35'=c(-5,5,15,25,35))
  
  #  c(10,-20), c(-5,35),
  
  for (var_pair_idx in c(1:length(var_pairs))) {
    
    var_pair <- var_pairs[[var_pair_idx]]
    
    labels <- getLabels(var_pair)
    
    if (class(lim)=='numeric' && length(lim) == 1) {
      xlim <- c(0,lim)
      ylim <- c(0,lim)
    }
    if (class(lim)=='numeric' && length(lim) == 2) {
      xlim <- lim
      ylim <- lim
    }
    if (class(lim)=='list' && length(lim)==npanels) {
      xlim <- c(0,lim[[var_pair_idx]][1])
      ylim <- c(0,lim[[var_pair_idx]][2])
    }
    if (class(lim)=='list' && length(lim)==(2*npanels)) {
      xlim <- lim[[(var_pair_idx*2)-1]]
      ylim <- lim[[ var_pair_idx*2   ]]
    }
    
    plot(-1000, -1000,
         main='', xlab='', ylab='', 
         xlim=xlim, ylim=ylim, 
         ax=F, bty='n',asp=asp)
    
    title(xlab=labels[1],line=3)
    title(ylab=labels[2],line=2)
    
    atx = tick_map[[sprintf('%d',xlim[2])]]
    aty = tick_map[[sprintf('%d',ylim[2])]]
    
    axis(side=1, at=atx, mgp=c(3,0.75,0))
    axis(side=2, at=aty, mgp=c(3,0.75,0))
    
    if (identity) {
      # print(atx)
      # print(aty)
      xsign <- sign(diff(atx[c(1,length(atx))]))
      ysign <- sign(diff(aty[c(1,length(aty))]))
      xrange <- range(atx*xsign)
      yrange <- range(aty*ysign)
      # print(list(xrange=xrange, yrange=yrange))
      id_min <- max(c(min(xrange),min(yrange)))
      id_max <- min(c(max(xrange),max(yrange)))
      id <- c(id_min,id_max)
      lines(x=xsign*id,y=ysign*id,col='#999999',lty=2)
      if (min(id) < 0 && max(id) > 0) {
        lines(x=range(atx), y=c(0,0), col='#999999',lty=2)
        lines(x=c(0,0), y=range(aty), col='#999999',lty=2)
      }
    }
    

    x <- df[,var_pair[1]]
    y <- df[,var_pair[2]]
    
    col <- colors[col_idx[var_pair_idx]]
    
    if (addregression == 'OLS') {
      
      add_OLS_regression(x=x, y=y,
                         col=col, pch=pch)
      
      if (add_info) {
        
        # collect the info:
        N <- length(x)
        
        # also: adjusted R-squared and p-value?
        linmod <- lm(y ~ x)
        Rsquared <- summary(linmod)$adj.r.squared
        f <- summary(linmod)$fstatistic
        p <- pf(f[1],f[2],f[3],lower.tail=F)
        attributes(p) <- NULL
        
        if (p < .001) {
          labels <- sprintf('N = %d\nadj. R² = %0.2f\np<.001', N, Rsquared)
        } else {
          labels <- sprintf('N = %d\nadj. R² = %0.2f\np=%0.2f', N, Rsquared, p)
        }
        
        text(x=xlim[1],
             y=ylim[2],
             labels=labels,
             adj=c(0,1))
  
        
      }
      
    }
    
    if (addregression == 'ODR') {

      add_ODR_regression(x=x, y=y,
                         col=col, pch=pch)
      
      if (add_info) {
        
        # collect the info:
        N <- length(x)
        
        # also: adjusted R-squared and p-value?
        
        odr <- Reach::odregress(x,y)
        R <- odr$r
        slope <- odr$coeff[1]
        labels <- sprintf('N = %d\nR² = %0.2f\nslope = %0.2f', N, R^2, slope)

        text(x=xlim[1],
             y=ylim[2],
             labels=labels,
             adj=c(0,1))
        
      }
      
      
    }
    
  }
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


figX_multi_scatters <- function(target='inline',
                          var_pairs,
                          filename,
                          col_idx,
                          pch=16,
                          lim=30,
                          identity=FALSE,
                          asp=NA,
                          exclude_groups=c(),
                          include_groups=c(),
                          add_info=TRUE,
                          info_offset=8,
                          addregression='OLS') {
  
  colors <- getColors()
  
  df <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  group_dfs <- list()
  
  for (sub_plot in c(1:length(exclude_groups)) ) {
    
    # drop exclude_groups
    all_groups <- unique(df$group)
    
    group_df <- df
    
    if (!is.null(include_groups[[sub_plot]])) {
      keep_groups <- c()
      for (include_group in include_groups[[sub_plot]]) {
        for (group in all_groups) {
          if (grepl(include_group, group, fixed=TRUE)) {
            keep_groups <- c(keep_groups, group)
          }
        }
      }
      group_df <- group_df[df$group %in% unique(keep_groups),]    
    }
    # print(unique(keep_groups))

    if (!is.null(exclude_groups[[sub_plot]])) {
      remove_groups <- c()
      for (exclude_group in exclude_groups[[sub_plot]]) {
        for (group in all_groups) {
          if (grepl(exclude_group, group, fixed=TRUE)) {
            remove_groups <- c(remove_groups, group)
          }
        }
      }
      # print(unique(remove_groups))
      group_df <- group_df[!df$group %in% unique(remove_groups),]
    }
    
    group_dfs[[sub_plot]] <- group_df
  }
  
  npanels <- length(var_pairs)
  
  width  <- c(4,8,7.5,6,NA,NA,NA,11)[npanels]
  height <- c(4,4,2.5,6,NA,NA,NA,4 )[npanels]
  
  if (target=='svg') {
    svglite::svglite(file=sprintf('doc/%s.svg',filename), width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename=sprintf('doc/%s.pdf',filename), width=width, height=height)
  }
  
  par(mar=c(4.5, 4.5, 0.5, 0.5))
  
  layout(mat=matrix(c(1:npanels), 
                    ncol=c(1,2,3,2,NA,NA,NA,4)[npanels], 
                    byrow=TRUE)
  )
  
  tick_map <- list('40'=c(0,10,20,30,40),
                   '30'=c(0,10,20,30),
                   '20'=c(0,5,10,15,20),
                   '1'=c(0,0.5,1),
                   '-20'=c(10,0,-10,-20),
                   '35'=c(-5,5,15,25,35))
  
  #  c(10,-20), c(-5,35),
  
  for (var_pair_idx in c(1:length(var_pairs))) {
    
    var_pair <- var_pairs[[var_pair_idx]]
    
    labels <- getLabels(var_pair)
    
    if (class(lim)=='numeric' && length(lim) == 1) {
      xlim <- c(0,lim)
      ylim <- c(0,lim)
    }
    if (class(lim)=='numeric' && length(lim) == 2) {
      xlim <- lim
      ylim <- lim
    }
    if (class(lim)=='list' && length(lim)==npanels) {
      xlim <- c(0,lim[[var_pair_idx]][1])
      ylim <- c(0,lim[[var_pair_idx]][2])
    }
    if (class(lim)=='list' && length(lim)==(2*npanels)) {
      xlim <- lim[[(var_pair_idx*2)-1]]
      ylim <- lim[[ var_pair_idx*2   ]]
    }
    
    plot(-1000, -1000,
         main='', xlab='', ylab='', 
         xlim=xlim, ylim=ylim, 
         ax=F, bty='n',asp=asp)
    
    title(xlab=labels[1],line=3)
    title(ylab=labels[2],line=2)
    
    atx = tick_map[[sprintf('%d',xlim[2])]]
    aty = tick_map[[sprintf('%d',ylim[2])]]
    
    axis(side=1, at=atx, mgp=c(3,0.75,0))
    axis(side=2, at=aty, mgp=c(3,0.75,0))
    
    if (identity) {
      lines(x=atx,y=aty,col='#999999',lty=2)
    }
    
    for (group_no in c(1:length(group_dfs))) {
      
      group_df <- group_dfs[[group_no]]
      
      x <- group_df[,var_pair[1]]
      y <- group_df[,var_pair[2]]
      
      col <- colors[col_idx[group_no]]
      
      if (addregression == 'OLS') {
        
        add_OLS_regression(x=x, y=y,
                      col=col, pch=pch)
        
        if (add_info) {
          
          # collect the info:
          N <- length(x)
          
          # also: adjusted R-squared and p-value?
          linmod <- lm(y ~ x)
          slope <- coef(linmod)[2]
          Rsquared <- summary(linmod)$adj.r.squared
          f <- summary(linmod)$fstatistic
          p <- pf(f[1],f[2],f[3],lower.tail=F)
          attributes(p) <- NULL
          
          if (p < .001) {
            labels <- sprintf('N = %d\nadj. R² = %0.2f\np<.001\nslope = %0.2f', N, Rsquared, slope)
          } else {
            labels <- sprintf('N = %d\nadj. R² = %0.2f\np=%0.2f\nslope = %0.2f', N, Rsquared, p, slope)
          }
          
          text(x=xlim[1]+((group_no-1)*info_offset),
               y=ylim[2],
               labels=labels,
               adj=c(0,1),
               col=col)
          
        }
        
      }

      if (addregression == 'ODR') {
        
        add_ODR_regression(x=x, y=y,
                           col=col, pch=pch)
        
        if (add_info) {
          
          # collect the info:
          N <- length(x)
          
          # also: adjusted R-squared and p-value?
          ODR <- Reach::odregress(x,y)
          R <- ODR$r
          slope <- ODR$coeff[1]

          labels <- sprintf('N = %d\nR² = %0.2f\nslope = %0.2f', N, R^2, slope)

          text(x=xlim[1]+((group_no-1)*info_offset),
               y=ylim[2],
               labels=labels,
               adj=c(0,1),
               col=col)
          
        }
        
      }
            
    }
    
  }
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}

# color utilities -----


getColors <- function(fmt='rgb', alpha=255) {
  
  colors <- c(  'orange'       = rgb(255, 147,  41, alpha, max = 255),     # replace with neon carrot? ("web-safe")
                'yorkred'      = rgb(229,  22,  54, alpha, max = 255),     # this one should not change
                'pink'         = rgb(207,   0, 216, alpha, max = 255), 
                'purple'       = rgb(127,   0, 216, alpha, max = 255), 
                'cobalt'       = rgb(  0,  71, 171, 255, max = 255),
                "neonblue"     = rgb( 46,  55, 254, alpha, max = 255),
                "curious"      = rgb( 23, 131, 232, alpha, max = 255),
                'turquoise'    = rgb(  0, 206, 209, alpha, max = 255)   )

                # cobalt:              #0047AB
                # dark turquoise:      #00CED1 
                # dark blue:           #00008B (0,0,139) # cobalt should be enough for that
  
  if (fmt == 'col') {
    colors <- t(col2rgb(colors))
    colors <- cbind(colors,rep(alpha,dim(colors)[1]))
    colnames(colors)[4] <- 'alpha'
    return(colors)
  }
  
  if (fmt == 'hsv') {
    return(t(rgb2hsv(col2rgb(colors))))
  }
  
  return(colors)

  
  #col.op <- c('#ff9329ff', '#e51636ff', '#7f00d8ff', '#cc00ccff', '#00ffffff')
  
}


setColorAlpha <- function(col, alpha = 34) {
  
  # store the names:
  colornames <- names(col)
  
  # print(colornames)

  # get RGB values for named color
  rgb.val <- t(col2rgb(col))
  
  # add alpha column:
  # rgb.val <- rbind(rgb.val, rep(alpha, dim(rgb.val)[1]))
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val,
               alpha = alpha,
               max   = 255)
  
  names(t.col) <- colornames
  
  return(t.col)
  
}


# plotting utilities -----


addRaincloud <- function(x, ori='h', lim=c(0,1), col, pch=16, cex=cex, FUN=mean) {
  
  # offsets for different parts of the rain cloud plot to fall between the set limits:
  offsets <- (diff(lim) * c(0.0,0.2,0.25,0.45,0.5,1.0)) + lim[1]
  # 95% confidence interval + mean/median  between  0.0   and  0.2
  # individual data points                 between  0.25  and  0.45
  # kernel density plot                    between  0.5   and  1.0
  
  X <- list()
  Y <- list()
  
  # get coordinates for the 95% confidence interval:
  CI <- Reach::getConfidenceInterval(x, FUN=FUN) # does this work with FUN = median?
  
  #print(CI)
  
  X$CI <- c(CI,rev(CI))
  Y$CI <- offsets[c(1,1,2,2)]
  
  X$central <- rep(FUN(x), 2)
  Y$central <- offsets[c(1,2)]
  
  # get coordinates for the individual points:
  X$points <- x
  Y$points <- (runif(length(x)) * diff(offsets[c(3,4)]))  + offsets[3]
  
  # get the density "cloud":
  dens <- density(x)
  dx <- dens$x
  dy <- (dens$y/max(dens$y)) * diff(offsets[c(5,6)])
    
  X$density_line <- dx
  Y$density_line <- dy + offsets[5]
  
  X$density_poly <- c(      dx[1], dx,            dx[length(dx)] )
  Y$density_poly <- c( offsets[5], dy+offsets[5], offsets[5]          )
  
  # if the orientation is vertical, we need to swap X & Y coordinates:
  if (ori == 'v') {
    Z <- X
    X <- Y
    Y <- Z
  }
  
  # now we do the actual plotting:
  polygon(x=X$CI,
          y=Y$CI,
          border=NA,
          col=setColorAlpha(col))

  lines(x=X$central,
        y=Y$central,
        col=col)

  points(x=X$points,
         y=Y$points,
         col=setColorAlpha(col),
         pch=pch) # can not set cex?
  
  polygon(x=X$density_poly,
          y=Y$density_poly,
          border=NA,
          col=setColorAlpha(col))
  
  lines(x=X$density_line,
        y=Y$density_line,
        col=col)
  
  return()
  
}

add_OLS_regression <- function(x, y, col, pch=16, alpha=34, cex=1) {
  
  idx <- intersect(which(!is.na(x)), which(!is.na(y)))
  x <- x[idx]
  y <- y[idx]
  
  points(x=x,y=y,pch=pch,col=setColorAlpha(col,alpha=alpha), cex=cex)
  
  at <- range(x)
  yx <- lm(y ~ x)
  
  coef <- yx$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col)
  
  ci <- predict( yx,
                 newdata=data.frame(x=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=setColorAlpha(col,alpha=alpha),border=NA)
  
}

add_ODR_regression <- function(x, y, col, pch=16, alpha=34, cex=1) {
  
  idx <- intersect(which(!is.na(x)), which(!is.na(y)))
  x <- x[idx]
  y <- y[idx]
  
  points(x=x,y=y,pch=pch,col=setColorAlpha(col,alpha=alpha), cex=cex)
  
  at <- range(x)
  yx <- Reach::odregress(x,y)
  
  coef <- yx$coeff
  lines(at, coef[2]+(at*coef[1]), col=col)
  
  # ci <- predict( yx,
  #                newdata=data.frame(x=seq(at[1],at[2],length.out=40)),
  #                interval = "confidence")
  # 
  # X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  # Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  # polygon(x=X,y=Y,col=setColorAlpha(col,alpha=alpha),border=NA)
  
}

# try-outs -----


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



# sex/gender (and other demographics?) ------

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

plotLengthEffects <- function(target='inline') {
  
  
  var_pairs <- list( 
    c('height_cm', 'aligned_training_sd'),
    c('height_cm', 'aligned_nocursor_sd'),
    c('height_cm', 'aligned_activelocalization_sd'),
    c('height_cm', 'aligned_passivelocalization_sd')
  )
  
  limits <- list( c(130,210), c(0,20),
                  c(130,210), c(0,20),
                  c(130,210), c(0,20),
                  c(130,210), c(0,20))
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig10a_height_sd',
                col_idx=c(1,2,3,4),
                pch=16,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=c('60'))
  
  
  var_pairs <- list( 
    c('height_cm', 'training_rate'),
    c('height_cm', 'training_asymptote'),
    c('height_cm', 'exclusion')
  )
  
  limits <- list( c(130,210), c(0,1),
                  c(130,210), c(15,45),
                  c(130,210), c(0,30))
  
  figX_scatters(target=target,
                var_pairs=var_pairs,
                filename='Fig10b_height_learning',
                col_idx=c(5,6,7),
                pch=16,
                lim=limits,
                identity=FALSE,
                asp=NA,
                exclude_groups=c('60'))
  
  
  
}