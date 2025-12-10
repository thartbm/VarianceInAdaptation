
# Gastrock et al 2020 ----

downloadGastrock2020 <- function() {
  
  # repo: https://osf.io/xdgh6/
  # we want the raw localization data from this repo
  
  Reach::downloadOSFdata(
    repository = "xdgh6",
    filelist = list(
      '/'=c(
        "30explicit_localization.csv",
        "30implicit_localization.csv",
        "cursorjump_localization.csv",
        "handview_localization.csv"
      )
    ),
    folder = "data/gastrock_2020/",
    overwrite = TRUE,
    unzip = FALSE,
    removezips = FALSE,
    wait=30
  )
  
}

loadGastrock2020 <- function() {
  
  data <- NA
  for (group in c('30implicit', '30explicit', 'cursorjump', 'handview')) {
    filepath <- file.path("data", "gastrock_2020", paste0(group, "_localization.csv"))
    groupdata <- read.csv(filepath)
    groupdata$group <- list('30implicit'='control', '30explicit'='instructed', 'cursorjump'='cursorjump', 'handview'='handview')[[group]]
    if (is.data.frame(data)) {
      data <- rbind(data, groupdata)
    } else {
      data <- groupdata
    }
  }
  
  # remoce the rotated parts:
  data <- data[which(data$rotated==0),] # only aligned data
  
  # remove extreme outliers from the arc:
  data <- data[which(abs(data$hand_angle - data$arc) <= 45),]
  
  # compute deviation:
  data$tap_deviation = data$tap_angle - data$hand_angle
  
  return(data)
}

processGastrock2020 <- function(data) {
  
  data <- loadGastrock2020()
  
  participants <- unique(data$participant)
  
  group <- c()
  participant <- c()
  passive_sd <- c()
  active_sd <- c()
  
  for (ppid in participants) {
    
    participant <- c(participant, ppid)
    group <- c(group, data$group[which(data$participant==ppid)][1])

    for (passive in c(0,1)) {
      subdata <- data[which(data$participant==ppid & data$passive==passive),]
      
      # fit smooth spline
      fit <- smooth.spline(subdata$hand_angle, subdata$tap_deviation, spar=0.5, nknots=9)
      
      pred_dev <- predict(fit, subdata$hand_angle)$y
      noise <- sd(subdata$tap_deviation - pred_dev)
      
      if (passive==1) {
        passive_sd <- c(passive_sd, noise)
      } else {
        active_sd <- c(active_sd, noise)
      } 

    }
    
  }
  
  return(data.frame(
    group = group,
    participant = participant,
    passive_sd = passive_sd,
    active_sd = active_sd
  ))
  
}

plotGastrock2020 <- function() {
  
  data <- processGastrock2020()
  
  
  plot(NULL,NULL,
       xlim=c(0,12),ylim=c(0,12),
       
       xlab="Active SD (deg)",
       ylab="Passive SD (deg)",
       main="",
       
       asp=1, bty="n", axes=FALSE
  )
  
  axis(1, at=seq(0,12,4))
  axis(2, at=seq(0,12,4))
  
  lines(x=c(0,12), y=c(0,12), lty=2, col="gray")
  
  points(data$active_sd, data$passive_sd, pch=16, col=rgb(0.1,0,0.9,0.5))
  
}

statsGastrock2020 <- function() {
  
  data <- processGastrock2020()
  
  cat('====>> effect not significant in Gastrock 2020 et al., 2020\n\n')
  
  print(t.test(data$active_sd, data$passive_sd, paired=TRUE))
  
  cat('====>> using effect size in original 2020 data:\n')
  
  print(
    power.t.test(
      n = NULL,
      delta = mean(data$passive_sd - data$active_sd),
      sd = sd(data$passive_sd - data$active_sd),
      power = 0.90,
      sig.level = 0.05,
      
      type = "paired",
      alternative = "one.sided"
    )
  )
  
  cat('====>> using effect size found in the current variance paper (0.3):\n')
  
  print(
    power.t.test(
      n = NULL,
      delta = .3,
      sd = sd(data$passive_sd - data$active_sd),
      power = 0.90,
      sig.level = 0.05,
      
      type = "paired",
      alternative = "one.sided"
    )
  )
  
}


# based on current data ----

AEpowerRecommendation <- function() {
  
  data <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  print(
    power.t.test(
      n = NULL,
      delta = 0.3,
      sd = sd(data$aligned_passivelocalization_sd - data$aligned_activelocalization_sd),
      power = 0.90,
      sig.level = 0.05,
      
      type = "paired",
      alternative = "one.sided"
    )
  )
  
}

RegressionPowerRecommendations <- function() {
  
  data <- read.csv('data/descriptors.csv', stringsAsFactors = FALSE)
  
  # remove pilot groups
  data <- data[which(!(data$group %in% c('org_control', 'org_instructed', 'org_control60', 'org_instructed60'))),]
  
  
  # c('aligned_activelocalization_sd', 'exclusion'),
  # c('aligned_passivelocalization_sd', 'exclusion'),
  
  cat('==>   exlcusion ~ active SD\n')
  print(
    pwr::pwr.r.test(n=NULL,
                    r=cor(data$aligned_activelocalization_sd, data$exclusion),
                    power=0.9,
                    sig.level=0.05,
                    alternative="two.sided"
    )
  )
  
  cat('==>   exlcusion ~ passive SD\n')
  print(
    pwr::pwr.r.test(n=NULL,
                    r=cor(data$aligned_passivelocalization_sd, data$exclusion),
                    power=0.9,
                    sig.level=0.05,
                    alternative="two.sided"
    )
  )
  
  cat('==>   exlcusion ~ active shift\n')
  print(
    pwr::pwr.r.test(n=NULL,
                    r=cor(data$activelocalization_shift, data$exclusion),
                    power=0.9,
                    sig.level=0.05,
                    alternative="two.sided"
    )
  )
  
  cat('==>   exlcusion ~ passive shift\n')
  print(
    pwr::pwr.r.test(n=NULL,
                    r=cor(data$passivelocalization_shift, data$exclusion),
                    power=0.9,
                    sig.level=0.05,
                    alternative="two.sided"
    )
  )
  
  
}


# variance simulation ----


varSimBasic <- function(bs = 1000) {
  
  df <- c()
  lo <- c()
  mid <- c()
  hi <- c()
  
  for (N in seq(3,101,2)) {
    # random samples
    
    simdat <- matrix(data=rnorm(N*bs, mean=0, sd=5), nrow=bs, ncol=N)
    simsds <- apply(simdat, 1, sd)
    CI <- quantile(simsds, probs=c(0.025, 0.500, 0.975))
    
    df <- c(df, N-1)
    lo <- c(lo, CI[1])
    mid <- c(mid, CI[2])
    hi <- c(hi, CI[3])
    
  }
  
  return(data.frame(
    df = df,
    lo = lo,
    hi = hi,
    mid = mid
  ))
  
}

plotVarSim <- function() {
  
  
  plot(NULL,NULL,
       xlim=c(0,100),ylim=c(0,10),
       
       xlab="Degrees of Freedom (N-1)",
       ylab="Standard Deviation Estimate (SD)",
       main="",
       
       bty="n", axes=FALSE
  )
  
  data <- varSimBasic(bs=1000)
  
  lines(data$df, rep(5, nrow(data)), lty=1, col="black")
  
  lines(data$df, data$lo, lty=2, col="red")
  lines(data$df, data$hi, lty=2, col="red")
  lines(data$df, data$mid, lty=2, col="blue")
  
  axis(side=1, at=seq(0,100,20))
  axis(side=2, at=seq(0,10,5))
  
}


varSimSpline <- function(bs=1000) {
  
  df <- c()
  lo <- c()
  mid <- c()
  hi <- c()
  
  for (N in seq(10,101,2)) {
    # random samples
    
    simdat <- matrix(data=rnorm(N*bs, mean=0, sd=5), nrow=bs, ncol=N)
    simsds <- apply(simdat, 1, splined.sd)
    CI <- quantile(simsds, probs=c(0.025, 0.500, 0.975))
    
    df <- c(df, N-1)
    lo <- c(lo, CI[1])
    mid <- c(mid, CI[2])
    hi <- c(hi, CI[3])
    
  }
  
  return(data.frame(
    df = df,
    lo = lo,
    hi = hi,
    mid = mid
  ))
  
  
}

splined.sd <- function(deviations) {
  
  # print(str(x))  
  
  x = seq(20,160, length.out=length(deviations))
  
  xr = (x/180)*pi
  
  isometry <- rnorm(1, mean=-2, sd=2)
  
  y = deviations + (isometry * (sin(2*xr)))
  
  fit <- smooth.spline(x = x,
                       y = y, spar=0.5, nknots=9)
  pred <- predict(fit, x)$y
  noise <- sd(y - pred)
  
  # noise <- sd(y)
  
  return(noise)
  
}


plotVarSplineSim <- function() {
  
  
  plot(NULL,NULL,
       xlim=c(0,100),ylim=c(0,10),
       
       xlab="Degrees of Freedom (N-1)",
       ylab="Standard Deviation Estimate (SD)",
       main="",
       
       bty="n", axes=FALSE
  )
  
  data <- varSimSpline(bs=1000)
  
  lines(data$df, rep(5, nrow(data)), lty=1, col="black")
  
  lines(data$df, data$lo, lty=2, col="red")
  lines(data$df, data$hi, lty=2, col="red")
  lines(data$df, data$mid, lty=2, col="blue")
  
  axis(side=1, at=seq(0,100,20))
  axis(side=2, at=seq(0,10,5))
  
}

plotBothSimulations <- function() {
  
  layout(matrix(c(1,2), nrow=2, ncol=1))
  
  plotVarSim()
  title(main="single point SD estimate")
  
  plotVarSplineSim()
  title(main="spline-based SD estimate across workspace")
  
}