

downloadGastrock2020 <- function() {
  
  # repo: https://osf.io/xdgh6/
  # we want the raw localization data from this repo
  
  Reach::downloadOSFdata(
    repository = "xdgh6",
    filelist = list(
      '/'=c(
        # "30explicit_localization.csv",
        # "30implicit_localization.csv",
        # "cursorjump_localization.csv",
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