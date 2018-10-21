downloadSPF <- function(survey="NGDP", type="mean"){

  # Make sure the survey is in upper case
  survey <- toupper(survey)

  # Create URLs
  if(type == "mean"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_",survey,"_level.xlsx",sep="")

  }
  else if(type == "median"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_",survey,"_level.xlsx",sep="")

  }
  else if(type == "growth"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_",survey,"_growth.xlsx",sep="")
  }

  # Download File
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- read_excel(tf)
  forecasts <- fi[,-c(1:2)]

  # Create Time Series Object of data
  startYear <- as.numeric(fi[1,1])
  startQuarter <- as.numeric(fi[1,2])

  forecasts <- ts(forecasts,start = c(startYear,startQuarter),frequency = 4)

  retList = structure(list(
    survey   = survey,
    type     = type,
    variable = "all",
    series   = forecasts
  ),class="survey")

  return(retList)

}
