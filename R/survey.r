#' Download survey data
#' @param survey The survey you wish to download
#' @param type The type you wish you to download, i.e. mean, median or annualized growth rates
#' @return downloadSPF returns an object of class "survey".
#'
#' An object of class "survey" is a list containing the following components
#' \item{survey}{The name of the survey}
#' \item{type}{Type of the survey, i.e. mean, median, growth, or individual (see individualSPF)}
#' \item{variable}{Name of the variable indicating the forecast horizon, e.g. "NGDP1", or "NGDP2", downloaded. If "all", all forecast horizons were downloaded}
#' \item{sseries}{An object of the class ts containing the downloaded series}
#' @examples
#' tmp <- downloadSPF(survey = "CPI", type = "growth")

downloadSPF <- function(survey="NGDP", type="mean"){

  # Make sure the survey name is in lower case
  survey <- tolower(survey)

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
  fi <- readxl::read_excel(tf)
  forecasts <- fi[,-c(1:2)]

  # Create Time Series Object of data
  startYear <- as.numeric(fi[1,1])
  startQuarter <- as.numeric(fi[1,2])
  forecasts <- ts(forecasts,start = c(startYear,startQuarter),frequency = 4)

  # Delete temporary files
  file.remove(tf)

  # create object of class "survey" and return it
  retList = structure(list(
    survey   = survey,
    type     = type,
    variable = "all",
    series   = forecasts
  ),class="survey")
  return(retList)

}
