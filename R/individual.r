#' @title Download individual mean survey data
#' @param survey The survey you wish to download
#' @param variable The variable name indicating the forecast horizon, e.g. "NGDP1" or "CPI2".
#' @return individual SPF returns an object of class "survey".
#'
#' An object of class "survey" is a list containing the following components
#' \item{survey}{The name of the survey}
#' \item{type}{Type of the survey, i.e. mean, median, growth, or individual (see individualSPF)}
#' \item{variable}{Name of the variable indicating the forecast horizon, e.g. "NGDP1", or "NGDP2", downloaded. If "all", all forecast horizons were downloaded}
#' \item{series}{An object of the class ts containing the downloaded series}
#' @details This function downloads an individual survey
#' @examples
#' \dontrun{
#' # download the growth rate forecasts for the consumer price index
#' tmp <- downloadSPF(survey = "CPI", type = "growth")
#' }
#'

individualSPF <- function(survey = "NGDP",variable="NGDP1"){

  # Make sure the survey name is in lower case for filename
  survey = tolower(survey)


  # Ditto with variable but in upper case
  variable = toupper(variable)

  # Download Data
  dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_",survey,".xlsx",sep="")
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- readxl::read_excel(tf)


  # unstack data
  individualFcs   <- unstck(fi,variable)
  startYear       <- as.numeric(fi[1,1])
  startQuarter    <- as.numeric(fi[1,2])
  tsIndividualFcs <- ts(individualFcs,start=c(startYear,startQuarter),frequency = 4)

  # Convert into tsibble
  colnames(tsIndividualFcs) <- paste("Forecaster",c(1:588))
  tblIndividualFcs <- tsibble::as.tsibble(tsIndividualFcs)


  # Delete temporary files
  file.remove(tf)



  # Create object of class survey and return it
  retList = structure(list(
    survey   = toupper(survey),
    type     = "individual",
    variable = variable,
    series = tsIndividualFcs
  ),class="survey")
  return(retList)

}

# unstack data
unstck <- function(df,variable){


  maxID <- max(df$ID) # How many individual forecaster
  individualForecasts <- matrix(NA,nrow=0,ncol=maxID) # Initial matrix

  # Loop over all years in df
  for(ii in min(df$YEAR):max(df$YEAR)){

    tmpFilter <- dplyr::filter(df,YEAR == ii)

    minQuarter <- min(tmpFilter$QUARTER)
    maxQuarter <- max(tmpFilter$QUARTER)

    # Loop over all quarters in a year
    for(jj in minQuarter:maxQuarter){

      tmpFilter2 <- dplyr::filter(tmpFilter,QUARTER == jj)
      values <- matrix(NA,nrow=1,ncol=maxID)

      # Loop over all IDs and store forecast appropriate place
      for(kk in tmpFilter2$ID){

        value <- dplyr::filter(tmpFilter2, ID == kk)
        values[1,kk] <- suppressWarnings(as.numeric(dplyr::select(value,variable)))

      }
      individualForecasts <- rbind(individualForecasts, values)
    }
  }
  return(individualForecasts)

}

#' @title download probability forecasts
#' @param survey The survey data to download
#' @return Returns an S3 object of the class probsurvey
#' @export
probabilitySPF <- function(survey ="prgdp"){
  survey = tolower(survey)

  # download file
  dlURL=paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_",survey,".xlsx",sep="")
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- readxl::read_excel(tf,na="#N/A")

  # remove industry
  fi <- fi[,-c(4)]

  # return(fi)
  retlist <- structure(list(forecasts = fi, type=survey),class="probsurvey")
  return(retlist)
}



