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


  retList = structure(list(
    survey   = toupper(survey),
    type     = "individual",
    variable = variable,
    series = tsIndividualFcs
  ),class="survey")
  return(retList)

}

unstck <- function(df,variable){

  # unstack data
  maxID <- max(df$ID)
  individualForecasts <- matrix(NA,nrow=0,ncol=maxID)

  for(ii in min(df$YEAR):max(df$YEAR)){

    tmpFilter <- dplyr::filter(df,YEAR == ii)

    minQuarter <- min(tmpFilter$QUARTER)
    maxQuarter <- max(tmpFilter$QUARTER)

    for(jj in minQuarter:maxQuarter){

      tmpFilter2 <- dplyr::filter(tmpFilter,QUARTER == jj)
      values <- matrix(NA,nrow=1,ncol=maxID)

      for(kk in tmpFilter2$ID){

        value <- dplyr::filter(tmpFilter2, ID == kk)
        values[1,kk] <- suppressWarnings(as.numeric(dplyr::select(value,variable)))

      }
      individualForecasts <- rbind(individualForecasts, values)
    }
  }
  return(individualForecasts)

}
