dispersion <- function(obj,...) UseMethod("dispersion")

dispersion.survey <- function(surveyObj, method = 1){

  # Check if survey object contains individual forecasts
  if(surveyObj$type != "individual"){
    stop("The survey object does not contain individual forecasts")
  }
  # Calculate dispersion

  if(method == 1){

    # Interquartile range

    nr <- nrow(x1$series)
    disp <- array(0,dim=c(nr))

    for(ii in 1:nr){

      xtmp <- quantile(surveyObj$series[ii,],na.rm=TRUE)[c(2,4)]
      disp[ii] <- xtmp[2] - xtmp[1]

    }

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

  }
  else if(method == 2){

    # Standard deviation

    nr <- nrow(x1$series)
    disp <- array(0,dim=c(nr))

    for(ii in 1:nr){

      xtmp <- sd(surveyObj$series[ii,],na.rm=TRUE)
      disp[ii] <- xtmp

    }

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

  }
  else if(method == 3){
    nc <- ncol(surveyObj$series)
    nr <- nrow(surveyObj$series)
    disp <- array(0,dim=c(nr-1))

    tstmp <- matrix(NA,ncol=nc,nrow=(nr-1))

    # Calculate Q/Q growth rates
    for(ii in 1:nc){
      tstmp[,ii] <- tis::growth.rate(surveyObj$series[,ii],simple=TRUE,lag = 1) / 400
    }

    # Calculate disperion measure
    for(ii in 1:(nr-1)){

      xtmp <- quantile(tstmp[ii,],na.rm=TRUE)[c(2,4)]
      disp[ii] <- xtmp[2] - xtmp[1]

    }

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

    if(startQuarter == 4){

      startYear <- startYear + 1
      startQuarter <- 1

    }
    else{

      startQuarter <- startQuarter + 1

    }
  }

  # Create time series object


  tsDisp <- ts(disp, start = c(startYear,startQuarter),frequency = 4)
  return(tsDisp)

}
