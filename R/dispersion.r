#' dispersion calculates cross-sectional measures of forecast dispersion. Currently there are 3 different dispersion measures implemented:
#'
#' @title dispersion
#'
#' \item{Interquartile Range}{75%-quantile minus 25%-quantile}
#' \item{standard deviation}{Standard deviation}
#' \item{Interquartile Range on Q/Q-growth}{75%-quantile minuse 25%-quantile of forecasts of Q/Q-growth}
#'
#' @param surveyObj an object of class survey
#' @param method the dispersion measure
#' \item{1}{Interquartile Range}
#' \item{2}{Standard deviation}
#' \item{3}{Interquartile Range on Q/Q-growth}
#'
#' @return The function returns an object of class ts
#' @examples
#' tmp <- individualSPF(survey = "CPI", variable = "CPI3")
#' dsp <- dispersion(tmp,method = 2)
#'
#' @rdname dispersion
#' @export dispersion


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


    tmp <- t(apply(surveyObj$series,1,quantile,na.rm=TRUE))
    disp <- tmp[,4] - tmp[,2]

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

  }
  else if(method == 2){

    # Standard deviation
    disp <- apply(surveyObj$series,1,sd,na.rm=TRUE)

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

    tmp <- t(apply(tstmp,1,quantile,na.rm=TRUE))
    disp <- tmp[,4] - tmp[,2]

    # Calculate starting year and starting quarter of the time series.
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
