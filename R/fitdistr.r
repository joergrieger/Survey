#' @title fit survey to distribution
#' @param srvObj survey
#' @param year year of forecast
#' @param quarter quarter of forecast
#' @param id forecaster
#' @param distr probability (cu)
#' @export

fit_distribution <- function(srvObj,year,quarter,id,distr="Beta",...) UseMethod("fit_distribution")

#' @export
fit_distribution.probsurvey <- function(srvObj,year,quarter,id,distr="Beta"){
  # Prepare %>% for use in function
  `%>%` <- magrittr::`%>%`

  # Filter out survey data
  filtsrv <- srvObj$forecasts %>%
    dplyr::filter(ID == id) %>%
    dplyr::filter(YEAR == year) %>%
    dplyr::filter(QUARTER == quarter)

  if(dim(filtsrv)[1] == 0){
    stop("Invalid inputs")
  }
  filtsrv <- filtsrv[,-c(1:3)]

  # get bins
  if(year < 1973 | (year == 1973 && quarter == 1)){
      #bins <- rev(c("10+","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1- -0.1","-2 - -1.1","-3 - -2.1","< -3"))
      binleft  <- c(-Inf,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10) # left side of the bin
      binright <- c(-3,-2.1,-1.1,-0.1,0.9,1.9,2.9,3.9,4.9,5.9,6.9,7.9,8.9,9.8,Inf) # right side of the bin

      # get empirical CDF
      srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
      empcdf <- cumsum(srvey)
      #fitted <- optimx::optimx(par=c(1,1),fn=fitbeta,lower=c(0.0,0.0),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)
      fitted <- optimx::optimx(par=c(0,1),fn=fitnorm,lower=c(-Inf,0.001),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)


  }
  return(fitted)

}

fitbeta <- function(x=c(1,1),empcdf = NULL, binright = NULL){

  nlength <- length(empcdf)
  fit <- 0

  for(ii in 1:nlength){

    betacdf <- pbeta(binright[ii],x[1],x[2])
    fit <- fit + (empcdf[ii] - betacdf)^2

  }
  return(fit)
}

fitnorm <- function(x=c(1,1),empcdf = NULL, binright = NULL){
  nlength <- length(empcdf)
  fit <- 0

  for(ii in 1:nlength){

    normcdf <- pnorm(binright[ii],mean=x[1],sd=x[2])
    fit <- fit + (empcdf[ii] - normcdf)^2
    print(fit)

  }
  return(fit)


}
