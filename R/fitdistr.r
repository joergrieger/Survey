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



  }
  else if(year == 1973 && quarter > 1 | year == 1974 && quarter < 4){
    binleft  <- c(-Inf, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    binright <- c(  -1, -0.1, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1974 && quarter == 4 | year > 1974 && year < 1981 | year == 1981 && quarter < 3){

    binleft  <- c(-Inf, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    binright <- c(3, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9, 12.9, 13.9, 14.9, 15.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1981 && quarter > 2 | year > 1981 && year < 1985 | year == 1985 && quarter == 1){

    binleft  <- c(-Inf, 4, 6, 8, 10, 12)
    binright <- c(4, 5.9, 7.9, 9.9, 11.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1985 && quarter > 1 | year > 1985 && year < 1992){
    binleft  <- c(-Inf, 2, 4, 6, 8, 10)
    binright <- c(2, 3.9, 5.9, 7.9, 9.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    empcdf <- cumsum(srvey)
  }
  else if(year > 1991 && year < 2014){

    binleft  <- c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, 8)
    binright <- c(0, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    empcdf <- cumsum(srvey)
  }
  if(distr == "Norm"){

    fitted <- optimx::optimx(par=c(0,1),fn=fitnorm,lower=c(-Inf,0.001),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)

  }
  else if(distr == "Beta"){

    fitted <- optimx::optimx(par=c(1,1),fn=fitbeta,lower=c(0.001,0.001),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)

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
