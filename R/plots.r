#' @title histogram for probabiliy forecasts
#' @param srvObj a S3-object created by probabilitySPF()
#' @param year Year of forecast
#' @param quarter quarter of forecast
#' @param id forecaster id
#' @export
#' @rdname plot_histogram

plot_histogram <- function(srvObj,year = NULL,quarter = NULL,...) UseMethod("plot_histogram")

#' @export
#' @rdname plot_histogram

plot_histogram.probsurvey <- function(srvObj,year = NULL,quarter = NULL, id = NULL){
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

  # Create bins
  filtsrv <- filtsrv[,-c(1:3)]
  if(srvObj$type=="prgdp"){

    binned_survey <- getbins_prgdp(filtsrv,year,quarter)

  }


  # To do: make graph nicer
  ggplot2::ggplot(data=srvey_df,mapping=ggplot2::aes_(x=~bins,y=~probability))+ggplot2::geom_bar(stat="identity")+ggplot2::scale_x_discrete(limits=bins)

}

getbins_prgdp <- function(filtsrv,year,quarter){

  if(year < 1973 | (year == 1973 && quarter == 1)){
    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- rev(c("10+","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1- -0.1","-2 - -1.1","-3 - -2.1","< -3"))
    srvey_df <- data.frame(probability = srvey,bins=bins)
  }
  else if((year == 1973 && quarter >1) | (year == 1974 && quarter < 4)){
    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- c("12+","11-11.9","10-10.9","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1 - 0.1","<-1")
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( (year == 1974 && quarter == 4) | (year > 1974 && year < 1981) | (year == 1981 && quarter < 3)){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- c("16+","15-15.9","14-14.9","13-13.9","12-12.9","11-11.9","10-10.9","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","<3")
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( (year == 1981 && quarter >2 ) | (year > 1981 && year < 1985) | (year == 1985 && quarter == 1)){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    bins <- c("12+","10-11.9","8-9.9","6-7.9","4-5.9","<4")
    srvey_df <- data.frame(probability = srvey,bins=bins)


  }
  else if( (year == 1985 && quarter > 1) | (year > 1985 && year < 1992) ){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    bins <- c("10+","8-9.9","6-7.9","4-5.9","2-3.9","<2")
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if (year > 1991 && year < 2014){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    bins <- c("8+","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","<0")
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( year > 2013){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    bins <- c("4+","3.5-3.9","3.0-3.4","2.5-2.9","2.0-2.4","1.5-1.9","1.0-1.4","0.5-0.9","0-0.4","<0")
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }

  return(srvey_df)

}
