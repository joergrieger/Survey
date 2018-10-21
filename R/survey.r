downloadSPF <- function(survey="NGDP", type="mean"){

  # Make sure the survey is in upper case
  survey <- toupper(survey)

  # Download surveys
  if(survey == "NGDP"){

    # Download forecasts for Nominal GDP

    # Define URLS for downloading data
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_ngdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$NGDP1))
      fi2 <- suppressWarnings(as.numeric(fi$NGDP2))
      fi3 <- suppressWarnings(as.numeric(fi$NGDP3))
      fi4 <- suppressWarnings(as.numeric(fi$NGDP4))
      fi5 <- suppressWarnings(as.numeric(fi$NGDP5))
      fi6 <- suppressWarnings(as.numeric(fi$NGDP6))
      fiA <- suppressWarnings(as.numeric(fi$NGDPA))
      fiB <- suppressWarnings(as.numeric(fi$NGDPB))

      forecasts <- data.frame(NGDP1 = fi1, NGDP2 = fi2, NGDP3 = fi3, NGDP4 = fi4, NGDP5 = fi5, NGDP6 = fi6, NGDPA = fiA, NGDPB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_ngdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- fi$NGDP1
      fi2 <- fi$NGDP2
      fi3 <- fi$NGDP3
      fi4 <- fi$NGDP4
      fi5 <- fi$NGDP5
      fi6 <- fi$NGDP6
      fiA <- fi$NGDPA
      fiB <- fi$NGDPB

      forecasts <- data.frame(NGDP1 = fi1, NGDP2 = fi2, NGDP3 = fi3, NGDP4 = fi4, NGDP5 = fi5, NGDP6 = fi6, NGDPA = fiA, NGDPB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_ngdp_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- fi$DNGDP2
      fi3 <- fi$DNGDP3
      fi4 <- fi$DNGDP4
      fi5 <- fi$DNGDP5
      fi6 <- suppressWarnings( as.numeric( fi$DNGDP6 ) )

      forecasts <- data.frame(DNGDP2 = fi2,DNGDP3 = fi3,DNGDP4 = fi4,DNGDP5 = fi5,DNGDP6 = fi6)

    }
  }
  else if(survey == "PGDP"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_pgdp_level.xlsx?la=en"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- fi$PGDP1
      fi2 <- fi$PGDP2
      fi3 <- fi$PGDP3
      fi4 <- fi$PGDP4
      fi5 <- fi$PGDP5
      fi6 <- fi$PGDP6
      fiA <- fi$PGDPA
      fiB <- fi$PGDPB

      forecasts <- data.frame(PGDP1 = fi1, PGDP2 = fi2, PGDP3 = fi3, PGDP4 = fi4, PGDP5 = fi5, PGDP6 = fi6, PGDPA = fiA, PGDPB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_pgdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- fi$PGDP1
      fi2 <- fi$PGDP2
      fi3 <- fi$PGDP3
      fi4 <- fi$PGDP4
      fi5 <- fi$PGDP5
      fi6 <- fi$PGDP6
      fiA <- fi$PGDPA
      fiB <- fi$PGDPB

      forecasts <- data.frame(PGDP1 = fi1, PGDP2 = fi2, PGDP3 = fi3, PGDP4 = fi4, PGDP5 = fi5, PGDP6 = fi6, PGDPA = fiA, PGDPB = fiB)
    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_pgdp_growth.xlsx"

      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- fi$DPGDP2
      fi3 <- fi$DPGDP3
      fi4 <- fi$DPGDP4
      fi5 <- fi$DPGDP5
      fi6 <- suppressWarnings( as.numeric(fi$DPGDP6) )

      forecasts <- data.frame(DPGDP2 = fi2, DPGDP3 = fi3, DPGDP4 = fi4, DPGDP5 = fi5, DPGDP6 = fi6)

    }
  }
  else if(survey == "CPROF"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_cprof_level.xlsx"

      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- fi$CPROF1
      fi2 <- fi$CPROF2
      fi3 <- fi$CPROF3
      fi4 <- fi$CPROF4
      fi5 <- fi$CPROF5
      fi6 <- fi$CPROF6
      fiA <- fi$CPROFA
      fiB <- fi$CPROFB

      forecasts <- data.frame(CPROF1 = fi1, CPROF2 = fi2, CPROF3 = fi3, CPROF4 = fi4, CPROF5 = fi5, CPROF6 = fi6, CPROFA = fiA, CPROFB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_cprof_level.xlsx"

      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- fi$CPROF1
      fi2 <- fi$CPROF2
      fi3 <- fi$CPROF3
      fi4 <- fi$CPROF4
      fi5 <- fi$CPROF5
      fi6 <- suppressWarnings( as.numeric( fi$CPROF6 ) )
      fiA <- suppressWarnings( as.numeric( fi$CPROFA ) )
      fiB <- suppressWarnings( as.numeric( fi$CPROFB ) )

      forecasts <- data.frame(CPROF1 = fi1, CPROF2 = fi2, CPROF3 = fi3, CPROF4 = fi4, CPROF5 = fi5, CPROF6 = fi6, CPROFA = fiA, CPROFB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_cprof_growth.xlsx"

      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)


      fi2 <- fi$DCPROF2
      fi3 <- fi$DCPROF3
      fi4 <- fi$DCPROF4
      fi5 <- fi$DCPROF5
      fi6 <- suppressWarnings(as.numeric( fi$DCPROF6 ))

      forecasts <- data.frame(DCPROF2 = fi2, DCPROF3 = fi3, DCPROF4 = fi4, DCPROF5 = fi5, DCPROF6 = fi6)

    }

    # Create Time Series Object of data
    startYear <- as.numeric(fi[1,1])
    startQuarter <- as.numeric(fi[1,2])

    forecasts <- ts(forecasts,start = c(startYear,startQuarter),frequency = 4)


  }
  else if(survey == "UNEMP"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_unemp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$UNEMP1) )
      fi2 <- suppressWarnings( as.numeric(fi$UNEMP2) )
      fi3 <- suppressWarnings( as.numeric(fi$UNEMP3) )
      fi4 <- suppressWarnings( as.numeric(fi$UNEMP4) )
      fi5 <- suppressWarnings( as.numeric(fi$UNEMP5) )
      fi6 <- suppressWarnings( as.numeric(fi$UNEMP6) )
      fiA <- suppressWarnings( as.numeric(fi$UNEMPA) )
      fiB <- suppressWarnings( as.numeric(fi$UNEMPB) )
      fiC <- suppressWarnings( as.numeric(fi$UNEMPC) )
      fiD <- suppressWarnings( as.numeric(fi$UNEMPD) )

      forecasts <- data.frame(UNEMP1 = fi1, UNEMP2 = fi2, UNEMP3 = fi3, UNEMP4 = fi4, UNEMP5 = fi5, UNEMP6 = fi6, UNEMPA = fiA,
                              UNEMPB = fiB, UNEMPC = fiC, UNEMPD = fiD)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_unemp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$UNEMP1) )
      fi2 <- suppressWarnings( as.numeric(fi$UNEMP2) )
      fi3 <- suppressWarnings( as.numeric(fi$UNEMP3) )
      fi4 <- suppressWarnings( as.numeric(fi$UNEMP4) )
      fi5 <- suppressWarnings( as.numeric(fi$UNEMP5) )
      fi6 <- suppressWarnings( as.numeric(fi$UNEMP6) )
      fiA <- suppressWarnings( as.numeric(fi$UNEMPA) )
      fiB <- suppressWarnings( as.numeric(fi$UNEMPB) )
      fiC <- suppressWarnings( as.numeric(fi$UNEMPC) )
      fiD <- suppressWarnings( as.numeric(fi$UNEMPD) )

      forecasts <- data.frame(UNEMP1 = fi1, UNEMP2 = fi2, UNEMP3 = fi3, UNEMP4 = fi4, UNEMP5 = fi5, UNEMP6 = fi6, UNEMPA = fiA,
                              UNEMPB = fiB, UNEMPC = fiC, UNEMPD = fiD)

    }
    else if(type == "growth"){

      warning("Growth not available for Unemployment")
    }
  }
  else if(survey == "EMP"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_emp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$EMP1) )
      fi2 <- suppressWarnings( as.numeric(fi$EMP2) )
      fi3 <- suppressWarnings( as.numeric(fi$EMP3) )
      fi4 <- suppressWarnings( as.numeric(fi$EMP4) )
      fi5 <- suppressWarnings( as.numeric(fi$EMP5) )
      fi6 <- suppressWarnings( as.numeric(fi$EMP6) )
      fiA <- suppressWarnings( as.numeric(fi$EMPA) )
      fiB <- suppressWarnings( as.numeric(fi$EMPB) )
      fiC <- suppressWarnings( as.numeric(fi$EMPC) )
      fiD <- suppressWarnings( as.numeric(fi$EMPD) )

      forecasts <- data.frame(EMP1 = fi1, EMP2 = fi2, EMP3 = fi3, EMP4 = fi4, EMP5 = fi5, EMP6 = fi6, EMPA = fiA,
                              EMPB = fiB, EMPC = fiC, EMPD = fiD)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_emp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$EMP1) )
      fi2 <- suppressWarnings( as.numeric(fi$EMP2) )
      fi3 <- suppressWarnings( as.numeric(fi$EMP3) )
      fi4 <- suppressWarnings( as.numeric(fi$EMP4) )
      fi5 <- suppressWarnings( as.numeric(fi$EMP5) )
      fi6 <- suppressWarnings( as.numeric(fi$EMP6) )
      fiA <- suppressWarnings( as.numeric(fi$EMPA) )
      fiB <- suppressWarnings( as.numeric(fi$EMPB) )
      fiC <- suppressWarnings( as.numeric(fi$EMPC) )
      fiD <- suppressWarnings( as.numeric(fi$EMPD) )

      forecasts <- data.frame(EMP1 = fi1, EMP2 = fi2, EMP3 = fi3, EMP4 = fi4, EMP5 = fi5, EMP6 = fi6, EMPA = fiA,
                              EMPB = fiB, EMPC = fiC, EMPD = fiD)
    }
    else if(type == "growth"){

      warning("Growth data not available for Nonfarm Payroll Employment")

    }
  }
  else if(survey == "INDPROD"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_indprod_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$INDPROD1) )
      fi2 <- suppressWarnings( as.numeric(fi$INDPROD2) )
      fi3 <- suppressWarnings( as.numeric(fi$INDPROD3) )
      fi4 <- suppressWarnings( as.numeric(fi$INDPROD4) )
      fi5 <- suppressWarnings( as.numeric(fi$INDPROD5) )
      fi6 <- suppressWarnings( as.numeric(fi$INDPROD6) )
      fiA <- suppressWarnings( as.numeric(fi$INDPRODA) )
      fiB <- suppressWarnings( as.numeric(fi$INDPRODB) )

      forecasts <- data.frame(INDPROD1 = fi1, INDPROD2 = fi2, INDPROD3 = fi3, INDPROD4 = fi4, INDPROD5 = fi5, INDPROD6 = fi6, INDPRODA = fiA,
                              INDPRODB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_indprod_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$INDPROD1) )
      fi2 <- suppressWarnings( as.numeric(fi$INDPROD2) )
      fi3 <- suppressWarnings( as.numeric(fi$INDPROD3) )
      fi4 <- suppressWarnings( as.numeric(fi$INDPROD4) )
      fi5 <- suppressWarnings( as.numeric(fi$INDPROD5) )
      fi6 <- suppressWarnings( as.numeric(fi$INDPROD6) )
      fiA <- suppressWarnings( as.numeric(fi$INDPRODA) )
      fiB <- suppressWarnings( as.numeric(fi$INDPRODB) )

      forecasts <- data.frame(INDPROD1 = fi1, INDPROD2 = fi2, INDPROD3 = fi3, INDPROD4 = fi4, INDPROD5 = fi5, INDPROD6 = fi6, INDPRODA = fiA,
                              INDPRODB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_indprod_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings( as.numeric(fi$DINDPROD2) )
      fi3 <- suppressWarnings( as.numeric(fi$DINDPROD3) )
      fi4 <- suppressWarnings( as.numeric(fi$DINDPROD4) )
      fi5 <- suppressWarnings( as.numeric(fi$DINDPROD5) )
      fi6 <- suppressWarnings( as.numeric(fi$DINDPROD6) )

      forecasts <- data.frame(DINDPROD2 = fi2, DINDPROD3 = fi3, DINDPROD4 = fi4, DINDPROD5 = fi5, DINDPROD6 = fi6, DINDPRODA = fiA,
                              DINDPRODB = fiB)

    }

  }
  else if(survey == "HOUSING"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_housing_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$HOUSING1) )
      fi2 <- suppressWarnings( as.numeric(fi$HOUSING2) )
      fi3 <- suppressWarnings( as.numeric(fi$HOUSING3) )
      fi4 <- suppressWarnings( as.numeric(fi$HOUSING4) )
      fi5 <- suppressWarnings( as.numeric(fi$HOUSING5) )
      fi6 <- suppressWarnings( as.numeric(fi$HOUSING6) )
      fiA <- suppressWarnings( as.numeric(fi$HOUSINGA) )
      fiB <- suppressWarnings( as.numeric(fi$HOUSINGB) )

      forecasts <- data.frame(HOUSING1 = fi1, HOUSING2 = fi2, HOUSING3 = fi3, HOUSING4 = fi4, HOUSING5 = fi5, HOUSING6 = fi6, HOUSINGA = fiA,
                              HOUSINGB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_housing_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$HOUSING1) )
      fi2 <- suppressWarnings( as.numeric(fi$HOUSING2) )
      fi3 <- suppressWarnings( as.numeric(fi$HOUSING3) )
      fi4 <- suppressWarnings( as.numeric(fi$HOUSING4) )
      fi5 <- suppressWarnings( as.numeric(fi$HOUSING5) )
      fi6 <- suppressWarnings( as.numeric(fi$HOUSING6) )
      fiA <- suppressWarnings( as.numeric(fi$HOUSINGA) )
      fiB <- suppressWarnings( as.numeric(fi$HOUSINGB) )

      forecasts <- data.frame(HOUSING1 = fi1, HOUSING2 = fi2, HOUSING3 = fi3, HOUSING4 = fi4, HOUSING5 = fi5, HOUSING6 = fi6, HOUSINGA = fiA,
                              HOUSINGB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_housing_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings( as.numeric(fi$DHOUSING2) )
      fi3 <- suppressWarnings( as.numeric(fi$DHOUSING3) )
      fi4 <- suppressWarnings( as.numeric(fi$DHOUSING4) )
      fi5 <- suppressWarnings( as.numeric(fi$DHOUSING5) )
      fi6 <- suppressWarnings( as.numeric(fi$DHOUSING6) )

      forecasts <- data.frame(DHOUSING2 = fi2, DHOUSING3 = fi3, DHOUSING4 = fi4, DHOUSING5 = fi5, DHOUSING6 = fi6)

    }

  }
  else if(survey == "TBILL"){
    if(type == "mean"){
      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_tbill_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$TBILL1) )
      fi2 <- suppressWarnings( as.numeric(fi$TBILL2) )
      fi3 <- suppressWarnings( as.numeric(fi$TBILL3) )
      fi4 <- suppressWarnings( as.numeric(fi$TBILL4) )
      fi5 <- suppressWarnings( as.numeric(fi$TBILL5) )
      fi6 <- suppressWarnings( as.numeric(fi$TBILL6) )
      fiA <- suppressWarnings( as.numeric(fi$TBILLA) )
      fiB <- suppressWarnings( as.numeric(fi$TBILLB) )
      fiC <- suppressWarnings( as.numeric(fi$TBILLC) )
      fiD <- suppressWarnings( as.numeric(fi$TBILLD) )

      forecasts <- data.frame(TBILL1 = fi1, TBILL2 = fi2, TBILL3 = fi3, TBILL4 = fi4, TBILL5 = fi5, TBILL6 = fi6, TBILL = fiA,
                              TBILLB = fiB, TBILLC = fiC, TBILLD = fiD)
    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_tbill_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$TBILL1) )
      fi2 <- suppressWarnings( as.numeric(fi$TBILL2) )
      fi3 <- suppressWarnings( as.numeric(fi$TBILL3) )
      fi4 <- suppressWarnings( as.numeric(fi$TBILL4) )
      fi5 <- suppressWarnings( as.numeric(fi$TBILL5) )
      fi6 <- suppressWarnings( as.numeric(fi$TBILL6) )
      fiA <- suppressWarnings( as.numeric(fi$TBILLA) )
      fiB <- suppressWarnings( as.numeric(fi$TBILLB) )
      fiC <- suppressWarnings( as.numeric(fi$TBILLC) )
      fiD <- suppressWarnings( as.numeric(fi$TBILLD) )

      forecasts <- data.frame(TBILL1 = fi1, TBILL2 = fi2, TBILL3 = fi3, TBILL4 = fi4, TBILL5 = fi5, TBILL6 = fi6, TBILLA = fiA,
                              TBILLB = fiB, TBILLC = fiC, TBILLD = fiD)

    }
    else if(type == "growth"){
      warning("No growth rate for forecasts of 3-Month Treasury Bill Rate available")
    }

  }
  else if(survey == "BOND"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_bond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$BOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$BOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$BOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$BOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$BOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$BOND6) )
      fiA <- suppressWarnings( as.numeric(fi$BONDA) )
      fiB <- suppressWarnings( as.numeric(fi$BONDB) )

      forecasts <- data.frame(BOND1 = fi1, BOND2 = fi2, BOND3 = fi3, BOND4 = fi4, BOND5 = fi5, BOND6 = fi6, BONDA = fiA,
                              BONDB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_bond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$BOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$BOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$BOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$BOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$BOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$BOND6) )
      fiA <- suppressWarnings( as.numeric(fi$BONDA) )
      fiB <- suppressWarnings( as.numeric(fi$BONDB) )

      forecasts <- data.frame(BOND1 = fi1, BOND2 = fi2, BOND3 = fi3, BOND4 = fi4, BOND5 = fi5, BOND6 = fi6, BONDA = fiA,
                              BONDB = fiB)

    }
    else if(type == "growth"){

      warning("Growth not available for forecasts of Moody's AAA Corporate Bond Yield")

    }
  }
  else if(survey == "BAABOND"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_baabond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$BAABOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$BAABOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$BAABOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$BAABOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$BAABOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$BAABOND6) )
      fiA <- suppressWarnings( as.numeric(fi$BAABONDA) )
      fiB <- suppressWarnings( as.numeric(fi$BAABONDB) )

      forecasts <- data.frame(BAABOND1 = fi1, BAABOND2 = fi2, BAABOND3 = fi3, BAABOND4 = fi4, BAABOND5 = fi5, BAABOND6 = fi6, BAABONDA = fiA,
                              BAABONDB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_baabond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$BAABOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$BAABOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$BAABOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$BAABOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$BAABOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$BAABOND6) )
      fiA <- suppressWarnings( as.numeric(fi$BAABONDA) )
      fiB <- suppressWarnings( as.numeric(fi$BAABONDB) )

      forecasts <- data.frame(BAABOND1 = fi1, BAABOND2 = fi2, BAABOND3 = fi3, BAABOND4 = fi4, BAABOND5 = fi5, BAABOND6 = fi6, BAABONDA = fiA,
                              BAABONDB = fiB)

    }
    else if(type == "growth"){

      warning("Growth not available for forecasts of Moody's BAA Corporate Bond Yield")

    }

  }
  else if(survey == "TBOND"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_tbond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$TBOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$TBOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$TBOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$TBOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$TBOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$TBOND6) )
      fiA <- suppressWarnings( as.numeric(fi$TBONDA) )
      fiB <- suppressWarnings( as.numeric(fi$TBONDB) )

      forecasts <- data.frame(TBOND1 = fi1, TBOND2 = fi2, TBOND3 = fi3, TBOND4 = fi4, TBOND5 = fi5, TBOND6 = fi6, TBONDA = fiA,
                              TBONDB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_tbond_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings( as.numeric(fi$TBOND1) )
      fi2 <- suppressWarnings( as.numeric(fi$TBOND2) )
      fi3 <- suppressWarnings( as.numeric(fi$TBOND3) )
      fi4 <- suppressWarnings( as.numeric(fi$TBOND4) )
      fi5 <- suppressWarnings( as.numeric(fi$TBOND5) )
      fi6 <- suppressWarnings( as.numeric(fi$TBOND6) )
      fiA <- suppressWarnings( as.numeric(fi$TBONDA) )
      fiB <- suppressWarnings( as.numeric(fi$TBONDB) )

      forecasts <- data.frame(TBOND1 = fi1, TBOND2 = fi2, TBOND3 = fi3, TBOND4 = fi4, TBOND5 = fi5, TBOND6 = fi6, TBONDA = fiA,
                              TBONDB = fiB)

    }
    else if(type == "growth"){

      warning("Growth not available for forecasts of 10 Year Treasury Bonds")

    }
  }
  else if(survey == "RGDP"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rgdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RGDP1))
      fi2 <- suppressWarnings(as.numeric(fi$RGDP2))
      fi3 <- suppressWarnings(as.numeric(fi$RGDP3))
      fi4 <- suppressWarnings(as.numeric(fi$RGDP4))
      fi5 <- suppressWarnings(as.numeric(fi$RGDP5))
      fi6 <- suppressWarnings(as.numeric(fi$RGDP6))
      fiA <- suppressWarnings(as.numeric(fi$RGDPA))
      fiB <- suppressWarnings(as.numeric(fi$RGDPB))
      fiC <- suppressWarnings(as.numeric(fi$RGDPC))
      fiD <- suppressWarnings(as.numeric(fi$RGDPD))

      forecasts <- data.frame(RGDP1 = fi1, RGDP2 = fi2, RGDP3 = fi3, RGDP4 = fi4, RGDP5 = fi5, RGDP6 = fi6, RGDPA = fiA,
                              RGDPB = fiB, RGDPC = fiC, RGDPD = fiD)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rgdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RGDP1))
      fi2 <- suppressWarnings(as.numeric(fi$RGDP2))
      fi3 <- suppressWarnings(as.numeric(fi$RGDP3))
      fi4 <- suppressWarnings(as.numeric(fi$RGDP4))
      fi5 <- suppressWarnings(as.numeric(fi$RGDP5))
      fi6 <- suppressWarnings(as.numeric(fi$RGDP6))
      fiA <- suppressWarnings(as.numeric(fi$RGDPA))
      fiB <- suppressWarnings(as.numeric(fi$RGDPB))
      fiC <- suppressWarnings(as.numeric(fi$RGDPC))
      fiD <- suppressWarnings(as.numeric(fi$RGDPD))

      forecasts <- data.frame(RGDP1 = fi1, RGDP2 = fi2, RGDP3 = fi3, RGDP4 = fi4, RGDP5 = fi5, RGDP6 = fi6, RGDPA = fiA,
                              RGDPB = fiB, RGDPC = fiC, RGDPD = fiD)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rgdp_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DRGDP2))
      fi3 <- suppressWarnings(as.numeric(fi$DRGDP3))
      fi4 <- suppressWarnings(as.numeric(fi$DRGDP4))
      fi5 <- suppressWarnings(as.numeric(fi$DRGDP5))
      fi6 <- suppressWarnings(as.numeric(fi$DRGDP6))

      forecasts <- data.frame(DRGDP2 = fi2, DRGDP3 = fi3, DRGDP4 = fi4, DRGDP5 = fi5, DRGDP6 = fi6)

    }

  }
  else if(survey == "RCONSUM"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rgdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RCONSUM1))
      fi2 <- suppressWarnings(as.numeric(fi$RCONSUM2))
      fi3 <- suppressWarnings(as.numeric(fi$RCONSUM3))
      fi4 <- suppressWarnings(as.numeric(fi$RCONSUM4))
      fi5 <- suppressWarnings(as.numeric(fi$RCONSUM5))
      fi6 <- suppressWarnings(as.numeric(fi$RCONSUM6))
      fiA <- suppressWarnings(as.numeric(fi$RCONSUMA))
      fiB <- suppressWarnings(as.numeric(fi$RCONSUMB))
      fiC <- suppressWarnings(as.numeric(fi$RCONSUMC))
      fiD <- suppressWarnings(as.numeric(fi$RCONSUMD))

      forecasts <- data.frame(RCONSUM1 = fi1, RCONSUM2 = fi2, RCONSUM3 = fi3, RCONSUM4 = fi4, RCONSUM5 = fi5, RCONSUM6 = fi6, RCONSUMA = fiA,
                              RCONSUMB = fiB, RCONSUMC = fiC, RCONSUMD = fiD)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rgdp_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RCONSUM1))
      fi2 <- suppressWarnings(as.numeric(fi$RCONSUM2))
      fi3 <- suppressWarnings(as.numeric(fi$RCONSUM3))
      fi4 <- suppressWarnings(as.numeric(fi$RCONSUM4))
      fi5 <- suppressWarnings(as.numeric(fi$RCONSUM5))
      fi6 <- suppressWarnings(as.numeric(fi$RCONSUM6))
      fiA <- suppressWarnings(as.numeric(fi$RCONSUMA))
      fiB <- suppressWarnings(as.numeric(fi$RCONSUMB))
      fiC <- suppressWarnings(as.numeric(fi$RCONSUMC))
      fiD <- suppressWarnings(as.numeric(fi$RCONSUMD))

      forecasts <- data.frame(RCONSUM1 = fi1, RCONSUM2 = fi2, RCONSUM3 = fi3, RCONSUM4 = fi4, RCONSUM5 = fi5, RCONSUM6 = fi6, RCONSUMA = fiA,
                              RCONSUMB = fiB, RCONSUMC = fiC, RCONSUMD = fiD)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rconsum_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)


      fi2 <- suppressWarnings(as.numeric(fi$DRCONSUM2))
      fi3 <- suppressWarnings(as.numeric(fi$DRCONSUM3))
      fi4 <- suppressWarnings(as.numeric(fi$DRCONSUM4))
      fi5 <- suppressWarnings(as.numeric(fi$DRCONSUM5))
      fi6 <- suppressWarnings(as.numeric(fi$DRCONSUM6))

      forecasts <- data.frame(DRCONSUM2 = fi2, DRCONSUM3 = fi3, DRCONSUM4 = fi4, DRCONSUM5 = fi5, DRCONSUM6 = fi6)

    }

  }
  else if(survey == "RNRESIN"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rnresin_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RNRESIN1))
      fi2 <- suppressWarnings(as.numeric(fi$RNRESIN2))
      fi3 <- suppressWarnings(as.numeric(fi$RNRESIN3))
      fi4 <- suppressWarnings(as.numeric(fi$RNRESIN4))
      fi5 <- suppressWarnings(as.numeric(fi$RNRESIN5))
      fi6 <- suppressWarnings(as.numeric(fi$RNRESIN6))
      fiA <- suppressWarnings(as.numeric(fi$RNRESINA))
      fiB <- suppressWarnings(as.numeric(fi$RNRESINB))

      forecasts <- data.frame(RNRESIN1 = fi1, RNRESIN2 = fi2, RNRESIN3 = fi3, RNRESIN4 = fi4, RNRESIN5 = fi5, RNRESIN6 = fi6, RNRESINA = fiA,
                              RNRESINB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rnresin_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RNRESIN1))
      fi2 <- suppressWarnings(as.numeric(fi$RNRESIN2))
      fi3 <- suppressWarnings(as.numeric(fi$RNRESIN3))
      fi4 <- suppressWarnings(as.numeric(fi$RNRESIN4))
      fi5 <- suppressWarnings(as.numeric(fi$RNRESIN5))
      fi6 <- suppressWarnings(as.numeric(fi$RNRESIN6))
      fiA <- suppressWarnings(as.numeric(fi$RNRESINA))
      fiB <- suppressWarnings(as.numeric(fi$RNRESINB))

      forecasts <- data.frame(RNRESIN1 = fi1, RNRESIN2 = fi2, RNRESIN3 = fi3, RNRESIN4 = fi4, RNRESIN5 = fi5, RNRESIN6 = fi6, RNRESINA = fiA,
                              RNRESINB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rnresin_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DRNRESIN2))
      fi3 <- suppressWarnings(as.numeric(fi$DRNRESIN3))
      fi4 <- suppressWarnings(as.numeric(fi$DRNRESIN4))
      fi5 <- suppressWarnings(as.numeric(fi$DRNRESIN5))
      fi6 <- suppressWarnings(as.numeric(fi$DRNRESIN6))


      forecasts <- data.frame(DRNRESIN2 = fi2,DRNRESIN3 = fi3, DRNRESIN4 = fi4, DRNRESIN5 = fi5, DRNRESIN6 = fi6)

    }
  }
  else if(survey == "RRESINV"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rresinv_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RRESINV1))
      fi2 <- suppressWarnings(as.numeric(fi$RRESINV2))
      fi3 <- suppressWarnings(as.numeric(fi$RRESINV3))
      fi4 <- suppressWarnings(as.numeric(fi$RRESINV4))
      fi5 <- suppressWarnings(as.numeric(fi$RRESINV5))
      fi6 <- suppressWarnings(as.numeric(fi$RRESINV6))
      fiA <- suppressWarnings(as.numeric(fi$RRESINVA))
      fiB <- suppressWarnings(as.numeric(fi$RRESINVB))

      forecasts <- data.frame(RRESINV1 = fi1, RRESINV2 = fi2, RRESINV3 = fi3, RRESINV4 = fi4, RRESINV5 = fi5, RRESINV6 = fi6, RRESINVVA = fiA,
                              RRESINVB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rresinv_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RRESINV1))
      fi2 <- suppressWarnings(as.numeric(fi$RRESINV2))
      fi3 <- suppressWarnings(as.numeric(fi$RRESINV3))
      fi4 <- suppressWarnings(as.numeric(fi$RRESINV4))
      fi5 <- suppressWarnings(as.numeric(fi$RRESINV5))
      fi6 <- suppressWarnings(as.numeric(fi$RRESINV6))
      fiA <- suppressWarnings(as.numeric(fi$RRESINVA))
      fiB <- suppressWarnings(as.numeric(fi$RRESINVB))

      forecasts <- data.frame(RRESINV1 = fi1, RRESINV2 = fi2, RRESINV3 = fi3, RRESINV4 = fi4, RRESINV5 = fi5, RRESINV6 = fi6, RRESINVVA = fiA,
                              RRESINVB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rresinv_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DRRESINV2))
      fi3 <- suppressWarnings(as.numeric(fi$DRRESINV3))
      fi4 <- suppressWarnings(as.numeric(fi$DRRESINV4))
      fi5 <- suppressWarnings(as.numeric(fi$DRRESINV5))
      fi6 <- suppressWarnings(as.numeric(fi$DRRESINV6))

      forecasts <- data.frame(DRRESINV2 = fi2, DRRESINV3 = fi3, DRRESINV4 = fi4, DRRESINV5 = fi5, DRRESINV6 = fi6)

    }
  }
  else if(survey == "RFEDGOV"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rfedgov_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RFEDGOV1))
      fi2 <- suppressWarnings(as.numeric(fi$RFEDGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$RFEDGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$RFEDGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$RFEDGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$RFEDGOV6))
      fiA <- suppressWarnings(as.numeric(fi$RFEDGOVA))
      fiB <- suppressWarnings(as.numeric(fi$RFEDGOVB))

      forecast <- data.frame(RFEDGOV1 = fi1,RFEDGOV2 = fi2,RFEDGOV3 = fi3,RFEDGOV4 = fi4,RFEDGOV5 = fi5,RFEDGOV6 = fi6,RFEDGOVA = fiA,
                             RFEDGOVB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rfedgov_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RFEDGOV1))
      fi2 <- suppressWarnings(as.numeric(fi$RFEDGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$RFEDGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$RFEDGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$RFEDGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$RFEDGOV6))
      fiA <- suppressWarnings(as.numeric(fi$RFEDGOVA))
      fiB <- suppressWarnings(as.numeric(fi$RFEDGOVB))

      forecast <- data.frame(RFEDGOV1 = fi1,RFEDGOV2 = fi2,RFEDGOV3 = fi3,RFEDGOV4 = fi4,RFEDGOV5 = fi5,RFEDGOV6 = fi6,RFEDGOVA = fiA,
                             RFEDGOVB = fiB)


    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rfedgov_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)


      fi2 <- suppressWarnings(as.numeric(fi$DRFEDGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$DRFEDGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$DRFEDGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$DRFEDGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$DRFEDGOV6))

      forecast <- data.frame(RFEDGOV2 = fi2,RFEDGOV3 = fi3,RFEDGOV4 = fi4,RFEDGOV5 = fi5,RFEDGOV6 = fi6)

    }

  }
  else if(survey == "RSLGOV"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rslgov_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RSLGOV1))
      fi2 <- suppressWarnings(as.numeric(fi$RSLGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$RSLGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$RSLGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$RSLGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$RSLGOV6))
      fiA <- suppressWarnings(as.numeric(fi$RSLGOVA))
      fiB <- suppressWarnings(as.numeric(fi$RSLGOVB))

      forecast <- data.frame(RSLGOV1 = fi1,RSLGOV2 = fi2,RSLGOV3 = fi3,RSLGOV4 = fi4,RSLGOV5 = fi5,RSLGOV6 = fi6,RSLGOVA = fiA,
                             RFEDGOVB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rslgov_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RSLGOV1))
      fi2 <- suppressWarnings(as.numeric(fi$RSLGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$RSLGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$RSLGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$RSLGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$RSLGOV6))
      fiA <- suppressWarnings(as.numeric(fi$RSLGOVA))
      fiB <- suppressWarnings(as.numeric(fi$RSLGOVB))

      forecast <- data.frame(RSLGOV1 = fi1,RSLGOV2 = fi2,RSLGOV3 = fi3,RSLGOV4 = fi4,RSLGOV5 = fi5,RSLGOV6 = fi6,RSLGOVA = fiA,
                             RFEDGOVB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rslgov_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DRSLGOV2))
      fi3 <- suppressWarnings(as.numeric(fi$DRSLGOV3))
      fi4 <- suppressWarnings(as.numeric(fi$DRSLGOV4))
      fi5 <- suppressWarnings(as.numeric(fi$DRSLGOV5))
      fi6 <- suppressWarnings(as.numeric(fi$DRSLGOV6))

      forecast <- data.frame(DRSLGOV2 = fi2,DRSLGOV3 = fi3,DRSLGOV4 = fi4,DRSLGOV5 = fi5,DRSLGOV6 = fi6)
    }
  }
  else if(survey == "RCBI"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rcbi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RCBI1))
      fi2 <- suppressWarnings(as.numeric(fi$RCBI2))
      fi3 <- suppressWarnings(as.numeric(fi$RCBI3))
      fi4 <- suppressWarnings(as.numeric(fi$RCBI4))
      fi5 <- suppressWarnings(as.numeric(fi$RCBI5))
      fi6 <- suppressWarnings(as.numeric(fi$RCBI6))
      fiA <- suppressWarnings(as.numeric(fi$RCBIA))
      fiB <- suppressWarnings(as.numeric(fi$RCBIB))

      forecast <- data.frame(RCBI1 = fi1,RCBI2 = fi2,RCBI3 = fi3,RCBI4 = fi4,RCBI5 = fi5,RCBI6 = fi6,RCBIA = fiA,
                             RCBIB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rcbi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$RCBI1))
      fi2 <- suppressWarnings(as.numeric(fi$RCBI2))
      fi3 <- suppressWarnings(as.numeric(fi$RCBI3))
      fi4 <- suppressWarnings(as.numeric(fi$RCBI4))
      fi5 <- suppressWarnings(as.numeric(fi$RCBI5))
      fi6 <- suppressWarnings(as.numeric(fi$RCBI6))
      fiA <- suppressWarnings(as.numeric(fi$RCBIA))
      fiB <- suppressWarnings(as.numeric(fi$RCBIB))

      forecast <- data.frame(RCBI1 = fi1,RCBI2 = fi2,RCBI3 = fi3,RCBI4 = fi4,RCBI5 = fi5,RCBI6 = fi6,RCBIA = fiA,
                             RCBIB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rcbi_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DRCBI2))
      fi3 <- suppressWarnings(as.numeric(fi$DRCBI3))
      fi4 <- suppressWarnings(as.numeric(fi$DRCBI4))
      fi5 <- suppressWarnings(as.numeric(fi$DRCBI5))
      fi6 <- suppressWarnings(as.numeric(fi$DRCBI6))

      forecast <- data.frame(RCBI2 = fi2,RCBI3 = fi3,RCBI4 = fi4,RCBI5 = fi5,RCBI6 = fi6)

    }
  }
  else if(survey == "REXPORT"){

    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rexport_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$REXPORT1))
      fi2 <- suppressWarnings(as.numeric(fi$REXPORT2))
      fi3 <- suppressWarnings(as.numeric(fi$REXPORT3))
      fi4 <- suppressWarnings(as.numeric(fi$REXPORT4))
      fi5 <- suppressWarnings(as.numeric(fi$REXPORT5))
      fi6 <- suppressWarnings(as.numeric(fi$REXPORT6))
      fiA <- suppressWarnings(as.numeric(fi$REXPORTA))
      fiB <- suppressWarnings(as.numeric(fi$REXPORTB))

      forecast <- data.frame(REXPORT1 = fi1,REXPORT2 = fi2,REXPORT3 = fi3,REXPORT4 = fi4,REXPORT5 = fi5,REXPORT6 = fi6,REXPORTA = fiA,
                             REXPORTB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_rexport_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$REXPORT1))
      fi2 <- suppressWarnings(as.numeric(fi$REXPORT2))
      fi3 <- suppressWarnings(as.numeric(fi$REXPORT3))
      fi4 <- suppressWarnings(as.numeric(fi$REXPORT4))
      fi5 <- suppressWarnings(as.numeric(fi$REXPORT5))
      fi6 <- suppressWarnings(as.numeric(fi$REXPORT6))
      fiA <- suppressWarnings(as.numeric(fi$REXPORTA))
      fiB <- suppressWarnings(as.numeric(fi$REXPORTB))

      forecast <- data.frame(REXPORT1 = fi1,REXPORT2 = fi2,REXPORT3 = fi3,REXPORT4 = fi4,REXPORT5 = fi5,REXPORT6 = fi6,REXPORTA = fiA,
                             REXPORTB = fiB)

    }
    else if(type == "growth"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_rexport_growth.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi2 <- suppressWarnings(as.numeric(fi$DREXPORT2))
      fi3 <- suppressWarnings(as.numeric(fi$DREXPORT3))
      fi4 <- suppressWarnings(as.numeric(fi$DREXPORT4))
      fi5 <- suppressWarnings(as.numeric(fi$DREXPORT5))
      fi6 <- suppressWarnings(as.numeric(fi$DREXPORT6))

      forecast <- data.frame(DREXPORT2 = fi2,DREXPORT3 = fi3,DREXPORT4 = fi4,DREXPORT5 = fi5,DREXPORT6 = fi6)

    }

  }
  else if(survey == "CPI"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_cpi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$CPI1))
      fi2 <- suppressWarnings(as.numeric(fi$CPI2))
      fi3 <- suppressWarnings(as.numeric(fi$CPI3))
      fi4 <- suppressWarnings(as.numeric(fi$CPI4))
      fi5 <- suppressWarnings(as.numeric(fi$CPI5))
      fi6 <- suppressWarnings(as.numeric(fi$CPI6))
      fiA <- suppressWarnings(as.numeric(fi$CPIA))
      fiB <- suppressWarnings(as.numeric(fi$CPIB))

      forecasts <- data.frame(CPI1 = fi1,CPI2 = fi2,CPI3 = fi3,CPI4 = fi4,CPI5 = fi5,CPI6 = fi6,CPIA = fiA,
                             CPIB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_cpi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$CPI1))
      fi2 <- suppressWarnings(as.numeric(fi$CPI2))
      fi3 <- suppressWarnings(as.numeric(fi$CPI3))
      fi4 <- suppressWarnings(as.numeric(fi$CPI4))
      fi5 <- suppressWarnings(as.numeric(fi$CPI5))
      fi6 <- suppressWarnings(as.numeric(fi$CPI6))
      fiA <- suppressWarnings(as.numeric(fi$CPIA))
      fiB <- suppressWarnings(as.numeric(fi$CPIB))

      forecasts <- data.frame(CPI1 = fi1,CPI2 = fi2,CPI3 = fi3,CPI4 = fi4,CPI5 = fi5,CPI6 = fi6,CPIA = fiA,
                             CPIB = fiB)


    }
    else if(type == "growth"){
      warning("Growth not availabe for CPI Inflation Rate")
    }


  }
  else if(survey == "PCE"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_pce_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$PCE1))
      fi2 <- suppressWarnings(as.numeric(fi$PCE2))
      fi3 <- suppressWarnings(as.numeric(fi$PCE3))
      fi4 <- suppressWarnings(as.numeric(fi$PCE4))
      fi5 <- suppressWarnings(as.numeric(fi$PCE5))
      fi6 <- suppressWarnings(as.numeric(fi$PCE6))
      fiA <- suppressWarnings(as.numeric(fi$PCEA))
      fiB <- suppressWarnings(as.numeric(fi$PCEB))

      forecasts <- data.frame(PCE1 = fi1,PCE2 = fi2,PCE3 = fi3,PCE4 = fi4,PCE5 = fi5,PCE6 = fi6,PCEA = fiA,
                             PCEB = fiB)

    }
    else if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_pce_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$PCE1))
      fi2 <- suppressWarnings(as.numeric(fi$PCE2))
      fi3 <- suppressWarnings(as.numeric(fi$PCE3))
      fi4 <- suppressWarnings(as.numeric(fi$PCE4))
      fi5 <- suppressWarnings(as.numeric(fi$PCE5))
      fi6 <- suppressWarnings(as.numeric(fi$PCE6))
      fiA <- suppressWarnings(as.numeric(fi$PCEA))
      fiB <- suppressWarnings(as.numeric(fi$PCEB))

      forecasts <- data.frame(PCE1 = fi1,PCE2 = fi2,PCE3 = fi3,PCE4 = fi4,PCE5 = fi5,PCE6 = fi6,PCEA = fiA,
                             PCEB = fiB)

    }
    else if(type == "growth"){

      warning("Growth not availabe for PCE Inflation Rate")

    }
  }
  else if(survey == "CORECPI"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_corecpi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$CORECPI1))
      fi2 <- suppressWarnings(as.numeric(fi$CORECPI2))
      fi3 <- suppressWarnings(as.numeric(fi$CORECPI3))
      fi4 <- suppressWarnings(as.numeric(fi$CORECPI4))
      fi5 <- suppressWarnings(as.numeric(fi$CORECPI5))
      fi6 <- suppressWarnings(as.numeric(fi$CORECPI6))
      fiA <- suppressWarnings(as.numeric(fi$CORECPIA))
      fiB <- suppressWarnings(as.numeric(fi$CORECPIB))

      forecasts <- data.frame(CORECPI1 = fi1,CORECPI2 = fi2,CORECPI3 = fi3,CORECPI4 = fi4,CORECPI5 = fi5,CORECPI6 = fi6,CORECPIA = fiA,
                              CORECPIB = fiB)

    }
    if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_corecpi_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$CORECPI1))
      fi2 <- suppressWarnings(as.numeric(fi$CORECPI2))
      fi3 <- suppressWarnings(as.numeric(fi$CORECPI3))
      fi4 <- suppressWarnings(as.numeric(fi$CORECPI4))
      fi5 <- suppressWarnings(as.numeric(fi$CORECPI5))
      fi6 <- suppressWarnings(as.numeric(fi$CORECPI6))
      fiA <- suppressWarnings(as.numeric(fi$CORECPIA))
      fiB <- suppressWarnings(as.numeric(fi$CORECPIB))

      forecasts <- data.frame(CORECPI1 = fi1,CORECPI2 = fi2,CORECPI3 = fi3,CORECPI4 = fi4,CORECPI5 = fi5,CORECPI6 = fi6,CORECPIA = fiA,
                              CORECPIB = fiB)


    }
    else if(type == "growth"){

      warning("Growth not available for Core CPI Inflation Rate")

    }
  }
  else if(survey == "COREPCE"){
    if(type == "mean"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_corepce_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$COREPCE1))
      fi2 <- suppressWarnings(as.numeric(fi$COREPCE2))
      fi3 <- suppressWarnings(as.numeric(fi$COREPCE3))
      fi4 <- suppressWarnings(as.numeric(fi$COREPCE4))
      fi5 <- suppressWarnings(as.numeric(fi$COREPCE5))
      fi6 <- suppressWarnings(as.numeric(fi$COREPCE6))
      fiA <- suppressWarnings(as.numeric(fi$COREPCEA))
      fiB <- suppressWarnings(as.numeric(fi$COREPCEB))

      forecasts <- data.frame(COREPCE1 = fi1,COREPCE2 = fi2,COREPCE3 = fi3,COREPCE4 = fi4,COREPCE5 = fi5,COREPCE6 = fi6,COREPCEA = fiA,
                              COREPCEB = fiB)

    }
    if(type == "median"){

      dlURL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_corepce_level.xlsx"
      tf = tempfile(fileext=".xlsx")
      download.file(url = dlURL,destfile = tf, mode = "wb")
      fi <- read_excel(tf)

      fi1 <- suppressWarnings(as.numeric(fi$COREPCE1))
      fi2 <- suppressWarnings(as.numeric(fi$COREPCE2))
      fi3 <- suppressWarnings(as.numeric(fi$COREPCE3))
      fi4 <- suppressWarnings(as.numeric(fi$COREPCE4))
      fi5 <- suppressWarnings(as.numeric(fi$COREPCE5))
      fi6 <- suppressWarnings(as.numeric(fi$COREPCE6))
      fiA <- suppressWarnings(as.numeric(fi$COREPCEA))
      fiB <- suppressWarnings(as.numeric(fi$COREPCEB))

      forecasts <- data.frame(COREPCE1 = fi1,COREPCE2 = fi2,COREPCE3 = fi3,COREPCE4 = fi4,COREPCE5 = fi5,COREPCE6 = fi6,COREPCEA = fiA,
                              COREPCEB = fiB)


    }
    else if(type == "growth"){

      warning("Growth not available for Core PCE Inflation Rate")

    }


  }

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
