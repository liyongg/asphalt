Create_Anomalies_List <- function(){
  # Initialise the Anomalies_List with given data.
  wegIDs <- str_match(list.files(pattern = glob2rx('LCMS_DB*')), 'LCMS_DB_(.*?).Rdata')[,2]
  Anomalies_List <- vector('list', length(wegIDs))
  Anomalies_DF_Template <- data.frame(Baan = character(),
                                      HmStart = numeric(),
                                      HmStop = numeric(),
                                      Strook = character(),
                                      Date = as.Date(character()),
                                      Laag = character(),
                                      stringsAsFactors = FALSE)
  
  for(i in seq_along(Anomalies_List)){
    Anomalies_List[[i]] <- Anomalies_DF_Template
  }
  names(Anomalies_List) <- wegIDs
  
  #### A7 ####
  # Rechts
  Anomalies_List$R007[1,] <- c('1HRR', 43.3, 44.1, '1RR', '2018-10-20', 'PA')
  Anomalies_List$R007[2,] <- c('1HRR', 43.3, 44.1, '2RR', '2018-10-20', 'PA')
  
  Anomalies_List$R007[3,] <- c('1HRR', 54.3, 54.7, '1WR', '2019-07-19', 'PA')
  
  # Links
  Anomalies_List$R007[4,] <- c('1HRL', 53.5, 53.3, '1RL', '2019-10-16', 'PA')
  Anomalies_List$R007[5,] <- c('1HRL', 53.5, 53.3, '2RL', '2019-10-16', 'PA')
  
  Anomalies_List$R007[6,] <- c('1HRL', 45.8, 45.7, '1RL', '2017-05-15', 'PA')
  Anomalies_List$R007[7,] <- c('1HRL', 45.8, 45.7, '2RL', '2017-05-15', 'PA')
  
  Anomalies_List$R007[8,] <- c('1HRL', 43.4, 42.4, '1RL', '2018-06-06', 'PEA')
  Anomalies_List$R007[9,] <- c('1HRL', 43.4, 42.4, '2RL', '2018-06-06', 'PEA')
  Anomalies_List$R007[10,] <- c('1HRL', 43.4, 42.4, '1WL', '2018-06-06', 'PEA')
  
  Anomalies_List$R007[11,] <- c('1HRL', 42.4, 37.5, '1RL', '2019-07-12', 'PEA')
  Anomalies_List$R007[12,] <- c('1HRL', 42.4, 37.5, '2RL', '2019-07-17', 'PEA')
  Anomalies_List$R007[13,] <- c('1HRL', 42.4, 37.5, '1WL', '2019-07-17', 'PEA')
  
  #### A44 ####
  # Rechts
  Anomalies_List$R044[1,] <- c('1HRR', 6.0, 6.1, 'ALL', '2009-06-01', 'PA')
  Anomalies_List$R044[2,] <- c('1HRR', 2.1, 2.9, '2RR', '2015-10-01', 'PEA')
  Anomalies_List$R044[3,] <- c('1HRR', 2.9, 5.5, '2RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[4,] <- c('1HRR', 5.5, 6.7, '2RR', '2015-10-01', 'PEA')
  Anomalies_List$R044[5,] <- c('1HRR', 6.7, 7.6, '2RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[6,] <- c('1HRR', 2.9, 3.0, '1RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[7,] <- c('1HRR', 3.3, 3.4, '1RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[8,] <- c('1HRR', 4.4, 4.7, '1RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[9,] <- c('1HRR', 6.0, 6.2, '2RR', '2018-04-09', 'PA')
  Anomalies_List$R044[10,] <- c('1HRR', 2.1, 2.3, 'ALL', '2018-09-25', 'PEA')
  Anomalies_List$R044[11,] <- c('1HRR', 2.3, 6.9, '1RR', '2018-09-25', 'PEA')
  Anomalies_List$R044[12,] <- c('1HRR', 2.3, 5.9, '2RR', '2018-09-25', 'PEA')
  Anomalies_List$R044[13,] <- c('1HRR', 5.9, 6.2, '2RR', '2018-01-10', 'PA')
  Anomalies_List$R044[14,] <- c('1HRR', 6.2, 6.9, '2RR', '2018-09-25', 'PEA')
  Anomalies_List$R044[15,] <- c('1HRR', 6.9, 7.6, '2RR', '2016-07-13', 'PEA')
  Anomalies_List$R044[16,] <- c('ALL', 7.5, 7.7, 'ALL', '1010-10-10', 'Bridge')
  Anomalies_List$R044[17,] <- c('ALL', 5.9, 6.1, 'ALL', '1010-10-10', 'Bridge')
  Anomalies_List$R044[18,] <- c('ALL', 2.3, 2.4, 'ALL', '1010-10-10', 'Viaduct')
  
  #### A50 ####
  Anomalies_List$R050[1,] <- c('ALL', 139.9, 140.3, 'ALL', '1010-10-10', 'Maas Bridge')
  Anomalies_List$R050[2,] <- c('ALL', 141.5, 141.7, 'ALL', '1010-10-10', 'Viaduct')
  Anomalies_List$R050[3,] <- c('ALL', 144.8, 144.9, 'ALL', '1010-10-10', 'Viaduct')
  Anomalies_List$R050[4,] <- c('ALL', 145.7, 145.8, 'ALL', '1010-10-10', 'Bridge')
  Anomalies_List$R050[5,] <- c('ALL', 147.1, 147.2, 'ALL', '1010-10-10', 'Bridge')
  
  # 2e traject
  Anomalies_List$R050[6,] <- c('ALL', 205.2, 205.3, 'ALL', '1010-10-10', 'Viaduct')
  Anomalies_List$R050[7,] <- c('ALL', 205.4, 205.5, '2RL', '2017-11-01', 'PA')
  Anomalies_List$R050[8,] <- c('ALL', 204.8, 205.4, 'ALL', '2014-08-14', 'PA')
  Anomalies_List$R050[9,] <- c('1HRL', 203.1, 203.6, '1RL', '2018-02-02', 'PA')
  Anomalies_List$R050[10,] <- c('1HRL', 203.6, 204.8, '1RL', '2018-11-29', 'PA')
  Anomalies_List$R050[11,] <- c('1HRL', 203.1, 204.8, '2RL', '2016-11-30', 'PA')
  
  # #### A6 ####
  # Rechts
  Anomalies_List$R006[1,] <- c('1HRR', 280.3, 280.4, '2RR', '2012-12-31', 'PA')
  Anomalies_List$R006[2,] <- c('1HRR', 280.4, 282.5, '2RR', '2013-06-13', 'PA')
  Anomalies_List$R006[3,] <- c('1HRR', 283.9, 284.1, '2RR', '2012-12-31', 'PA')
  Anomalies_List$R006[4,] <- c('1HRR', 280.3, 280.5, '2RR', '2016-10-21', 'PA')
  Anomalies_List$R006[5,] <- c('1HRR', 280.6, 280.8, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[6,] <- c('1HRR', 282.0, 282.1, '2RR', '2018-08-20', 'PA')
  Anomalies_List$R006[7,] <- c('1HRR', 282.3, 282.4, '2RR', '2018-06-26', 'PA')
  Anomalies_List$R006[8,] <- c('1HRR', 282.5, 282.8, '2RR', '2018-10-20', 'PA')
  Anomalies_List$R006[9,] <- c('1HRR', 282.8, 283.0, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[10,] <- c('1HRR', 283.2, 283.4, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[11,] <- c('1HRR', 283.5, 283.7, '2RR', '2018-08-20', 'PA')
  Anomalies_List$R006[12,] <- c('1HRR', 283.7, 284.0, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[13,] <- c('1HRR', 284.1, 284.3, '2RR', '2016-07-28', 'PA')
  Anomalies_List$R006[14,] <- c('1HRR', 284.4, 284.6, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[15,] <- c('1HRR', 285.8, 286.0, '2RR', '2016-10-01', 'PA')
  Anomalies_List$R006[16,] <- c('1HRR', 287.2, 287.3, '2RR', '2018-08-20', 'PA')
  Anomalies_List$R006[17,] <- c('1HRR', 287.8, 288.6, '2RR', '2018-09-05', 'PADI')
  Anomalies_List$R006[18,] <- c('1HRR', 288.6, 288.9, '2RR', '2019-05-22', 'PA')
  Anomalies_List$R006[19,] <- c('1HRR', 289.9, 290.4, '2RR', '2018-09-05', 'PA')
  Anomalies_List$R006[20,] <- c('1HRR', 292.3, 292.8, '2RR', '2018-09-04', 'PA')
  Anomalies_List$R006[21,] <- c('1HRR', 295.2, 295.5, '2RR', '2018-09-04', 'PA')
  
  return(Anomalies_List)
}

Anomalies_List <- Create_Anomalies_List()

