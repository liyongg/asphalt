Create_Status_List <- function(){
  # Initialise the Status_List with given data.
  wegIDs <- str_match(list.files(pattern = glob2rx('LCMS_DB*')), 'LCMS_DB_(.*?).Rdata')[,2]
  Status_List <- vector('list', length(wegIDs))
  Status_DF_Template <- data.frame(Baan = character(),
                                      HmStart = numeric(),
                                      HmStop = numeric(),
                                      Strook = character(),
                                      Date = as.Date(character()),
                                      Laag = character(),
                                      stringsAsFactors = FALSE)
  
  for(i in seq_along(Status_List)){
    Status_List[[i]] <- Status_DF_Template
  }
  names(Status_List) <- wegIDs
  
  #### A7 ####
  # Rechts
  Status_List$R007[1,] <- c('1HRR', 37.5, 56.1, 'ALL', '2008-08-01', 'GO')
  Status_List$R007[2,] <- c('1HRL', 37.5, 54.6, 'ALL', '2008-08-01', 'GO')
  
  #### A6 ####
  # Rechts
  Status_List$R006[1,] <- c('1HRR', 280.2, 288, 'ALL', '2005-11-15', 'GO')
  Status_List$R006[2,] <- c('1HRR', 288, 295.8, 'ALL', '2005-08-31', 'GO')
  
  #### A44 ####
  # Rechts
  Status_List$R044[1,] <- c('1HRR', 2.1, 7.7, 'ALL', '2002-09-09', 'GO')
  
  #### A50 ####
  # 1e Traject
  # Rechts
  Status_List$R050[1,] <- c('1HRR', 139.9, 140.3, 'ALL', '2010-03-18', 'GO')
  Status_List$R050[2,] <- c('1HRR', 140.3, 141.6, 'ALL', '2012-11-08', 'GO')
  Status_List$R050[3,] <- c('1HRR', 141.6, 142.2, 'ALL', '2013-04-16', 'GO')
  Status_List$R050[4,] <- c('1HRR', 142.2, 142.9, 'ALL', '2012-03-30', 'GO')
  Status_List$R050[5,] <- c('1HRR', 142.9, 146.7, 'ALL', '2013-03-19', 'GO')
  Status_List$R050[6,] <- c('1HRR', 146.7, 148.4, 'ALL', '2012-09-17', 'GO')
  
  # Links
  Status_List$R050[7,] <- c('1HRL', 139.9, 140.3, 'ALL', '2006-06-04', 'GO')
  Status_List$R050[8,] <- c('1HRL', 140.3, 146.7, 'ALL', '2013-05-07', 'GO')
  Status_List$R050[9,] <- c('1HRL', 146.7, 148.5, 'ALL', '2012-09-17', 'GO')
  
  # 2e Traject
  Status_List$R050[10,] <- c('1HRL', 205.4, 205.5, '1RL', '2007-11-01', 'GO')
  Status_List$R050[11,] <- c('1HRL', 205.4, 205.5, '2RL', '2002-07-12', 'GO')
  Status_List$R050[12,] <- c('1HRL', 204.8, 205.4, 'ALL', '2002-01-01', 'GO')
  Status_List$R050[13,] <- c('1HRL', 203.2, 204.8, 'ALL', '2002-12-31', 'GO')
  Status_List$R050[14,] <- c('1HRL', 203.1, 203.2, 'ALL', '2003-01-01', 'GO')
  Status_List$R050[15,] <- c('1HRL', 202.9, 203.1, '1RL', '2003-01-01', 'GO')
  Status_List$R050[16,] <- c('1HRL', 202.9, 203.1, '2RL', '2002-07-12', 'GO')
  
  return(Status_List)
}

Status_List <- Create_Status_List()
