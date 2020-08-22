Create_Weg <- function(Weg_DF){
  setkey(Weg_DF, Baan, HmStart, Strook, Datum_tijd)
  # Define and determine the amount of unique Banen from the given Weg.
  UniqueBanen <- unique(Weg_DF$Baan)
  
  Current_Weg <- Weg.template
  Current_Weg@wegID <- unique(Weg_DF$Weg)
  
  Banen_List.length <- length(UniqueBanen)
  # Define and preallocate the Banen_List.
  Banen_List <- vector('list', length = Banen_List.length)
  
  for(B in seq_along(UniqueBanen)){
    BaaN <- UniqueBanen[B]
    # Take subset of the given data based on Baan.
    Weg_DF_Baan <- Weg_DF[.(BaaN)]
    
    # Create a Baan object and fill.
    Current_Baan <- temporal.Baan.template
    Current_Baan@baanID <- BaaN
    Current_Baan@hmVakVector <- unique(Weg_DF_Baan$HmStart)
    
    # Define and preallocate the hmVakken_List.
    hmVakken_List.length <- length(Current_Baan@hmVakVector)
    hmVakken_List <- vector('list', length = hmVakken_List.length)
    
    for(hmV in seq_along(Current_Baan@hmVakVector)){
      hmVak <- Current_Baan@hmVakVector[hmV]
      
      # Take subset of the given data based on Baan and hmVak.
      Weg_DF_Baan_hmVak <- Weg_DF_Baan[.(BaaN, hmVak)]
      
      # Create a hmVak object and fill.
      Current_hmVak <- temporal.hmVak.template
      Current_hmVak@hmStartPos <- hmVak
      Current_hmVak@strookVector <- unique(Weg_DF_Baan_hmVak$Strook)
      
      # Define and preallocate the Stroken_List
      Stroken_List.length <- length(Current_hmVak@strookVector)
      Stroken_List <- vector('list', length = Stroken_List.length)
      
      for(S in seq_along(Current_hmVak@strookVector)){
        StrooK <- Current_hmVak@strookVector[S]
        
        # Take subset of the given data based on Baan, hmVak and Strook.
        Weg_DF_Baan_hmVak_Strook <- Weg_DF_Baan_hmVak[.(BaaN, hmVak, StrooK)]
        
        # Create a Strook object and fill.
        Current_Strook <- temporal.Strook.template
        Current_Strook@strookID <- StrooK
        Current_Strook@dateVector <- Weg_DF_Baan_hmVak_Strook$Datum_tijd
        
        LCMS_Traces_List.length <- nrow(Weg_DF_Baan_hmVak_Strook)
        LCMS_Traces_List <- vector('list', length = LCMS_Traces_List.length)
      
        for(D in seq_along(Current_Strook@dateVector)){
          Date <- Current_Strook@dateVector[D]
          # Create an LCMS_Trace object and fill.
          Current_LCMS_Trace <- temporal.LCMS_Trace.template
          Current_LCMS_Trace@Datum_tijd <- Date
          Current_LCMS_Trace@Vehicle <- Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date]$Vehicle
          Current_LCMS_Trace@Errorcode <- Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date]$Errorcode
          Current_LCMS_Trace@lengte_meting <- Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date]$lengte_meting
          Current_LCMS_Trace@overallData <- as.numeric(Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date, c(ColNames('Overall', 'No'))])
          Current_LCMS_Trace@leftData <- as.numeric(Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date, c(ColNames('Left', 'No'))])
          Current_LCMS_Trace@rightData <- as.numeric(Weg_DF_Baan_hmVak_Strook[Datum_tijd == Date, c(ColNames('Right', 'No'))])
          
          # Fill the LCMS_Traces_List.
          LCMS_Traces_List[D] <- Current_LCMS_Trace
          
        }
        
        Current_Strook@temporal.LCMS_Traces <- LCMS_Traces_List
        Stroken_List[S] <- Current_Strook
      }
      
      Current_hmVak@temporal.Stroken <- Stroken_List
      hmVakken_List[hmV] <- Current_hmVak
    }
    
   Current_Baan@temporal.hmVakken <- hmVakken_List
   Banen_List[B] <- Current_Baan
  }
  
  Current_Weg@banen <- Banen_List
  return(Current_Weg)
}

# Save the Weg objects.

# {
# for(W in unique(All_DF$Weg)){
#   Current_DF <- All_DF[Weg == W]
#   assign(paste0(W, '_DF'), Current_DF)
#   Current_Weg <- Create_Weg(Current_DF)
#   saveit(Weg = Current_Weg, string = W, file = paste0('LCMS_DB_', W, '.Rdata'))
# }