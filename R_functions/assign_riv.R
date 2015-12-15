assign_riv <- function(name, underscore=FALSE, single_code=TRUE){
  code <- NA
  if(underscore==FALSE & single_code==TRUE){
    if(all(name %in% "Suwannee River")) code <- "S"
    if(all(name %in% c("Ochlockonee River", "Carabelle River", "Sopchoppy River"))) code <- "K"
    if(all(name %in% c("Apalachicola River", "St. Marks River", "East River"))) code <- "A"
    if(all(name %in% "Choctawhatchee River")) code <- "C"
    if(all(name %in% "Escambia River")) code <- "E"
    if(all(name %in% "Blackwater River")) code <- "B"
    if(all(name %in% "Yellow River")) code <- "Y"
    if(all(name %in% "Pascagoula River")) code <- "P"
    if(all(name %in% "Pearl River")) code <- "L"
    if(all(name %in% "Perdido River")) code <- "X"
  }

  if(underscore==TRUE & single_code==FALSE){
    if(all(name %in% "Suwannee_River")) code <- "SR"
    if(all(name %in% c("Ochlockonee_River", "Carabelle_River", "Sopchoppy_River"))) code <- "OR"
    if(all(name %in% c("Apalachicola_River", "Brothers_River"))) code <- "AR"
    if(all(name %in% c("Choctawhatchee_River", "Choctawhatchee_Bay"))) code <- "CR"
    if(all(name %in% c("Escambia_River", "Escambia_Bay"))) code <- "ER"
    if(all(name %in% "Blackwater_River")) code <- "BR"
    if(all(name %in% "Yellow_River")) code <- "YR"
    if(all(name %in% "Pascagoula_River")) code <- "PR"
    if(all(name %in% c("Pearl_River", "Bogue_Chitto_River"))) code <- "PE"
    if(all(name %in% "Perdido_River")) code <- "X"
  }
  if(is.na(code)) stop("cannot assign code for multiple river drainages or function not set up for underscore/code type combination")


    return(code)
}