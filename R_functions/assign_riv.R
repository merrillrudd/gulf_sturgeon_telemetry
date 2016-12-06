assign_riv <- function(name){
  code <- NA
  if(grepl("Suwannee", name)) code <- "S"
  if(grepl("Ochlockonee", name)) code <- "K"
  if(grepl("Carabelle", name)) code <- "K"
  if(grepl("Sopchoppy", name)) code <- "K"
  if(grepl("Apalachicola", name)) code <- "A"
  if(grepl("Marks", name)) code <- "A"
  if(grepl("East", name)) code <- "A"
  if(grepl("Choctawhatchee", name)) code <- "C"
  if(grepl("Escambia", name)) code <- "E"
  if(grepl("Blackwater", name)) code <- "B"
  if(grepl("Yellow", name)) code <- "Y"
  if(grepl("Pascagoula", name)) code <- "P"
  if(grepl("Pearl", name)) code <- "L"
  if(grepl("Perdido", name)) code <- "X"
 
  if(is.na(code)) stop("cannot assign code")


    return(code)
}