	## year code - fall to fall
convert_year <- function(yr, mo){
		yrcode <- NA
		if(yr=="2010") yrcode <- "Yr1"
		if(yr=="2011" & mo %in% c(1:2)) yrcode <- "Yr1"
		if(yr=="2011" & mo %in% c(3:12)) yrcode <- "Yr2"
		if(yr=="2012" & mo %in% c(1:2)) yrcode <- "Yr2"
		if(yr=="2012" & mo %in% c(3:12)) yrcode <- "Yr3"
		if(yr=="2013" & mo %in% c(1:2)) yrcode <- "Yr3"
		if(yr=="2013" & mo %in% c(3:12)) yrcode <- "Yr4"
		if(yr=="2014" & mo %in% c(1:2)) yrcode <- "Yr4"
		if(yr=="2014" & mo %in% c(3:12)) yrcode <- "Yr5"
		if(yr=="2015" & mo %in% c(1:2)) yrcode <- "Yr5"
		if(yr=="2015" & mo %in% c(3:12)) yrcode <- "Yr6"
		if(yr=="2016" & mo %in% c(1:2)) yrcode <- "Yr6"
		return(yrcode)
}