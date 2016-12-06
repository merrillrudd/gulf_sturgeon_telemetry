### Updated Simulation Code by MBR 12/2013
### Comments by MBR 12/2013

######################################################################################################
# This code simulates data based on the design of the NOAA acoustic telemetry program
# River-specific spatial collapse method (Rudd et al. in review)
# Loops over several survival and detection models, 3 sample sizes of deployed transmitters annually
# Room for improvement (reduce for loops, reduce generalization, improve realism, etc.)
######################################################################################################

#globally define short term  (1) or long term (2)
#could re-write to loop over time frame length
#specifies directory here, but also the number of months for simulation in the next section ("Setup Simulation")
term <- 1
if(term==1)
{setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Telemetry Simulations\\Update_Dec2013\\RiverModel")}

term <- 2
if(term==2)
{setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Telemetry Simulations\\Update_Dec2013\\RiverModel (long)")}


#############################################################
# Setup Simulation 
#############################################################
SetupSim <- function(low,med,high) #low, medium, or high number of tags - can be adjusted when running the function
{

	#vectors to have total number of tags and initial locations for each tag ready
	tnum <- low+med+high #total number of tags deployed annually
	tloc <- c(rep("A",low),rep("B",med),rep("C",high)) #names of river states - A is representative of a western river, B of the Suwannee River (standard 20 tags per year), C representative of Choctawhatchee River

	#number of months and names for capture history matrix depending on time frame (globally named at beginning of code)
	if(term==1) #short term
	{
		months <- 25
		matnames <- c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    	"Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
	}
	if(term==2) #long term
	{
		months <- 60	
		matnames <- c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    	"Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6","Yr3/7","Yr3/8","Yr3/9","Yr3/10","Yr3/11","Yr3/12","Yr3/1","Yr3/2","Yr4/3","Yr4/4","Yr4/5","Yr4/6",
		"Yr4/7","Yr4/8","Yr4/9","Yr4/10","Yr4/11","Yr4/12","Yr4/1","Yr4/2","Yr5/3","Yr5/4","Yr5/5","Yr5/6","Yr5/7","Yr5/8","Yr5/9","Yr5/10","Yr5/11","Yr5/12","Yr5/1",
		"Yr5/2","Yr6/3","Yr6/4","Yr6/5")
	}

	#create matrix with correct dimensions for capture history - months are columns and rows are number of tags deployed annually
	tagmat1 <- matrix(0,nrow=tnum,ncol=months)	#year 1 of tagging
		colnames(tagmat1) <- matnames
	tagmat2 <- matrix(0,nrow=tnum,ncol=months)	#year 2 of tagging
		colnames(tagmat2) <- matnames

	#all tags are detected in the marine location during september of the first year
	#tagged during the summer/fall, we assume all fish tagged that year enter the marine state for the fall/winter
	tagmat1[,which(colnames(tagmat1)=="Yr1/9")] <- "M"	
	tagmat2[,which(colnames(tagmat2)=="Yr2/9")] <- "M"

		#total matrix of tagged fish from years 1 and 2
		#number of rows (number of tags) the same for short and long term, number of columns (months) increases
		#could add another tagmat3 to more realistically indicate the number of tags (rows) in the NMFS telemetry program
		tagmat <- rbind(tagmat1,tagmat2)
	
	#output the general setup for the simulation: capture history matrix (tagmat) and initial tagging locations (tloc)
	Outs <- NULL
	Outs$tagmat <- tagmat
	Outs$tloc <- tloc
	return(Outs)
}
	
#############################################################
# SimFun: Pop dy simulation
#############################################################

SimFun <- function(focal.riv,Rsurv=0.9,Msurv=0.8,pon=0.3,poff=0.05,emig=0.2) #annual rates set, but can be changed
{ #start simulation function
	
	#read setup info
	SimInfo <- SetupSim(low=10,med=20,high=50)
	tagmat <- SimInfo$tagmat
	tloc <- SimInfo$tloc

		#repeat the tagging locations for the number of years of tagging to get all of the initial locations
		initloc <- rep(tloc,2)

	### monthly survival rate --- Rsurv and Msurv apply to 6 months each (half year in each environment)
	phiR <- Rsurv^(1/6)
	phiM <- Msurv^(1/6)	

	#monthly detection probability - pon and poff are rates for each of the 4 seasons (summer, fall, winter, spring - 3 months each)
	p_on <- pon/3
	p_off <- poff/3	
	
	#convert initial tagging info to 1s and 0s ---- is the tag deployed yet, yes (1) or no (0)?
	tagmat.init <- tagmat
	tagmat.init[which(tagmat.init!=0)] <- 1
	class(tagmat.init) <- "numeric"
	
	################# hypothetical locations ####################
	
	#defines location matrix to be filled - same dimensions as were setup in the function SetupSim
	locmat <- tagmat
	
	#Bernoulli trial matrices - S1 = does the fish return to its natal river? S2 = if it does move into a different river, where?
	S1=matrix(runif(nrow(tagmat)*ncol(tagmat)),nrow=nrow(tagmat),ncol=ncol(tagmat))
	S2=matrix(runif(nrow(tagmat)*ncol(tagmat)),nrow=nrow(tagmat),ncol=ncol(tagmat))

	### series of functions used to simulate movement
		#mo is sequence of applicable months
		#locmat is location matrix prior to the movement the function is specifying - work off of what has already been simulated
	#stay function: filling in spring through fall with in-migration river -- no chance for movement into another state
	stay <- function(mo,locmat)
	{
		for(i in 1:nrow(tagmat))
		{
			for(j in mo)
			{
				prevloc <- locmat[i,(j-1)]	#previous location
				locmat[i,j] <- ifelse(sum(tagmat.init[i,1:(j-1)])>0,prevloc,tagmat[i,j]) #if the tag in the given row has been deployed, then the new location is the previous location, else 0
			}
		}
		return(locmat)
	}
	
	#moveout function: fish moves out of the river in the first month of fall, stays in the marine environment for the rest of fall
	moveout <- function(mo,locmat)
	{
		for(i in 1:nrow(tagmat))
		{
			for(j in mo)
			{
				prevloc <- locmat[i,(j-1)]	#previous location
				if(prevloc!="0")	#if the tag has been deployed
				{
					locmat[i,j] <- "M" #location is definitely M as the fish moves out of the river and into marine environment
				}
			}
		}
		return(locmat)
	}
	
	#movein function: wherever fish happens to be during in-migration months becomes summer location
	movein <- function(mo,locmat)
	{
		for(i in 1:nrow(tagmat))
		{
			for(j in mo)
			{
				tagloc <- initloc[i] #initial tagging location (natal river)
				prevloc <- locmat[i,(j-1)] #previous location
				if(tagloc=="A"){emigs <- c("B","C")} #possible rivers the fish would migrate to if they don't return to natal river
				if(tagloc=="B"){emigs <- c("A","C")}
				if(tagloc=="C"){emigs <- c("A","B")}
				
				if(prevloc!="0") #if the tag has been deployed
				{
					#0.8 probability of returning to natal river (fidelity)
					#if the fish does not return to the natal river, 0.5 probability of moving into either of the other rivers
					locmat[i,j] <- ifelse(S1[i,j]>emig,tagloc,
						ifelse(S2[i,j]<=0.5,emigs[1],
						ifelse(S2[i,j]>0.5,emigs[2],taglog)))
				}
			}
		}
		return(locmat)
	}
	
	#define months for each movement period
	#locmat is the returned object from the previous season's simulated locations
	#needs to go in order because movements may depend on what has previously happened (tag deployment, emigration)
	#number of months will vary by length of simulated monitoring time
	if(term==1)
	{
		sum1 <- stay(mo=2:3,locmat=locmat)
		fall1 <- moveout(mo=4:6,locmat=sum1)
		win1 <- stay(mo=7:9,locmat=fall1)
		spr1 <- movein(mo=10:12,locmat=win1)
		sum2 <- stay(mo=13:15,locmat=spr1)
		fall2 <- moveout(mo=16:18,locmat=sum2)
		win2 <- stay(mo=19:21,locmat=fall2)
		spr2 <- movein(mo=22:24,locmat=win2)
		sum3 <- stay(mo=25,locmat=spr2)

		locmat <- sum3
	}
	if(term==2)
	{
		sum1 <- stay(mo=2:3,locmat=locmat)
		fall1 <- moveout(mo=4:6,locmat=sum1)
		win1 <- stay(mo=7:9,locmat=fall1)
		spr1 <- movein(mo=10:12,locmat=win1)
		sum2 <- stay(mo=13:15,locmat=spr1)
		fall2 <- moveout(mo=16:18,locmat=sum2)
		win2 <- stay(mo=19:21,locmat=fall2)
		spr2 <- movein(mo=22:24,locmat=win2)
		sum3 <- stay(mo=25:27,locmat=spr2)
		fall3 <- moveout(mo=28:30,locmat=sum3)
		win3 <- stay(mo=31:33,locmat=fall3)
		spr3 <- movein(mo=34:36,locmat=win3)
		sum4 <- stay(mo=37:39,locmat=spr3)
		fall4 <- moveout(mo=40:42,locmat=sum4)
		win4 <- stay(mo=43:45,locmat=fall4)
		spr4 <- movein(mo=46:48,locmat=win4)
		sum5 <- stay(mo=49:51,locmat <- spr4)
		fall5 <- moveout(mo=52:54,locmat=sum5)
		win5 <- stay(mo=55:57,locmat=fall5)
		spr5 <- movein(mo=58:60,locmat=win5)

		locmat <- spr5
	}

	##################### survival ############################
	
	## does fish survive?
		#different survival rate -river and marine states
		#assume fish survive their tagged year
		
	#create survival matrix initially equal to location matrix (dimensions same as tagmat)
	survmat <- locmat
	
	#Survival Bernoulli trial matrix
	S4 <- matrix(runif(nrow(survmat)*ncol(survmat)),nrow=nrow(survmat),ncol=ncol(survmat))
	
	for(i in 1:nrow(survmat))
	{
		for(j in 2:ncol(survmat))
		{
			
			#avoid survival algorithm for fish that were not yet deployed
			if(all(locmat[i,1:j]=="0")) {next}
			
			prevloc <- locmat[i,(j-1)] #previous location
			if(prevloc=="M") #if previous location was M, we know the tag was already deployed & apply marine survival rate
			{
				#if random number is greater than the marine survival rate and it is not the month that the tag was deployed, the fish dies, otherwise it is in the hypothetical location defined in locmat
				survmat[i,j] <- ifelse(S4[i,(j-1)]>phiM&tagmat[i,j]==0,0,locmat[i,j]) 
			}
			
			if(prevloc!="M"&prevloc!="0") #if previous location was not M or 0 - aka it was in a river state
			{
				#if random number greater than riverine survival and it is not the month the tag was deployed, the fish dies, otherwise was located in the hypothetical location defined in locmat 
				survmat[i,j] <- ifelse(S4[i,(j-1)]>phiR&tagmat[i,j]==0,0,locmat[i,j])
			}
			
			#if fish dies, it remains dead for the rest of the time series
			prevdead <- survmat[i,(j-1)] #previous location to check if it is dead
			if(prevdead==0&locmat[i,j]!=0&tagmat[i,j]==0) 
			{
				#dead if the prior state is 0 
				#AND the location matrix had a river or marine state defined 
				#AND it is not the month the fish was tagged
				survmat[i,j]=0
			}
		}
	}
	
	##################### detection ############################

	## is the fish detected?
		## assume constant detection probability across all rivers
		## season-specific detection probability (on or off seasons)

	##create a detection matrix initially equal to survival matrix (same dimensions as locmat and tagmat)
	detmat <- survmat

	## Detection Bernoulli trial matrix
	P1=matrix(runif(nrow(tagmat)*ncol(tagmat)),nrow=nrow(tagmat),ncol=ncol(tagmat))
	
	#define on and off season months from total time series
	if(term==1)
	{
		off=c(2:3,7:9,13:15,19:21,25) #summer and winter
		on=c(4:6,10:12,16:18,22:24) #fall and spring
	
	}
	if(term==2)
	{
		off=c(2:3,7:9,13:15,19:21,25:27,31:33,37:39,43:45,49:51,55:57) #summer and winter
		on=c(4:6,10:12,16:18,22:24,28:30,34:36,40:42,46:48,52:54,58:60) #fall and spring
	
	}
	
	for(i in 1:nrow(tagmat))
	{
		for(j in off)
		{
			#if the random number is less than or equal to the probability of detection in the off season, the fish is detected (same location as defined in survmat - may have died)
			#the second ifelse term ensures that the detection function does not give the capture history a 0 in the month the fish was tagged
			detmat[i,j]=ifelse(P1[i,j]<=p_off,survmat[i,j],ifelse(tagmat[i,j]==0,0,survmat[i,j]))
		}
			
		for(j in on)
		{
			#same as off season above
			detmat[i,j]=ifelse(P1[i,j]<=p_on,survmat[i,j],ifelse(tagmat[i,j]==0,0,survmat[i,j]))
		}
	}
	
	#### resulting detmat = generated data! same as field data

####---------------------------------- CONVERT MONTHLY TO 4-SEASON TIME SCALE --------------------------------------####
	#begin process of collapsing generated data the same way field data is collapsed

	##first monthly collapsed to 4 seasons per year
	#column names depend on short or long term
	if(term==1)
	{
		matnames4 <- c("Yr1/Su","Yr1/Fa","Yr1/Wi","Yr2/Sp","Yr2/Su","Yr2/Fa","Yr2/Wi","Yr3/Sp","Yr3/Su")
	}
	if(term==2)
	{
		matnames4 <- c("Yr1/Su","Yr1/Fa","Yr1/Wi","Yr2/Sp","Yr2/Su","Yr2/Fa","Yr2/Wi","Yr3/Sp",
		"Yr3/Su","Yr3/Fa","Yr3/Wi","Yr4/Sp","Yr4/Su","Yr4/Fa","Yr4/Wi","Yr5/Sp",
		"Yr5/Su","Yr5/Fa","Yr5/Wi","Yr6/Sp")
	}
	#create matrix with the proper dimensions- number of rows (tags) does not change but the number of columns will vary depending on short or long term (months)
	detmat.season <- matrix(0,nrow=nrow(detmat),ncol=length(matnames4))
		colnames(detmat.season) <- matnames4
		
	#vector stating the years that occur during the time series: Yr1,Yr2,Yr3, optional: Yr4,Yr5
	years=unique(substr(colnames(detmat.season),1,3))

	#function that collapses the 12 months each year into 4 seasons
	detmat.season.fun <- function(monum,mosym,detmat.season) #monum= month numbers within the year, mosym= symbols for the season, detmat.season= prior season's detection matrix (see locmat in the movement functions above)
	{
		for(i in 1:nrow(detmat))
		{
			for(z in 1:length(years))
			{
				subyrdet <- cbind(detmat[,which(substr(colnames(detmat),1,3)==years[z])]) #subset year based on the substring of the column names (start with the year)
					mo <- substr(colnames(subyrdet),5,6) #vector of months based on the substring of months wtihin the chosen year
				submodet <- cbind(subyrdet[,which(mo %in% monum)]) #subset of months that match the months within the season that is being collapsed (monum)
				
				season <- which(substr(colnames(detmat.season),5,6)==mosym) #matching the season to the substring of the season name within the column names
				yr <- which(substr(colnames(detmat.season),1,3)==years[z]) #matching the year to the substring of the year within the column names
				
				#identify the proper columns within the new detection matrix based on the season and year
				#if all of the detections within the month subset are 0, then no state is defined for that season
				#if the fish is detected at a state at any point during the season, it is assigned the last state in which it was detected
					#(based on the assumption that the fish does not live in multiple rivers during the summer months, and enters the last river in which it was detected)
				detmat.season[i,season[which(season %in% yr)]] <- ifelse(all(submodet[i,]==0),"0",submodet[i,which(submodet[i,]!=0)[length(which(submodet[i,]!=0))]])
			}
		}
		return(detmat.season)
	}

	#conduct this collapse for each season, but do not need to be in order chronologically
	#can only do one season at a time and need to build on adjusted detmats (starting with initial ddetmat.season each time would result in 4 separate collapsed matrices for each season)
	summer=detmat.season.fun(monum=6:8,mosym="Su",detmat.season=detmat.season)
	fall=detmat.season.fun(monum=9:11,mosym="Fa",detmat.season=summer)
	winter=detmat.season.fun(monum=c(12,1,2),mosym="Wi",detmat.season=fall)
	spring=detmat.season.fun(monum=3:5,mosym="Sp",detmat.season=winter)	

	detmat.season=spring


	## 4 seasons per year collapsed to 2 seasons per year (spring/summer and fall/winter)
	#column names depend on length of monitoring time
	if(term==1)
	{
		matnames2 <- c("Yr1/SpSu","Yr1/FaWi","Yr2/SpSu","Yr2/FaWi","Yr3/SpSu")
	}
	if(term==2)
	{
		matnames2 <- c("Yr1/SpSu","Yr1/FaWi","Yr2/SpSu","Yr2/FaWi",
			"Yr3/SpSu","Yr3/FaWi","Yr4/SpSu","Yr4/FaWi","Yr5/SpSu","Yr5/FaWi")
	}

	#setup detmat with 2 seasons per year - same number of rows (tags) as previous matrices but columns for only 2 seasons per year (varies by number of years)
	detmat2=matrix(0,nrow=nrow(detmat.season),ncol=length(matnames2))
		colnames(detmat2) <- matnames2

	#function that collapses the months from 4 to 2 per year
	detmat2.fun=function(mo2,mosym,detmat2) #mo2= the months within the longer seasons, mosym= is the symbols for the seasons included in each grouping, detmat2 is the previously filled detmat
	{
		for(i in 1:nrow(detmat.season))
		{
			for(z in 1:length(years))
			{
				subyrdet=cbind(detmat.season[,which(substr(colnames(detmat.season),1,3)==years[z])]) #subset chosen year
					mo2=substr(colnames(subyrdet),5,8) #vector of month numbers based on column names from subset of that year
				submodet=cbind(subyrdet[,which(mo2 %in% mosym)]) #subset of months that are within the chosen season
				
				season=which(substr(colnames(detmat2),5,8)==paste(mosym[1],mosym[2],sep="")) #match seasons from substring within column names of new matrix to chosen season symbols pasted together
				yr=which(substr(colnames(detmat2),1,3)==years[z]) #match years from substring within column names of new matrix with chosen year
				
				#in columns for the appropriate season based on symbols of seasons pasted together
				#if there are no detections within that season, then then there are no detections wtihin the collapsed new season
				#if there is a detection within any of the seasons within the larger season, then the state is the last state in which the fish is detected
				#based on the assumption that the later state is where the fish will be for that season (spring/summer = summer residence, fall/winter = marine residence for winter)
				detmat2[i,season[which(season %in% yr)]]=ifelse(all(submodet[i,]==0),"0",submodet[i,which(submodet[i,]!=0)[length(which(submodet[i,]!=0))]])
			}
		}
		return(detmat2)
	}	

	#run detmat function that collapes detections into 2 states per year, based on months of those seasons and the symbols of 2 seasons from detmat.season matrix/function
	SpSu=detmat2.fun(mo2=3:8,mosym=c("Sp","Su"),detmat2=detmat2)
	FaWi=detmat2.fun(mo2=c(9:12,1:2),mosym=c("Fa","Wi"),detmat2=SpSu)	

	detmat2=FaWi

	#detmat2 = fully collapsed generated data ready for analysis 

	##################### create capture history ############################	
	natal <- initloc #create natal river group for each capture history
	detmat.group=cbind(detmat2,natal) #add natal river group
	
	#code to collapse unique capture histories + natal river group
	#natal river group is attached to the end of the capture history to identify unique combinations
	x=apply(detmat.group,1,paste,collapse="")
	counts=vector("numeric",length=length(unique(x)))

	for(i in match(x,unique(x)))
	{
		counts[i]=counts[i]+1
	}

	#use substr function to separate capture history from natal river group
	#different value within string depending on monitoring time 
	if(term==1){ch <- substr(unique(x),1,5)}
	if(term==2){ch <- substr(unique(x),1,10)}

	#number of unique capture history+natal river combos
	freq=as.numeric(apply(cbind(counts,rep("",length=length(counts))),1,paste,collapse=""))

	#use substr function to separate natal river group from capture history
	#different value within string depending on monitoring time 
	if(term==1){natal <- substr(unique(x),6,6)}
	if(term==2){natal <- substr(unique(x),11,11)}
	
	#data in format for MARK
	dfout=data.frame(ch,freq,stringsAsFactors=FALSE)
	dfout=data.frame(dfout,natal)
  
  #choose data for only 1 focal river at a time (same to format used for field data)
  #focal river is specified in the simulation function
  dfout.focal1=dfout[which(dfout$natal==focal.riv),]
	dfout.focal2=dfout.focal1[,1:2]	
	
	
return(dfout.focal2)

} #end simulation function

#############################################################
# Create Simulated Datasets
#############################################################
#pro: (1) can apply multiple models to same datasets within loop (2) do not need to spend time generating data within loop over models
#con: (1) need to run in the beginning and save until all models/iterations run (2) list form is difficult to save for future runs

looplen=1000 #number of datasets to generate
#store datasets in a list, separately for each focal river
#***to increase speed: could re-write simulation function to generate data, and then separate by focal river, without running simulation 3 times separately for each focal river
store.dataA <- as.list(1:looplen)
store.dataB <- store.dataC <- store.dataA

for(z in 1:looplen)
{ 
  if(z %% 10 ==0){print(z)}
  store.dataA[[z]] <- SimFun(focal.riv="A")
  store.dataB[[z]] <- SimFun(focal.riv="B")
  store.dataC[[z]] <- SimFun(focal.riv="C")
}

#############################################################
# RunModel: runs RMark and simulation loops
#############################################################
library(RMark)

#function allows for loop over several models
	#output= generated data
	#surv.model = 1 (natal survival),2 (state survival),3 (year survival), or 4 (year*state survival)
	#det.model = 1 (constant), 2 (migration season)
RunModel <- function(output,surv.model,det.model)
{ #start model function
  
	####-------------------------------------------- RMark ----------------------------------------------####
		#process data
		simdata.processed <- process.data(output,model="Multistrata")	

		#create default design matrix
		simdata.ddl <- make.design.data(simdata.processed,parameters=list(Psi=list(pim.type="time")))

		### ------ COVARIATES ------- ###

			#make S year covariate
			simdata.ddl$S$year <- 0
				simdata.ddl$S$year[simdata.ddl$S$time==1] <- 1
				simdata.ddl$S$year[simdata.ddl$S$time %in% c(2,3)] <- 2
				simdata.ddl$S$year[simdata.ddl$S$time %in% c(4,5)] <- 3
				simdata.ddl$S$year[simdata.ddl$S$time %in% c(6,7)] <- 4
				simdata.ddl$S$year[simdata.ddl$S$time %in% c(8,9)] <- 5

			#make p season covariate
			simdata.ddl$p$season <- 0
        		simdata.ddl$p$season[simdata.ddl$p$time %in% c(2,4)] <- 1 #out-migration
        		simdata.ddl$p$season[simdata.ddl$p$time %in% c(1,3,5)] <- 2 #in-migration
        
		### ------ FORMULAS ------- ###

			##### transitions
			#state-dependent transitions for natal rivers transmitters only == transitions by state and natal river (Markovian)
			Psi.form <- list(formula=~-1+stratum:tostratum)
        
      		##### detection
			#constant detection
			if(det.model==1)
			{
				p.form <- list(formula=~1)
			}
			#migration season detection
			if(det.model==2)
			{
				p.form <- list(formula=~season)
			}
				
			##### survival
			#natal river
			if(surv.model==1)
			{
				S.form <- list(formula=~1,link="sin")
			}
			#state
			if(surv.model==2)
			{
				S.form <- list(formula=~-1+stratum,link="sin")
			}
			#year (sin link doesn't work)
			if(surv.model==3)
			{
				S.form <- list(formula=~year)
			}
			#year*state (sin link doesn't work)
			if(surv.model==4)
			{
				S.form <- list(formula=~-1+stratum*year)
			}

		### ------ RUN MODEL ------- ###

		model <- mark(simdata.processed,simdata.ddl,model.parameters=list(S=S.form,p=p.form,Psi=Psi.form),threads=-1)

		#methods for getting transition matrix from MARK manual
		psilist <- get.real(model,"Psi",vcv=TRUE)
		psivalues <- psilist$estimates
		transmat <- TransitionMatrix(psivalues[psivalues$time==2,],vcv.real=psilist$vcv.real)
		
		#function outputs results and transition matrix
		Outs <- NULL
		Outs$results <- model$results$real
		Outs$transmat <- transmat
		return(Outs)

} #end model function

################################################################
# Loop over rivers, survival and detection models, and datasets
################################################################
rivs <- c("A","B","C") #low, medium, and high -- would need to adjust certain parts of code to add more possible river states, but can definitely be done to add complexity and realism
surv.models <- 1:4 #coded for 4 different survival models
det.models <- 1:2 #coded for 2 different detection prob models
nrun <- 1000 #total runs - can test the loop with fewer, but eventually want to match looplen (number of generated datasets)
start.time <- Sys.time() #start time of loops

for(ss in 1:length(surv.models))
{#start loop over survival models
	for(dd in 1:length(det.models))
	{#start loop over detection models
		for(rr in 1:length(rivs))
		 {#start loop over rivers
		
        	###----- setup matrices to save results for each combo ------###
        	
        	#transitions
        	rr.est <- matrix(0,nrow=nrun,ncol=3)
	          colnames(rr.est) <- c("A","B","C")
	        rr.se <- rr.lcl <- rr.ucl <- rr.est
        	
        	#detections
        	#constant detection
        	if(det.models[dd]==1)
        	{
				p.est <- matrix(0,nrow=nrun,ncol=1)
					colnames(p.est) <- "Constant"
				p.se <- p.lcl <- p.ucl <- p.est
			}
      		#migration season detection
			if(det.models[dd]==2)
			{
				p.est <- matrix(0,nrow=nrun,ncol=2)
					colnames(p.est) <- c("out","in")
				p.se <- p.lcl <- p.ucl <- p.est 
			}
			
			#survival
			#natal river
			if(surv.models[ss]==1)
			{
				S.est <- matrix(0,nrow=nrun,ncol=1)
					colnames(S.est) <- "natal"
				S.se <- S.lcl <- S.ucl <- S.est
			}
			#state
			if(surv.models[ss]==2)
			{
				S.est <- matrix(0,nrow=nrun,ncol=4)
					colnames(S.est) <- c("A","B","C","M")
				S.se <- S.lcl <- S.ucl <- S.est
			}
			#year
			if(surv.models[ss]==3)
			{
				if(term==1)
          		{
            		S.est <- matrix(0,nrow=nrun,ncol=3)
              			colnames(S.est) <- c("Y1","Y2","Y3")
          		}
				if(term==2)
          		{
            		S.est <- matrix(0,nrow=nrun,ncol=5)
              			colnames(S.est) <- c("Y1","Y2","Y3","Y4","Y5")
          		}
				S.se <- S.lcl <- S.ucl <- S.est
			}
			#year*state
			if(surv.models[ss]==4)
			{
				if(term==1)
          		{
            		S.est <- matrix(0,nrow=nrun,ncol=4*3)
              			colnames(S.est) <- c("A1","A2","A3","B1","B2","B3","C1","C2","C3","M1","M2","M3")
          		}
				if(term==2)
          		{
            		S.est <- matrix(0,nrow=nrun,ncol=4*5)
              			colnames(S.est) <- c("A1","A2","A3","A4","A5","B1","B2","B3","B4","B5","C1","C2","C3","C4","C5","M1","M2","M3","M4","M5")
          		}
				S.se <- S.lcl <- S.ucl <- S.est
			}
			
			for(z in 1:nrun)
			{#start loop over datasets
			   	if(z %% 1 == 0){print(paste(rivs[rr],surv.models[ss],det.models[dd],z,(Sys.time()-start.time),sep=" "))} #choose information to print so you know how code is doing as it runs
			   
			   	#choose generated data based on focal river in loop
			   	if(rivs[rr]=="A"){simdata <- store.dataA[[z]]}
			   	if(rivs[rr]=="B"){simdata <- store.dataB[[z]]}
			   	if(rivs[rr]=="C"){simdata <- store.dataC[[z]]}
			   	
		  #check that data has more than 1 state
          chk <- NULL
          for(i in 1:nrow(simdata))
          {
            x <- strsplit(simdata$ch[i],split=0)[[1]]
            chk <- c(chk,x)
          }
          #if not - cannnot run multi state model with this data (never detected in a riverine state)
          #*** in future - should adjust code to run CJS (survival and detection without transition parameter) or fix Psi to 0
          #skipping this model combination keeps zeros in the dataset output and adds negative bias (but does not happen often)
          if(length(unique(chk))<=2){next} 
          
          cleanup(ask=FALSE) #VITAL - re-writes mark files and prevents them from stacking up
			
				#Run model for given model within loop
        		Run <- RunModel(output=simdata,surv.model=surv.models[ss],det.model=det.models[dd])
        
        		results <- Run$results #save results from run
        		transmat <- Run$transmat #save transmat from run
        
        		S.ind <- which(substr(rownames(results),1,1)=="S") #indices for rows from results output
        		if(surv.models[ss]==2) #state-dept surv
        		{
        			states <- substr(rownames(results)[S.ind],4,4) #list of states using substring from rownames of results output
        		    match.ind <- which(colnames(S.est) %in% states) #match observed states with colnames from output matrix - possible the fish did not move into all riverine states during the simulation
        		    
        		    #plug in results into proper columns
        		    S.est[z,match.ind] <- as.matrix(results[S.ind,1]) #estimate
		 			S.se[z,match.ind] <- as.matrix(results[S.ind,2]) #standard error
		  			S.lcl[z,match.ind] <- as.matrix(results[S.ind,3]) #lower 95% CI
	  			    S.ucl[z,match.ind] <- as.matrix(results[S.ind,4]) #upper 95% CI
		  		}
		  		if(surv.models[ss]==4) #state- and year-dept surv
               	{
                	states <- unique(substr(rownames(results)[S.ind],4,4)) #list of states using substring from rownames of results output
                	years <- 1:(length(S.ind)/length(states)) #number of years, easily calculated from the number of indices/number of states
                	match.col <- NULL #empty area to save results
                	for(ii in 1:length(states))
                	{
                  		for(jj in 1:length(years))
                  		{
                    		match.col <- c(match.col,paste(states[ii],years[jj],sep="")) #match all states and years to create vector
                    		#*** could be re-written using apply and expand.grid functions
                  		}
                	}
                	match.ind <- which(colnames(S.est) %in% match.col) #match observed states and years with colnames of output matrix
                
                	#plug in results into proper column
                	S.est[z,match.ind] <- as.matrix(results[S.ind,1]) #estimate
		 			S.se[z,match.ind] <- as.matrix(results[S.ind,2]) #standard error
		  			S.lcl[z,match.ind] <- as.matrix(results[S.ind,3]) #lower 95% CI
	  				S.ucl[z,match.ind] <- as.matrix(results[S.ind,4]) #upper 95% CI
	  			}
                if(surv.models[ss]%in% c(1,3)) #natal and year-dept survival models
		  		{
		  			#simply use indices that point to survival estimates from results dataframe
		  			S.est[z,] <- as.matrix(results[S.ind,1]) 
		 			S.se[z,] <- as.matrix(results[S.ind,2])
		  			S.lcl[z,] <- as.matrix(results[S.ind,3])
	  			    S.ucl[z,] <- as.matrix(results[S.ind,4])
  			    }
                 
		    	p.ind <- which(substr(rownames(results),1,1)=="p")	#indices for detection probability parameters from results dataframe
		    		#number of indices will match number of parameters (number of rows of output matrix p.est,etc.)
		  			p.est[z,] <- as.matrix(results[p.ind,1])
		  			p.se[z,] <- as.matrix(results[p.ind,2])
		  			p.lcl[z,] <- as.matrix(results[p.ind,3])
		  			p.ucl[z,] <- as.matrix(results[p.ind,4])
		  			
		  		riv.states <- colnames(transmat$TransitionMat)[which(colnames(transmat$TransitionMat)!="M")] #riverine states specific to the states that the fish migrates into during time series
		  		rr.ind <- which(colnames(rr.est) %in% riv.states) #match riverine states to output matrix columns
		  		transmat.ind <- which(colnames(transmat$TransitionMat) %in% riv.states) #ID columns from transition matrix that have the fidelity rates we are looking for
					rr.est[z,rr.ind] <- as.matrix(transmat$TransitionMat["M",transmat.ind])
     				rr.se[z,rr.ind] <- as.matrix(transmat$se.TransitionMat["M",transmat.ind])
      				rr.lcl[z,rr.ind] <- as.matrix(transmat$lcl.TransitionMat["M",transmat.ind])
      				rr.ucl[z,rr.ind] <- as.matrix(transmat$ucl.TransitionMat["M",transmat.ind])
        
      		}#end loop over simulated data
      
      	#save results into directory as Rdata files - faster than csv if only using within R
      		#make results available for models that have already run while loops are still running
     	write.table(S.est,paste("S.est.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(S.se,paste("S.se.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(S.lcl,paste("S.lcl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(S.ucl,paste("S.ucl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	
     	write.table(p.est,paste("p.est.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(p.se,paste("p.se.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(p.lcl,paste("p.lcl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(p.ucl,paste("p.ucl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	
     	write.table(rr.est,paste("rr.est.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(rr.se,paste("rr.se.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(rr.lcl,paste("rr.lcl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	write.table(rr.ucl,paste("rr.ucl.",rivs[rr],surv.models[ss],det.models[dd],".Rdata",sep=""),sep=",")
     	 	
		}#end loop over rivers
	}#end loop over detection models
}#end loop over survival models

##############################################################
# Read in results
##############################################################
#A, B, C to specify low, med, or high number of transmitters deployed annually
#1,2,3, or 4 to specify survival model
#1 or 2 to specify detection model
#.s means short term and .l means long term
##############################################################
# Read in short-term results
##############################################################
setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Telemetry Simulations\\Update_Dec2013\\RiverModel")
#natal river survival, constant detection prob
S.est.A11.s <- read.table("S.est.A11.Rdata",sep=",")
S.lcl.A11.s <- read.table("S.lcl.A11.Rdata",sep=",")
S.ucl.A11.s <- read.table("S.ucl.A11.Rdata",sep=",")

p.est.a11.s <- read.table("p.est.a11.rdata",sep=",")
p.lcl.A11.s <- read.table("p.lcl.A11.Rdata",sep=",")
p.ucl.A11.s <- read.table("p.ucl.A11.Rdata",sep=",")

rr.est.A11.s <- read.table("rr.est.A11.Rdata",sep=",")
rr.lcl.A11.s <- read.table("rr.lcl.A11.Rdata",sep=",")
rr.ucl.A11.s <- read.table("rr.ucl.A11.Rdata",sep=",")

S.est.B11.s <- read.table("S.est.B11.Rdata",sep=",")
S.lcl.B11.s <- read.table("S.lcl.B11.Rdata",sep=",")
S.ucl.B11.s <- read.table("S.ucl.B11.Rdata",sep=",")

p.est.B11.s <- read.table("p.est.B11.Rdata",sep=",")
p.lcl.B11.s <- read.table("p.lcl.B11.Rdata",sep=",")
p.ucl.B11.s <- read.table("p.ucl.B11.Rdata",sep=",")

rr.est.B11.s <- read.table("rr.est.B11.Rdata",sep=",")
rr.lcl.B11.s <- read.table("rr.lcl.B11.Rdata",sep=",")
rr.ucl.B11.s <- read.table("rr.ucl.B11.Rdata",sep=",")

S.est.C11.s <- read.table("S.est.C11.Rdata",sep=",")
S.lcl.C11.s <- read.table("S.lcl.C11.Rdata",sep=",")
S.ucl.C11.s <- read.table("S.ucl.C11.Rdata",sep=",")

p.est.C11.s <- read.table("p.est.C11.Rdata",sep=",")
p.lcl.C11.s <- read.table("p.lcl.C11.Rdata",sep=",")
p.ucl.C11.s <- read.table("p.ucl.C11.Rdata",sep=",")

rr.est.C11.s <- read.table("rr.est.C11.Rdata",sep=",")
rr.lcl.C11.s <- read.table("rr.lcl.C11.Rdata",sep=",")
rr.ucl.C11.s <- read.table("rr.ucl.C11.Rdata",sep=",")

#natal river survival, migration season detection prob
S.est.A12.s <- read.table("S.est.A12.Rdata",sep=",")
S.lcl.A12.s <- read.table("S.lcl.A12.Rdata",sep=",")
S.ucl.A12.s <- read.table("S.ucl.A12.Rdata",sep=",")

p.est.A12.s <- read.table("p.est.A12.Rdata",sep=",")
p.lcl.A12.s <- read.table("p.lcl.A12.Rdata",sep=",")
p.ucl.A12.s <- read.table("p.ucl.A12.Rdata",sep=",")

rr.est.A12.s <- read.table("rr.est.A12.Rdata",sep=",")
rr.lcl.A12.s <- read.table("rr.lcl.A12.Rdata",sep=",")
rr.ucl.A12.s <- read.table("rr.ucl.A12.Rdata",sep=",")

S.est.B12.s <- read.table("S.est.B12.Rdata",sep=",")
S.lcl.B12.s <- read.table("S.lcl.B12.Rdata",sep=",")
S.ucl.B12.s <- read.table("S.ucl.B12.Rdata",sep=",")

p.est.B12.s <- read.table("p.est.B12.Rdata",sep=",")
p.lcl.B12.s <- read.table("p.lcl.B12.Rdata",sep=",")
p.ucl.B12.s <- read.table("p.ucl.B12.Rdata",sep=",")

rr.est.B12.s <- read.table("rr.est.B12.Rdata",sep=",")
rr.lcl.B12.s <- read.table("rr.lcl.B12.Rdata",sep=",")
rr.ucl.B12.s <- read.table("rr.ucl.B12.Rdata",sep=",")

S.est.C12.s <- read.table("S.est.C12.Rdata",sep=",")
S.lcl.C12.s <- read.table("S.lcl.C12.Rdata",sep=",")
S.ucl.C12.s <- read.table("S.ucl.C12.Rdata",sep=",")

p.est.C12.s <- read.table("p.est.C12.Rdata",sep=",")
p.lcl.C12.s <- read.table("p.lcl.C12.Rdata",sep=",")
p.ucl.C12.s <- read.table("p.ucl.C12.Rdata",sep=",")

rr.est.C12.s <- read.table("rr.est.C12.Rdata",sep=",")
rr.lcl.C12.s <- read.table("rr.lcl.C12.Rdata",sep=",")
rr.ucl.C12.s <- read.table("rr.ucl.C12.Rdata",sep=",")

#state survival, constant detection prob
S.est.A21.s <- read.table("S.est.A21.Rdata",sep=",")
S.lcl.A21.s <- read.table("S.lcl.A21.Rdata",sep=",")
S.ucl.A21.s <- read.table("S.ucl.A21.Rdata",sep=",")

p.est.A21.s <- read.table("p.est.A21.Rdata",sep=",")
p.lcl.A21.s <- read.table("p.lcl.A21.Rdata",sep=",")
p.ucl.A21.s <- read.table("p.ucl.A21.Rdata",sep=",")

rr.est.A21.s <- read.table("rr.est.A21.Rdata",sep=",")
rr.lcl.A21.s <- read.table("rr.lcl.A21.Rdata",sep=",")
rr.ucl.A21.s <- read.table("rr.ucl.A21.Rdata",sep=",")

S.est.B21.s <- read.table("S.est.B21.Rdata",sep=",")
S.lcl.B21.s <- read.table("S.lcl.B21.Rdata",sep=",")
S.ucl.B21.s <- read.table("S.ucl.B21.Rdata",sep=",")

p.est.B21.s <- read.table("p.est.B21.Rdata",sep=",")
p.lcl.B21.s <- read.table("p.lcl.B21.Rdata",sep=",")
p.ucl.B21.s <- read.table("p.ucl.B21.Rdata",sep=",")

rr.est.B21.s <- read.table("rr.est.B21.Rdata",sep=",")
rr.lcl.B21.s <- read.table("rr.lcl.B21.Rdata",sep=",")
rr.ucl.B21.s <- read.table("rr.ucl.B21.Rdata",sep=",")

S.est.C21.s <- read.table("S.est.C21.Rdata",sep=",")
S.lcl.C21.s <- read.table("S.lcl.C21.Rdata",sep=",")
S.ucl.C21.s <- read.table("S.ucl.C21.Rdata",sep=",")

p.est.C21.s <- read.table("p.est.C21.Rdata",sep=",")
p.lcl.C21.s <- read.table("p.lcl.C21.Rdata",sep=",")
p.ucl.C21.s <- read.table("p.ucl.C21.Rdata",sep=",")

rr.est.C21.s <- read.table("rr.est.C21.Rdata",sep=",")
rr.lcl.C21.s <- read.table("rr.lcl.C21.Rdata",sep=",")
rr.ucl.C21.s <- read.table("rr.ucl.C21.Rdata",sep=",")

#state survival, migration det prob
S.est.A22.s <- read.table("S.est.A22.Rdata",sep=",")
S.lcl.A22.s <- read.table("S.lcl.A22.Rdata",sep=",")
S.ucl.A22.s <- read.table("S.ucl.A22.Rdata",sep=",")

p.est.A22.s <- read.table("p.est.A22.Rdata",sep=",")
p.lcl.A22.s <- read.table("p.lcl.A22.Rdata",sep=",")
p.ucl.A22.s <- read.table("p.ucl.A22.Rdata",sep=",")

rr.est.A22.s <- read.table("rr.est.A22.Rdata",sep=",")
rr.lcl.A22.s <- read.table("rr.lcl.A22.Rdata",sep=",")
rr.ucl.A22.s <- read.table("rr.ucl.A22.Rdata",sep=",")

S.est.B22.s <- read.table("S.est.B22.Rdata",sep=",")
S.lcl.B22.s <- read.table("S.lcl.B22.Rdata",sep=",")
S.ucl.B22.s <- read.table("S.ucl.B22.Rdata",sep=",")

p.est.B22.s <- read.table("p.est.B22.Rdata",sep=",")
p.lcl.B22.s <- read.table("p.lcl.B22.Rdata",sep=",")
p.ucl.B22.s <- read.table("p.ucl.B22.Rdata",sep=",")

rr.est.B22.s <- read.table("rr.est.B22.Rdata",sep=",")
rr.lcl.B22.s <- read.table("rr.lcl.B22.Rdata",sep=",")
rr.ucl.B22.s <- read.table("rr.ucl.B22.Rdata",sep=",")

S.est.C22.s <- read.table("S.est.C22.Rdata",sep=",")
S.lcl.C22.s <- read.table("S.lcl.C22.Rdata",sep=",")
S.ucl.C22.s <- read.table("S.ucl.C22.Rdata",sep=",")

p.est.C22.s <- read.table("p.est.C22.Rdata",sep=",")
p.lcl.C22.s <- read.table("p.lcl.C22.Rdata",sep=",")
p.ucl.C22.s <- read.table("p.ucl.C22.Rdata",sep=",")

rr.est.C22.s <- read.table("rr.est.C22.Rdata",sep=",")
rr.lcl.C22.s <- read.table("rr.lcl.C22.Rdata",sep=",")
rr.ucl.C22.s <- read.table("rr.ucl.C22.Rdata",sep=",")

#year survival, constant det prob
S.est.A31.s <- read.table("S.est.A31.Rdata",sep=",")
S.lcl.A31.s <- read.table("S.lcl.A31.Rdata",sep=",")
S.ucl.A31.s <- read.table("S.ucl.A31.Rdata",sep=",")

p.est.A31.s <- read.table("p.est.A31.Rdata",sep=",")
p.lcl.A31.s <- read.table("p.lcl.A31.Rdata",sep=",")
p.ucl.A31.s <- read.table("p.ucl.A31.Rdata",sep=",")

rr.est.A31.s <- read.table("rr.est.A31.Rdata",sep=",")
rr.lcl.A31.s <- read.table("rr.lcl.A31.Rdata",sep=",")
rr.ucl.A31.s <- read.table("rr.ucl.A31.Rdata",sep=",")

S.est.B31.s <- read.table("S.est.B31.Rdata",sep=",")
S.lcl.B31.s <- read.table("S.lcl.B31.Rdata",sep=",")
S.ucl.B31.s <- read.table("S.ucl.B31.Rdata",sep=",")

p.est.B31.s <- read.table("p.est.B31.Rdata",sep=",")
p.lcl.B31.s <- read.table("p.lcl.B31.Rdata",sep=",")
p.ucl.B31.s <- read.table("p.ucl.B31.Rdata",sep=",")

rr.est.B31.s <- read.table("rr.est.B31.Rdata",sep=",")
rr.lcl.B31.s <- read.table("rr.lcl.B31.Rdata",sep=",")
rr.ucl.B31.s <- read.table("rr.ucl.B31.Rdata",sep=",")

S.est.C31.s <- read.table("S.est.C31.Rdata",sep=",")
S.lcl.C31.s <- read.table("S.lcl.C31.Rdata",sep=",")
S.ucl.C31.s <- read.table("S.ucl.C31.Rdata",sep=",")

p.est.C31.s <- read.table("p.est.C31.Rdata",sep=",")
p.lcl.C31.s <- read.table("p.lcl.C31.Rdata",sep=",")
p.ucl.C31.s <- read.table("p.ucl.C31.Rdata",sep=",")

rr.est.C31.s <- read.table("rr.est.C31.Rdata",sep=",")
rr.lcl.C31.s <- read.table("rr.lcl.C31.Rdata",sep=",")
rr.ucl.C31.s <- read.table("rr.ucl.C31.Rdata",sep=",")

#year survival, migration season det prob
S.est.A32.s <- read.table("S.est.A32.Rdata",sep=",")
S.lcl.A32.s <- read.table("S.lcl.A32.Rdata",sep=",")
S.ucl.A32.s <- read.table("S.ucl.A32.Rdata",sep=",")

p.est.A32.s <- read.table("p.est.A32.Rdata",sep=",")
p.lcl.A32.s <- read.table("p.lcl.A32.Rdata",sep=",")
p.ucl.A32.s <- read.table("p.ucl.A32.Rdata",sep=",")

rr.est.A32.s <- read.table("rr.est.A32.Rdata",sep=",")
rr.lcl.A32.s <- read.table("rr.lcl.A32.Rdata",sep=",")
rr.ucl.A32.s <- read.table("rr.ucl.A32.Rdata",sep=",")

S.est.B32.s <- read.table("S.est.B32.Rdata",sep=",")
S.lcl.B32.s <- read.table("S.lcl.B32.Rdata",sep=",")
S.ucl.B32.s <- read.table("S.ucl.B32.Rdata",sep=",")

p.est.B32.s <- read.table("p.est.B32.Rdata",sep=",")
p.lcl.B32.s <- read.table("p.lcl.B32.Rdata",sep=",")
p.ucl.B32.s <- read.table("p.ucl.B32.Rdata",sep=",")

rr.est.B32.s <- read.table("rr.est.B32.Rdata",sep=",")
rr.lcl.B32.s <- read.table("rr.lcl.B32.Rdata",sep=",")
rr.ucl.B32.s <- read.table("rr.ucl.B32.Rdata",sep=",")

S.est.C32.s <- read.table("S.est.C32.Rdata",sep=",")
S.lcl.C32.s <- read.table("S.lcl.C32.Rdata",sep=",")
S.ucl.C32.s <- read.table("S.ucl.C32.Rdata",sep=",")

p.est.C32.s <- read.table("p.est.C32.Rdata",sep=",")
p.lcl.C32.s <- read.table("p.lcl.C32.Rdata",sep=",")
p.ucl.C32.s <- read.table("p.ucl.C32.Rdata",sep=",")

rr.est.C32.s <- read.table("rr.est.C32.Rdata",sep=",")
rr.lcl.C32.s <- read.table("rr.lcl.C32.Rdata",sep=",")
rr.ucl.C32.s <- read.table("rr.ucl.C32.Rdata",sep=",")

#year*state survival, constant det prob
S.est.A41.s <- read.table("S.est.A41.Rdata",sep=",")
S.lcl.A41.s <- read.table("S.lcl.A41.Rdata",sep=",")
S.ucl.A41.s <- read.table("S.ucl.A41.Rdata",sep=",")

p.est.A41.s <- read.table("p.est.A41.Rdata",sep=",")
p.lcl.A41.s <- read.table("p.lcl.A41.Rdata",sep=",")
p.ucl.A41.s <- read.table("p.ucl.A41.Rdata",sep=",")

rr.est.A41.s <- read.table("rr.est.A41.Rdata",sep=",")
rr.lcl.A41.s <- read.table("rr.lcl.A41.Rdata",sep=",")
rr.ucl.A41.s <- read.table("rr.ucl.A41.Rdata",sep=",")

S.est.B41.s <- read.table("S.est.B41.Rdata",sep=",")
S.lcl.B41.s <- read.table("S.lcl.B41.Rdata",sep=",")
S.ucl.B41.s <- read.table("S.ucl.B41.Rdata",sep=",")

p.est.B41.s <- read.table("p.est.B41.Rdata",sep=",")
p.lcl.B41.s <- read.table("p.lcl.B41.Rdata",sep=",")
p.ucl.B41.s <- read.table("p.ucl.B41.Rdata",sep=",")

rr.est.B41.s <- read.table("rr.est.B41.Rdata",sep=",")
rr.lcl.B41.s <- read.table("rr.lcl.B41.Rdata",sep=",")
rr.ucl.B41.s <- read.table("rr.ucl.B41.Rdata",sep=",")

S.est.C41.s <- read.table("S.est.C41.Rdata",sep=",")
S.lcl.C41.s <- read.table("S.lcl.C41.Rdata",sep=",")
S.ucl.C41.s <- read.table("S.ucl.C41.Rdata",sep=",")

p.est.C41.s <- read.table("p.est.C41.Rdata",sep=",")
p.lcl.C41.s <- read.table("p.lcl.C41.Rdata",sep=",")
p.ucl.C41.s <- read.table("p.ucl.C41.Rdata",sep=",")

rr.est.C41.s <- read.table("rr.est.C41.Rdata",sep=",")
rr.lcl.C41.s <- read.table("rr.lcl.C41.Rdata",sep=",")
rr.ucl.C41.s <- read.table("rr.ucl.C41.Rdata",sep=",")

#year*state survival, migration season det prob
S.est.A42.s <- read.table("S.est.A42.Rdata",sep=",")
S.lcl.A42.s <- read.table("S.lcl.A42.Rdata",sep=",")
S.ucl.A42.s <- read.table("S.ucl.A42.Rdata",sep=",")

p.est.A42.s <- read.table("p.est.A42.Rdata",sep=",")
p.lcl.A42.s <- read.table("p.lcl.A42.Rdata",sep=",")
p.ucl.A42.s <- read.table("p.ucl.A42.Rdata",sep=",")

rr.est.A42.s <- read.table("rr.est.A42.Rdata",sep=",")
rr.lcl.A42.s <- read.table("rr.lcl.A42.Rdata",sep=",")
rr.ucl.A42.s <- read.table("rr.ucl.A42.Rdata",sep=",")

S.est.B42.s <- read.table("S.est.B42.Rdata",sep=",")
S.lcl.B42.s <- read.table("S.lcl.B42.Rdata",sep=",")
S.ucl.B42.s <- read.table("S.ucl.B42.Rdata",sep=",")

p.est.B42.s <- read.table("p.est.B42.Rdata",sep=",")
p.lcl.B42.s <- read.table("p.lcl.B42.Rdata",sep=",")
p.ucl.B42.s <- read.table("p.ucl.B42.Rdata",sep=",")

rr.est.B42.s <- read.table("rr.est.B42.Rdata",sep=",")
rr.lcl.B42.s <- read.table("rr.lcl.B42.Rdata",sep=",")
rr.ucl.B42.s <- read.table("rr.ucl.B42.Rdata",sep=",")

S.est.C42.s <- read.table("S.est.C42.Rdata",sep=",")
S.lcl.C42.s <- read.table("S.lcl.C42.Rdata",sep=",")
S.ucl.C42.s <- read.table("S.ucl.C42.Rdata",sep=",")

p.est.C42.s <- read.table("p.est.C42.Rdata",sep=",")
p.lcl.C42.s <- read.table("p.lcl.C42.Rdata",sep=",")
p.ucl.C42.s <- read.table("p.ucl.C42.Rdata",sep=",")

rr.est.C42.s <- read.table("rr.est.C42.Rdata",sep=",")
rr.lcl.C42.s <- read.table("rr.lcl.C42.Rdata",sep=",")
rr.ucl.C42.s <- read.table("rr.ucl.C42.Rdata",sep=",")

##############################################################
# Read in long-term sim results
##############################################################
setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Telemetry Simulations\\Update_Dec2013\\RiverModel (long)")
#natal river survival, constant detection prob
S.est.A11.l <- read.table("S.est.A11.Rdata",sep=",")
S.lcl.A11.l <- read.table("S.lcl.A11.Rdata",sep=",")
S.ucl.A11.l <- read.table("S.ucl.A11.Rdata",sep=",")

p.est.A11.l <- read.table("p.est.A11.Rdata",sep=",")
p.lcl.A11.l <- read.table("p.lcl.A11.Rdata",sep=",")
p.ucl.A11.l <- read.table("p.ucl.A11.Rdata",sep=",")

rr.est.A11.l <- read.table("rr.est.A11.Rdata",sep=",")
rr.lcl.A11.l <- read.table("rr.lcl.A11.Rdata",sep=",")
rr.ucl.A11.l <- read.table("rr.ucl.A11.Rdata",sep=",")

S.est.B11.l <- read.table("S.est.B11.Rdata",sep=",")
S.lcl.B11.l <- read.table("S.lcl.B11.Rdata",sep=",")
S.ucl.B11.l <- read.table("S.ucl.B11.Rdata",sep=",")

p.est.B11.l <- read.table("p.est.B11.Rdata",sep=",")
p.lcl.B11.l <- read.table("p.lcl.B11.Rdata",sep=",")
p.ucl.B11.l <- read.table("p.ucl.B11.Rdata",sep=",")

rr.est.B11.l <- read.table("rr.est.B11.Rdata",sep=",")
rr.lcl.B11.l <- read.table("rr.lcl.B11.Rdata",sep=",")
rr.ucl.B11.l <- read.table("rr.ucl.B11.Rdata",sep=",")

S.est.C11.l <- read.table("S.est.C11.Rdata",sep=",")
S.lcl.C11.l <- read.table("S.lcl.C11.Rdata",sep=",")
S.ucl.C11.l <- read.table("S.ucl.C11.Rdata",sep=",")

p.est.C11.l <- read.table("p.est.C11.Rdata",sep=",")
p.lcl.C11.l <- read.table("p.lcl.C11.Rdata",sep=",")
p.ucl.C11.l <- read.table("p.ucl.C11.Rdata",sep=",")

rr.est.C11.l <- read.table("rr.est.C11.Rdata",sep=",")
rr.lcl.C11.l <- read.table("rr.lcl.C11.Rdata",sep=",")
rr.ucl.C11.l <- read.table("rr.ucl.C11.Rdata",sep=",")

#natal river survival, migration season detection prob
S.est.A12.l <- read.table("S.est.A12.Rdata",sep=",")
S.lcl.A12.l <- read.table("S.lcl.A12.Rdata",sep=",")
S.ucl.A12.l <- read.table("S.ucl.A12.Rdata",sep=",")

p.est.A12.l <- read.table("p.est.A12.Rdata",sep=",")
p.lcl.A12.l <- read.table("p.lcl.A12.Rdata",sep=",")
p.ucl.A12.l <- read.table("p.ucl.A12.Rdata",sep=",")

rr.est.A12.l <- read.table("rr.est.A12.Rdata",sep=",")
rr.lcl.A12.l <- read.table("rr.lcl.A12.Rdata",sep=",")
rr.ucl.A12.l <- read.table("rr.ucl.A12.Rdata",sep=",")

S.est.B12.l <- read.table("S.est.B12.Rdata",sep=",")
S.lcl.B12.l <- read.table("S.lcl.B12.Rdata",sep=",")
S.ucl.B12.l <- read.table("S.ucl.B12.Rdata",sep=",")

p.est.B12.l <- read.table("p.est.B12.Rdata",sep=",")
p.lcl.B12.l <- read.table("p.lcl.B12.Rdata",sep=",")
p.ucl.B12.l <- read.table("p.ucl.B12.Rdata",sep=",")

rr.est.B12.l <- read.table("rr.est.B12.Rdata",sep=",")
rr.lcl.B12.l <- read.table("rr.lcl.B12.Rdata",sep=",")
rr.ucl.B12.l <- read.table("rr.ucl.B12.Rdata",sep=",")

S.est.C12.l <- read.table("S.est.C12.Rdata",sep=",")
S.lcl.C12.l <- read.table("S.lcl.C12.Rdata",sep=",")
S.ucl.C12.l <- read.table("S.ucl.C12.Rdata",sep=",")

p.est.C12.l <- read.table("p.est.C12.Rdata",sep=",")
p.lcl.C12.l <- read.table("p.lcl.C12.Rdata",sep=",")
p.ucl.C12.l <- read.table("p.ucl.C12.Rdata",sep=",")

rr.est.C12.l <- read.table("rr.est.C12.Rdata",sep=",")
rr.lcl.C12.l <- read.table("rr.lcl.C12.Rdata",sep=",")
rr.ucl.C12.l <- read.table("rr.ucl.C12.Rdata",sep=",")

#state survival, constant detection prob
S.est.A21.l <- read.table("S.est.A21.Rdata",sep=",")
S.lcl.A21.l <- read.table("S.lcl.A21.Rdata",sep=",")
S.ucl.A21.l <- read.table("S.ucl.A21.Rdata",sep=",")

p.est.A21.l <- read.table("p.est.A21.Rdata",sep=",")
p.lcl.A21.l <- read.table("p.lcl.A21.Rdata",sep=",")
p.ucl.A21.l <- read.table("p.ucl.A21.Rdata",sep=",")

rr.est.A21.l <- read.table("rr.est.A21.Rdata",sep=",")
rr.lcl.A21.l <- read.table("rr.lcl.A21.Rdata",sep=",")
rr.ucl.A21.l <- read.table("rr.ucl.A21.Rdata",sep=",")

S.est.B21.l <- read.table("S.est.B21.Rdata",sep=",")
S.lcl.B21.l <- read.table("S.lcl.B21.Rdata",sep=",")
S.ucl.B21.l <- read.table("S.ucl.B21.Rdata",sep=",")

p.est.B21.l <- read.table("p.est.B21.Rdata",sep=",")
p.lcl.B21.l <- read.table("p.lcl.B21.Rdata",sep=",")
p.ucl.B21.l <- read.table("p.ucl.B21.Rdata",sep=",")

rr.est.B21.l <- read.table("rr.est.B21.Rdata",sep=",")
rr.lcl.B21.l <- read.table("rr.lcl.B21.Rdata",sep=",")
rr.ucl.B21.l <- read.table("rr.ucl.B21.Rdata",sep=",")

S.est.C21.l <- read.table("S.est.C21.Rdata",sep=",")
S.lcl.C21.l <- read.table("S.lcl.C21.Rdata",sep=",")
S.ucl.C21.l <- read.table("S.ucl.C21.Rdata",sep=",")

p.est.C21.l <- read.table("p.est.C21.Rdata",sep=",")
p.lcl.C21.l <- read.table("p.lcl.C21.Rdata",sep=",")
p.ucl.C21.l <- read.table("p.ucl.C21.Rdata",sep=",")

rr.est.C21.l <- read.table("rr.est.C21.Rdata",sep=",")
rr.lcl.C21.l <- read.table("rr.lcl.C21.Rdata",sep=",")
rr.ucl.C21.l <- read.table("rr.ucl.C21.Rdata",sep=",")

#state survival, migration det prob
S.est.A22.l <- read.table("S.est.A22.Rdata",sep=",")
S.lcl.A22.l <- read.table("S.lcl.A22.Rdata",sep=",")
S.ucl.A22.l <- read.table("S.ucl.A22.Rdata",sep=",")

p.est.A22.l <- read.table("p.est.A22.Rdata",sep=",")
p.lcl.A22.l <- read.table("p.lcl.A22.Rdata",sep=",")
p.ucl.A22.l <- read.table("p.ucl.A22.Rdata",sep=",")

rr.est.A22.l <- read.table("rr.est.A22.Rdata",sep=",")
rr.lcl.A22.l <- read.table("rr.lcl.A22.Rdata",sep=",")
rr.ucl.A22.l <- read.table("rr.ucl.A22.Rdata",sep=",")

S.est.B22.l <- read.table("S.est.B22.Rdata",sep=",")
S.lcl.B22.l <- read.table("S.lcl.B22.Rdata",sep=",")
S.ucl.B22.l <- read.table("S.ucl.B22.Rdata",sep=",")

p.est.B22.l <- read.table("p.est.B22.Rdata",sep=",")
p.lcl.B22.l <- read.table("p.lcl.B22.Rdata",sep=",")
p.ucl.B22.l <- read.table("p.ucl.B22.Rdata",sep=",")

rr.est.B22.l <- read.table("rr.est.B22.Rdata",sep=",")
rr.lcl.B22.l <- read.table("rr.lcl.B22.Rdata",sep=",")
rr.ucl.B22.l <- read.table("rr.ucl.B22.Rdata",sep=",")

S.est.C22.l <- read.table("S.est.C22.Rdata",sep=",")
S.lcl.C22.l <- read.table("S.lcl.C22.Rdata",sep=",")
S.ucl.C22.l <- read.table("S.ucl.C22.Rdata",sep=",")

p.est.C22.l <- read.table("p.est.C22.Rdata",sep=",")
p.lcl.C22.l <- read.table("p.lcl.C22.Rdata",sep=",")
p.ucl.C22.l <- read.table("p.ucl.C22.Rdata",sep=",")

rr.est.C22.l <- read.table("rr.est.C22.Rdata",sep=",")
rr.lcl.C22.l <- read.table("rr.lcl.C22.Rdata",sep=",")
rr.ucl.C22.l <- read.table("rr.ucl.C22.Rdata",sep=",")

#year survival, constant det prob
S.est.A31.l <- read.table("S.est.A31.Rdata",sep=",")
S.lcl.A31.l <- read.table("S.lcl.A31.Rdata",sep=",")
S.ucl.A31.l <- read.table("S.ucl.A31.Rdata",sep=",")

p.est.A31.l <- read.table("p.est.A31.Rdata",sep=",")
p.lcl.A31.l <- read.table("p.lcl.A31.Rdata",sep=",")
p.ucl.A31.l <- read.table("p.ucl.A31.Rdata",sep=",")

rr.est.A31.l <- read.table("rr.est.A31.Rdata",sep=",")
rr.lcl.A31.l <- read.table("rr.lcl.A31.Rdata",sep=",")
rr.ucl.A31.l <- read.table("rr.ucl.A31.Rdata",sep=",")

S.est.B31.l <- read.table("S.est.B31.Rdata",sep=",")
S.lcl.B31.l <- read.table("S.lcl.B31.Rdata",sep=",")
S.ucl.B31.l <- read.table("S.ucl.B31.Rdata",sep=",")

p.est.B31.l <- read.table("p.est.B31.Rdata",sep=",")
p.lcl.B31.l <- read.table("p.lcl.B31.Rdata",sep=",")
p.ucl.B31.l <- read.table("p.ucl.B31.Rdata",sep=",")

rr.est.B31.l <- read.table("rr.est.B31.Rdata",sep=",")
rr.lcl.B31.l <- read.table("rr.lcl.B31.Rdata",sep=",")
rr.ucl.B31.l <- read.table("rr.ucl.B31.Rdata",sep=",")

S.est.C31.l <- read.table("S.est.C31.Rdata",sep=",")
S.lcl.C31.l <- read.table("S.lcl.C31.Rdata",sep=",")
S.ucl.C31.l <- read.table("S.ucl.C31.Rdata",sep=",")

p.est.C31.l <- read.table("p.est.C31.Rdata",sep=",")
p.lcl.C31.l <- read.table("p.lcl.C31.Rdata",sep=",")
p.ucl.C31.l <- read.table("p.ucl.C31.Rdata",sep=",")

rr.est.C31.l <- read.table("rr.est.C31.Rdata",sep=",")
rr.lcl.C31.l <- read.table("rr.lcl.C31.Rdata",sep=",")
rr.ucl.C31.l <- read.table("rr.ucl.C31.Rdata",sep=",")

#year survival, migration season det prob
S.est.A32.l <- read.table("S.est.A32.Rdata",sep=",")
S.lcl.A32.l <- read.table("S.lcl.A32.Rdata",sep=",")
S.ucl.A32.l <- read.table("S.ucl.A32.Rdata",sep=",")

p.est.A32.l <- read.table("p.est.A32.Rdata",sep=",")
p.lcl.A32.l <- read.table("p.lcl.A32.Rdata",sep=",")
p.ucl.A32.l <- read.table("p.ucl.A32.Rdata",sep=",")

rr.est.A32.l <- read.table("rr.est.A32.Rdata",sep=",")
rr.lcl.A32.l <- read.table("rr.lcl.A32.Rdata",sep=",")
rr.ucl.A32.l <- read.table("rr.ucl.A32.Rdata",sep=",")

S.est.B32.l <- read.table("S.est.B32.Rdata",sep=",")
S.lcl.B32.l <- read.table("S.lcl.B32.Rdata",sep=",")
S.ucl.B32.l <- read.table("S.ucl.B32.Rdata",sep=",")

p.est.B32.l <- read.table("p.est.B32.Rdata",sep=",")
p.lcl.B32.l <- read.table("p.lcl.B32.Rdata",sep=",")
p.ucl.B32.l <- read.table("p.ucl.B32.Rdata",sep=",")

rr.est.B32.l <- read.table("rr.est.B32.Rdata",sep=",")
rr.lcl.B32.l <- read.table("rr.lcl.B32.Rdata",sep=",")
rr.ucl.B32.l <- read.table("rr.ucl.B32.Rdata",sep=",")

S.est.C32.l <- read.table("S.est.C32.Rdata",sep=",")
S.lcl.C32.l <- read.table("S.lcl.C32.Rdata",sep=",")
S.ucl.C32.l <- read.table("S.ucl.C32.Rdata",sep=",")

p.est.C32.l <- read.table("p.est.C32.Rdata",sep=",")
p.lcl.C32.l <- read.table("p.lcl.C32.Rdata",sep=",")
p.ucl.C32.l <- read.table("p.ucl.C32.Rdata",sep=",")

rr.est.C32.l <- read.table("rr.est.C32.Rdata",sep=",")
rr.lcl.C32.l <- read.table("rr.lcl.C32.Rdata",sep=",")
rr.ucl.C32.l <- read.table("rr.ucl.C32.Rdata",sep=",")

#year*state survival, constant det prob
S.est.A41.l <- read.table("S.est.A41.Rdata",sep=",")
S.lcl.A41.l <- read.table("S.lcl.A41.Rdata",sep=",")
S.ucl.A41.l <- read.table("S.ucl.A41.Rdata",sep=",")

p.est.A41.l <- read.table("p.est.A41.Rdata",sep=",")
p.lcl.A41.l <- read.table("p.lcl.A41.Rdata",sep=",")
p.ucl.A41.l <- read.table("p.ucl.A41.Rdata",sep=",")

rr.est.A41.l <- read.table("rr.est.A41.Rdata",sep=",")
rr.lcl.A41.l <- read.table("rr.lcl.A41.Rdata",sep=",")
rr.ucl.A41.l <- read.table("rr.ucl.A41.Rdata",sep=",")

S.est.B41.l <- read.table("S.est.B41.Rdata",sep=",")
S.lcl.B41.l <- read.table("S.lcl.B41.Rdata",sep=",")
S.ucl.B41.l <- read.table("S.ucl.B41.Rdata",sep=",")

p.est.B41.l <- read.table("p.est.B41.Rdata",sep=",")
p.lcl.B41.l <- read.table("p.lcl.B41.Rdata",sep=",")
p.ucl.B41.l <- read.table("p.ucl.B41.Rdata",sep=",")

rr.est.B41.l <- read.table("rr.est.B41.Rdata",sep=",")
rr.lcl.B41.l <- read.table("rr.lcl.B41.Rdata",sep=",")
rr.ucl.B41.l <- read.table("rr.ucl.B41.Rdata",sep=",")

S.est.C41.l <- read.table("S.est.C41.Rdata",sep=",")
S.lcl.C41.l <- read.table("S.lcl.C41.Rdata",sep=",")
S.ucl.C41.l <- read.table("S.ucl.C41.Rdata",sep=",")

p.est.C41.l <- read.table("p.est.C41.Rdata",sep=",")
p.lcl.C41.l <- read.table("p.lcl.C41.Rdata",sep=",")
p.ucl.C41.l <- read.table("p.ucl.C41.Rdata",sep=",")

rr.est.C41.l <- read.table("rr.est.C41.Rdata",sep=",")
rr.lcl.C41.l <- read.table("rr.lcl.C41.Rdata",sep=",")
rr.ucl.C41.l <- read.table("rr.ucl.C41.Rdata",sep=",")

#year*state survival, migration season det prob
S.est.A42.l <- read.table("S.est.A42.Rdata",sep=",")
S.lcl.A42.l <- read.table("S.lcl.A42.Rdata",sep=",")
S.ucl.A42.l <- read.table("S.ucl.A42.Rdata",sep=",")

p.est.A42.l <- read.table("p.est.A42.Rdata",sep=",")
p.lcl.A42.l <- read.table("p.lcl.A42.Rdata",sep=",")
p.ucl.A42.l <- read.table("p.ucl.A42.Rdata",sep=",")

rr.est.A42.l <- read.table("rr.est.A42.Rdata",sep=",")
rr.lcl.A42.l <- read.table("rr.lcl.A42.Rdata",sep=",")
rr.ucl.A42.l <- read.table("rr.ucl.A42.Rdata",sep=",")

S.est.B42.l <- read.table("S.est.B42.Rdata",sep=",")
S.lcl.B42.l <- read.table("S.lcl.B42.Rdata",sep=",")
S.ucl.B42.l <- read.table("S.ucl.B42.Rdata",sep=",")

p.est.B42.l <- read.table("p.est.B42.Rdata",sep=",")
p.lcl.B42.l <- read.table("p.lcl.B42.Rdata",sep=",")
p.ucl.B42.l <- read.table("p.ucl.B42.Rdata",sep=",")

rr.est.B42.l <- read.table("rr.est.B42.Rdata",sep=",")
rr.lcl.B42.l <- read.table("rr.lcl.B42.Rdata",sep=",")
rr.ucl.B42.l <- read.table("rr.ucl.B42.Rdata",sep=",")

S.est.C42.l <- read.table("S.est.C42.Rdata",sep=",")
S.lcl.C42.l <- read.table("S.lcl.C42.Rdata",sep=",")
S.ucl.C42.l <- read.table("S.ucl.C42.Rdata",sep=",")

p.est.C42.l <- read.table("p.est.C42.Rdata",sep=",")
p.lcl.C42.l <- read.table("p.lcl.C42.Rdata",sep=",")
p.ucl.C42.l <- read.table("p.ucl.C42.Rdata",sep=",")

rr.est.C42.l <- read.table("rr.est.C42.Rdata",sep=",")
rr.lcl.C42.l <- read.table("rr.lcl.C42.Rdata",sep=",")
rr.ucl.C42.l <- read.table("rr.ucl.C42.Rdata",sep=",")

##############################################################
# Plots and tables for paper
##############################################################
setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Telemetry Simulations\\Update_Dec2013") #save plots and and tables into general directory

#### true values (from simulation above)
survR=0.9 #riverine survival
survM=0.8 #marine survival
survA <- 0.85 #average over the whole year

#monthly detection probability - on months == 0.6 total, off months == 0.06 total
#calculate seasonal detection probability
		p_on=0.3/3
		p_off=0.05/3
			pvec=c(rep(p_on,3),rep(p_off,3))
			pvec2=vector(length=length(pvec))
			pvec2[1]=1-pvec[1]
			for(i in 2:length(pvec2))
			{
				pvec2[i]=pvec2[i-1]*(1-pvec[i])
			}
			pseason=1-pvec2[length(pvec2)]


		#small amounts of emigration during winter months
		emig=0.2

### boxplot showing model unbiased
#add 5 year estimates next to 2 year estimates when available
ax.labels <- apply(expand.grid(c("2yrs","5yrs"),c(10,20,50)),1,paste,collapse=", ")
ylim <- c(-0.05,1.12)
yaxis <- c(0,1)
par(mfrow=c(2,2),mai=c(0,0,0,0),omi=c(1,1,0.5,0.5),bg="white")
boxplot(S.est.A21.s[,"A"],S.est.A21.l[,"A"],
		S.est.B21.s[,"B"],S.est.B21.l[,"B"],
		S.est.C21.s[,"C"],S.est.C21.l[,"C"],
		col=c(rep(c("gray45","gray85"),3)),ylim=ylim,axes=FALSE,xaxs="i",yaxs="i",xpd=TRUE)
	abline(h=survR,col="black",lwd=5,lty=2)
	axis(2,at=pretty(yaxis),las=2,cex.axis=1.2)
	mtext("Riverine Survival",side=3,line=-1.5,font=2,cex=1.2)
	box()
boxplot(S.est.A21.s[,"M"],S.est.A21.l[,"M"],
		S.est.B21.s[,"M"],S.est.B21.l[,"M"],
		S.est.C21.s[,"M"],S.est.C21.l[,"M"],
		col=c(rep(c("gray45","gray85"),3)),ylim=ylim,axes=FALSE,xaxs="i",yaxs="i",xpd=TRUE)
	abline(h=survM,col="black",lwd=5,lty=2)
	mtext("Marine Survival",side=3,line=-1.5,font=2,cex=1.2)
	box()
boxplot(p.est.A21.s[,1],p.est.A21.l[,1],
		p.est.B21.s[,1],p.est.B21.l[,1],
		p.est.C21.s[,1],p.est.C21.l[,1],
		col=c(rep(c("gray45","gray85"),3)),ylim=ylim,axes=FALSE,xaxs="i",yaxs="i",xpd=TRUE)
	abline(h=pseason,col="black",lwd=5,lty=2)
	axis(1,at=1:length(ax.labels),labels=ax.labels,cex.axis=1.2)
	axis(2,at=pretty(yaxis),las=2,cex.axis=1.2)
	mtext("Detection Probability",side=3,line=-1.5,font=2,cex=1.2)
	box()
boxplot(rr.est.A21.s[,"A"],rr.est.A21.l[,"A"],
		rr.est.B21.s[,"B"],rr.est.B21.l[,"B"],
		rr.est.C21.s[,"C"],rr.est.C21.l[,"C"],
		col=c(rep(c("gray45","gray85"),3)),ylim=ylim,axes=FALSE,xaxs="i",yaxs="i",xpd=TRUE)
	abline(h=(1-emig),col="black",lwd=5,lty=2)
	axis(1,at=1:length(ax.labels),labels=ax.labels,cex.axis=1.2)
	mtext("Fidelity Rate",side=3,line=-1.5,font=2,cex=1.2)
	box()
mtext("Monitoring time, Simulated sample size of Gulf sturgeon tagged annually",side=1,outer=TRUE,line=3,cex=1.2)
mtext("Estimates from Simulation",side=2,outer=TRUE,line=3.3,cex=1.2)

### calculate probabilities of inaccuracy and estimation failure
rivs <- c("A","B","C")
surv.models <- 1:4
det.models <- 1:2
terms <- c("s","l")
combos <- apply(expand.grid(rivs,surv.models,det.models), 1, paste, collapse="")
combos.t <- apply(expand.grid(combos,terms),1,paste,collapse=".")
S.file.names <- c("S.est","S.lcl","S.ucl")
S.files <- apply(expand.grid(S.file.names,combos.t),1,paste,collapse=".")

PIA.fun <- function(smodel,est,lcl,ucl) #specify survival model, est, lcl, and ucl matrices (different for each river and model)
{
	col <- ncol(est) #number of survival parameters
	row <- nrow(est) #number of iterations
	dim <- row*col #total number of parameters estimated

	if(smodel %in% c(1,3)) #natal river and year-dept survival models
	{
		cover <- 0 #create object to save the number of true iterations
		for(i in 1:row)
		{
			for(j in 1:col)
			{
				if(is.na(lcl[i,j])){next} #not within 95% CI if LCL=NA
				if(is.na(ucl[i,j])){next} #not within 95% CI if UCL=NA
				cover <- ifelse(ucl[i,j]>=survA&lcl[i,j]<=survA,cover+1,cover) #if the survival rate is within 95% CI, count that iteration in the # that are "accurate"
			}
		}
		PIA <- (dim-cover)/dim #total number of survival parameters estimated - the number that were "accurate" = the number that were inaccurate
		#looking for the probability of inaccuracy = the number that were inaccurate / total number of survival parameters estimated
	}
	if(smodel==2) #state-dept survival model
	{
		#create separate objects because we are comparing the 95% CI to separate true values
		coverR <- 0 
		coverM <- 0
		colM <- which(colnames(est)=="M") #ID marine surv parameters
		colR <- which(colnames(est)!="M") #ID riverine surv parameters
		for(i in 1:row)
		{
			for(j in colR)
			{
				if(is.na(lcl[i,j])){next} #not within 95% CI if LCL=NA
				if(is.na(ucl[i,j])){next} #not within 95% CI if UCL=NA
				coverR <- ifelse(ucl[i,j]>=survR&lcl[i,j]<=survR,coverR+1,coverR) #if the survival rate is within 95% CI, count that iteration in the # that are "accurate"
			}

			if(is.na(lcl[i,colM])){next} #not within 95% CI if LCL=NA
			if(is.na(ucl[i,colM])){next} #not within 95% CI if UCL=NA
			coverM <- ifelse(ucl[i,colM]>=survM&lcl[i,colM]<=survM,coverM+1,coverM) #if the survival rate is within 95% CI, count that iteration in the # that are "accurate"
		}
		PIA <- (dim-coverR-coverM)/dim #dim-coverR-coverM is the number of parameters estimated that are inaccurate for the respective rate (marine or riverine)
	}
	if(smodel==4) #year*state survival
	{
		#create separate objects because we are comparing the 95% CI to separate true values
		coverR <- 0
		coverM <- 0
		colM <- which(substr(colnames(est),1,1)=="M") #ID marine surv parameters
		colR <- which(substr(colnames(est),1,1)!="M") #ID riverine surv parameters
		for(i in 1:row)
		{
			for(j in colR)
			{
				if(is.na(lcl[i,j])){next} #not within 95% CI if LCL=NA
				if(is.na(ucl[i,j])){next} #not within 95% CI if UCL=NA
				coverR <- ifelse(ucl[i,j]>=survR&lcl[i,j]<=survR,coverR+1,coverR)
			}
			for(k in colM)
			{
				if(is.na(lcl[i,k])){next} #not within 95% CI if LCL=NA
				if(is.na(ucl[i,k])){next} #not within 95% CI if UCL=NA
				coverM <- ifelse(ucl[i,k]>=survM&lcl[i,k]<=survM,coverM+1,coverM)
			}
			PIA <- (dim-coverR-coverM)/dim #dim-coverR-coverM is the number of parameters estimated that are inaccurate for the respective rate (marine or riverine)
		}
	}
	return(PIA)
}


PEF.fun <- function(smodel,est,lcl,ucl) #specify survival model, est, lcl, and ucl matrices (different for each river and model)
{
	col <- ncol(est) #number of survival parameters
	row <- nrow(est) #number of iterations
	dim <- row*col #total number of parameters estimated

	all <- 0 #object for estimation failures (EFs) when the CI cover the entire range (0-1)
	na <- 0 #object for EFs when at least 1 CI is NA
	eq <- 0	#object for EFs when the CIs are equal to each other
	for(i in 1:row)
	{
		for(j in 1:col)
		{
			na <- ifelse(is.na(lcl[i,j]),na+1,
					ifelse(is.na(ucl[i,j]),na+1,na)) #if either of the 95% CIs are NA, count as EF
			if(is.na(lcl[i,j])){next} #if either of the 95% CIs are NA, skip to next value because will encounter issues in the other calcs otherwise
			if(is.na(ucl[i,j])){next}
			all <- ifelse(lcl[i,j]<0.0001&ucl[i,j]>=0.995,all+1,all) #if CIs cover range approx 0-1, count as EF
			eq <- ifelse(lcl[i,j]==ucl[i,j],eq+1,eq) #if CIs are equivalent, count as EF
		}
	}
	PEF <- (all+na+eq)/dim #total number of EFs / total number of parameters to be estimated = prob of estimation failure

	return(PEF)
}

#create matrix to save P(inaccuracy) and P(estimation failure)
probs.mat <- matrix(0,nrow=2,ncol=length(rivs)*length(surv.models)*length(det.models)*length(terms))
	colnames(probs.mat) <- combos.t
	rownames(probs.mat) <- c("PIA","PEF")

for(pp in 1:ncol(probs.mat))
{
		print(pp/ncol(probs.mat)) #print the percentage of the runs done
		smodel <- as.numeric(substr(combos.t[pp],2,2)) #ID survival model based on substring within names of models
		est <- get(S.files[which(substr(S.files,7,11)==combos.t[pp])][1]) #specify est, lcl, and ucl matrices based on model in loop
		lcl <- get(S.files[which(substr(S.files,7,11)==combos.t[pp])][2])
		ucl <- get(S.files[which(substr(S.files,7,11)==combos.t[pp])][3])	

		probs.mat[1,pp] <- PIA.fun(smodel=smodel,est=est,lcl=lcl,ucl=ucl) #place PIA and PEF in matrix
		probs.mat[2,pp] <- PEF.fun(smodel=smodel,est=est,lcl=lcl,ucl=ucl)
}

write.table(probs.mat,"pmat.short.Rdata") #save matrix into directory