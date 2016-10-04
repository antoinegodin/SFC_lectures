library(pdfetch)

# Selecting the flows for nasa_nf_tr
names<-c("B6G","B6N","P3")
NF_TR_raw = pdfetch_EUROSTAT("nasa_10_nf_tr", UNIT="CP_MNAC",NA_ITEM=names,SECTOR=c("S14_S15"),DIRECT = "PAID")
NF_TR<-as.data.frame(NF_TR_raw)

#selection of the flows for nama_10_nfa_bs (dwellings - net)
names<-c("N11N")
NFA_BS_raw = pdfetch_EUROSTAT("nama_10_nfa_bs", UNIT="CP_MNAC",ASSET10=names,SECTOR=c("S14_S15"))
NFA_BS<-as.data.frame(NFA_BS_raw)

#selection of the flows for nasa_10_f_bs (BF90 - net financial assets)
names<-c("BF90")
F_BS_raw = pdfetch_EUROSTAT("nasa_10_f_bs", UNIT="MIO_NAC",CO_NCO="CO",NA_ITEM=names,SECTOR=c("S14_S15"))
F_BS<-as.data.frame(F_BS_raw)

# Automatic procedure to remove the non-interesting bit of the colnames
HHdata<-c()
newcoln<-c()
countries<-c()

# For the the NF_TR data.frame
coln<-colnames(NF_TR)
for(i in 1:length(coln)){
	name<-coln[i]
	tname<-strsplit(name,"\\.")[[1]]
	newname<-paste(tname[c(4,6)],collapse=".")
	#Adding the current country code to the list of country codes
	countries<-c(countries,tname[6])
	# If the column contains only NA, remove it from the dataset
	if(sum(is.na(NF_TR[,i]))!=length(NF_TR[,i])){
		newcoln<-c(newcoln,newname) 
		#creating the HHdata dataset by column binding the exisiting HHdata with the new column
		HHdata<-cbind(HHdata,NF_TR[,i])
	}
}

# For the the NFA_BS data.frame
coln<-colnames(NFA_BS)
for(i in 1:length(coln)){
	name<-coln[i]
	tname<-strsplit(name,"\\.")[[1]]
	newname<-paste(tname[c(4,5)],collapse=".")
	#Adding the current country code to the list of country codes
	countries<-c(countries,tname[5])
	# If the column contains only NA, remove it from the dataset
	if(sum(is.na(NFA_BS[,i]))!=length(NFA_BS[,i])){
		newcoln<-c(newcoln,newname) 
		#creating the HHdata dataset by column binding the exisiting HHdata with the new column
		HHdata<-cbind(HHdata,NFA_BS[18:38,i])
	}
}

# For the the F_BS data.frame
coln<-colnames(F_BS)
for(i in 1:length(coln)){
	name<-coln[i]
	tname<-strsplit(name,"\\.")[[1]]
	newname<-paste(tname[c(6,7)],collapse=".")
	#Adding the current country code to the list of country codes
	countries<-c(countries,tname[7])
	# If the column contains only NA, remove it from the dataset
	if(sum(is.na(F_BS[,i]))!=length(F_BS[,i])){
		newcoln<-c(newcoln,newname) 
		#creating the HHdata dataset by column binding the exisiting HHdata with the new column
		HHdata<-cbind(HHdata,F_BS[,i])
	}
}

HHdata<-as.data.frame(HHdata)
colnames(HHdata)<-newcoln
rownames(HHdata)<-rownames(F_BS)
# getting the unique country codes
countries<-unique(countries)

#For each country
for(country in countries){
	#Get all the column in which the country code exists
	indices<-grep(country,newcoln)
	#Plotting the data only if I have the 5 timeseries I need
	if(length(indices)==5){
		#This will create a jpg with the country code as name
		jpeg(filename=paste(country,".jpg",sep=""))
		#Dividing the plot into 4 sub-plots (2 by 2)
		par(mfrow = c(2,2))
		#Plot 1 gross disposable income over total wealth
		timeseries<-HHdata[,paste("B6G",country,sep=".")]/(HHdata[,paste("N11N",country,sep=".")]+HHdata[,paste("BF90",country,sep=".")])
		plot(1995:2015,timeseries,type="l",main="B6G/Wealth")
		#Plot 2 net disposable income over total wealth
		timeseries<-HHdata[,paste("B6N",country,sep=".")]/(HHdata[,paste("N11N",country,sep=".")]+HHdata[,paste("BF90",country,sep=".")])
		plot(1995:2015,timeseries,type="l",main="B6N/Wealth")
		#Plot 3 total consumption over total wealth
		timeseries<-HHdata[,paste("P3",country,sep=".")]/(HHdata[,paste("N11N",country,sep=".")]+HHdata[,paste("BF90",country,sep=".")])
		plot(1995:2015,timeseries,type="l",main="P3/Wealth")
		#Plot 4 total consumption over net disposable income
		timeseries<-HHdata[,paste("P3",country,sep=".")]/(HHdata[,paste("B6N",country,sep=".")])
		plot(1995:2015,timeseries,type="l",main="P3/B6N")
		#This finished the jpeg creation
		dev.off()
	}
}