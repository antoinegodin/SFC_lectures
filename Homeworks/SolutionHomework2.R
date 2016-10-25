library(pdfetch)

##Begin by creating the average tax rate
# Selecting just GDP
names<-c("B1GQ")
GDP_raw <- pdfetch_EUROSTAT("nama_10_gdp", UNIT="CP_MNAC",NA_ITEM=names, from ="1994-12-31")
GDP <- as.data.frame(GDP_raw)

# Selecting gov revenue (taxes)
names<-c("OTR")
TAX_raw <- pdfetch_EUROSTAT("nasa_10_nf_tr", UNIT="CP_MNAC", NA_ITEM=names, SECTOR=c("S13"), DIRECT = c("RECV"), from="1994-12-31")
TAX <- as.data.frame(TAX_raw)

# Selecting gov revenue (taxes)
names<-c("B9")
NL_raw <- pdfetch_EUROSTAT("nasa_10_nf_tr", UNIT="CP_MNAC", NA_ITEM=names, SECTOR=c("S2","S13"), DIRECT = c("RECV"), from="1994-12-31")
NL <- as.data.frame(NL_raw)


##Next create the fiscal ratio using the Average Tax Rate you just created
# Selecting gov expenditure
names<-c("OTE")
GOVSPEND_raw <- pdfetch_EUROSTAT("nasa_10_nf_tr", UNIT="CP_MNAC", NA_ITEM=names, SECTOR=c("S13"), DIRECT = c("PAID"), from="1994-12-31")
GOVSPEND <- as.data.frame(GOVSPEND_raw)

##Now create the trade ratio using the average propensity to import
# Selecting total imports
names<-c("P7","P6")
TRADE_raw <- pdfetch_EUROSTAT("nama_10_gdp", UNIT="CP_MNAC", NA_ITEM=names, SECTOR=c("S1"), DIRECT = c("PAID"), from="1994-12-31")
TRADE <- as.data.frame(TRADE_raw)

coltradenames<-colnames(TRADE)
countrytrade<-as.data.frame(strsplit(coltradenames,"\\."),stringsAsFactors = F)[4,]
coltaxnames<-colnames(TAX)
countrytax<-as.data.frame(strsplit(coltaxnames,"\\."),stringsAsFactors = F)[6,]
colgdpnames<-colnames(GDP)
countrygdp<-as.data.frame(strsplit(colgdpnames,"\\."),stringsAsFactors = F)[4,]
colspendnames<-colnames(GOVSPEND)
countrygspend<-as.data.frame(strsplit(colspendnames,"\\."),stringsAsFactors = F)[6,]
colnlnames<-colnames(NL)
countrynl<-as.data.frame(strsplit(colnlnames,"\\."),stringsAsFactors = F)[6,]

allcountries<-unlist(unique(c(countrytrade,countrytax,countrygdp,countrygspend,countrynl)))
mycountries<-allcountries[c(2,3,8,9,14,15,19,22,24)]
mycountry_names<-c("Austria","Belgium","Germany","Denmark","Greece","Spain","France","Ireland","Italy")

png(filename=paste("AllCountries_NLRoW",".png",sep=""))
#Dividing the plot into 4 sub-plots (2 by 2)
par(mfrow = c(3,3))
#country<-allcountries[2]
for(i in 1:length(mycountries)){
	country<-mycountries[i]
	tax<-TAX[,grep(country,coltaxnames)]
	gdp<-GDP[,grep(country,colgdpnames)]
	govspend<-GOVSPEND[,grep(country,colspendnames)]
	trade<-TRADE[,grep(country,coltradenames)]
	nl<-NL[,grep(country,colnlnames)]
	if(ncol(cbind(tax,gdp,govspend,trade))==5){
		# creating new data frame for the Average Tax Rate
		average_tax <- tax/gdp
		# creating the fiscal ratio
		fiscal_ratio <- govspend/average_tax
		# creating the average propensity to import
		prop_import <- trade[,2]/gdp
		# creating the trade ratio itself
		trade_ratio <- trade[,1]/prop_import
		###Finally create the combined fiscal and trade ration
		comb_fiscal_trade <- (govspend+trade[,1])/(average_tax+prop_import)
		#This will create a jpg with the country code as name
		
		matplot(cbind(trade[,2]-trade[,1],nl[2]),type="l",main=mycountry_names[i],col=1:2,lty=1:2,ylab="",xlab="")
		#Plot 1 .
		#matplot(1995:2015,cbind(gdp,comb_fiscal_trade),type="l",main=mycountry_names[i],col=1:2,lty=1:2,ylab="",xlab="")
	}
}
dev.off()

png(filename=paste("AllCountries_NL_Gov",".png",sep=""))
#Dividing the plot into 4 sub-plots (2 by 2)
par(mfrow = c(3,3))
#country<-allcountries[2]
for(i in 1:length(mycountries)){
	country<-mycountries[i]
	tax<-TAX[,grep(country,coltaxnames)]
	gdp<-GDP[,grep(country,colgdpnames)]
	govspend<-GOVSPEND[,grep(country,colspendnames)]
	trade<-TRADE[,grep(country,coltradenames)]
	nl<-NL[,grep(country,colnlnames)]
	if(ncol(cbind(tax,gdp,govspend,trade))==5){
		# creating new data frame for the Average Tax Rate
		average_tax <- tax/gdp
		# creating the fiscal ratio
		fiscal_ratio <- govspend/average_tax
		# creating the average propensity to import
		prop_import <- trade[,2]/gdp
		# creating the trade ratio itself
		trade_ratio <- trade[,1]/prop_import
		###Finally create the combined fiscal and trade ration
		comb_fiscal_trade <- (govspend+trade[,1])/(average_tax+prop_import)
		#This will create a jpg with the country code as name
		
		matplot(cbind(tax-govspend,nl[1]),type="l",main=mycountry_names[i],col=1:2,lty=1:2,ylab="",xlab="")
		#Plot 1 .
		#	matplot(1995:2015,cbind(gdp,comb_fiscal_trade),type="l",main=mycountry_names[i],col=1:2,lty=1:2,ylab="",xlab="")
	}
}

dev.off()
#Plot 2 .
matplot(1995:2015,cbind(gdp,trade_ratio),type="l",main="Trade Ratio",col=1:2,lty=1:2,ylab="",xlab="")
#Plot 3 .
matplot(1995:2015,cbind(gdp,comb_fiscal_trade),type="l",main="Combined Ratio",col=1:2,lty=1:2,ylab="",xlab="")
#This finished the jpeg creation

