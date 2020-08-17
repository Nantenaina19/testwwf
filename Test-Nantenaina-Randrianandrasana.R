## Install packages
install.packages("readr")
install.packages("dplyr")
require(readr)
require(dplyr)

## Load csv
#baseline <- read.csv(file="D:\\Exported.csv")
baseline=read.table(file="D:\\Exported.txt", sep=";",dec=" ", header=T)
baseline

## Anonymisation column « NOM_ENQUETE_CHR »
baseline$NOM_ENQUETE_CHR
require(digest)

anonymise <- function(data, column, algo = "sha256")
{
  anonymise <- subset(data, select = column)
  unname(apply(anonymise, 1, digest, algo = algo))
}
baseline$NOM_ENQUETE_CHR <- anonymise(baseline, "NOM_ENQUETE_CHR");baseline$NOM_ENQUETE_CHR

## Change date format to format POSIXct
baseline$DATE
dateFormat <- function(data, column){
  row = nrow(data)
  datemy = ls()
  column <- subset(data, select = column)
    for(i in 1:row){
		# date format
        	date <- as.POSIXct(toString(column[i,]),format='%d%m%Y')
		
		# get only year and month
		datemy[i] <- format(date, "%m-%Y")
	}
  return (datemy)
}

baseline$DATE<- dateFormat(baseline, "DATE");baseline$DATE

## Operation
# Taux d’utilisation par type de source d’éclairage par ménage
# J'ai pas bien compris la question

taux.Using = function(data, column, price){
	taux.using = list()
	eclairage = list()
	cout = list()
	taux = list()
	index = 1
	column <- subset(data, select = column)
	price <- subset(data, select = price)
	row = nrow(data)
	'%ni%' <- Negate('%in%')
	 for(i in 1:row){
		if(column[i,] %ni% eclairage){
			eclairage[index] = column[i,]
			cout[index] = price[i,]
			taux[index] = 1
			index <- indx + 1
		}
		else{
			indx = match(column[i,],eclairage)
			cout[indx] <- as.numeric(cout[indx]) + as.numeric(price[i,])
			taux[indx] <- as.numeric(taux[indx]) + 1 
		}
	}
	taux = (as.numeric(taux[])/row) * 100
	cout =  (as.numeric(cout[])/row)
	taux.using = cbind(eclairage,taux ,cout)
	return (taux.using)
}

taux.Using(baseline, "Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_1", "Q1_COUT_MENSUEL_CONSOMMATION_NUM_1")

# Moyenne et l’écart-type de toutes les variables de type numérique
  moyenne.EcartType = function(data){
  predict = c()
  cln = ncol(data)
  row = nrow(data)
  mean = ls()
  sd = ls()
  indx = 0
  for(i in 1:cln){
	dataColn <- subset(data, select = colnames(data)[i])
	if(is.numeric(dataColn[1,1])){
		mean[indx] <- mean(dataColn[,], na.rm=TRUE)
		sd[indx] <- sd(dataColn[,], na.rm=TRUE)
		indx <- indx + 1
	}
  }
  return (list(Moyenne=mean, EcartType=sd))
}

moyenne.EcartType(baseline)
