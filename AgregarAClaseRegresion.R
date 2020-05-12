library(wbstats)
library(data.table)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(ggthemes)
datosVuelos <- wb(indicator = "IS.AIR.PSGR",startdate = 2010,enddate = 2020)
datosVuelos <- as.data.table(datosVuelos)[date==max(date),by=list(iso3c)]


infoCovid <- fread("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
popData <- fread("https://covid.ourworldindata.org/data/ecdc/locations.csv")
infoCovid <- infoCovid[popData,on="location",pop:=population]

infoCovid <- infoCovid[,iso3c:=countrycode(sourcevar = location,origin = "country.name",destination = "iso3c")]
infoCovid <- infoCovid[,continente:=countrycode(sourcevar = location,origin = "country.name",destination = "continent")]
infoCovid <- infoCovid[!location %in% c("International","Kosovo","Timor","World")]
infoCovid <- infoCovid[date==("2020-05-11"),by=list(iso3c)]
infoCovid <- infoCovid[datosVuelos,on="iso3c",airTransport:=value]
infoCovid <- infoCovid[,deathsPC:=total_deaths/pop]
infoCovid <- infoCovid[,airtransportPC:=airTransport/pop]
ggplot(data = infoCovid[total_deaths>100 & pop>1000000 & !iso3c %in% "IRL"],
       mapping=aes(x=airtransportPC, y=deathsPC)) +
  geom_point() + geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(mapping = aes(label=iso3c)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size=14)) +
  labs(x="Pasajeros en avión per cápita",
       y="Muertes per cápita COVID/19 acumuladas al 11 de mayo")
  
