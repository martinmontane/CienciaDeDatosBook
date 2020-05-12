## ----echo=FALSE------------------------------------------------------------------------------------------------
knitr::include_graphics(path="Figuras/Capitulo 5/WhoIsWho.png")


## --------------------------------------------------------------------------------------------------------------
titanic <- read.csv(file = "https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/titanic.csv",
                    stringsAsFactors = FALSE,
                    sep = ';')


## --------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(C50)


## --------------------------------------------------------------------------------------------------------------
glimpse(titanic)


## --------------------------------------------------------------------------------------------------------------
titanic <- titanic %>%
           mutate(survived = factor(survived),
                  sex = factor(sex))


## --------------------------------------------------------------------------------------------------------------
primerArbol <- C5.0(formula= survived ~.,
                    data = titanic)


## --------------------------------------------------------------------------------------------------------------
plot(primerArbol)


## --------------------------------------------------------------------------------------------------------------
proporcionSobrevivientes <- table(titanic$survived)[2]/nrow(titanic)
proporcionSobrevivientes # Aproximadamente un 41% de los pasajeros sobrevivieron
# Formula de Entropía
-0.59*log2(0.59)-(0.41)*log2(0.41)


## --------------------------------------------------------------------------------------------------------------
-0.5*log2(0.5)-(0.5)*log2(0.5) # Máxima entropía


## --------------------------------------------------------------------------------------------------------------
-0.000001*log2(0.000001)-(1)*log2(1) # Casi cero


## --------------------------------------------------------------------------------------------------------------
table(titanic$survived,titanic$sex)


## --------------------------------------------------------------------------------------------------------------
# Entropía mujeres
-(96/(292+96))*log2(96/(292+96))-(292/(292+96))*log2(292/(292+96))
entropiaMujeres <- -(96/(292+96))*log2(96/(292+96))-(292/(292+96))*log2(292/(292+96))


## --------------------------------------------------------------------------------------------------------------
# Entropía hombres
-(522/(522+135))*log2(522/(522+135))-(135/(522+135))*log2(135/(522+135))
entropiaHombres <- -(522/(522+135))*log2(522/(522+135))-(135/(522+135))*log2(135/(522+135))


## --------------------------------------------------------------------------------------------------------------
entropiaGenero <- entropiaHombres*(657/1045)+entropiaMujeres*(388/1045)
entropiaGenero


## --------------------------------------------------------------------------------------------------------------
informationGainGenero <- 0.9765-entropiaGenero
informationGainGenero


## --------------------------------------------------------------------------------------------------------------
summary(primerArbol)


## --------------------------------------------------------------------------------------------------------------
primerArbol <- C5.0(formula= survived ~.,
                    data = titanic,
                    rules=TRUE)
summary(primerArbol)


## --------------------------------------------------------------------------------------------------------------
load(file=url("https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/independientes.RData"))
str(independientes)


## --------------------------------------------------------------------------------------------------------------
primerArbol <- C5.0(formula = REGISTRADO ~.,
                    data = independientes)
summary(primerArbol)


## --------------------------------------------------------------------------------------------------------------
table(independientes$REGISTRADO)/nrow(independientes)


## --------------------------------------------------------------------------------------------------------------
independientes <- independientes %>%
  mutate(PREDICCION = predict(primerArbol,
                              newdata = independientes %>% select(-REGISTRADO)))
table(independientes$REGISTRADO, independientes$PREDICCION)


## --------------------------------------------------------------------------------------------------------------
sum(diag(table(independientes$REGISTRADO, independientes$PREDICCION))) /
  nrow(independientes) * 100


## --------------------------------------------------------------------------------------------------------------
# Eliminamos la variable que tiene la predección
independientes <- independientes %>% select(-PREDICCION)
set.seed(2)
# Creamos la "Grid Search" de dos parámetros
cfOpciones <- seq(0.8,1,0.01)
minCasesOpciones <- seq(0,50,1)
# Generamos los índices (números de filas) que van a ser de testing
indexTest <- sample.int(n = nrow(independientes),size = 0.3*nrow(independientes))
independientesTest <- independientes[indexTest, ]
independientesTraining <- independientes[-indexTest, ]
modelPerformance <- list()
for(cf in cfOpciones){
  for(minCases in minCasesOpciones) {
    # Para cambiar los parámetros presten atención a que debemos usar la función C5.0Control
    model <- C5.0(REGISTRADO ~.,
                  data = independientesTraining,
                  control= C5.0Control(CF = cf,
                              minCases = minCases))
    
  prediccionesTrain <- predict(model, independientesTraining)
  trainAcc <- sum(prediccionesTrain==independientesTraining$REGISTRADO)/nrow(independientesTraining)
  prediccionesTest <- predict(model, newdata = independientesTest)
  testAcc <- sum(prediccionesTest==independientesTest$REGISTRADO)/nrow(independientesTest)
  salida <- data.frame(cf, minCases,trainAcc,testAcc)
  modelPerformance <- c(modelPerformance, list(salida))
  }
}
modelPerformance <- plyr::rbind.fill(modelPerformance)


## --------------------------------------------------------------------------------------------------------------
modelPerformance <- modelPerformance %>%
                    group_by(minCases) %>%
                    summarise(Training = mean(trainAcc),
                              Testing = mean(testAcc)) %>%
                    gather(key = "dataset",value="acc",-minCases)
# Esta librería nos da la opción de agregar nuevos "temas" de ggplot
# que no vienen con la librería
library(ggthemes)
ggplot(modelPerformance) + 
  geom_line(aes(x = minCases,y = acc, color = dataset), size = 1.5) +
  theme_fivethirtyeight() + scale_color_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_reverse() +
  labs(title = "La forma del overfitting",
       subtitle = "Accuracy según el valor del parámetro minCases",
       caption = "Elaboración propia con base en datos de ENAPROSS 2015") +
  theme(legend.title = element_blank())


## --------------------------------------------------------------------------------------------------------------
avisosInmuebles <-read.table(file = url("https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/datosProperati.csv"),
                            sep=';',header = TRUE,stringsAsFactors = FALSE)


## --------------------------------------------------------------------------------------------------------------
glimpse(avisosInmuebles)


## --------------------------------------------------------------------------------------------------------------
avisosInmuebles <- avisosInmuebles %>%
                   filter(property_type %in% c("Casa","Departamento","PH"))


## --------------------------------------------------------------------------------------------------------------
sum(is.na(avisosInmuebles$rooms))
sum(is.na(avisosInmuebles$bathrooms))
sum(is.na(avisosInmuebles$surface_covered))


## --------------------------------------------------------------------------------------------------------------
avisosInmuebles <- avisosInmuebles %>%
  mutate(ambientes=str_extract(pattern = "(?i)\\d.amb", string= title)) %>%
  mutate(ambientes=ifelse(is.na(ambientes),
                          str_extract(pattern = "(?i)\\d.amb", string=description), ambientes)) %>%
  mutate(ambientes=as.numeric(str_extract(pattern='\\d',ambientes))) %>%
  mutate(ambientes=ifelse(ambientes == 0,NA,ambientes))


## --------------------------------------------------------------------------------------------------------------
table(avisosInmuebles$ambientes==avisosInmuebles$rooms)


## --------------------------------------------------------------------------------------------------------------
avisosInmuebles <- avisosInmuebles %>%
                   mutate(rooms = ifelse(is.na(rooms), ambientes, rooms))
sum(is.na(avisosInmuebles$rooms))


## --------------------------------------------------------------------------------------------------------------
avisosInmuebles <- avisosInmuebles %>%
  select(-created_on,-currency,-title,-description,-operation_type,-ambientes) %>%
  filter(complete.cases(.))


## --------------------------------------------------------------------------------------------------------------
require(rpart)
require(rpart.plot)
avisosInmuebles <- avisosInmuebles %>%
                   mutate(USDm2=price/surface_total)
arbolRegresion <- rpart(formula = USDm2 ~ rooms + BARRIO + bathrooms + property_type,
                        data = avisosInmuebles,control = rpart.control(cp = 0.01))

rpart.plot(arbolRegresion)


## --------------------------------------------------------------------------------------------------------------
round(mean(avisosInmuebles$USDm2),0)


## --------------------------------------------------------------------------------------------------------------
View(rpart.rules(arbolRegresion))


## --------------------------------------------------------------------------------------------------------------
round(mean(avisosInmuebles$USDm2[avisosInmuebles$BARRIO=="PUERTO MADERO"]),0)


## --------------------------------------------------------------------------------------------------------------
prediccionInicial <- mean(avisosInmuebles$USDm2)
rmseInicial <- sqrt(mean((prediccionInicial-avisosInmuebles$USDm2)^2))
rmseInicial


## --------------------------------------------------------------------------------------------------------------
prediccionBarrios <- ifelse(avisosInmuebles$BARRIO == "PUERTO MADERO", 6137,
                            ifelse(avisosInmuebles$BARRIO %in% c("BELGRANO, COUGHLAN","COLEGIALES","NUÑEZ","PALERMO","RECOLETA","RETIRO"),3363,
                                   2332))
rmseBarrios <- sqrt(mean((prediccionBarrios-avisosInmuebles$USDm2)^2))
rmseBarrios
rmseBarrios / rmseInicial - 1

