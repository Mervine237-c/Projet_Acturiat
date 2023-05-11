
############################### PACKAGES ##############################

install.packages("forecast")

install.packages("dplyr")

install.packages("ggplot2")

install.packages("openxlsx")

install.packages("readr")

library(readxl)

library(dplyr)

library(ggplot2)

library(forecast)

library(openxlsx)

# ****************** PROJET ACTUARIAT VIE ***************************************

########################### TAUX DE SURVIE ############################

####### DONNEES

Male_life_table <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/mltper_1x1.xlsx", range = "A3:J9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

Female_life_table <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/fltper_1x1.xlsx", range = "A3:J9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

par(mfcol = c(1, 2))

###### Taux de survie Femme

Female_life_table$Age[is.na(Female_life_table$Age)]<-110

Female_life_table<- Female_life_table%>%mutate(taux_survie = 1 - Female_life_table$qx)

Ages_female<-unique(Female_life_table$Age)

tsa_female_1933<-Female_life_table%>%select(Year, taux_survie)%>%subset(Year == 1933)

plot(Ages_female, tsa_female_1933$taux_survie, type = "l", xlab = "Age", ylab = "Taux de survie", main = "Taux de survie Femme", xlim = c(0,150), ylim = c(0,1), col = grey(1))


###trace de la courbe
for (a in 1934:2019){
  
  tsa_female<- Female_life_table%>%select(Age, Year, taux_survie)%>%subset(Year == a)
  
  lines(Ages_female, tsa_female$taux_survie, col = gray((a-1934)/100))
  
  polygon(c(130,130,143,143),(c(a,a-1,a-1,a)-1933)/100, border=NA, col = gray((a-1934)/100))
}

for (a in seq(from = 1933,to= 2019, by = 43)){
  
  text(150, (a-1933)/100, a)
}

###### Taux de survie Homme

Male_life_table$Age[is.na(Male_life_table$Age)]<-110

Male_life_table<-Male_life_table%>%mutate(taux_survie = 1 - Male_life_table$qx)

Ages_male<-unique(Male_life_table$Age)

tsa_male_1933<-Male_life_table%>%select(Year,taux_survie)%>%subset(Year == 1933)


###trace de la courbe
plot(Ages_male, tsa_male_1933$taux_survie, type = "l", xlab = "Age", ylab = "Taux de survie", main = "Taux de survie hommes", xlim = c(0,150), ylim = c(0,1), col = gray(1))

for(a in 1934:2019){
  
  tsa_male<- Male_life_table%>%select(Age, Year, taux_survie)%>%subset(Year == a)
  
  lines(Ages_male, tsa_male$taux_survie, col = gray((a-1934)/100))
  
  polygon(c(130,130,143,143),(c(a,a-1,a-1,a)-1933)/100, border = NA, col = gray((a-1934)/100))
}

for(a in seq(from = 1933, to=2019, by = 43)){
  
  text(150, (a-1933)/100, a)
}

################### PROJECTION CENTRALE SUR 20 ANS ####################

####### Donnees

Deaths_number <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/Deaths_1x1.xlsx", range = "A3:E9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

Population <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/Population.xlsx", sheet = "Population", range = "A3:E9882", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

####### Formation de la base de taux de mortalité annuel

Deaths_number<-Deaths_number%>%group_by(Year)%>%summarise(Total_Death=sum(Total))

Population<-Population%>%group_by(Year)%>%summarise(Total_Population=sum(Total))

Population<-na.omit(Population)

Taux_brut<-Population%>%inner_join(Deaths_number, by=c(Year= "Year"))

Taux_brut<-Taux_brut%>%mutate(Taux_brut = Total_Death/Total_Population)

write.xlsx(Taux_brut, "C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/OUTPUT_PROJET/Taux_brut.xlsx")

####### Projection

tsdata<-ts(Taux_brut$Taux_brut, frequency = 1, start = c(1933, 1), end =2019)

plot(tsdata)

autoarima_ts<-auto.arima(tsdata)

forecast_ts <- forecast(autoarima_ts, h=20)

forecast_ts

plot(forecast_ts)

plot(forecast_ts$residuals)

qqnorm(forecast_ts$residuals)

acf(forecast_ts$residuals)

pacf(forecast_ts$residuals)

accuracy(autoarima_ts)

################## ESPERANCE DE VIE DU PORTEFEUILLE ###################

####### Donnees

Births <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/Births.xlsx", range = "A3:D90")

Deaths_number <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/Deaths_1x1.xlsx", range = "A3:E9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

####### Nombres de personnes du portefeuille

nombre_femmes<- as.matrix(Births%>%select(Female, Year)%>%filter(Year == "1959"))

nombre_femmes_portef<-round((nombre_femmes[1])*0.3)

nombre_hommes<- as.matrix(Births%>%select(Male, Year)%>%filter(Year == "1955"))

nombre_hommes_portef<-(nombre_hommes[1])*0.7

nombre_total_portef<-nombre_femmes_portef + nombre_hommes_portef

####### Calcul de l'esperance de vie des femmes du portefeuille

esperance_vie_femme<-data.frame(Year = c(1959:2019))

esperance_vie_femme<-esperance_vie_femme%>%mutate(Age = Year - 1959)

esperance_vie_femme<-esperance_vie_femme%>%left_join(Deaths_number, by=(c(Year = "Year" , Age = "Age")))

esperance_vie_femme<-esperance_vie_femme%>%select(!c(Male, Total))

esperance_vie_femme<-esperance_vie_femme%>%mutate(nombre_deces=esperance_vie_femme$Female * 0.3)

esperance_vie_femme<-esperance_vie_femme%>%mutate(lx = "0")

esperance_vie_femme$lx<-as.double(esperance_vie_femme$lx)

esperance_vie_femme$lx[1]<-nombre_femmes_portef - esperance_vie_femme$nombre_deces[1]

for (a in seq(from = 2,to= 61, by = 1)) {
  esperance_vie_femme$lx[a]<- esperance_vie_femme$lx[a-1] - esperance_vie_femme$nombre_deces[a]
}

esperance_vie_femme<-esperance_vie_femme%>%mutate(ex = "0")

esperance_vie_femme$ex<-as.double(esperance_vie_femme$ex)

for (i in seq(from = 1, to = 61, by = 1)) {
  
  a = i+1
  esperance_vie_femme$ex[i]<-(sum(esperance_vie_femme$lx[a:61])/esperance_vie_femme$lx[i])+0.5
}

esperance_vie_femme$ex[is.na(esperance_vie_femme$ex)]<-0

write.xlsx(esperance_vie_femme, "C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/OUTPUT_PROJET/Esperance_vie_femme.xlsx")

####### Calcul de l'esperance de vie des hommes du portefeuille

esperance_vie_homme<-data.frame(Year = c(1955:2019))

esperance_vie_homme<-esperance_vie_homme%>%mutate(Age = Year - 1955)

esperance_vie_homme<-esperance_vie_homme%>%left_join(Deaths_number, by=c(Year ="Year", Age = "Age"))

esperance_vie_homme<-esperance_vie_homme%>%select(!c(Female, Total))

esperance_vie_homme<-esperance_vie_homme%>%mutate(nombre_deces = esperance_vie_homme$Male * 0.7, lx = "0", ex = "0")

esperance_vie_homme$lx<-as.double(esperance_vie_homme$lx)

esperance_vie_homme$ex<-as.double(esperance_vie_homme$ex)

esperance_vie_homme$lx[1]<-nombre_hommes_portef - esperance_vie_homme$nombre_deces[1]

for (a in seq(from = 2,to= 65, by = 1)) {
  esperance_vie_homme$lx[a]<- esperance_vie_homme$lx[a-1] - esperance_vie_homme$nombre_deces[a]
}

for (i in seq(from = 1, to = 65, by = 1)) {
  
  a = i+1
  esperance_vie_homme$ex[i]<-(sum(esperance_vie_homme$lx[a:65])/esperance_vie_homme$lx[i])+0.5
}

esperance_vie_homme$ex[is.na(esperance_vie_homme$ex)]<-0

write.xlsx(esperance_vie_homme, "C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/OUTPUT_PROJET/Esperance_vie_homme.xlsx")

################## VALEUR ACTUELLE PROBABLE ET PROVISION MATHEMATIQUE ##################

####### Donnees

Life_table <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/bltper_1x1.xlsx", range = "A3:J9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

Male_life_table <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/mltper_1x1.xlsx", range = "A3:J9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

Female_life_table <- read_excel("C:/Users/PROBOOK/Documents/ESPRIT/ACTUARIAT/ACTUARIAT VIE/USA/PROJET_ACTUARIAT_DATA/fltper_1x1.xlsx", range = "A3:J9660", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

Life_table_2010<-Life_table%>%filter(Year == "2010")

Life_table_2010$Age[is.na(Life_table_2010$Age)]<-110

Life_table_2010_female<-Female_life_table%>%filter(Year == "2010")

Life_table_2010_female$Age[is.na(Life_table_2010_female$Age)]<-110

Life_table_2010_male<-Male_life_table%>%filter(Year == "2010")

Life_table_2010_male$Age[is.na(Life_table_2010_male$Age)]<-110

####### Implementation de la fonction VAP

VAP<-function(age, n){
  i<-0.5/100
  lx<-Life_table_2010$lx[age+1]
  v<-1/(1+i)
  Dx<-lx * v^(age)
  Mx<-0
  Mxn<-0
  sommeMx<-0
  sommeMxn<-0
  
  for(a in seq(age, 64, by = 1)){
    dx<-Life_table_2010$dx[a+1]
    Cx<-dx * v^(a + 0.5)
    Mx<-Mx + Cx
    sommeMx<-sommeMx+Mx
  }
  
  for (a in seq(age, 64+n, by = 1)) {
    dxn<-Life_table_2010$dx[a+1]
    Cxn<-dxn * v^(a+ 0.5)
    Mxn<-Mxn + Cxn
    sommeMxn<-sommeMxn+Mxn
  }
  
  (sommeMx - sommeMxn)/Dx
}

####### Exemple de Calcul la VAP sans differenciation

VAP(15,20)

####### Calcul de la VAP avec differenciation

###female

VAP_female<-function(age, n){
  i<-0.5/100
  lx<-Life_table_2010_female$lx[age+1]
  v<-1/(1+i)
  Dx<-lx * v^(age)
  Mx<-0
  Mxn<-0
  sommeMx<-0
  sommeMxn<-0
  
  for(a in seq(age, 60, by = 1)){
    dx<-Life_table_2010_female$dx[a+1]
    Cx<-dx * v^(a + 0.5)
    Mx<-Mx + Cx
    sommeMx<-sommeMx+Mx
  }
  
  for (a in seq(age, 60+n, by = 1)) {
    dxn<-Life_table_2010_female$dx[a+1]
    Cxn<-dxn * v^(a+ 0.5)
    Mxn<-Mxn + Cxn
    sommeMxn<-sommeMxn+Mxn
  }
  
  (sommeMx - sommeMxn)/Dx
}

VAP_female(51,20)

###male

VAP_male<-function(age, n){
  i<-0.5/100
  lx<-Life_table_2010_male$lx[age+1]
  v<-1/(1+i)
  Dx<-lx * v^(age)
  Mx<-0
  Mxn<-0
  sommeMx<-0
  sommeMxn<-0
  
  for(a in seq(age, 64, by = 1)){
    dx<-Life_table_2010_male$dx[a+1]
    Cx<-dx * v^(a + 0.5)
    Mx<-Mx + Cx
    sommeMx<-sommeMx+Mx
  }
  
  for (a in seq(age, 64+n, by = 1)) {
    dxn<-Life_table_2010_male$dx[a+1]
    Cxn<-dxn * v^(a+ 0.5)
    Mxn<-Mxn + Cxn
    sommeMxn<-sommeMxn+Mxn
  }
  
  (sommeMx - sommeMxn)/Dx
}

VAP_male(55,20)

####### PROVISION MATHEMATIQUE

VAP_assureur<-function(capital,age, n){
  i<-0.5/100
  lx<-Life_table_2010$lx[age+1]
  v<-1/(1+i)
  Dx<-lx * v^(age)
  Mx<-0
  Mxn<-0
  sommeMx<-0
  sommeMxn<-0
  
  for(a in seq(age, 64, by = 1)){
    dx<-Life_table_2010$dx[a+1]
    Cx<-dx * v^(a + 0.5)
    Mx<-Mx + Cx
    sommeMx<-sommeMx+Mx
  }
  
  for (a in seq(age, 64+n, by = 1)) {
    dxn<-Life_table_2010$dx[a+1]
    Cxn<-dxn * v^(a+ 0.5)
    Mxn<-Mxn + Cxn
    sommeMxn<-sommeMxn+Mxn
  }
  
  ((sommeMx - sommeMxn)/Dx)*capital
}

PM <-VAP_assureur(100000, 55, 20) - VAP(55, 20)
