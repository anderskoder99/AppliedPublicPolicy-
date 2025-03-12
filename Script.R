# 1. Indlæs pakker -----------------------------------------------------------
install.packages("readxl") #Installerer pakken readxl
install.packages("here") #Installerer pakken here
install.packages("tidyverse") #Installerer pakken tidyverse

library(readxl) #Bruges til at indlæse data i excel-form
library(here) #Bruges til at skabe en relative path til data
library(tidyverse) 

# 2. Indlæs data -------------------------------------------------------------

#Sikre at alle brugere har samme path til data
data <- read_excel(here("Data", "RV data_anonymiseret 25feb2025.xlsx"))

View(data) #Viser data i R-studio for at tjekke at det blev loaded korrekt

# 3. Datarensning -----------------------------------------------------------
glimpse(data) #Viser dataens struktur 

unique(data$Udrulning_DW)
sum(data$Udrulning_DW == "Udenfor distrikt") #580 rækker har denne værdi
data <- data[data$Udrulning_DW != "Udenfor distrikt", ] #Fjerner rækker med denne værdi



#Datatype
#Tjek udfaldsvariables udfaldsmuligheder
#summary  
#Fjern missing variables - 
#Evt. ændre "NULL" til "0" 




