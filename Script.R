# 1. Indlæs pakker -----------------------------------------------------------
library(readxl) #Bruges til at indlæse data i excel-form
library(here) #Bruges til at skabe en relative path til data
library(tidyverse) 

# 2. Indlæs data -------------------------------------------------------------

#Sikre at alle brugere har samme path til data
data <- read_excel(here("Data", "RV data_anonymiseret 25feb2025.xlsx"))

View(data) #Viser data i R-studio for at tjekke at det blev loaded korrekt

# 3. Datarensning -----------------------------------------------------------
glimpse(data) #Viser dataens struktur 




