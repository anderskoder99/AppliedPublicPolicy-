
# 0.Kommandoer til at pushe kode til GitHub --------------------------------------
#Push-til GitHUb
#git status
#git add.
#git commit -m "beskrivelse"
#git push origin main


# 1. Nødvendige pakker -----------------------------------------------------------
install.packages("readxl") #Installerer pakken readxl
install.packages("here") #Installerer pakken here
install.packages("tidyverse") #Installerer pakken tidyverse
install.packages("panelView")

# 2. Indlæs data -------------------------------------------------------------

#Sikre at alle brugere har samme path til data
data <- read_excel(here("Data", "RV data_anonymiseret 25feb2025.xlsx"))

# 3. Indlæs pakker -----------------------------------------------------------
library(readxl) #Bruges til at indlæse data i excel-form
library(here) #Bruges til at skabe en relative path til data
library(tidyverse) 
library(panelView)

View(data) #Viser data i R-studio for at tjekke at det blev loaded korrekt

# 3. Datarensning -----------------------------------------------------------

# 3.1 Tjek af missing data ------------------------------------------------

unique(data$Udrulning_DW) #Viser unikke værdier i kolonnen "Udrulning_DW"
sum(data$Udrulning_DW == "Udenfor distrikt") #580 rækker har denne værdi
data <- data[data$Udrulning_DW != "Udenfor distrikt", ] #Fjerner rækker med denne værdi
unique(data$Ydelse) #OBS på denne: 260 unikke værdier - skal vi gruppere?

#Ændrer alle NULL-værdier til NA, fjerner omkring hlavdelen af rækker
data$Fraværsprocent[data$Fraværsprocent == "NULL"] <- NA
data$AntalFraværsdage[data$AntalFraværsdage == "NULL"] <- NA
data$AntalSkoledage[data$AntalSkoledage == "NULL"] <- NA
data$AntalSkoledage[data$YdelseDatoStart == "NULL"] <- NA
data$AntalSkoledage[data$YdelseDatoStop == "NULL"] <- NA

# 3.2 Korrekt variabeltype ------------------------------------------------
glimpse(data$Halvår_DW) #Viser samtlige variables type. 
data <- data %>%
  mutate(Fraværsprocent = as.numeric(Fraværsprocent))


#Datatype
#summary  
#Fjern missing variables - 
#Evt. ændre "NULL" til "0" 


# 4.1 Plot udvikling på udfaldsvariable: fraværsprocent, underretnigner (anntal + farvekoder) og ydelsesgruppering + ydelseslængde?  -----------------------------------
# Lave Dato variable til dato format
str(data$DatoFraJoin_DW)
data$DatoFraJoin_DW <- as.Date(data$DatoFraJoin_DW)
str(data$DatoTilJoin_DW)
data$DatoTilJoin_DW <- as.Date(data$DatoTilJoin_DW)

data <- data %>%
  mutate(
    YdelseDatoStart = na_if(YdelseDatoStart, "NULL"),  # Konverter "NULL" til NA
    YdelseDatoStart = as.Date(YdelseDatoStart, format = "%m/%d/%Y")  # Konverter til dato
  )

data <- data %>%
  mutate(
    YdelseDatoStop = na_if(YdelseDatoStop, "NULL"),  # Konverter "NULL" til NA
    YdelseDatoStop = as.Date(YdelseDatoStop, format = "%m/%d/%Y")  # Konverter til dato
  )


# Lav variabel for længde af ydelse 
data$ydelseslængde_dage <- as.Date(data$YdelseDatoStop) - as.Date(data$YdelseDatoStart)


str(data$Fraværsprocent)

# Fravær
data |> 
  group_by(Udrulning_DW, DatoFraJoin_DW) |>  
  summarise(gns_fravær = mean(Fraværsprocent, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = DatoFraJoin_DW, y = gns_fravær, group = Udrulning_DW, color = Udrulning_DW)) + 
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2023-02-01", "2023-09-01", "2024-02-01", "2024-09-01", "2025-02-01"))), 
             linetype = "dashed", color = "black") +
  theme_minimal()

# Underretninger
data |> 
  group_by(Udrulning_DW, DatoFraJoin_DW) |>  
  summarise(gns_underretning = mean(Underretning_DW, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = DatoFraJoin_DW, y = gns_underretning, group = Udrulning_DW, color = Udrulning_DW)) + 
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2023-02-01", "2023-09-01", "2024-02-01", "2024-09-01", "2025-02-01"))), 
             linetype = "dashed", color = "black") +
  theme_minimal()

# Ydelseslængde
data |> 
  group_by(Udrulning_DW, DatoFraJoin_DW) |>  
  summarise(gns_længde = mean(ydelseslængde_dage, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = DatoFraJoin_DW, y = gns_længde, group = Udrulning_DW, color = Udrulning_DW)) + 
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2023-02-01", "2023-09-01", "2024-02-01", "2024-09-01", "2025-02-01"))), 
             linetype = "dashed", color = "black") +
  theme_minimal()

# Farvekode
data %>%
  filter(Farvekode_DW != "NULL") %>%  # Fjerner "NULL"
  group_by(DatoFraJoin_DW, Farvekode_DW) %>%  # Gruppér efter dato og farve
  summarise(antal_sager = n(), .groups = "drop") %>%  # Tæl antal sager
  ggplot(aes(x = DatoFraJoin_DW, y = antal_sager, color = Farvekode_DW, group = Farvekode_DW)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date(c("2023-02-01", "2023-09-01", "2024-02-01", "2024-09-01", "2025-02-01"))), 
             linetype = "dashed", color = "black") +
  theme_minimal()

# Ydelsesgruppering
data %>%
  filter(Ydelsesgruppering_DW != "NULL") %>%  # Fjerner "NULL"
  group_by(DatoFraJoin_DW, Ydelsesgruppering_DW) %>%  # Gruppér efter dato og farve
  summarise(antal_sager = n(), .groups = "drop") %>%  # Tæl antal sager
  ggplot(aes(x = DatoFraJoin_DW, y = antal_sager, color = Ydelsesgruppering_DW, group = Ydelsesgruppering_DW)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date(c("2023-02-01", "2023-09-01", "2024-02-01", "2024-09-01", "2025-02-01"))), 
             linetype = "dashed", color = "black") +
  theme_minimal()
unique(data$Ydelsesgruppering_DW)

#forsøg______________________________________________
#Lav treatment variabel 
data <- data |> 
  mutate(treatment = ifelse(Periode_DW %in% c("Periode_1", "Periode_2", "Periode_3", "Periode_4", "Periode_5"),1,0))
#Check
sum(data$treatment == 1 & data$Periode_DW == "Ikke-indrullet")



#PLAN__________________________________________________________________________________

#1. Færdiggør rens  
#2. Udførlig plan for DiD i R (husk evt. dannelse af nye variable) 

#Plot udfaldsvariable over til for tjek af PT
#Lav TWFE 
#Lav event-study
#Lav Bacon dekoponering
#Estimér ved brug af anden estiamtor
#Test for heterogenitet, eks. køn og aldersgrupper 


