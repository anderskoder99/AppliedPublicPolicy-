
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
install.packages("did")

# 2. Indlæs data -------------------------------------------------------------

#Sikre at alle brugere har samme path til data
data <- read_excel(here("Data", "RV data_anonymiseret 25feb2025.xlsx"))

# 3. Indlæs pakker -----------------------------------------------------------
library(readxl) #Bruges til at indlæse data i excel-form
library(here) #Bruges til at skabe en relative path til data
library(tidyverse) 
library(panelView)
library(did) #Bruges til Dif-inDif

View(data) #Viser data i R-studio for at tjekke at det blev loaded korrekt

# 3. Datarensning -----------------------------------------------------------

# 3.1 Tjek af missing data ------------------------------------------------

unique(data$Udrulning_DW) #Viser unikke værdier i kolonnen "Udrulning_DW"
sum(data$Udrulning_DW == "Udenfor distrikt") #580 rækker har denne værdi
data <- data[data$Udrulning_DW != "Udenfor distrikt", ] #Fjerner rækker med denne værdi
unique(data$Ydelse) #OBS på denne: 260 unikke værdier - skal vi gruppere?
data %>% 
  count(YdelseDatoStop == "9999-12-31") #79494 ydelser er stadig aktive pr. 31/1-25
data <- data %>% 
  mutate(YdelseDatoStopNy = if_else(YdelseDatoStop == as.Date("9999-12-31"), NA, YdelseDatoStop))


#Ændrer alle NULL-værdier til NA, fjerner omkring hlavdelen af rækker
data$Fraværsprocent[data$Fraværsprocent == "NULL"] <- NA
data$AntalFraværsdage[data$AntalFraværsdage == "NULL"] <- NA
data$AntalSkoledage[data$AntalSkoledage == "NULL"] <- NA
data$YdelseDatoStart[data$YdelseDatoStart == "NULL"] <- NA
data$YdelseDatoStop[data$YdelseDatoStop == "NULL"] <- NA

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
data$ydelseslængde_dage <- as.Date(data$YdelseDatoStopNy) - as.Date(data$YdelseDatoStart)

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


# 5.0 Callaway-modeller -------------------------------------------------------


# 5.1 Forberedelse --------------------------------------------------------

#Tidsvariabel -  vi skaber en numerisk tidsvariabe til modellen
data <- data |> 
  mutate(
    time = case_when(
      str_detect(Halvår_DW, "Forår")  ~ as.numeric(str_extract(Halvår_DW, "\\d+")),         # Fx "Forår2019" → 2019.0
      str_detect(Halvår_DW, "Efterår") ~ as.numeric(str_extract(Halvår_DW, "\\d+")) + 0.5   # Fx "Efterår2019" → 2019.5
    )
  )

#Vi skaber en treatment variabel til modellen, der passer hvert udrul og tidsvariablen ovenfor
data <- data |> 
  mutate(
    G = case_when(
      Udrulning_DW == "Udrul_1" ~ 2023.0,
      Udrulning_DW == "Udrul_2" ~ 2023.5,
      Udrulning_DW == "Udrul_3" ~ 2024.0,
      Udrulning_DW == "Udrul_4" ~ 2024.5,
      Udrulning_DW == "Udrul_5" ~ NA_real_  # or 0 if you prefer to keep them as never-treated controls
    )
  )

data <- data |> 
  mutate(
    G = if_else(is.na(G), 0, G)  # Replace NA with 0 for never-treated units
  )

#Oprette unikt ID til skoledistrikterne
data <- data %>% 
  mutate(SkoledistriktID = as.factor(SkoledistriktNavn))

# 5.2 Fraværsmodellen  ----------------------------------------------------
#Modellen  
att_gt_results <- att_gt(
  yname = "Fraværsprocent",
  tname = "time",
  idname = "SkoledistriktID",
  gname = "G",
  data = data,,
  panel = FALSE

)

agg_dynamic <- aggte(att_gt_results, type = "dynamic")
summary(agg_dynamic)
plot(agg_dynamic)

#Plottet:

event_study_df <- tibble(
  event_time = agg_dynamic$egt,
  att = agg_dynamic$att.egt,
  se = agg_dynamic$se.egt,
  ci_low = att - 1.96 * se,
  ci_high = att + 1.96 * se
)

ggplot(event_study_df, aes(x = event_time, y = att)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "gray40") +  # pre-treatment baseline
  labs(
    title = "Dynamisk DiD: Effekter relativt til behandlingstidspunkt",
    x = "Tid relativt til treatment",
    y = "ATT (gennemsnitlig effekt)"
  ) +
  theme_minimal()



# 5.3 Antal Underretninger ------------------------------------------------
att_gt_results_underretninger <- att_gt(
  yname = "Underretning_DW",
  tname = "time",
  idname = "SkoledistriktID",
  gname = "G",
  data = data,,
  panel = FALSE
  
)

agg_dynamic_underretninger <- aggte(att_gt_results_underretninger, type = "dynamic")
summary(agg_dynamic_underretninger)

#Plottet:

event_study_df_underretninger <- tibble(
  event_time = agg_dynamic_underretninger$egt,
  att = agg_dynamic_underretninger$att.egt,
  se = agg_dynamic_underretninger$se.egt,
  ci_low = att - 1.96 * se,
  ci_high = att + 1.96 * se
)

ggplot(event_study_df_underretninger, aes(x = event_time, y = att)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_vline(xintercept = -0, linetype = "dashed", color = "gray40") +  # pre-treatment baseline
  labs(
    title = "Dynamisk DiD: Effekter relativt til behandlingstidspunkt",
    x = "Tid relativt til treatment",
    y = "ATT (gennemsnitlig effekt)"
  ) +
  theme_minimal()


# 5.4 Ydelsesgruppering  --------------------------------------------------
data <- data |> 
  mutate(
    forebyggende_dummy = if_else(Ydelsesgruppering_DW == "Tidligt forebyggende indsatser", 1, 0)
  )


att_gt_results_forebyggende <- att_gt(
  yname = "forebyggende_dummy",
  tname = "time",
  idname = "SkoledistriktID",
  gname = "G",
  data = data,,
  panel = FALSE
  
)

agg_dynamic_forebyggende <- aggte(att_gt_results_forebyggende, type = "dynamic")
summary(agg_dynamic_forebyggende)

#Plottet:

event_study_df_forebyggende <- tibble(
  event_time = agg_dynamic_forebyggende$egt,
  att = agg_dynamic_forebyggende$att.egt,
  se = agg_dynamic_forebyggende$se.egt,
  ci_low = att - 1.96 * se,
  ci_high = att + 1.96 * se
)

ggplot(event_study_df_forebyggende, aes(x = event_time, y = att)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_vline(xintercept = -0, linetype = "dashed", color = "gray40") +  # pre-treatment baseline
  labs(
    title = "Dynamisk DiD: Effekter relativt til behandlingstidspunkt",
    x = "Tid relativt til treatment",
    y = "ATT (gennemsnitlig effekt)"
  ) +
  theme_minimal() 

#PLAN__________________________________________________________________________________

#1. Færdiggør rens  
#2. Udførlig plan for DiD i R (husk evt. dannelse af nye variable) 

#Plot udfaldsvariable over til for tjek af PT
#Lav TWFE 
#Lav event-study
#Lav Bacon dekoponering
#Estimér ved brug af anden estiamtor
#Test for heterogenitet, eks. køn og aldersgrupper 


