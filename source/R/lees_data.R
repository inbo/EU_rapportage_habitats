# Voorbereiding (projectdir = workdir)

## Laad benodigde functionaliteiten

library(tidyverse)
library(googlesheets4)
library(assertthat)
library(rprojroot)
library(n2khab)
source('source/R/_functions_lees_data.R')

################################
## Lees fiches
################################

load("data/processed/habitat_fiches.Rdata") #indien reeds doorlopen

fiches_source <- list(
  zilt     = "1XcuXlEbK3DS9RcgOTMPVtxNYmYciONJJpAjKG9yOakw",
  kustduin = "1kKKeHgyIGBsMclqxYUyCtMBxeMS7xFRmUH-YYpKV5gI",
  water    = "1eG3aE4e0G9jl4ooW1pL9tPKyO9xAjXppZ1m8HCFBcjU",
  heide    = "1EeNeXdG_nlR1-kh5yW1_ehp66lKHYrOeDN8ffZ3QgKk",
  gras     = "1uuJbioyRB8uyaljOIRnzl6QtayzXD2GZ7Tmk-jUZwnI",
  veen     = "1dysOn9O_yPer4xFP7Szx89gKv-BqVbiGuMWXtYihlnw",
  rots     = "1iAocy2Gu5WDSXtSyhiNEVjYVmcdiHXqnzk2IdtYom5o",
  bos      = "1pT_HN9m6iX0rMPvUpjSUI31Cii2cVmLNLedPrLy-Q78")


habitattypes <- get_habitattypes()
fch_zilt     <- read_form(fiches_source[["zilt"]],
                          chapter = "zilt",
                          habitattypes = habitattypes)
fch_kustduin <- read_form(fiches_source[["kustduin"]],
                          chapter = "kustduin",
                          habitattypes = habitattypes)
fch_water    <- read_form(fiches_source[["water"]],
                          chapter = "water",
                          habitattypes = habitattypes)
fch_heide    <- read_form(fiches_source[["heide"]],
                          chapter = "heide",
                          habitattypes = habitattypes)
fch_gras     <- read_form(fiches_source[["gras"]],
                          chapter = "gras",
                          habitattypes = habitattypes)
fch_veen     <- read_form(fiches_source[["veen"]],
                          chapter = "veen",
                          habitattypes = habitattypes)
fch_rots     <- read_form(fiches_source[["rots"]],
                          chapter = "rots",
                          habitattypes = habitattypes)
fch_bos      <- read_form(fiches_source[["bos"]],
                          chapter = "bos",
                          habitattypes = habitattypes)

#---------------------source data------------------------------------#

data_fiches <- ls(pattern = "^fch_", envir = .GlobalEnv) %>%
  mget(envir = .GlobalEnv) %>%
  setNames(sub("^fch_", "", names(.)))

#---------------------Pressures and Threats------------------------------------#
#map_dfr is superseded en zou kunnen vervangen worden door map2 en list_rbind

data_pt <-
  map2_dfr(data_fiches, names(data_fiches),
       ~ extract_pressuresthreats(.x, hab = .y))

#-----------------------------Areaal-------------------------------------------#

data_acreage <- map2_dfr(data_fiches, names(data_fiches),
                ~extract_acreage(.x, hab = .y))

#-----------------------------Oppervlakte--------------------------------------#

data_area <- map2_dfr(data_fiches, names(data_fiches),
                        ~extract_area(.x, hab = .y))

#---------------------Specifieke structuren en functies------------------------#

data_struc_func_final <- map2_dfr(data_fiches, names(data_fiches),
                                  ~extract_struc_func_final(.x, hab = .y))


#---------------------Instandhoudingsmaatregelen-------------------------------#

data_ihm <-  map2_dfr(data_fiches, names(data_fiches),
                      ~extract_ihm(.x, hab = .y))

#---------------------------Toekomstperspectieven------------------------------#

data_toekomst <- map2_dfr(data_fiches, names(data_fiches),
                          ~extract_future(.x, hab = .y))


#---------------------------------Conclusie------------------------------------#

data_conclusie <- map2_dfr(data_fiches, names(data_fiches),
                          ~extract_conclusion(.x, hab = .y))

#==============================================================================

###########################
#### Habitatkwaliteit
###########################



#lees habitatkwaliteit
files_habitatstatus <-
  list.files(path = "data/raw/BijlageHabitatKwal",
             pattern = "StatusHabitat*")

files_indicatoren <-
  list.files(path = "data/raw/BijlageHabitatKwal",
             pattern = "Indicatoren_AandeelGunstig*")

#-----------lees sheets over status van de habitats in vlaanderen--------------#

#Sommige habitats hebben een grenswaarde van 90, anderen van 75: 10 ha totale oppervlakte als grens voor toepassing van de 90%-regel (Paelinckx et al 2019)
grenswaarden <- data_area %>%
  filter(type == "area") %>%
  mutate(value = as.numeric(str_replace(value, ',', '.')) * 100, #km²->ha
         Grenswaarde = ifelse(value <= 10, 90, 75)) %>%
  dplyr::select(code, Grenswaarde)


data_statushabitat <- files_habitatstatus |>
  map(.f = read_status_habitat,
      limit_values = grenswaarden,
      dir = "data/raw/BijlageHabitatKwal")|>
  list_rbind()

#--lees sheets over het aandeel dat als gunstig wordt beschouwd in Vlaanderen--#

data_aandeelgunstig <- files_indicatoren |>
  map(.f = read_fraction_favorable,
      limit_values = grenswaarden,
      dir = "data/raw/BijlageHabitatKwal")|>
  list_rbind()


#-indicatorscores van de criteria ‘Typische soorten’ en ‘Ruimtelijke samenhang’-#

#Bijlage 4 uit https://pureportal.inbo.be/nl/publications/regionale-staat-van-instandhouding-voor-de-habitattypen-van-de-ha
data_soorten_samenhang <-
  readxl::read_xlsx(
    path = paste0("data/raw/BijlageHabitatKwal/",
                  "Bijlage 4_Habitatkwaliteit 2019 en 2013",
                  " + belang indicatoren.xlsx"),
    sheet = "Kwaliteit habitattypen",
    skip = 1) %>%
  rename(Criterium = `Criterium...2`, Indicator = `Indicator...3`,
         Belang = `Belangrijk - Zeer Belangrijk`,
         Toestand = ends_with("Oosterlynck"),
         Habitattype = `Habitat_Code`) %>%
  dplyr::select(Habitattype, Criterium, Indicator, Belang, Toestand) %>%
  filter(Criterium %in% c("Typische soorten",
                          "Ruimtelijke samenhang (niveau VL)",
                          "Ruimtelijke samenhang (niveau VL)")) %>%
  mutate(Toestand = ifelse(is.na(Toestand), "n.v.t.", Toestand))


#------------------data oppervlaktes voor bar charts---------------------------#

data_opp2018 <- readxl::read_xlsx(
  "data/raw/oppervlaktes.xlsx",
  sheet = "2018",
  skip = 1) %>%
  mutate(jaar = 2018)

data_opp <- readxl::read_xlsx(
  path = "data/raw/oppervlaktes.xlsx",
  sheet = "2012",
  skip = 1) %>%
  mutate(jaar = 2012) |>
  bind_rows(data_opp2018)

############---------------BEWAAR DE DATA---------------------------############

save(data_aandeelgunstig, data_statushabitat, data_area, data_areaal,
     data_conclusie, data_ihm, data_pt,
     data_struc_func_final, data_toekomst, data_soorten_samenhang, data_opp,
     file = "data/processed/habitatdata.Rdata")

save(data_fiches,
     file = "data/processed/habitat_fiches.Rdata")


