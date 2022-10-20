
# lees fiches
if (file.exists(find_root_file("data/processed/habitat_fiches.Rdata",
                               criterion =
                               has_file("EU_rapportage_habitats.Rproj")))) {
load(find_root_file("data/processed/habitat_fiches.Rdata",
                    criterion = has_file("EU_rapportage_habitats.Rproj")))
} else{
  library(googlesheets4)
  library(assertthat)
  fiches <- list(zilt = "https://docs.google.com/spreadsheets/d/1XcuXlEbK3DS9RcgOTMPVtxNYmYciONJJpAjKG9yOakw/edit#gid=860716314",
             kustduin = "https://docs.google.com/spreadsheets/d/1kKKeHgyIGBsMclqxYUyCtMBxeMS7xFRmUH-YYpKV5gI/edit#gid=744463599",
             water = "https://docs.google.com/spreadsheets/d/1eG3aE4e0G9jl4ooW1pL9tPKyO9xAjXppZ1m8HCFBcjU/edit#gid=744463599",
             heide = "https://docs.google.com/spreadsheets/d/1EeNeXdG_nlR1-kh5yW1_ehp66lKHYrOeDN8ffZ3QgKk/edit#gid=744463599",
             gras = "https://docs.google.com/spreadsheets/d/1uuJbioyRB8uyaljOIRnzl6QtayzXD2GZ7Tmk-jUZwnI/edit#gid=744463599",
             veen = "https://docs.google.com/spreadsheets/d/1dysOn9O_yPer4xFP7Szx89gKv-BqVbiGuMWXtYihlnw/edit?usp=drive_web&ouid=115300910729380317098",
             rots = "https://docs.google.com/spreadsheets/d/1iAocy2Gu5WDSXtSyhiNEVjYVmcdiHXqnzk2IdtYom5o/edit?usp=sharing",
             bos = "https://docs.google.com/spreadsheets/d/1pT_HN9m6iX0rMPvUpjSUI31Cii2cVmLNLedPrLy-Q78/edit#gid=744463599")

  lees_fiche <- function(link = "https://docs.google.com/spreadsheets/d/1XcuXlEbK3DS9RcgOTMPVtxNYmYciONJJpAjKG9yOakw/edit#gid=860716314",
                       hoofdstuk = "zilt"){
    sheet_names <- sheet_names(ss = link)
    hab <- habitattypes %>% filter(id == hoofdstuk &
                                   !(main_type %in% c('9110', '9150', '1110') |
                                       str_detect(main_type, "rbb"))) %>%
      dplyr::select(main_type) %>%
      unique() %>%
      mutate(main_type = factor(main_type))
    hab <- hab$main_type
    assert_that(sum(!(hab %in% sheet_names)) == 0,
              msg = sprintf("Sommige habitatcodes van %s zijn niet in de fiche",
                            hoofdstuk))
    data <- lapply(hab, FUN = function(code){
      d <- read_sheet(ss = link, sheet = as.character(code), range = "A:C",
               col_types = "ccc")
      columns <- colnames(d)
      d <- d %>% rename(colnames = columns[1],
             flanders = columns[2],
             extra = columns[3]) %>%
        filter(!is.na(colnames) | !is.na(flanders) | !is.na(extra))
      return(d)
    })
    return(data)
  }

  data_fiches <- fiches %>% map(function(x)
    lees_fiche(link = x,
             hoofdstuk = names(fiches)[fiches == x]))#this only works if each habitat has a unique link
}

#---------------------Pressures and Threats------------------------------------#
extract_pressuresthreats <- function(x, hab){
  data_pt <- lapply(x, FUN = function(data){
    pressuresstart <-
      which(data$colnames == "7.1 Characterisation of pressures")
    threathsstart <- which(data$colnames == "7.1 Characterisation of threats")
    pressuressend <- threathsstart - 1
    threathsend <- which(
      data$colnames == "7.2 Sources of information, only for High (optional)"
      ) - 1
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    pressuresthreats <- data.frame(
      pt = c(rep('pressure', threathsstart - pressuresstart),
             rep('threath', threathsend - threathsstart + 1)),
      type = unname(unlist(data[pressuresstart:threathsend, 'flanders'])),
      ranking = unname(unlist(data[pressuresstart:threathsend, 'extra'])),
      hab = hab,
      code = code)
    return(pressuresthreats)
  })
  data_pt <- do.call(rbind, data_pt)
  return(data_pt)
}

data_pt <- lapply(1:length(data_fiches),
                  FUN = function(i){
                    x <- data_fiches[[i]]
                    hab <- names(data_fiches)[i]
                  return(extract_pressuresthreats(x, hab))})
data_pt <- do.call(rbind, data_pt)

#-----------------------------AREAAL-------------------------------------------#
extract_areaal <- function(x,hab){
  data_areaal <- lapply(x, FUN = function(data){
    trend <-
      which(data$colnames ==
              "4.3 Short-term trend Direction = only genuine change")
    frr <- which(
      data$colnames ==
        "4.10 Favourable reference range b) operators or c) x unknown")
    conclusion <- which(data$colnames == "10.1 Range")
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    areaal <- data.frame(
      type = c("trend", "frr", "conclusie"),
      value = unname(unlist(data[c(trend, frr, conclusion), 'flanders'])),
      hab = hab,
      code = code) #%>%
      #mutate(value = ifelse(value == "≈", "\U2245", value))
    return(areaal)
  })
  data_areaal <- do.call(rbind, data_areaal)
  return(data_areaal)
}

data_areaal <- lapply(1:length(data_fiches),
                  FUN = function(i){
                    x <- data_fiches[[i]]
                    hab <- names(data_fiches)[i]
                    return(extract_areaal(x, hab))})
data_areaal <- do.call(rbind, data_areaal)

#-----------------------------Oppervlakte--------------------------------------#
extract_area <- function(x,hab){
  data_area <- lapply(x, FUN = function(data){
    bestarea <- which(data$colnames ==
                        "5.2 Surface area: Best Single Value in km²")
    bestareasbz <- which(data$colnames ==
                           "11.1 Best Single Value in km²")
    trend <-
      which(data$colnames ==
              "5.6 Short-term trend Direction = only genuine change")
    fra <- which(
      data$colnames ==
        "5.13 Favourable reference area b) operators or c) x unknown")
    conclusion <- which(data$colnames == "10.2 Area")
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    area <- data.frame(
      type = c("area", "areasbz", "trend", "fra", "conclusie"),
      value = unname(unlist(data[c(bestarea, bestareasbz, trend, fra,
                                   conclusion), 'flanders'])),
      hab = hab,
      code = code)
    return(area)
  })
  data_area <- do.call(rbind, data_area)
  return(data_area)
}

data_area <- lapply(1:length(data_fiches),
                      FUN = function(i){
                        x <- data_fiches[[i]]
                        hab <- names(data_fiches)[i]
                        return(extract_area(x, hab))})
data_area <- do.call(rbind, data_area)


#De meer gedetailleerde data voor de bar charts moet waarschijnlijk eerder uit deze sheet gehaald worden
# https://docs.google.com/spreadsheets/d/120mvNWUdo6IXTNoKMeyDTK_2l9Atr-cT-sK_rW3jIDc/edit#gid=1041184742

#---------------------SPECIFIEKE STRUCTUREN EN FUNCTIES------------------------#
# figuur 5 en 6: https://drive.google.com/drive/folders/1_eeHq-6oHvLN_BAgvFKjyqhUaJZ4JChF
# tabel 15: kan ik niet terug vinden
# table 16
extract_struc_func_final <- function(x,hab){
  data_struc_func_final <- lapply(x, FUN = function(data){
    trend <-
      which(data$colnames ==
              "6.4 Short-term trend of habitat area in good condition. Direction = only genuine change")
    conclusion <- which(
      data$colnames ==
        "10.3 Specific structure and  functions (incl. typical species)")
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    struc_func_final <- data.frame(
      type = c("trend", "Oosterlynckx", "TJollyn"),
      value = c(unname(unlist(data[c(trend, conclusion), 'flanders'])),
                unname(unlist(data[conclusion, 'extra']))),
      hab = hab,
      code = code)
    return(struc_func_final)
  })
  data_struc_func_final <- do.call(rbind, data_struc_func_final)
  return(data_struc_func_final)
}

data_struc_func_final <- lapply(1:length(data_fiches),
                    FUN = function(i){
                      x <- data_fiches[[i]]
                      hab <- names(data_fiches)[i]
                      return(extract_struc_func_final(x, hab))})
data_struc_func_final <- do.call(rbind, data_struc_func_final)

#---------------------Instandhoudingsmaatregelen-------------------------------#
# eventueel hier (https://docs.google.com/spreadsheets/d/1KJgjjwr29wHK06kQH3bipfwVpYnC5whivkLeru2r8ao/edit#gid=552048168)
# maar beter op uit de fiche te halen mss
extract_ihm <- function(x, hab){
  data_ihm <- lapply(x, FUN = function(data){
    ihmstart <-
      which(str_detect(data$colnames, "8.5 List of the main conservation measures"))
    ihmend <- which(str_detect(data$colnames,
                     "8.6 Additional information")) - 1
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    ihm <- data.frame(
      ihm = unname(unlist(data[dput(ihmstart:ihmend), 'flanders'])),
      hab = hab,
      code = code)
    return(ihm)
  })
  data_ihm <- do.call(rbind, data_ihm)
  return(data_ihm)
}

data_ihm <- lapply(1:length(data_fiches),
                  FUN = function(i){
                    x <- data_fiches[[i]]
                    hab <- names(data_fiches)[i]
                    return(extract_ihm(x, hab))})
data_ihm <- do.call(rbind, data_ihm)

#---------------------------Toekomstperspectieven------------------------------#
extract_toekomst <- function(x, hab){
  data_toekomst <- lapply(x, FUN = function(data){
    areaal <-
      which(data$colnames == "9.1 Future prospects of a) range")
    struct <-
      which(data$colnames == "9.1 Future prospects of c) structure and functions")
    conclusie <-
      which(data$colnames == "10.4 Future prospects")
    oppervlakte <-
      which(data$colnames == "9.1 Future prospects of b) area")
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    toekomst <- data.frame(
      type = c("Areaal", "Oppervlakte", "Structuren en functie", "Conclusie"),
      score = unname(unlist(data[c(areaal, oppervlakte, struct, conclusie), "flanders"])),
      hab = hab,
      code = code)
    return(toekomst)
  })
  data_toekomst <- do.call(rbind, data_toekomst)
  return(data_toekomst)
}

data_toekomst <- lapply(1:length(data_fiches),
                   FUN = function(i){
                     x <- data_fiches[[i]]
                     hab <- names(data_fiches)[i]
                     return(extract_toekomst(x, hab))})
data_toekomst <- do.call(rbind, data_toekomst)

#---------------------------------Conclusie------------------------------------#
extract_conclusie <- function(x, hab){
  data_conclusie <- lapply(x, FUN = function(data){
    areaal <-
      which(data$colnames == "10.1 Range")
    struct <-
      which(data$colnames == "10.3 Specific structure and  functions (incl. typical species)")
    toekomst <-
      which(data$colnames == "10.4 Future prospects")
    oppervlakte <-
      which(data$colnames == "10.2 Area")
    eindconclusie <-
      which(data$colnames == "10.5 Overall assessment of Conservation Status")
    totaaltrend <-
      which(data$colnames == "10.6 Overall trend in Conservation Status")
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),
                              "flanders"])
    conclusie <- data.frame(
      type = c("Areaal", "Oppervlakte", "Structuren en functie",
               "Toekomstperspectieven", "Eindconclusie", "Totaal-trend"),
      score = unname(unlist(data[c(areaal, oppervlakte, struct, toekomst,
                                   eindconclusie, totaaltrend), "flanders"])),
      hab = hab,
      code = code)
    return(conclusie)
  })
  data_conclusie <- do.call(rbind, data_conclusie)
  return(data_conclusie)
}

data_conclusie <- lapply(1:length(data_fiches),
                        FUN = function(i){
                          x <- data_fiches[[i]]
                          hab <- names(data_fiches)[i]
                          return(extract_conclusie(x, hab))})
data_conclusie <- do.call(rbind, data_conclusie)

#lees habitatkwaliteit
files_habitatkwaliteit <- list.files(
  path = find_root_file("data/raw/BijlageHabitatKwal",
                        criterion = has_file("EU_rapportage_habitats.Rproj")))
#-----------lees sheets over status van de habitats in vlaanderen--------------#
#Sommige habitats hebben een grenswaarde van 90, anderen van 75: 10 ha totale oppervlakte als grens voor toepassing van de 90%-regel (Paelinckx et al 2019)
Grenswaarde <- data_area %>% filter(type == "area") %>%
  mutate(value = as.numeric(str_replace(value, ',', '.')) * 100, #van vierkante kilometer naar hectare
         Grenswaarde = ifelse(value <= 10, 90, 75)) %>%
  dplyr::select(code, Grenswaarde)
lees_statushabitat <- function(x){
  data <- read.csv2(
    find_root_file(sprintf("data/raw/BijlageHabitatKwal/%s", x),
                   criterion = has_file("EU_rapportage_habitats.Rproj")),
    stringsAsFactors = FALSE) %>%
    mutate(Habitattype = as.character(Habitattype)) %>%
    left_join(Grenswaarde, by = c('Habitattype' = 'code')) %>%# grenswaarde is 90 voor zeldzame habitattypes die <10ha beslagen
    mutate(Uitspraak = factor(
      ifelse(is.na(AandeelGunstig_LLCI), "Onbekend",
             ifelse(AandeelGunstig_LLCI > Grenswaarde, "Gunstig",
                    ifelse(AandeelGunstig_ULCI < Grenswaarde,
                           "Ongunstig", "Onbekend"))))) %>%
    dplyr::select("Schaal", "Periode", "TypeResultaat", "Versie", "Habitattype",
                  "Habitatsubtype",	"SBZH",	"nObs",	"sumWeights",
                  "AandeelGunstig", "AandeelGunstig_LLCI",
                  "AandeelGunstig_ULCI", "Grenswaarde", "Uitspraak")
}

data_statushabitat <- lapply(files_habitatkwaliteit[
  str_detect(files_habitatkwaliteit, regex("StatusHabitat", ignore_case = T))],
  FUN = function(x){lees_statushabitat(x)}
)
data_statushabitat <- do.call(rbind, data_statushabitat)

#--lees sheets over het aandeel dat als gunstig wordt beschouwd in Vlaanderen--#
lees_aandeelgunstig <- function(x){
  data <- read.csv2(
    find_root_file(sprintf("data/raw/BijlageHabitatKwal/%s", x),
                   criterion = has_file("EU_rapportage_habitats.Rproj")),
    stringsAsFactors = FALSE) %>%
    mutate(Habitattype = as.character(Habitattype),
           Versiebis =
             ifelse(Versie == "Versie 2.0",
                    "T'jollyn et al. 2009 (LSVI versie 2)",
                    ifelse(Versie == "Versie 3",
                           "Oosterlynck et al. 2018 (LSVI versie 3)",
                           "CHECK VERSIE!!"))) %>%
    left_join(Grenswaarde, by = c('Habitattype' = 'code')) %>%# grenswaarde is 90 voor zeldzame habitattypes die <10ha beslagen
    mutate(Uitspraak =
             factor(ifelse(is.na(AandeelGunstig_LLCI), "Onbekend",
                           ifelse(AandeelGunstig_LLCI > Grenswaarde, "Gunstig",
                                  ifelse(AandeelGunstig_ULCI < Grenswaarde,
                                         "Ongunstig", "Onbekend"))))) %>%
    dplyr::select("Schaal", "Periode", "TypeResultaat", "Versie", "Habitattype",
      "Habitatsubtype", "Criterium", "Indicator", "Belang", "SBZH",
      "nObs", "sumWeights", "AandeelGunstig", "AandeelGunstig_LLCI",
      "AandeelGunstig_ULCI", "Versiebis", "Grenswaarde", "Uitspraak"
    )
}
data_aandeelgunstig <- lapply(files_habitatkwaliteit[
  str_detect(files_habitatkwaliteit, "Indicatoren_AandeelGunstigVlaanderen")],
  FUN = function(x){lees_aandeelgunstig(x)})
data_aandeelgunstig <- do.call(rbind, data_aandeelgunstig)


save(data_aandeelgunstig, data_statushabitat, data_area, data_areaal,
     data_conclusie, data_ihm, data_pt,
     data_struc_func_final, data_toekomst,
     file = find_root_file("data/processed/habitatdata.Rdata",
                           criterion =
                             has_file("EU_rapportage_habitats.Rproj")))
save(data_fiches,
     file = find_root_file("data/processed/habitat_fiches.Rdata",
                           criterion =
                             has_file("EU_rapportage_habitats.Rproj")))


