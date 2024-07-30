
# Een lijst met alle habitattypes uit n2khab

get_habitattypes <- function () {
  habitattypes <- n2khab::read_types(lang = "nl") %>%
    mutate(id = case_when(
      typeclass == "CH" ~ "zilt",
      typeclass %in% c("ID", "HS", "SS") ~ "heide",
      typeclass == "CD" ~ "kustduin",
      typeclass == "FW" ~ "water",
      typeclass == "GR" ~ "gras",
      typeclass == "BMF" ~ "veen",
      typeclass == "RC" ~ "rots",
      TRUE ~ "bos"
    ),
    id = as.factor(id))
  habitattypes
}

#-------------------------------------------------------

# Lees de fiche-inhoud in

read_form <- function(link = "google-sheet-id",
                      chapter = 'for example: zilt',
                      habitattypes = "output from get_habitattypes"){

  #helper function
  read_form_page_cols <- function(code) {
    d <- read_sheet(ss = link,
                    sheet = as.character(code),
                    range = "A:C",
                    col_types = "ccc")
    columns <- colnames(d)
    d <- d |>
      rename(colnames = columns[1],
             flanders = columns[2],
             extra = columns[3]) |>
      filter(!is.na(colnames) | !is.na(flanders) | !is.na(extra))
    return(d)
  }

  sheet_names <- sheet_names(ss = link)
  hab <- habitattypes |>
    filter(id == chapter &
             !(main_type %in% c('9110', '9150', '1110') | #enkele boshabitats
                 str_detect(main_type, "rbb"))) |>  #meerdere fiches
    # & !(main_type %in% c('9110', '9150', '1110') |
    #     str_detect(main_type, "rbb"))) %>%
    dplyr::select(main_type) |>
    unique() |>
    mutate(main_type = factor(main_type))
  hab <- hab$main_type
  assert_that(sum(!(hab %in% sheet_names)) == 0,
              msg = sprintf("Sommige habitatcodes van %s zijn niet in de fiche",
                            chapter))

  #loop through pages in sheet
  data <- lapply(hab, FUN = read_form_page_cols)
  names(data) <- hab
  return(data)
}

#----------------------------------------------------------

extract_pressuresthreats <- function(x, hab){

  extract_function <- function(data) {
    pressuresstart <-
      which(data$colnames == "7.1 Characterisation of pressures")
    threathsstart <- which(data$colnames == "7.1 Characterisation of threats")
    pressuressend <- threathsstart - 1
    threathsend <- which(
      data$colnames == "7.2 Sources of information, only for High (optional)") - 1
    code <- as.character(data[which(data$colnames == "1.2 Habitat code"),"flanders"])
    pressuresthreats <- data.frame(
      pt = c(rep('pressure', threathsstart - pressuresstart),
             rep('threat', threathsend - threathsstart + 1)),
      type = unname(unlist(data[pressuresstart:threathsend, 'flanders'])),
      ranking = unname(unlist(data[pressuresstart:threathsend, 'extra'])),
      hab = hab,
      code = code)
    return(pressuresthreats)
  }
  map_dfr(x, extract_function)
}

#----------------------------------------------------------

extract_acreage <- function(x,hab){

  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#---------------------------------------------------------------

extract_area <- function(x,hab){

  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#-------------------------------------------------------------------------------

extract_struc_func_final <- function(x,hab){

  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#-------------------------------------------------------------------------------

extract_ihm <- function(x, hab){
  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#-------------------------------------------------------------------------------

extract_future <- function(x, hab){

  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#-------------------------------------------------------------------------------

extract_conclusion <- function(x, hab){

  extract_function <- function(data){
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
  }
  map_dfr(x, extract_function)
}

#---------------------------------------------------------------------------------

read_status_habitat <- function(x, limit_values, dir) {
  data <- read_csv2(file.path(dir, x)) |>
    mutate(Habitattype = as.character(Habitattype),
           Habitatsubtype = as.character(Habitattype),
           Periode = as.character(Periode),
           naam =  str_replace(string = x,
                               replacement = "\\1",
                               pattern = "StatusHabitat_(.+)\\.csv")) |>
    left_join(limit_values, by = join_by(Habitattype == code)) |>
    mutate(Uitspraak = case_when(is.na(AandeelGunstig_LLCI) ~ "Onbekend",
                                 AandeelGunstig_LLCI > Grenswaarde ~ "Gunstig",
                                 AandeelGunstig_ULCI < Grenswaarde ~ "Ongunstig",
                                 TRUE ~ "Onbekend")) |>
    dplyr::select(naam, Schaal, Periode, TypeResultaat, Versie, Habitattype,
                  Habitatsubtype,	SBZH,	nObs,	sumWeights,
                  AandeelGunstig, AandeelGunstig_LLCI,
                  AandeelGunstig_ULCI, Grenswaarde, Uitspraak)
  data
}

#---------------------------------------------------------------------------------


read_fraction_favorable <- function(x, limit_values, dir) {
  data <- read_csv2(file.path(dir, x)) |>
    mutate(Habitattype = as.character(Habitattype),
           Habitatsubtype = as.character(Habitattype),
           Periode = as.character(Periode),
           naam =  str_replace(string = x,
                               replacement = "\\1",
                               pattern = ".*Vlaanderen_(.+)\\.csv.*"),
           Versiebis =
             ifelse(Versie == "Versie 2.0",
                    "T'jollyn et al. 2009 (LSVI versie 2)",
                    ifelse(Versie == "Versie 3",
                           "Oosterlynck et al. 2018 (LSVI versie 3)",
                           "CHECK VERSIE!!"))) |>
    left_join(limit_values, join_by(Habitattype == code)) |>
    mutate(Uitspraak = case_when(is.na(AandeelGunstig_LLCI) ~ "Onbekend",
                                 AandeelGunstig_LLCI > Grenswaarde ~ "Gunstig",
                                 AandeelGunstig_ULCI < Grenswaarde ~ "Ongunstig",
                                 TRUE ~ "Onbekend"))
  data
}

#-----------------------------------------------------------------------

#write 3-level list to csv

write_data_fiches_to_csv <- function(x, base_path = "output/") {
  walk(names(x), \(top_level_name) {
    top_level_item <- x[[top_level_name]]

    walk(names(top_level_item), \(second_level_name) {
      df <- top_level_item[[second_level_name]]
      file_path <- glue::glue("{base_path}{top_level_name}_{second_level_name}.csv")
      write_excel_csv2(df, file_path)
    })
  })
  # Write structure information
  structure_info <- map_depth(x, 2, function(x) class(x)[1])
  write_json(structure_info, glue::glue("{base_path}structure__info.json"))
}

#----------------------------------------------------------------------------

#read 3-level list back to r object

read_csv_to_data_fiches_list <- function(base_path = "output/") {
  # Read structure information
  structure_info <- read_json(glue::glue("{base_path}structure__info.json"))
  reconstructed_list <-
    map(names(structure_info), \(top_level_name) {
      second_level <- structure_info[[top_level_name]]
      map(names(second_level), \(second_level_name) {
        file_path <- glue::glue("{base_path}{top_level_name}_{second_level_name}.csv")
        df <- read_csv2(file_path)
        df
      }) %>% set_names(names(second_level))
    }) %>% set_names(names(structure_info))

  reconstructed_list
}


#---------------------------------------------------------------------------

read_habitatdata <- function(base_path = "output/", starts_with = "data_") {

  #list csv files
  csv_files <- list.files(path = base_path,
                          pattern = glue("^{starts_with}.*\\.csv$"),
                          full.names = TRUE)

  walk(csv_files, \(path) {
    object_name <- path |> basename() |> str_remove("\\.csv$")
    data <- read_csv2(path)
    assign(object_name, data, envir = .GlobalEnv)
  })
}

