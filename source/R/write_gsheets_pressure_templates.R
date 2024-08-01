library(n2khab)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(googlesheets4)
library(googledrive)
conflicted::conflicts_prefer(dplyr::filter)

# Setup for googledrive authentication. Set the appropriate env vars in
# .Renviron and make sure you ran drive_auth() interactively with these settings
# for the first run (or to renew an expired Oauth token)
if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

pressures_folder_id <- as_id("1SOH3H1UmgP4y9VEz1IVA06skzHG5lJlE")

# create data frame and new googlesheets:

template_data <-
  read_types(lang = "nl") |>
  filter(typelevel == "main_type", !str_detect(type, "rbb")) |>
  select(
    habitatgroep = typeclass_name,
    habitattype = type
  ) |>
  crossing(
    read_env_pressures(lang = "nl") |>
      select(druk = ep_abbrev)
  ) |>
  mutate(
    timing_2025 = NA_character_,
    scope_2025 = NA_character_,
    impact_2025 = NA_character_
  ) |>
  nest(habitattypedata = -c(matches("habitat"))) |>
  nest(habitatgroepdata = -habitatgroep) |>
  # slice(1) |>
  mutate(ss_id = map_chr(habitatgroep, function(tc) {
    id <- gs4_create(tc)
    drive_mv(id, pressures_folder_id)
    id
  }))

# fill googlesheets with template data:

walk2(
  template_data$ss_id,
  template_data$habitatgroepdata,
  function(id, tcd) {
    walk2(tcd$habitattype, tcd$habitattypedata, function(hab, df) {
      write_sheet(df, ss = id, sheet = as.character(hab))
    })
    # delete sheet 'Blad1':
    sheet_delete(id, 1)
  }
)

# write a legend googlesheet:

id_legend <- gs4_create("LEGENDE")
drive_mv(id_legend, pressures_folder_id)

read_types(lang = "nl") |>
  filter(typelevel == "main_type", !str_detect(type, "rbb")) |>
  select(
    habitatgroep = typeclass_name,
    habitattype = type,
    verkorte_naam = type_shortname,
    naam = type_name
  ) |>
  write_sheet(ss = id_legend, sheet = "habitattypes")

read_env_pressures(lang = "nl") |>
  select(
    afkorting = ep_abbrev,
    naam = ep_name,
    drukklasse = ep_class_name,
    uitleg = explanation,
    opmerking = remarks
  ) |>
  write_sheet(ss = id_legend, sheet = "drukken")

sheet_delete(id_legend, 1)
