library(protocolhelper)
library(tidyverse)
library(rprojroot)
library(parsermd)
library(googledrive)

#download the docx files from google drive
folder <- "https://drive.google.com/drive/folders/10cZDtfWFMglGF4ICvBzv6HXkfsqT3eP1"
folder_id <- drive_get(as_id(folder))
files <- drive_ls(folder_id)
#check whether the receiving folder exists
if (!file.exists(
  find_root_file("data/raw",
                 criterion = has_file("EU_rapportage_habitats.Rproj")))) {
  dir.create(find_root_file("data/raw",
                            criterion =
                              has_file("EU_rapportage_habitats.Rproj")))
}
#download all files in the local folder
for (i in seq_along(files$name)) {
  drive_download(
    as_id(files$id[i]),
    path =
      find_root_file("data/raw", files$name[i],
                     criterion = has_file("EU_rapportage_habitats.Rproj")),
    overwrite = TRUE
  )
  }
# de paragrafen moeten hetzelfde zijn als de namen van de hoofdstukken in de
# docx bestanden
paragrafen <- c("Inleiding", "Areaal", "Oppervlakte", "Regionale_toestand",
                "Drukken_bedreigingen", "Instandhoudingsmaatregelen",
                "Toekomstperspectieven", "Conclusies")
for (hoofdstuk in c("zilt", "kustduin", "water", "heide", "gras", "veen",
                   "rots", "bos")) {
  convert_docx_to_rmd(
    from = find_root_file("data/raw",
                        sprintf("%s.docx", hoofdstuk),
                        criterion = has_file("EU_rapportage_habitats.Rproj")),
    to = find_root_file("source/bookdown/habitatauteurs_paragrafen",
                      sprintf("%s.Rmd", hoofdstuk),
                      criterion = has_file("EU_rapportage_habitats.Rproj")),
    overwrite = TRUE)
  rmd <- parsermd::parse_rmd(
    find_root_file("source/bookdown/habitatauteurs_paragrafen",
                  sprintf("%s.Rmd", hoofdstuk),
                  criterion = has_file("EU_rapportage_habitats.Rproj"))) %>%
    as_tibble()
  assertthat::assert_that(sum(!(paragrafen %in% rmd$sec_h1)) == 0 ,
                          msg =  sprintf("These chapters are missing in the word file:%s",
                                         paste(
                                           paragrafen[!(paragrafen %in% rmd$sec_h1)],
                                           collapse = ", ")))
  for (x in paragrafen) {
    rmd_select(rmd, by_section(x)) %>%
    rmd_select(has_type("rmd_markdown")) %>% # do not print the headers, only the text
    as_document() %>% str_replace("\\\\", "\\") %>% #otherwise, double backslash is printed in the md file
    writeLines(useBytes = TRUE,
               con = find_root_file("source/bookdown/habitatauteurs_paragrafen",
                                    sprintf("%s_%s.md",hoofdstuk,x),
                                    criterion =
                                      has_file("EU_rapportage_habitats.Rproj")))
    }
}
#save habitatauteurs to txt file to be able to put it on github.
auteurs <- readxl::read_excel(
  path = find_root_file("data/raw/habitatauteurs.xlsx",
                        criterion = has_file("EU_rapportage_habitats.Rproj")))
write_delim(auteurs,
            find_root_file("data/raw/habitatauteurs.txt",
                           criterion = has_file("EU_rapportage_habitats.Rproj")))
