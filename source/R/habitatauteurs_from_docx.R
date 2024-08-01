library(protocolhelper)
library(tidyverse)
library(rprojroot)
library(parsermd)
library(googledrive)
library(jsonlite)
#library(synthesisr) might be interesting to find and remove duplicates from the
#however, they do not allow to keep the labels that the bib items already have.

#download the docx files from google drive
folder <- "https://drive.google.com/drive/folders/10cZDtfWFMglGF4ICvBzv6HXkfsqT3eP1"
folder_id <- drive_get(as_id(folder))
files <- drive_ls(folder_id) %>%
  filter(name != "literatuur")
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
      as_document() %>%
      str_replace_all("\\\\", "\\") %>% #otherwise, double backslash is printed in the md file
      str_replace_all("\\[", "[") %>%
      str_replace_all("\\]", "]") %>%
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

# download and merge all bibliography files
folder <- "https://drive.google.com/drive/folders/1iPCilJi0VX6qtMduInqvmR186N6YGffw"
folder_id <- drive_get(as_id(folder))
files <- drive_ls(folder_id)

if (!file.exists(
  find_root_file("data/raw/literatuur",
                 criterion = has_file("EU_rapportage_habitats.Rproj")))) {
  dir.create(find_root_file("data/raw/literatuur",
                            criterion =
                              has_file("EU_rapportage_habitats.Rproj")))
}

combined_bib <- ""

for (i in seq_along(files %>%
                    filter(str_detect(name, pattern = "\\.bib")) %>%
                    dplyr::pull(name))) {
  drive_download(
    as_id(files$id[i]),
    path =
      find_root_file("data/raw/literatuur", files$name[i],
                     criterion = has_file("EU_rapportage_habitats.Rproj")),
    overwrite = TRUE
  )
  fileCon <- file(
    find_root_file("data/raw/literatuur", files$name[i],
                   criterion = has_file("EU_rapportage_habitats.Rproj")))
  content <- readLines(fileCon)
  close(fileCon)

  combined_bib <- paste0(combined_bib, "\n", "\n", trimws(paste0(content, collapse="\n")))
}

cat(combined_bib,
    file = find_root_file("source", "bookdown", "combined_references.bib",
                          criterion = has_file("EU_rapportage_habitats.Rproj")),
    "\n")
