library(protocolhelper)
library(tidyverse)
library(rprojroot)
library(parsermd)


paragrafen <- c("Inleiding", "Areaal", "Oppervlakte", "Regionale_toestand",
                "Drukken_bedreigingen", "Instandhoudingsmaatregelen",
                "Toekomstperspectieven", "Conclusies") # dit moet hetzelde zijn als de namen van de hoofdstukken, ook
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
