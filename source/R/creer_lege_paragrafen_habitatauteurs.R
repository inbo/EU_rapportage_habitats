#Voor ieder van de habitattypes en voor ieder van de paragrafen wordt er een md file gecreerd. Enkel de referenties naar tabellen en figuren uit het hoofdstuk worden getoond.
library(tidyverse)
library(rprojroot)


# maak instructies met de handles voor alle figuren en tabellen
for (id in c("zilt", "kustduin", "water", "heide", "gras", "veen",
             "rots", "bos")) {
  for (paragraaf in c("instructies")) {
    rmarkdown::render(
      find_root_file("source/r/creer_lege_paragrafen_habitatauteurs_instructies.Rmd",
                     criterion = has_file("EU_rapportage_habitats.Rproj")),
      output_file = find_root_file("source/bookdown/habitatauteurs_paragrafen",
                                   paste0(id, "_", paragraaf, '.pdf'),
                                   criterion =
                                     has_file("EU_rapportage_habitats.Rproj")),
      params = list(id = id,
                    paragraaf = paragraaf)
    )
  }
}
