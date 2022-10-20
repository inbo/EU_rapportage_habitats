# Een lijst met alle habitattypes uit n2khab
habitattypes <- read_types(lang = "nl") %>%
  mutate(id = ifelse(typeclass == "CH",
                     "zilt",
                     ifelse(typeclass %in% c("ID", "HS", "SS"),
                            "heide",
                            ifelse(typeclass == "CD",
                                   "kustduin",
                                   ifelse(typeclass == "FW",
                                          "water",
                                          ifelse(typeclass == "GR",
                                                 "gras",
                                                 ifelse(typeclass == "BMF",
                                                        "veen",
                                                        ifelse(typeclass == "RC",
                                                               "rots",
                                                               "bos"))))))),
         id = as.factor(id))

