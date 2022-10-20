trend_translate <- data.frame(trendeng = c("unknown", "uncertain", "decreasing",
                                           "increasing", "stable",
                                           "deteriorating", "improving"),
                    trendnl = c("onbekend", "onzeker", "\U2198", "\U2197", "=",
                                "verslechterend", "verbeterend"),
                    trendlegende = c("onbekend", "onzeker", "afname",
                                     "toename", "stabiel",
                                     "verslechterend", "verbeterend"))
kleurenpallet <- data.frame(
  waarde = c("onbekend", "onzeker", "\U2198", "afname", "\U2197", "toename",
             "=", "stabiel", "U2 zeer ongunstig", "U1 matig ongunstig",
             "FV gunstig", "goed", "slecht", "matig", "verslechterend",
             "verbeterend"),
  kleur = c("lightgrey", "lightgrey", "#7E1C00", "#7E1C00", "#65E060",
            "#65E060", "#B56024", "#B56024", "#7E1C00", "#B56024", "#65E060",
            "#65E060", "#7E1C00", "#B56024", "#7E1C00", "#65E060"))

status_translate <- data.frame(
  statuseng = c("Unknown (XX)", "unknown", "uncertain", "Bad (U2)",
                "Inadequate (U1)", "Favourable (FV)", "Good"),
  statusnl = c("onbekend", "onbekend", "onzeker", "U2 zeer ongunstig",
               "U1 matig ongunstig", "FV gunstig", "goed"))

toekomst_translate <- data.frame(
  toekomsteng = c("Unknown", "Bad", "Poor", "Good"),
  toekomstnl = c("onbekend", "slecht", "matig", "goed"))

frr_translate <- data.frame(
  symbool = c("≈", ">", ">>", "<", "<<"),
  frr_areaal = c("FRR ongeveer gelijk aan actueel areaal",
                 "FRR groter (≤ 10\\%) dan actueel areaal",#dubbele \ om latex problemen te voorkomen
                 "FRR meer dan 10\\% groter dan actueel areaal",
                 "FRR kleiner (≤ 10\\%) dan actueel areaal",
                 "FRR meer dan 10\\% kleiner dan actueel areaal"))
fra_translate <- data.frame(
  symbool = c("≈", ">", ">>", "<", "<<"),
  fra_area = c("FRA ongeveer gelijk aan actueel areaal",
                 "FRA groter (≤ 10\\%) dan actueel areaal",#dubbele \ om latex problemen te voorkomen
                 "FRA meer dan 10\\% groter dan actueel areaal",
                 "FRA kleiner (≤ 10\\%) dan actueel areaal",
                 "FRA meer dan 10\\% kleiner dan actueel areaal"))

#pressures & threats categories uit https://docs.google.com/spreadsheets/d/1InQHG_ZJUqSVjSA1VyyJd9qlQGtF5Kui/edit#gid=1546634614

translate_pt <- data.frame(code = c('A','B','C','D','E','F', 'G','H','I','J','K','L','M','N','X'),
           categorie = c("Agriculture", "Forestry",
                         "Extraction of resources (minerals, peat, non-renewable energy resources)",
                         "Energy production processes and related infrastructure development",
                         "Development and operation of transport systems",
                         "Development, construction and use of residential, commercial, industrial and recreational infrastructure and areas.",
                         "Extraction and cultivation of biological living resources (other than agriculture and forestry)",
                         "Military action, public safety measures, and other human intrusions",
                         "Alien and problematic species",
                         "Mixed source pollution",
                         "Human-induced changes in water regimes",
                         "Natural processes (excluding catastrophes and processes induced by human activity or climate change)",
                         "Geological events, natural catastrophes",
                         "Climate change",
                         "Unknown pressures, no pressures and pressures from outside the Member State"))



legende_caption <- function(trend = NA, FRR_areaal = NA, FRA_area = NA){
  trendlegende <- NA
  frrlegende <- NA
  fralegende <- NA
  if (!is.null(trend)) {
    d <- data.frame(trendeng = trend) %>% distinct() %>%#input = wat er in de fiches staat
      filter(!(trendeng %in% c("unknown", "uncertain"))) %>%
      left_join(trend_translate)
    trendlegende <- str_c(d$trendnl, d$trendlegende, sep = " ", collapse = ", ")
    trendlegende <- str_c("trend: ", trendlegende)
  }
  if (!is.null(FRR_areaal)) {
    d <- data.frame(symbool = FRR_areaal) %>% distinct() %>%#input = wat er in de fiches staat
      left_join(frr_translate)
    frrlegende <- str_c(d$symbool, d$frr_areaal, sep = " ", collapse = ", ")
    frrlegende <- str_c("FRR: ", frrlegende)
  }
  if (!is.null(FRA_area)) {
    d <- data.frame(symbool = FRA_area) %>% distinct() %>%#input = wat er in de fiches staat
      left_join(fra_translate)
    fralegende <- str_c(d$symbool, d$fra_area, sep = " ", collapse = ", ")
    fralegende <- str_c("FRA: ", fralegende)
  }
  legende <- c(trendlegende, frrlegende, fralegende)
  legende <- legende[!is.na(legende)]
  if (length(legende) > 1) {
    legende <- str_c(legende, collapse = "; ")
  }
  return(legende)
}
