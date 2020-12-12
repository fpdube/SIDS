# telecharger_bm
# ==============

# Cette bibliothèque procure des fonctions de base pour télécharger des données
# de la base de données WDI de la Banque mondiale. Elle permet aussi de les
# croiser avec une base de données de petits États insulaires en développement
# (provenant du WDI).

# Attention: décommenter ce qui suit à l'intérieur du projet actuel amène
# le projet à se charger lui-même, d'où récursivité.
# library(ProjectTemplate)
# load.project()
# library(wbstats)
# library(countrycode)
# library(tidyverse)
# library(DataExplorer) # pour plot_missing()

telech_bm <- function(indic, debut = 2010, fin = year(today()) - 1, cache = wb_cache, lang = "en") {
  wb_data(country = "countries_only", 
      indicator = c(indic, 
                    "GDP_per_capita_ppp" = "NY.GDP.PCAP.PP.CD",
                    "GDP_per_capita_USD" = "NY.GDP.PCAP.CD",
                    "GDP" = 	"NY.GDP.MKTP.CD", 
                    "GDP_ppp" = "NY.GDP.MKTP.PP.CD", 
                    "GDP_2017" = "NY.GDP.MKTP.KD",
                    "GPD_ppp_2017" = "NY.GDP.PCAP.PP.KD",
                    "pop" = "SP.POP.TOTL"), 
      freq = "Y",
      start_date = debut, 
      end_date = fin,
      return_wide = TRUE,
      cache = cache,
      lang = lang
      )
}

telech_bm_restreint <- function(indic, cache = wb_cache, lang = "en") {
  wb_data(country = "countries_only", 
          indicator = c(indic, 
                        "GDP_per_capita" = "NY.GDP.PCAP.PP.CD",
                        "pop" = "SP.POP.TOTL"), 
          # Most Recent Values to return, in replacement to start_date/end_date
          mrv = 5, 
          return_wide = TRUE,
          cache = cache,
          lang = lang
          )
}

fusionner_peid <- function(td) {
  left_join(td, select(peid, -country), by = "iso2c")
}

restreindre <- function(td = .) {
  group_by(.data = td, country) %>% 
    arrange(date) %>% 
    slice_tail() %>%
    ungroup()
}

