# Préparation des données sur la pauvreté et le PIB
# =================================================

# --- Pauvreté ----
pauv_indic <- WDIsearch("Poverty headcount ratio at .*% of population)", cache = new_cache)
pauv_indic <- as_tibble(pauv_indic)
pauv_indic <- pauv_indic %>% filter(indicator %in% c("SI.POV.DDAY", "SI.POV.LMIC", "SI.POV.UMIC"))

WDIsearch('gdp.*growth.*')

# Pauvreté à divers niveaux, PPA 2011
pauv_complet <- WDI(
  country = "all",
  indicator = c(
    "pauv1.90" = "SI.POV.DDAY",
    "pauv3.20" = "SI.POV.LMIC",
    "pauv5.50" = "SI.POV.UMIC",
    "GDP_per_capita" = "NY.GDP.PCAP.PP.CD"
  ),
  start = 1990,
  year(today()),
  extra = FALSE,
  cache = new_cache
)

pauv_complet <- pauv_complet %>% select(-country) %>% left_join(., peid, by = "iso2c")
pauv_complet <- pauv_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
pauv_complet <- pauv_complet %>% filter(!is.na(lending))

pauv_complet$label <- paste0("(", pauv_complet$iso3c, ", ", pauv_complet$year, ")")

cache("pauv_complet")

# ---- Croissance du PIB par habitant ---- 

# "NY.GDP.PCAP.KD.ZG"    "GDP per capita growth (annual %)"  

pib_complet <- WDI(country = "all", 
                   indicator = c("gdp_per_capita_growth" = "NY.GDP.PCAP.KD.ZG",
                                 "GDP_per_capita_curr" = "NY.GDP.PCAP.PP.CD", 
                                 "GDP_per_capita_konst" = "NY.GDP.PCAP.PP.KD"),
                   start = 1990, 
                   year(today()), 
                   extra = FALSE, 
                   cache = new_cache)

pib_complet <- pib_complet %>% 
  left_join(., select(peid, -country), by = "iso2c")

pib_complet <- pib_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays

pib_complet$label <- paste0("(", pib_complet$iso3c, ", ", pib_complet$year, ")")

cache("pib_complet")



