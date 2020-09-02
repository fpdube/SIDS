library(ProjectTemplate)
load.project()


# ---- Proportion de la population sous les différents seuils de la pauvreté ----

pov_indic <- WDIsearch("Poverty headcount ratio at .*% of population)", cache = new_cache)
pov_indic <- as_tibble(pov_indic)
pov_indic <- pov_indic %>% filter(indicator %in% c("SI.POV.DDAY", "SI.POV.LMIC", "SI.POV.UMIC"))

WDIsearch('gdp.*capita.*PPP')

# Pauvreté à 1,90$/j, PPA 2011
pov <- WDI(indicator = c("pov1.90" = "SI.POV.DDAY", 
                           "pov3.20" = "SI.POV.LMIC", 
                           "pov5.50" = "SI.POV.UMIC", 
                           "GDP_per_capita" = "NY.GDP.PCAP.PP.KD"),
             start = 1990, 
             end = 2020, 
             extra = TRUE, 
             cache = new_cache)
pov$capital <- NULL
pov$longitude <- NULL
pov$latitude <- NULL
pov <- pov %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
pov[pov$lending == "Not classified", "lending"] <- NA

# Après avoir confirmé qu'il n'y a pas de situation où on n'a qu'un ou deux des trois mesures de pauvreté
# (on en 0 ou 3), et qu'il n'y en a que peu où on a les mesures de pauvreté mais que le PIB/hab manque, 
# on peut éliminer les NA.
pov <- pov[complete.cases(pov), ]

# On croise avec la liste des SIDS
pov <-  full_join(x = pov, y = sids, by = "iso3c")
pov$sids <- !is.na(pov$un)

# On nettoie
pov$un <- NULL
pov$name <- NULL
pov$un.name.en <- NULL
pov <- pov %>% filter(!is.na(income))

# On crée un couple pays-année pour étiquetter le graphique
pov$label <- paste0("(", pov$iso3c, ", ", pov$year, ")")

# On crée trois sous-ensembles: lic, lmic, umic
lic <- pov %>% filter(income == "Low income")
lmic <- pov %>% filter(income == "Lower middle income")
umic <- pov %>% filter(income == "Upper middle income")

# ---- Graphiques sur la distribution de la pauvreté ----

ggplot(data = pov, mapping = aes(x = GDP_per_capita, y = pov3.20, colour = sids)) + 
  scale_x_log10() + geom_point(alpha = 0.2) + 
  xlab("GDP per capita, PPP (constant 2017 international $)") +
  ylab("Percentage of the population living at $3.20 a day (2011 PPP)") +
  facet_wrap(vars(income)) +
  geom_smooth()
  



  
       