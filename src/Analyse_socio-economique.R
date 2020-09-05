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

# On croise avec la liste des SIDS
pov <-  full_join(x = pov, y = sids, by = "iso3c")
pov$SIDS <- !is.na(pov$un)
pov[pov$SIDS == TRUE, "SIDS"] <- "SIDS"
pov[pov$SIDS == FALSE, "SIDS"] <- "Others"


# On nettoie
pov$un <- NULL
pov$name <- NULL
pov$un.name.en <- NULL
pov <- pov %>% filter(!is.na(income))

# On crée un couple pays-année pour étiquetter le graphique
pov$label <- paste0("(", pov$iso3c, ", ", pov$year, ")")

# Après avoir confirmé qu'il n'y a pas de situation où on n'a qu'un ou deux des trois mesures de pauvreté
# (on en 0 ou 3), et qu'il n'y en a que peu où on a les mesures de pauvreté mais que le PIB/hab manque, 
# on peut éliminer les NA.
pov_complet <- pov
pov <- pov %>% filter(income != "High income")
pov <- pov[complete.cases(pov), ]

# On crée un ensemble ne comportant que la mesure la plus récente pour chaque pays
pov_restreint <-  pov %>% group_by(country) %>% arrange(year) %>% slice_tail()


# ---- Graphiques sur la distribution de la pauvreté ----

p0 <- ggplot(data = pov_restreint, mapping = aes(x = GDP_per_capita, y = pov3.20, colour = SIDS)) + 
  scale_x_log10() + 
  geom_point(alpha=0.5) + 
  geom_text_repel(aes(label = label), data = pov_restreint_sids) +
  geom_smooth(method = "glm") +
  xlab("GDP per capita, PPP (constant 2017 international $) (log scale)") +
  ylab("Percentage") +
  ggtitle("SIDS have slightly less income poverty at similar income per capita", 
          subtitle = "Proportion of the population living at $3.20 a day in developing countries") +
  theme_minimal()
p0


  # ---- Égalité entre les sexes ----

gender_indic <- as_tibble(WDIsearch("*women.*index*", cache = new_cache))
gender_indic <- gender_indic %>% filter(indicator == "SG.LAW.INDX ") 

# WDI ne peut télécharger SG.LAW.INDEX: on fait donc une opération manuelle
# gender <- WDI(indicator = c("wbl_index" = "SG.LAW.INDX ", 
#                             "GDP_per_capita" = "NY.GDP.PCAP.PP.KD"),
#            start = 1990, 
#            end = 2020, 
#            extra = TRUE, 
#            cache = new_cache)
# gender$capital <- NULL
# gender$longitude <- NULL
# gender$latitude <- NULL
# gender <- gender %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
# gender[gender$lending == "Not classified", "lending"] <- NA

gender <-  read_csv("./data/WBL50YearPanelDetailsWeb01Jun2020.csv")
gender <- left_join(pov_complet, gender)

# On nettoie
gender$un <- NULL
gender$name <- NULL
gender$un.name.en <- NULL
gender <- gender %>% filter(!is.na(income))
gender$pov1.90 <- NULL
gender$pov3.20 <- NULL
gender$pov5.50 <- NULL

# On ne garde que les cas complets, après avoir sauvegardé
gender_complet <- gender
gender <- gender[complete.cases(gender), ]

# On crée un ensemble ne comportant que la mesure la plus récente pour chaque pays
gender_restreint <-  gender %>% group_by(country) %>% arrange(year) %>% slice_tail()
gender_restreint_sids <- gender_restreint %>% filter(SIDS == "SIDS")
# ---- Graphiques EFH ----

g0 <- ggplot(data = gender_restreint, mapping = aes(x = GDP_per_capita, y = wbl_index, colour = SIDS)) + 
  scale_x_log10() + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = label), data = gender_restreint_sids) +
  xlab("GDP per capita, PPP (constant 2017 international $) (log scale)") +
  ylab("Women, Business, and the Law Index (WB)") +
  ggtitle("SIDS tend to lower scores on gender equality at similar income per capita...", 
          subtitle = "but their relatively poor perfomance reverses as they become richer") +
  theme_minimal()
g0


