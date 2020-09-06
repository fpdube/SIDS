library(ProjectTemplate)
load.project()


# ---- Proportion de la population sous les différents seuils de la pauvreté ----

pov_indic <- WDIsearch("Poverty headcount ratio at .*% of population)", cache = new_cache)
pov_indic <- as_tibble(pov_indic)
pov_indic <- pov_indic %>% filter(indicator %in% c("SI.POV.DDAY", "SI.POV.LMIC", "SI.POV.UMIC"))

WDIsearch('gdp.*capita.*PPP')

# Pauvreté à 1,90$/j, PPA 2011
pov_complet <- WDI(indicator = c("pov_complet1.90" = "SI.pov_complet.DDAY", 
                           "pov_complet3.20" = "SI.pov_complet.LMIC", 
                           "pov_complet5.50" = "SI.pov_complet.UMIC", 
                           "GDP_per_capita" = "NY.GDP.PCAP.PP.KD"),
             start = 1990, 
             end = 2020, 
             extra = TRUE, 
             cache = new_cache)
pov_complet$capital <- NULL
pov_complet$longitude <- NULL
pov_complet$latitude <- NULL
pov_complet <- pov_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
pov_complet[pov_complet$lending == "Not classified", "lending"] <- NA

# On croise avec la liste des SIDS
pov_complet <-  full_join(x = pov_complet, y = sids, by = "iso3c")
pov_complet$SIDS <- !is.na(pov_complet$un)
pov_complet[pov_complet$SIDS == TRUE, "SIDS"] <- "SIDS"
pov_complet[pov_complet$SIDS == FALSE, "SIDS"] <- "Others"


# On nettoie
pov_complet$un <- NULL
pov_complet$name <- NULL
pov_complet$un.name.en <- NULL
pov_complet <- pov_complet %>% filter(!is.na(income))

# On crée un couple pays-année pour étiquetter le graphique et on ajuste les facteurs
pov_complet$label <- paste0("(", pov_complet$iso3c, ", ", pov_complet$year, ")")
pov_complet$income <- factor(pov_complet$income, labels = c("Low Income", "Lower middle income", 
                                            "Upper middle income", "High income"), 
                     ordered = TRUE)
pov_complet$lending <- factor(pov_complet$lending, levels = c("IDA", "Blend", "IBRD"), ordered = TRUE)
pov_complet$SIDS <- factor(pov_complet$SIDS, levels = c("SIDS", "Others"), ordered = FALSE)
pov_complet$region <- factor(pov_complet$region)

# Après avoir confirmé qu'il n'y a pas de situation où on n'a qu'un ou deux des trois mesures de pauvreté
# (on en 0 ou 3), et qu'il n'y en a que peu où on a les mesures de pauvreté mais que le PIB/hab manque, 
# on peut éliminer les NA.
pov <- pov_complet
pov <- pov %>% filter(income != "High income")
pov <- pov[complete.cases(pov), ]

# On crée un ensemble ne comportant que la mesure la plus récente pour chaque pays
pov_restreint <-  pov %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>% 
  ungroup()


# ---- Graphiques sur la distribution de la pauvreté ----

p0 <- ggplot(data = pov_restreint, mapping = aes(x = GDP_per_capita, y = pov3.20, colour = SIDS)) + 
  scale_x_log10() + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "glm") +
  xlab("GDP per capita, PPP (constant 2017 international $) (log scale)") +
  ylab("Percentage") +
  ggtitle("SIDS have slightly less income poverty at similar income per capita", 
          subtitle = "Proportion of the population living at $3.20 a day in developing countries") +
 # scale_colour_viridis(option = "C", discrete = TRUE) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_ipsum()
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

gender_complet <-  read_csv("./data/WBL50YearPanelDetailsWeb01Jun2020.csv")
gender_complet <- left_join(pov_complet, gender_complet)

# On nettoie
gender_complet$un <- NULL
gender_complet$name <- NULL
gender_complet$un.name.en <- NULL
gender_complet <- gender_complet %>% filter(!is.na(income))
gender_complet$pov1.90 <- NULL
gender_complet$pov3.20 <- NULL
gender_complet$pov5.50 <- NULL

# On ne garde que les cas complets, après avoir sauvegardé
gender <- gender_complet
gender <- gender[complete.cases(gender), ]

# On crée un ensemble ne comportant que la mesure la plus récente pour chaque pays
gender_restreint <-  gender %>%
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>%
  ungroup()
gender_restreint_sids <- gender_restreint %>% filter(SIDS == "SIDS")


# ---- Graphiques EFH ----

g0 <- ggplot(data = gender_restreint, mapping = aes(x = GDP_per_capita, y = wbl_index, colour = SIDS)) + 
  scale_x_log10() + 
  geom_point(alpha = 1) + 
  geom_point(data = gender_restreint_sids) +
  geom_smooth(method = "lm") +
  xlab("GDP per capita, PPP (constant 2017 international $) (log scale)") +
  ylab("Women, Business, and the Law Index (WB)") +
  ggtitle("SIDS tend to have lower scores on gender equality at similar income...", 
          subtitle = "... but their relatively poor perfomance reverses as they become richer") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_ipsum()
g0


