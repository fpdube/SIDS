library(ProjectTemplate)
load.project()

# ---- Préparation des données sur la pauvreté ----

pov_indic <- WDIsearch("Poverty headcount ratio at .*% of population)", cache = new_cache)
pov_indic <- as_tibble(pov_indic)
pov_indic <- pov_indic %>% filter(indicator %in% c("SI.POV.DDAY", "SI.POV.LMIC", "SI.POV.UMIC"))

WDIsearch('gdp.*growth.*')

# Pauvreté à divers niveaux, PPA 2011
pov_complet <- WDI(indicator = c("pov1.90" = "SI.POV.DDAY", 
                           "pov3.20" = "SI.POV.LMIC", 
                           "pov5.50" = "SI.POV.UMIC", 
                           "GDP_per_capita" = "NY.GDP.PCAP.PP.KD"),
             start = 1990, 
             year(today()), 
             extra = TRUE, 
             cache = new_cache)
pov_complet$capital <- NULL
pov_complet$longitude <- NULL
pov_complet$latitude <- NULL
pov_complet <- pov_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
pov_complet[pov_complet$lending == "Not classified", "lending"] <- NA

# On croise avec la liste des SIDS
pov_complet <-  full_join(x = pov_complet, y = sids, by = "iso3c")
z <- which(is.na(pov_complet$dac_sids))
pov_complet[z, "dac_sids"] <- FALSE
rm(z)
pov_complet <- pov_complet %>% rename(SIDS = dac_sids)
pov_complet[pov_complet$SIDS == TRUE, "SIDS"] <- "SIDS"
pov_complet[pov_complet$SIDS == FALSE, "SIDS"] <- "Others"
# Test: on a 32 SIDS
# pov_complet %>% group_by(SIDS) %>% summarize((n_distinct(iso3c)))

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

cache("pov_complet")
cache("pov")
cache("pov_restreint")

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

# ---- Préparation des données sur la croissance ----

# "NY.GDP.PCAP.KD.ZG"    "GDP per capita growth (annual %)"  

pib_complet <- WDI(country = "all", 
                   indicator = c("gdp__per_capita_growth" = "NY.GDP.PCAP.KD.ZG"),
    start = 1990, 
    year(today()), 
    extra = TRUE, 
    cache = new_cache)
pib_complet$capital <- NULL
pib_complet$longitude <- NULL
pib_complet$latitude <- NULL
pib_complet <- pib_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
pib_complet[pib_complet$lending == "Not classified", "lending"] <- NA

pib_complet <- left_join(pib_complet, pov_complet[, c("year", "iso3c", "SIDS", "label")], 
                         by = c("year", "iso3c"))

# Nettoyage
pib_complet$income <- factor(pib_complet$income, labels = c("Low Income", "Lower middle income", 
                                                            "Upper middle income", "High income"), 
                             ordered = TRUE)
pib_complet$lending <- factor(pib_complet$lending, levels = c("IDA", "Blend", "IBRD"), ordered = TRUE)
pib_complet$SIDS <- factor(pib_complet$SIDS, levels = c("SIDS", "Others"), ordered = FALSE)
pib_complet$region <- factor(pib_complet$region)

# Après avoir confirmé qu'il n'y a pas de situation où on n'a qu'un ou deux des trois mesures de pauvreté
# (on en 0 ou 3), et qu'il n'y en a que peu où on a les mesures de pauvreté mais que le PIB/hab manque, 
# on peut éliminer les NA.
pib <- pib_complet
pib <- pib %>% filter(income != "High income")
pib <- pib[complete.cases(pib), ]
cache("pib_complet")
cache("pib")


pib0 <- pib %>%
  ggplot() +
  aes(x = "", y = pib__per_capita_growth, colour = SIDS, group = SIDS) +
  geom_boxplot() +
  scale_fill_viridis(option = "D") +
  labs(x = "",
       y = "GDP per capita annual growth (%)", 
       title = "SIDS tend to have lower growth per capita than countries\nin the same income group", 
       subtitle = "This effect is strongest in the richer income groups", 
       caption = "World Bank, World Development Indicators (2020), latest year, indic. NY.GDP.PCAP.KD.ZG") +
  ylim(c(-10, 10)) +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))
pib0


# ---- Graphique sur le PIB ----

pib0 <- ggplot(data = pib_complet, 
             mapping = aes(x = pib__per_capita_growth, colour = SIDS), facet_wrap(lending)) + 
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
pib0

# ---- Préparation des données sur l'égalité femmes-hommes ----

gender_indic <- as_tibble(WDIsearch("*women.*index*", cache = new_cache))
gender_indic <- gender_indic %>% filter(indicator == "SG.LAW.INDX ") 

# WDI ne peut télécharger SG.LAW.INDEX: on fait donc une opération manuelle
# gender <- WDI(indicator = c("wbl_index" = "SG.LAW.INDX ", 
#                             "GDP_per_capita" = "NY.GDP.PCAP.PP.KD"),
#            start = 1990, 
#            year(today()), 
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


