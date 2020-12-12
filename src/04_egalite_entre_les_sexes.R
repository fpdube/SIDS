# Analyse égalité entre les sexes
# ===============================

# ---- Tableau simplifié ----

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
titres <- labs(x = "GDP per capita, PPP (constant 2017 international $) (log scale)",
  y = "Women, Business, and the Law Index (WB)",
  title = "SIDS tend to have lower scores on GE at similar income...",
  subtitle = "... but they converge as they become richer",
  caption = "Source: WDI (2020), indicator NY.GDP.PCAP.PP.KD, and\nWomen, Business, and the Law (2020), latest year available",
  colour = ""
)
nom_fichier <- "SIDS_EFH.png"

graphe <- ggplot(data = gender_restreint, mapping = aes(x = GDP_per_capita_konst, y = wbl_index, colour = SIDS)) + 
  geom_point(size = 1L) +
  geom_smooth(span = 0.75) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  titres +
  hrbrthemes::theme_ipsum_ps()

ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")
