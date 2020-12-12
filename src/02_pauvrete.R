library(ProjectTemplate)
load.project()

# ---- Préparation des données sur la pauvreté ----

pauv <- pauv_complet
pauv <- pauv %>% filter(income != "High income")
pauv <- pauv[complete.cases(pauv), ]

# On crée un ensemble ne comportant que la mesure la plus récente pour chaque pays
pauv_restreint <-  pauv %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>% 
  ungroup()

# ---- Graphiques sur la distribution de la pauvreté ----

graphe <- ggplot(pauv_restreint) +
  aes(x = GDP_per_capita, y = pauv3.20, colour = SIDS) +
        geom_point(size = 1L) +
        geom_smooth(span = 0.75) +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(trans = "log10", labels = scales::comma) +
        labs(x = "GDP per capita, PPP (constant 2017 international $) (log scale)", 
             y = "Percentage of the pop. living with $3.20/day or less", 
             title = "Fewer poor in SIDS at same income levels", 
             caption = "Source: WDI (2020), indicators SI.POV.LMIC and NY.GDP.PCAP.PP.CD, latest year available", 
             color = "") +
        hrbrthemes::theme_ipsum_ps()

nom_fichier <- "SIDS_pauvrete3.20.png"
ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")
