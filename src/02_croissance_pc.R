# Croissance du PIB -- Analyse
# =============================

# ---- Tableaux simplifiés ----

pib <- pib_complet
pib <- pib %>% filter(income != "High income")
pib <- pib[complete.cases(pib), ]



# ---- Graphiques ----


titres <- labs(x = "", 
               y = "GDP per capita annual growth (%)", 
               title = "SIDS' growth tend to be lower than non-SIDS' in same income group", 
               caption = "Source: WDI (2020), indicator NY.GDP.PCAP.KD.ZG, latest year available", 
               color = "")
nom_fichier <- "SIDS_growth_pc.png"

graphe <- ggplot(pib) +
  aes(x = "", y = gdp_per_capita_growth, colour = SIDS, group = SIDS)  +
    geom_boxplot() +
  scale_color_brewer(palette = "Set1") +
  titres +
  ylim(c(-10, 10)) +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))

ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")
