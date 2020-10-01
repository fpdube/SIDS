# Corruption -- Analyse
# ======================

# ---- Tableaux simplifiés ----

corruption <- corruption_complet %>% drop_na()

corruption_restreint  <- corruption %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>% 
  ungroup()

corr_sids <- corruption_restreint %>% filter(SIDS == "SIDS")

# ---- Graphique corruption ----

# gg_theme_FP <- ggplot() +
#   scale_color_brewer(palette = "Set1") +
#   scale_x_continuous(labels = scales::comma) +
#   hrbrthemes::theme_ipsum_ps()

corruption_restreint_peid <- corruption_restreint %>% filter(SIDS == "SIDS")

graphe <- ggplot(corruption_restreint) +
  aes(x = gdp_pc, y = control_corr, colour = SIDS) +
  geom_point(size = 1L) +
  geom_smooth(span = 0.75) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  labs(x = "GDP per capita, current USD (log scale)", 
       y = "Control of corruption estimate (WGI)", 
       title = "At equal revenue, SIDS tend to be less corrupt", 
       caption = "Source: WDI (2020), indicators CC.EST and NY.GDP.PCAP.CD, latest year available", 
       color = "") +
  #  ggrepel::geom_text_repel(data = corruption_restreint_peid, aes(label = iso2c)) +
  hrbrthemes::theme_ipsum_ps()

nom_fichier <- "SIDS_corruption.png"
ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")

