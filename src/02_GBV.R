

gbv_complet <- WDI(country = "all", indicator = c("prop_women_subjected_violence_12mo" = "SG.VAW.1549.ZS", 
                                   "spousal_sexual_violence_12mo" = "SG.VAW.MARR.ZS"),
    start = 2010,
    end = 2020, 
    extra = FALSE,
    cache = new_cache)

gbv_complet <- left_join(gbv_complet, peid, by = "iso2c")
gbv_complet <- gbv_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
gbv_complet$country.y <- NULL
gbv_complet <- gbv_complet %>% rename(country = country.x)

gbv  <-  gbv_complet %>% drop_na(.) 
gbv_restreint <- gbv %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>%
  ungroup()

titres <- labs(y = "Proportion of women subjected to physical or sexual\nviolence in the past 12 mo", 
     title = "GBV is particularly accute in the Pacific...", 
     subtitle = "... and, to a lesser extend, in the Caribbean (2010-2017)", 
     caption = "Source: Word Devlelopment Indicators (2020), most recent year, indic. SG.VAW.MARR.ZS",
     fill = "") 
nom_fichier <- "SIDS_GBV.png"

graphe <- ggplot(gbv_restreint) +
 aes(x = "", y = prop_women_subjected_violence_12mo, fill = SIDS, group = SIDS) +
 geom_boxplot() +
 scale_fill_brewer(palette = "Paired") +
  ylim(0, 40) +
  titres +
 facet_wrap(vars(region)) +
  hrbrthemes::theme_ipsum()

ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")
