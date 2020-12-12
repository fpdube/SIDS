# Exploration de la dépendance commerciale des PEID

# ---- Tourisme ---- 
tourisme  <-  tourisme_complet %>% drop_na(.) 
tourisme_restreint <- tourisme %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>%
  ungroup()

titres <- labs(y = "Proportion of exports earned from tourism", 
     title = "A greater share of SIDS export revenues comes from tourism\nthan is the case for other countries in same income group ", 
     caption = "Source: WDI (2020), indicator ST.INT.RCPT.XP.ZS, latest year available", 
     fill = "")
nom_fichier <- "SIDS_tourisme.png"

graphe <- ggplot(tourisme_restreint) +
  aes(x = "", y = tourisme_xp, fill = SIDS) +
  geom_boxplot() +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  titres +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))

ggsave(graphe, filename = nom_fichier, 
       path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")

# ---- Nourriture ----

# Tableau de comparaison: Proportion de la nourriture qui est importée (en % du PIB), selon le niveau de revenu
td1 <- nourriture_restreint %>% filter(SIDS == "Others") %>% group_by(income) %>% summarize(food_imp_others = mean(nourr_prop_pib))
td2 <- nourriture_restreint %>% filter(SIDS == "SIDS") %>% group_by(income) %>% summarize(food_imp_sids = mean(nourr_prop_pib))
imp_nourriture <- left_join(td1, td2)
imp_nourriture$ratio <- imp_nourriture$food_imp_sids / imp_nourriture$food_imp_others
rm(list = c("td1", "td2"))

# Boxplot
nom_fichier <- "SIDS_imp_nourriture.png"

graphe <- ggplot(nourriture_restreint) +
  aes(x = "", y = nourr_prop_pib, fill = SIDS, group = SIDS) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Food imports as a proportion of GDP (%)", 
       title = "The median SIDS spends at least twice as much on food imports\nas the median country in its comparable group",
       caption = "Source: Author's calculations from WDI (2020), indicators NY.GDP.MKTP.CD,\nTM.VAL.MRCH.CD.WT, and TM.VAL.FOOD.ZS.UN, latest year available", 
       fill = "") + 
  ylim(0, 15) +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))

ggsave(graphe, filename = nom_fichier, 
      path = "./plots", height = 12, width = 18, units = "cm", dpi = "print")
