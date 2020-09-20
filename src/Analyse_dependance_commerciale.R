# Analyse de la dépendance commerciale des PEID

# ---- Tourisme ---- 
ggplot(tourisme_restreint) +
  aes(x = "", y = tourisme_xp, fill = SIDS) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Proportion of exports earned from tourism", 
       title = "A greater share of SIDS export revenues comes from tourism\nthan is the case for other countries in same income group ", 
       fill = "SIDS", 
       caption = "Source: Author's calculations from World Bank, World Development Indicators (2020)") +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))


# ---- Nourriture ----

# Tableau de comparaison: Proportion de la nourriture qui est importée (en % du PIB), selon le niveau de revenu
td1 <- nourriture_restreint %>% filter(SIDS == "Others") %>% group_by(income) %>% summarize(food_imp_others = mean(nourr_prop_pib))
td2 <- nourriture_restreint %>% filter(SIDS == "SIDS") %>% group_by(income) %>% summarize(food_imp_sids = mean(nourr_prop_pib))
imp_nourriture <- left_join(td1, td2)
imp_nourriture$ratio <- imp_nourriture$food_imp_sids / imp_nourriture$food_imp_others
rm(list = c("td1", "td2"))

# Histogramme
ggplot(nourriture_restreint) +
  aes(x = nourr_prop_pib, fill = SIDS, group = SIDS) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 10) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The median SIDS spends at least twice as much on\nfood imports as the median country in a comparable group", caption = "Source: World Bank, World Development Indicators (2020)") +
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))

# Boxplot
ggplot(nourriture_restreint) +
  aes(x = "", y = nourr_prop_pib, fill = SIDS, group = SIDS) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Food imports as a proportion of GDP (%)", 
       title = "The median SIDS spends at least twice as much on\nfood imports as the median country in a comparable group",
       caption = "Source: Author's calculations from World Bank, World Development Indicators (2020)") + 
  hrbrthemes::theme_ipsum() +
  facet_wrap(vars(income))