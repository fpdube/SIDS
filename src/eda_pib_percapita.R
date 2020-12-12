
# Tester si pib_complet est si complet que ça.

test <- expand_grid(pib_complet$iso3c, 1990:2019)
test <- test %>% rename(iso3c =`pib_complet$iso3c`, year = `1990:2019` )
test <- left_join(test, pib_complet[, c("iso3c", "year", "SIDS", "gdp_per_capita_growth")])

test %>% filter(SIDS == "Others") %>% plot_missing() # 5,8% manquant
test %>% filter(SIDS == "SIDS") %>% plot_missing() # 6,56% manquant 
rm(test)

# Conclusion: suffisamment complet

# Boxplot
ggplot(pib_complet) +
  aes(x = year, y = gdp_per_capita_growth, colour = SIDS) +
  geom_point(size = 1L, position = "jitter", alpha = 0.2) +
  geom_smooth(method = "loess") +
  scale_color_hue() +
  ylim(c(-10, 10)) +
  theme_minimal() +
  facet_grid(vars(income))

# Tableau
moy <- pib_complet %>% group_by(SIDS, income) %>% 
  summarize(mean(gdp_per_capita_growth, na.rm = TRUE))
ecart_type <-  pib_complet %>% group_by(SIDS, income) %>% 
  summarize(sd(gdp_per_capita_growth, na.rm = TRUE)) 
vol <- left_join(moy, ecart_type)

  

z <-  lm(gdp_per_capita_growth ~ SIDS, pib_complet, na.rm = TRUE)
summary(z)
