# Trade Dependency - Munging
# ==========================

# trade_complet <- trade_original 
# trade_complet <- left_join(trade_complet, peid, by = "country")
# 
# trade_complet <- trade_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
# 
# # Nettoyage
# trade_complet$income <- factor(trade_complet$income, labels = c("Low Income", "Lower middle income", 
#                                                             "Upper middle income", "High income"), 
#                              ordered = TRUE)
# trade_complet$lending <- factor(trade_complet$lending, levels = c("IDA", "Blend", "IBRD"), ordered = TRUE)
# trade_complet$SIDS <- factor(trade_complet$SIDS, levels = c("SIDS", "Others"), ordered = FALSE)
# trade_complet$region <- factor(trade_complet$region)


# ---- tourisme ----

# "ST.INT.RCPT.CD"           "International tourism, receipts (current US$)"                                  
# "ST.INT.RCPT.XP.ZS"        "International tourism, receipts (% of total exports)"  

tourisme_complet <- WDI(country = "all", indicator = c("tourisme_usd" = "ST.INT.RCPT.CD", 
                                   "tourisme_xp" = "ST.INT.RCPT.XP.ZS"),
    start = 1990, 
    end = 2020,
    extra = FALSE,
    cache = new_cache)

tourisme_complet <- left_join(tourisme_complet, peid, by = "iso2c")
tourisme_complet <- tourisme_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
tourisme_complet <- tourisme_complet %>% filter(!is.na(lending))
tourisme_complet$country.y <- NULL
tourisme_complet <- rename(tourisme_complet, country = country.x)

cache("tourisme_complet")



# ---- Nourriture ----
# NY.GDP.MKTP.CD	GDP (current US$)
# NE.IMP.GNFS.CD	Imports of goods and services (current US$)
# TM.VAL.ENGY.CD.WB	POL and other energy imports (current US$)
# TM.VAL.MRCH.CD.UN	Merchandise imports (UN, current US$)
# TM.VAL.MRCH.CD.WT	Merchandise imports, WTO (current US$)

nourriture_complet <- WDI(country = "all", indicator = c("nourriture_prop_marchandises" = "TM.VAL.FOOD.ZS.UN",
                                                         "marchandises_imp_usd" = "TM.VAL.MRCH.CD.WT",
                                                       "pib_usd" = "NY.GDP.MKTP.CD"),
                        start = 1990, 
                        end = 2020,
                        extra = FALSE,
                        cache = new_cache)

nourriture_complet <- left_join(nourriture_complet, peid, by = "iso2c")
nourriture_complet <- nourriture_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
nourriture_complet <- nourriture_complet %>% filter(!is.na(lending))
nourriture_complet$country.y <- NULL
nourriture_complet <- rename(nourriture_complet, country = country.x)


# On calcule la proportion des importations de nourriture sur le PIB
nourriture_complet <- nourriture_complet %>% 
  mutate(nourr_prop_pib = (nourriture_prop_marchandises) * marchandises_imp_usd / pib_usd)


nourriture  <-  nourriture_complet %>% drop_na(.) 
nourriture_restreint <- nourriture %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>%
  ungroup()

# --- Énergie ----

# [À FAIRE]
