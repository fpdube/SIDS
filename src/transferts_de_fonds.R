# transferts de fonds
# ====================

library(ProjectTemplate)
load.project()

transferts_complet <- WDI(country = "all", 
                          indicator = "GFDD.OI.13", 
                          start = 2010, 
                          end = 2020, 
                          cache = new_cache)

transferts_complet <- left_join(transferts_complet, states, by = "iso2c")
transferts_complet <- transferts_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
transferts_complet <- transferts_complet %>% filter(!is.na(lending))
transferts_complet$country.y <- NULL
transferts_complet <- transferts_complet %>% rename(country = country.x)

transferts  <-  transferts_complet %>% drop_na(.) 
transferts_restreint <- transferts %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail() %>%
  ungroup()

transferts_restreint %>% filter(SIDS == "SIDS") %>% group_by(income) %>% arrange(GFDD.OI.13) %>% View

