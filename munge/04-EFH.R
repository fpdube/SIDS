# ---- Préparation des données sur l'égalité femmes-hommes ----

gender_indic <- as_tibble(WDIsearch("*women.*index*", cache = new_cache))
gender_indic <- gender_indic %>% filter(indicator == "SG.LAW.INDX ") 

# WDI ne peut télécharger SG.LAW.INDEX: on fait donc une opération manuelle plus bas

gender_complet <-  read_csv("./data/WBL50YearPanelDetailsWeb01Jun2020.csv")
gender_complet <- left_join(gender_complet, peid)

# On fusionne avec PIB p.c.
gender_complet <- pib_complet %>% 
  select(iso2c, year, GDP_per_capita_curr, GDP_per_capita_konst) %>% 
  right_join(gender_complet, ., by = c("iso2c", "year"))
  
cache("gender_complet")




