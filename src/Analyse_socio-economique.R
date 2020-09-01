library(ProjectTemplate)
load.project()

# ---- Proportion de la population sous les différents seuils de la pauvreté ----

pov <- WDIsearch("Poverty headcount ratio at .*% of population)", cache = new_cache)
pov <- as_tibble(pov)
pov <- pov %>% filter(indicator %in% c("SI.POV.DDAY", "SI.POV.LMIC", "SI.POV.UMIC"))

