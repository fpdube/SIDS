# Ce fichier crée un tableau des pays et des groupements régionaux auxquels ils
# appartiennent, que ce soit à l'ONU, à l'OCDE ou à la BM.


# ---- Faire un tableau à partir des PEID du CAD et des pays de la BM ----

# Téléchargement de la liste des indicateurs de la BM, qu'on met en cache.
cache_wdi <- WDIcache() # À éliminer progressivement
cache_wbstats <- wbstats::wb_cache()
cache("wdi_cache")
cache("cache_wbstats")

# On télécharge un indicateur bidon de la BM et on ne garde que les descripteurs
pays <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", start = 2019, end = 2019, extra = TRUE, cache = cache_wdi)
pays <- pays %>% select(country, 
                        iso2c,
                        iso3c,
                        region,
                        income,
                        lending)

# Enlever les agrégats et ne garder que les pays
pays <- pays %>% filter(region != "Aggregates") 

# On fait des colonnes de facteurs 
pays$income <- factor(pays$income, ordered = FALSE)
pays$income <- ordered(pays$income, levels = c("Low income",
                                               "Lower middle income", 
                                               "Upper middle income", 
                                               "High income"))        

pays$lending <- factor(pays$lending, levels = c("IDA", 
                                                "Blend", 
                                                "IBRD", 
                                                "Not classified"), 
                       ordered = FALSE)
pays$region <- factor(pays$region)


# ---- Sous-ensemble des PEID du Comité d'aide au développement (OCDE) ----
DAC_SIDS <- c(
  "COM", 
  "GNB",
  "STP", 
  "CPV", 
  "MDV",
  "MUS", 
  "SYC", 
  "HTI", 
  "GUY", 
  "ATG", 
  "BLZ", 
  "CUB", 
  "DMA", 
  "DOM",
  "GRD", 
  "JAM", 
  "MSR", 
  "LCA", 
  "VCT",
  "SUR", 
  "KIR", 
  "SLB", 
  "SLB", 
  "TLS",
  "TUV", 
  "VUT", 
  "FSM", 
  "PNG", 
  "WSM", 
  "COK",
  "FJI",
  "MHL",
  "NRU", 
  "NIU", 
  "PLW", 
  "TON"
)

pays <- pays %>% 
  mutate(SIDS = (if_else(iso3c %in% DAC_SIDS, "SIDS", "Non-SIDS", missing = NA_character_)))

pays$SIDS <- factor(pays$SIDS, levels = c("SIDS", "Non-SIDS"), ordered = FALSE)


# ---- Préparation de la classification de l'ONU ---- 

# La classification des PMA, des PEID et des pays enclavés de l'ONU réside dans
# un fichier Excel. À noter: la CNUCED et l'UNOHRLLS ont chacun la leur, qui
# comporte respectivement 27 pays et 56 pays.

url <- "https://unctadstat.unctad.org/EN/Classifications/MemoItems_DevelopmentStatus_Hierarchy.xls"
destfile <- "./tmp/dev_status.xls"
download.file(url = url, destfile = destfile)
z<- read_excel(destfile, skip = 4)
z$Code <- as.numeric(z$Code)

which(z$Code > 1000) # Les codes plus grands que 1000 dénotent des regroupements de pays
# Voyons ces regroupements
z %>% filter(Code > 1000)

# PEID: liste de la CNUCED --  vecteur de codes iso3c
sids_unctad <- z[(which(z$Code == 2231) + 1):(which(z$Code == 2250) - 1) , ]
sids_unctad <- sids_unctad %>% filter(Code < 1000)
sids_unctad <- countrycode(sids_unctad$Code, origin = "un", destination =  "iso3c")

# PEID: liste de UNOHRLLS -- vecteur de codes iso3c
sids_unohrlls <- z[(which(z$Code == 2250) + 1):dim(z)[1], ]
sids_unohrlls <- sids_unohrlls %>% filter(Code < 1000)
sids_unohrlls <- countrycode(sids_unohrlls$Code, origin = "un", destination = "iso3c")

# Pays sans littoral -- vecteur de codes iso3c
lldc <- z[(which(z$Code == 2220) + 1):(which(z$Code == 2230) - 1) , ]
lldc <- lldc %>% filter(Code < 1000)
lldc <- countrycode(lldc$Code, origin = "un", destination =  "iso3c")

# Pays les moins avancés
ldc <- z[(which(z$Code == 2211) + 1):(which(z$Code == 2220) - 1) , ]
ldc <- ldc %>% filter(Code < 1000) %>% distinct()
ldc <- countrycode(ldc$Code, origin = "un", destination =  "iso3c")

# On intègre ces quatre catégories au tableau pays
pays <- pays %>% 
  mutate(UNOHRLLS = (if_else(iso3c %in% sids_unohrlls, "SIDS", "Non-SIDS", missing = NA_character_)))
pays <- pays %>% 
  mutate(UNCTAD = (if_else(iso3c %in% sids_unctad, "SIDS", "Non-SIDS", missing = NA_character_)))
pays <- pays %>% 
  mutate(LLDC = (if_else(iso3c %in% lldc, "LLDC", "Non-LLDC", missing = NA_character_)))
pays <- pays %>% 
  mutate(LDC = (if_else(iso3c %in% ldc, "LDC", "Non-LDC", missing = NA_character_)))

# On les transfère en facteurs
pays$UNOHRLLS <- factor(pays$UNOHRLLS, levels = c("SIDS", "Non-SIDS"), ordered = FALSE)
pays$UNCTAD <- factor(pays$UNCTAD, levels = c("SIDS", "Non-SIDS"), ordered = FALSE)
pays$LLDC <- factor(pays$LLDC, levels = c("LLDC", "Non-LLDC"), ordered = FALSE)
pays$LDC <- factor(pays$LDC, levels = c("LDC", "Non-LDC"), ordered = FALSE)


# ---- États fragiles, classification de la BM ----

url <- "https://databank.worldbank.org/data/download/site-content/CLASS.xls"
destfile <- "./tmp/bm_categories_economies.xls"
download.file(url = url, destfile = destfile)
z<- read_excel(destfile, sheet = 3)

fcas <- z %>% 
  filter(GroupCode == "FCS") %>% 
  select(CountryCode)
fcas <- fcas[["CountryCode"]]

# On intègre au tableau pays
pays <- pays %>% 
  mutate(FCAS = (if_else(iso3c %in% fcas, "FCAS", "Non-FCAS", missing = NA_character_)))

# En facteur
pays$FCAS <- factor(pays$FCAS, levels = c("FCAS", "Non-FCAS"), ordered = FALSE)

# ---- Nettoyage et cache ---- 
cache("pays")
rm(destfile)
rm(url)
rm(DAC_SIDS)
