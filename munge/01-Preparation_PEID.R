# # This file downloads the latest WDI indicators in a local, cached copy. It
# then creates a tibble of country codes and country names only for Small Island
# Developing States (SIDS), a UN concept that must be made to work with our
# World Bank's WDI nomenclature.


# ---- Downloading the latest WB datasets -----
new_cache <- WDIcache()
cache("new_cache")


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

# ---- Faire un tableau à partir des PEID du CAD et des pays de la BM ----

# On télécharge un indicateur bidon de la BM et on ne garde que les descripteurs
peid <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", start = 2019, end = 2019, extra = TRUE, cache = new_cache)
peid <- peid %>% select(country, 
                        iso2c,
                        iso3c,
                        region,
                        income,
                        lending)
peid <- peid %>% mutate(tmp = (iso3c %in% DAC_SIDS))

# On crée une petite table de correspondances
temp <- data_frame(tmp = c(TRUE, FALSE), SIDS = c("SIDS", "Others"))

# ... que l'on fusionne au tableau précédent
peid <- left_join(peid, temp) %>% 
  select(-tmp)
peid <- peid %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays

# ---- On fait des colonnes de facteurs ----
peid$income <- factor(peid$incom, ordered = FALSE)
peid$income <- ordered(peid$income, levels = c( "Low Income", "Lower middle income",
                                               "Upper middle income", 
                                               "High income"))
peid$lending <- factor(peid$lending, levels = c("IDA", "Blend", "IBRD"), ordered = FALSE)
peid$SIDS <- factor(peid$SIDS, levels = c("SIDS", "Others"), ordered = FALSE)
peid$region <- factor(peid$region)

#---- Cleaning up ----
cache("peid")
rm(temp)


# ---- ARCHIVE: Preparing UN country classification -----

# # This enables us to create a list of SIDS vs non-SIDS
# 
# url <- "https://unctadstat.unctad.org/EN/Classifications/MemoItems_DevelopmentStatus_Hierarchy.xls"
# destfile <- "./tmp/dev_status.xls"
# download.file(url = url, destfile = destfile)
# z<- read_excel(destfile, skip = 4)
# z$Code <- as.numeric(z$Code)
# 
# which(z$Code > 1000) # Codes above 1000 denote a regrouping of countries
# # We must capture only those between header 2050 and the end of the table.
# z %>% filter(Code > 1000)
# 
# # La liste de la CNUCED --  ce n'est pas celle qu'on utilise
# # sids <- z[(which(z$Code == 2231) + 1):(which(z$Code == 2250) - 1) , ]
# 
# # La liste de UNOHRLLS
# sids <- z[(which(z$Code == 2250) + 1):dim(z)[1], ]
# sids <- rename(sids, un = Code)
# sids <- rename(sids, un.name.en = Label)
# sids <- sids %>% filter(un < 1000)
# rm(z)
# 
# # Converting UN country names & codes to WB
# sids$name <- countrycode(sids$un, origin = "un", destination = "country.name.en")
# sids$iso3c <- countrycode(sids$un, origin = "un", destination = "iso3c")
# 
# 
# 
# 
# # Merging with SIDS
# states <- left_join(states, sids[, c("iso3c", "dac_sids")], by = "iso3c")
# 
# z <- which(is.na(states$dac_sids))
# states[z, "dac_sids"] <- FALSE
# rm(z)
# states <- states %>% rename(SIDS = dac_sids)
# states[states$SIDS == TRUE, "SIDS"] <- "SIDS"
# states[states$SIDS == FALSE, "SIDS"] <- "Others"
# 
# # Cleaning Up
# states <- states %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
# #states[states$lending == "Not classified", "lending"] <- NA
# 
# states$income <- factor(states$income, labels = c("Low Income", "Lower middle income", 
#                                                             "Upper middle income", "High income"), 
#                              ordered = FALSE)
# states$lending <- factor(states$lending, levels = c("IDA", "Blend", "IBRD"), ordered = FALSE)
# states$SIDS <- factor(states$SIDS, levels = c("SIDS", "Others"), ordered = FALSE)
# states$region <- factor(states$region)
# cache("states")
# rm(destfile)
# rm(url)
# 
# 

