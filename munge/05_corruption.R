# Control of Corruption -- Munging
# =================================

# ---- Télécharger les données ----

# "CC.EST"  "Control of Corruption: Estimate"            
# "CC.PER.RNK"         "Control of Corruption: Percentile Rank"   
# "CC.PER.RNK.LOWER" "Control of Corruption: Percentile Rank, Lower Bound of 90% Confidence Interval"
# "CC.PER.RNK.UPPER" "Control of Corruption: Percentile Rank, Upper Bound of 90% Confidence Interval"
# "CC.STD.ERR" "Control of Corruption: Standard Error"

corruption_complet <- WDI(country = "all", indicator = c(
  control_corr_rk = "CC.PER.RNK",
  control_corr = "CC.EST", 
  gdp_pc = "NY.GDP.PCAP.CD", 
  gpd_pc_ppp = "NY.GDP.PCAP.PP.KD"), 
  start = 2010,
  end = year(now()) - 1,
  extra = FALSE,
  cache = new_cache)

# On croise avec le tableau des PEID
corruption_complet <- left_join(corruption_complet, peid, by = c("country", "iso2c"))

# Nettoyage
corruption_complet <- corruption_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
corruption_complet$lending <- corruption_complet$lending %>% 
  as.character(.) %>% 
  tidyr::replace_na("Non-Borrower") %>% 
  as.factor()  

cache("corruption_complet")


# ---- Paradis fiscaux ----

# Les pays représentés dans le fichier paradis_fiscaux_ue_fmi_fatf.csv sont ceux qui
# sont à la fois représentés dans la liste des 35 pays du CAD considérés comme
# PEID ET qui sont aparaissent dans la liste suivante: 

# - FMI et FATF: Zoromé, A.
# (2007). Concept of offshore financial centers: In search of an operational
# definition (No. 7-87). International Monetary Fund.
# https://www.imf.org/external/pubs/ft/wp/2007/wp0787.pdf, tableau 10, p 23

# - UE: https://ec.europa.eu/taxation_customs/tax-common-eu-list_en#heading_3 

paradis <- peid %>% filter(SIDS == "SIDS") %>% 
  select(country, iso3c, region, income, lending) %>% 
  right_join(., paradis_fiscaux_ue_fmi_fatf) %>% 
  filter(IMF | FATF | EU)

write_csv(paradis, path = "./tmp/paradis_fiscaux.csv")


# ---- Stats sur l'investissement direct à l'étranger du Canada ---

# Téléchargement des données sur l'IDE du Canada
url_IDE <- "https://www150.statcan.gc.ca/n1/tbl/csv/36100008-eng.zip"

temp <- tempfile()
download.file(url_IDE, temp)
IDE_complet <- read_csv(unz(temp, "36100008.csv"))
unlink(temp)
rm(temp)

IDE_complet$iso2c <- countrycode(IDE_complet$`Countries or regions`, origin = "country.name", destination = "iso2c")
# Ceci donne l'avertissement suivant: Some values were not matched
# unambiguously: Africa, All countries, Asia/Oceania, Czechoslovakia, Europe,
# German Democratic Republic (East), Netherlands Antilles, North America, South
# and Central America, Yugoslavia 

# Ça va: nous ignorons les agrégats et les pays
# disparus (DDR, Yougoslavie) ainsi que le territoires d'outre-mer (Antilles
# néerlandaises).

IDE_complet <- IDE_complet %>% rename(variable = `Canadian and foreign direct investment`,
                                      country = `Countries or regions`, 
                                      year = REF_DATE,
                                      fdi_millions = VALUE)

IDE_complet <- IDE_complet %>% select(year, iso2c, country, variable, fdi_millions)

# On croise avec le tableau des PEID
IDE_complet <- left_join(IDE_complet, peid, by = c("country", "iso2c"))

# Nettoyage
IDE_complet$lending <- IDE_complet$lending %>% 
  as.character(.) %>% 
  tidyr::replace_na("Non-Borrower") %>% 
  as.factor()  

IDE <- IDE_complet %>% drop_na()

IDE_restreint  <- IDE %>%
  filter(year == max(.$year)) %>% 
  filter(variable == "Canadian direct investment abroad - Total Book Value") %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  ungroup()

IDE_restreint %>% 
#  filter(SIDS == "SIDS") %>% 
  arrange(-fdi_millions) %>%
  summarize(fdi = sum(fdi_millions) /1000)

IDE_restreint %>% filter(SIDS == "SIDS") %>% arrange(-fdi_millions) %>%
  summarize(fdi = sum(fdi_millions) /1000)

