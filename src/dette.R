# Dette
# ==========

# ---- Télécharger les indicateurs de la dette de la BM ----

td_indic <- read_csv("./tmp/Indic_metadata_imf.csv")
td_indic <- td_indic %>% 
  rename(code = Code, indic = `Indicator Name`, def = `Short definition`, topic = Topic) %>% 
  select(code, indic, def, topic )

dette_complet <- WDI(country = "all", 
             indicator = td_indic$code, 
             start = 2010, 
             end = year(now())-1, 
             cache = new_cache
)

# On croise avec les données du PIB à prix courants en dollars des ÉU

pib_us <- WDI(country = "all", 
              indicator = c("pib" = "NY.GDP.MKTP.CD",
                            "revenus_prop_pib" = "GC.REV.XGRT.GD.ZS"), 
              start=2010, 
              end = year(now())-1, 
              cache = new_cache)
pib_us$revenus <- pib_us$revenus_prop_pib * pib_us$pib / 100

dette_complet <- left_join(dette_complet, pib_us, by = c("iso2c", 'country', "year"))

# On croise avec le tableau des PEID
dette_complet <- left_join(dette_complet, states, by = c("country", "iso2c"))

# Nettoyage
dette_complet <- dette_complet %>% filter(region != "Aggregates") # Enlever les agrégats et ne garder que les pays
dette_complet$lending <- dette_complet$lending %>% 
  as.character(.) %>% 
  tidyr::replace_na("Non-Borrower") %>% 
  as.factor()

dette_complet <- dette_complet %>% rename(
  "service_total" = "DT.TDS.DEGG.CD",
  "service_off" = "DT.TDS.OFFT.GG.CD",
  "service_priv" = "DT.TDS.PRVT.GG.CD",
  "dette_val_actu" = "DT.DOD.PVLX.CD",
  "dette_off" = "DT.DOD.OFFT.GG.CD",
  "dette_priv" = "DT.DOD.PRVT.GG.CD",
  "exp_total" = "BX.GSR.TOTL.CD")


# Sous-ensemble comprenant la dette. On corrige le fait que beaucoup des données
# de la valeur actualisée de la dette sont à zéro, et on tolère les NA.
dette <- dette_complet %>% select(country,
                                  year,
                                  dette_val_actu,
                                  dette_off,
                                  dette_priv,
                                  exp_total,
                                  pib,
                                  income, 
                                  iso3c, 
                                  lending, 
                                  SIDS) %>% 
  drop_na(.)
dette$dette_val_actu <- na_if(dette$dette_val_actu, 0)

dette_restreint <- 

# Sous-ensemble comprenant les indicateurs du service de la dette
service <- dette_complet %>% select(country,
                                  year,
                                  service_total,
                                  service_off,
                                  service_priv,
                                  exp_total,
                                  pib,
                                  income, 
                                  iso3c, 
                                  lending, 
                                  SIDS) %>% 
  drop_na(.)

service$serv_exp <- service$service_total / service$exp_total
service$serv_pib <- service$service_total / service$pib

service_restreint <- service %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  slice_tail()





# --- Test avec les données du FMI (ne pas utiliser) ----
  
library(imfr)  
terme <- paste0(".*IFS.*", year(now()))
db <- imf_ids(return_raw = FALSE, times = 3)
db_code <- db[grep(terme, db$description), ] %>% 
  arrange(., database_id) %>% 
  slice_tail() %>% select(database_id)

db_codelist <-  imf_codelist(db_code, return_raw = FALSE)
db_indic <- db_codelist[grep("INDICATOR", db_codelist$codelist), "codelist"]

IFS_indic_df <- imf_codes(codelist = db_indic)
IFS_indic_df %>% select(description) %>% grep(".*evenue", .) 

imf_data(database_id = db_code,
         indicator = indic,
         country = "all",
         start = 2000,
         end = current_year(),
         freq = "A",
         return_raw = FALSE,
         print_url = FALSE,
         times = 3
         )




z <- WDI(country = "all", indicator = "DT.TDS.DEGG.CD", 
         start = 2000, end = year(now()), extra = FALSE, cache = new_cache)




dette_complet$dette_pib <- dette_complet$`2020`

dette  <-  dette_complet %>% drop_na(.) 
dette_restreint <- dette %>% select(Country, dette_pib, income, iso3c, lending, SIDS)

dette_restreint %>% filter(SIDS == "SIDS") %>% group_by(income) %>% arrange(dette_pib) %>% View(
  
)
