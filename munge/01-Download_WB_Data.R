# # This file downloads the latest WDI indicators in a local, cached copy. It
# then creates a tibble of country codes and country names only for Small Island
# Developing States (SIDS), a UN concept that must be made to work with our
# World Bank's WDI nomenclature.


# ---- Downloading the latest WB datasets -----
new_cache <- WDIcache()
cache("new_cache")

# ---- Preparing UN country classification -----
# This enables us to create a list of SIDS vs non-SIDS

url <- "https://unctadstat.unctad.org/EN/Classifications/MemoItems_DevelopmentStatus_Hierarchy.xls"
destfile <- "./tmp/dev_status.xls"
download.file(url = url, destfile = destfile)
z<- read_excel(destfile, skip = 4)
z$Code <- as.numeric(z$Code)

which(z$Code > 1000) # Codes above 1000 denote a regrouping of countries
# We must capture only those between header 2050 and the end of the table.
z %>% filter(Code > 1000)

# La liste de la CNUCED --  ce n'est pas celle qu'on utilise
# sids <- z[(which(z$Code == 2231) + 1):(which(z$Code == 2250) - 1) , ]

# La liste de UNOHRLLS
sids <- z[(which(z$Code == 2250) + 1):dim(z)[1], ]
sids <- rename(sids, un = Code)
sids <- rename(sids, un.name.en = Label)
sids <- sids %>% filter(un < 1000)
rm(z)

# Converting UN country names & codes to WB
sids$name <- countrycode(sids$un, origin = "un", destination = "country.name.en")
sids$iso3c <- countrycode(sids$un, origin = "un", destination = "iso3c")

# Cleaning up
sids <- sids[complete.cases(sids), ] # Removes what doesn't have a WB equivalent
cache("sids")
rm(destfile)
rm(url)
