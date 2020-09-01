

# ---- Downloading the latest WB datasets -----
new_cache <- WDIcache()
saveRDS(new_cache, file = "./cache/new_cache.feather")

