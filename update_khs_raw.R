library(jsonlite)
library(readr)

khs_raw <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")

write_rds(khs_raw, "khs_raw.rds")