library(jsonlite)
library(readr)
library(vroom)

khs_raw <- vroom("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.csv")

write_rds(khs_raw, "khs_raw.rds")
