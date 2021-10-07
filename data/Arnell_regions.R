library(data.table)
library(countrycode)
library(fst)

dd <- fread('data/Arnell_regions.csv')
dd <- dd[,.(country = unlist(strsplit(iso3s, ","))), by = "region"]
dd[, iso3 := countrycode(country, 'country.name', 'iso3c')]

#fix incompatibility for south sudan
dd[iso3 == "SSD", iso3 := "SDS"]

write_fst(dd, 'data/arnell_region_iso3.fst')
