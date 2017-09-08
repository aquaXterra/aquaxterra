# Manipulation of spider database.
# QDR 08 Sep 2017

# Update this with most recent database.
spider_families <- read.csv('C:/Users/Q/google_drive/SROP2017/spider_database_jul13.csv', stringsAsFactors = FALSE)
### NOTE by Q on 08 Sep: currently, for most families only one species (the first species in each family) is filled in.
### An important task would be to clean this up.
### I did a temporary fix for Simone, but it will be important to note in which cases the trait applies to the entire
### family, and in which cases it only applies to 1 or more species/genera.

library(dplyr)
library(reshape2)

# Get rid of trailing spaces in spider genus
spider_families <- mutate(spider_families, Genus = gsub('\\ ', '', Genus), Family = gsub('\\ ', '', Family))
spider_families$Web.Type[spider_families$Web.Type=='Orb Webs'] <- 'Orb Web'

# Convert to long format (1 row per taxon-trait combination)
# Taxon may be species or some higher taxon.

trait_cols <- 10:18 # identify columns with trait data in them.

spider_long <- melt(spider_families, measure.vars = trait_cols, variable.name = "TraitName", value.name = "TraitValue")

