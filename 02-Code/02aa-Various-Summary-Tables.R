# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 02aa-Various-Summary-Tables.R
# subtitle: Preparing various summary tables by family, genus, species, ward etc.
# abstract: 
# Project: AGB_Pune
# Date created: 2020-Dec-17 13:27:35 Thursday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("02-Code/util_AGB_Pune.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse")
install(Packages); rm(Packages)
options(dplyr.summarise.inform = FALSE)

# 01a-Preparing the data ----------------------------------------------------------------------------------
load("03-Tables/00aa-01-pune.raw.processed.rdata")
load("03-Tables/01aa-01-Pune-AGB-MC-Simulation.Rdata")
load("03-Tables/00aa-02-Pune_species_processed.Rdata")

# Removing unnecessary columns from sp data.frame
sp <- sp %>%  
  select(-c("n", "n.dbh", "mean.dbh", "mean.height", "mean.canopy")) %>% 
  rename(rarity = is_rare) %>% 
  mutate(rarity = recode(rarity, true = "Rare", false = "Common"))

head(sp, 10)

# Merging the species information with the main data.frame
df <- left_join(df, sp)

# Merging the agb simulation values
df <- cbind(df, resultMC$AGB_simu)

resultMC$AGB_simu <- NULL
gc()

# Summary by Species ------------------------------------------------------------------------------------
agbBySpecies <- agbsummary(df = df, group_by = "botanical_name", col.dbh = "dbh", col.height = "height_m")

head(agbBySpecies, 10)

write_csv.adv(agbBySpecies, "02aa-01-Pune-AGB-Summary-by-Species")
save(agbBySpecies, file =  "03-Tables/02aa-01-Pune-AGB-Summary-by-Species.Rdata")
rm(agbBySpecies)

# Summary by Genus ---------------------------------------------------------------------------------------
spByGenus <- sp %>% 
  group_by(genus) %>% 
  summarise(n = n(),
            n.sp = n_distinct(botanical_name),
            n.family = n_distinct(family),
            n.order = n_distinct(order.APG))
head(spByGenus, 10)

agbByGenus <- agbsummary(df = df, group_by = "genus", col.dbh = "dbh", col.height = "height_m") %>% 
  left_join(x = ., y = spByGenus, by = "genus")
head(agbByGenus, 10)

write_csv.adv(agbByGenus, "02aa-02-Pune-AGB-Summary-by-Genus")
save(agbByGenus, spByGenus, file =  "03-Tables/02aa-02-Pune-AGB-Summary-by-Genus.Rdata")
rm(agbByGenus, spByGenus)

# Summary by Family ---------------------------------------------------------------------------------------
spByFamily <- sp %>% 
  group_by(family) %>% 
  summarise(n = n(),
            n.sp = n_distinct(botanical_name),
            n.genus = n_distinct(genus),
            n.order = n_distinct(order.APG))
head(spByFamily, 10)

agbByFamily <- agbsummary(df = df, group_by = "family", col.dbh = "dbh", col.height = "height_m") %>% 
  left_join(x = ., y = spByFamily, by = "family")
head(agbByFamily, 10)

write_csv.adv(agbByFamily, "02aa-03-Pune-AGB-Summary-by-Family")
save(agbByFamily, spByFamily, file =  "03-Tables/02aa-03-Pune-AGB-Summary-by-Family.Rdata")
rm(agbByFamily, spByFamily)

# Summary by Wards --------------------------------------------------------------------------------------
spByWards <- df %>% 
  group_by(ward) %>%
  summarise(n.sp = n_distinct(botanical_name),
            n.genus = n_distinct(genus),
            n.family = n_distinct(family),
            n.order = n_distinct(order.APG))
head(spByWards, 10)

agbByWards <- agbsummary(df = df, group_by = "ward", col.dbh = "dbh", col.height = "height_m") %>% 
  left_join(x = ., y = spByWards, by = "ward")
head(agbByWards, 10)

write_csv.adv(agbByWards, "02aa-04-Pune-AGB-Summary-by-Wards")
save(agbByWards, spByWards, file =  "03-Tables/02aa-04-Pune-AGB-Summary-by-Wards.Rdata")
rm(agbByWards)

# Summary by Economic Status -----------------------------------------------------------------------------
spByEcono <- sp %>% 
  group_by(economic_i) %>% 
  summarise(n = n(),
            n.sp = n_distinct(botanical_name),
            n.genus = n_distinct(genus),
            n.family = n_distinct(family),
            n.order = n_distinct(order.APG))
spByEcono

agbByEcono <- agbsummary(df = df, group_by = "economic_i", col.dbh = "dbh", col.height = "height_m") %>% 
  left_join(x = ., y = spByEcono, by = "economic_i")
agbByEcono

write_csv.adv(agbByEcono, "02aa-05-Pune-AGB-Summary-by-Economic-Status")
save(agbByEcono, spByEcono, file =  "03-Tables/02aa-05-Pune-AGB-Summary-by-Economic-Status.Rdata")
rm(agbByEcono, spByEcono)

# Summary by Rarity -----------------------------------------------------------------------------
spByRarity <- sp %>% 
  group_by(rarity) %>% 
  summarise(n = n(),
            n.sp = n_distinct(botanical_name),
            n.genus = n_distinct(genus),
            n.family = n_distinct(family),
            n.order = n_distinct(order.APG))
spByRarity

agbByRarity <- agbsummary(df = df, group_by = "rarity", col.dbh = "dbh", col.height = "height_m") %>% 
  left_join(x = ., y = spByRarity, by = "rarity")

write_csv.adv(agbByRarity, "02aa-06-Pune-AGB-Summary-by-Rarity")
save(agbByRarity, spByRarity, file =  "03-Tables/02aa-06-Pune-AGB-Summary-by-Rarity.Rdata")
rm(agbByRarity, spByRarity)
