# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 99aa-AGB-Calculation-Example-on-Subsample-data.R
# subtitle: An example script to calculate AGB using Chave et al. 2014 equation implemented using the BIOMASS package. 
# abstract: ...
# Project: AGB_Pune
# Date created: 2022-Mar-07 12:52:00 Monday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("02-Code/util_Global.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse", "BIOMASS")
install(Packages); rm(Packages)

data <- read_csv("01-Data/02-Processed/Processed_Curated_data_for_BIOMASS.csv")  

sp_raw <- data %>% 
  group_by(botanical_name, common_name, local_name, economic_i, phenology, flowering) %>% 
  summarise(n = n(), # Total data entries
            n.dbh = sum.adv(!is.na(dbh)), # Population with non NA dbh
            n.ward = n_distinct(ward), # Present in how many wards
            mean.dbh = mean(dbh, na.rm = TRUE), # Mean dbh values
            mean.height = mean(height_m, na.rm = TRUE), # Mean tree-height
            mean.canopy = mean(canopy_dia_m, na.rm = TRUE)) |>  # Mean canopy diameter
  ungroup()

sp.mod <- sp_raw |> 
  separate(botanical_name, into = c("genus", "species"), sep = " ", extra = "drop", remove = FALSE) |> 
  mutate(correctTaxo(genus = genus, species = species, verbose = FALSE)  %>% 
           rename(genus = genusCorrected,
                  species = speciesCorrected,
                  mod.name = nameModified)) %>% 
  mutate(getTaxonomy(genus = genus, findOrder = T) %>% # Getting family names
           rename(order.APG = order) %>% 
           select(-inputGenus))

sp <- sp.mod %>% 
  mutate(getWoodDensity(genus = genus,
                        species = species,
                        family = family,
                        stand = NULL, verbose = FALSE) %>%
           rename(wd.mean = meanWD,
                  wd.sd = sdWD) %>%
           select(-c(family, genus, species)))

# Listing the species synonyms  
sp.syn <- sp %>% 
  group_by(genus, species, family) %>% 
  summarise(n = n(), names = paste(botanical_name, collapse = "; ")) %>% 
  filter(n > 1)

sp.syn

df <- data |> 
  select(id, botanical_name, dbh, height_m) |>
  rename(height = height_m) %>% 
  left_join(x = ., 
            y = sp |>   
              select(botanical_name, genus, species, family, wd.mean, wd.sd),
            by = "botanical_name")

df$agb = computeAGB(D = df$dbh, WD = df$wd.mean, H = df$height)

resultMC <- AGBmonteCarlo(D = df$dbh, H = df$height, WD = df$wd.mean, errWD = df$wd.sd, Dpropag = "chave2004", errH = 0.05*df$height, n = 50)
colnames(resultMC$AGB_simu) <- paste0("simu.", 1:ncol(resultMC$AGB_simu))
df <- cbind(df, resultMC$AGB_simu)

agbBySpecies <- agbSummary(df = df, group_by = "botanical_name", col.dbh = "dbh", col.height = "height")

ggplot(agbBySpecies, aes(reorder(botanical_name, -agb.median, sum), agb.median)) +
  geom_col() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))